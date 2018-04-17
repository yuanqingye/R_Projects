# source('~/Rfile/R_impala.R')
library(naivebayes)
library(mclust)
library(e1071)
library(randomForest)
library(readxl)
library(caret)

#Calculate the f1, the important criteria for classification model
#' Title
#'
#' @param real 
#' @param pred 
#'
#' @return
#' @export
#'
#' @examples
calf1 = function(real,pred){
  accuracy = sum(real==pred)/length(real)
  #precise定义为当你认为是真时，实际是真的比例
  precise = sum(real==1&pred==1)/sum(pred==1)
  #spec定义为实际是假时，你也认为是假的比例
  spec = sum(real==0&pred==0)/sum(real==0)
  #当实际是真时，你也认为是真的比例recall/sensitivity
  recall=sens = sum(real==1&pred==1)/sum(real==1)
  f1 = 2*precise*recall/(precise+recall)
  return(list(f1=f1,accuracy = accuracy,specificity = spec,precise = precise,recall = recall))
}

#Calculate the f1, the important criteria for classification model
#' Title
#'
#' @param real 
#' @param pred 
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
calAR = function(real,pred,type="accuracy"){
  accuracy = sum(real==pred)/length(real)
  #precise定义为当你认为是真时，实际是真的比例
  precise = sum(real==1&pred==1)/sum(pred==1)
  #spec定义为实际是假时，你也认为是假的比例
  spec = sum(real==0&pred==0)/sum(real==0)
  #当实际是真时，你也认为是真的比例recall/sensitivity
  recall=sens = sum(real==1&pred==1)/sum(real==1)
  #f1 the important judgement of the algorithm
  f1 = 2*precise*recall/(precise+recall)
  if(type == "accuracy"){
    return(accuracy)
  }
  if(type == "precise"){
    return(precise)
  }
  if(type == "specification"){
    return(spec)
    }
  if(type == "recall"){
    return(recall)
  }
  if(type == "f1"){
    return(f1)
  }
  stop("You may not define a correct type for calculate",call. = TRUE)
}

#Given the real and pred result, calculate the AUC
#' Title
#'
#' @param real 
#' @param pred 
#'
#' @return
#' @export
#'
#' @examples
calAUC = function(real,pred){
  library(AUC)
  test_roc = roc(pred,real)
  test_auc = auc(test_roc)
  return(test_auc)
}

#' Title
#'
#' @param model 
#' @param newset 
#' @param keycol 
#' @param calf 
#' @param caltype 
#'
#' @return
#' @export
#'
#' @examples
cal_model_accuracy = function(model,newset,keycol,calf,caltype)
{
  newset = newset[complete.cases(newset),]
  model_result = predict(object = model,newdata = newset)
  model_cal = calf(newset[,keycol],model_result,caltype)
  return(model_cal)
}

#' Title
#'
#' @param v 
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
makeSQLListFromVec = function(v,type = "string"){
  result = "("
  k = 1
  for(i in v){
      if(type == "string"){
        result = paste0(result,"'",i,"'")
      }
      else{
        result = paste0(result,i)
      }
    if(k!=length(v)){
      result = paste0(result,",")
    }
    k= k+1
  }
  result = paste0(result,")")
  return(result)
}

#So the time is the ending time~ all the activity before that time is recorded
#This function is for get the base umid related data set,with umid_sql to restrict the umid set
#the extracted field is fixed
#' Title
#'
#' @param umid_sql 
#' @param specDate 
#' @param mode 
#' @param mobile 
#'
#' @return
#' @export
#'
#' @examples
get_base_with_restrict = function(umid_sql,specDate = spec.date.str,mode = "base",mobile = TRUE){
  if(mode == "base"){
    system = " "
    if(mobile){
      system = " and platf_lv1='APP' "
    }
    sql = paste0("select base.*,umid_count_same_ip,",specDate," from 
                 (select u_mid,l_ip,l_city,d_model,max(cast(p_stay_time as int)) max_stay,min(cast(p_stay_time as int)) min_stay,avg(cast(p_stay_time as int)) avg_stay,count(*) pv 
                 from dm.dm_app_pageview_info p where p.u_mid in (",umid_sql,") and dt <= '",specDate,"' and path = 'z' and cast(p_stay_time as int) < 1000*3600*6 and cast(p_stay_time as int) > 0",system,
                 "group by u_mid,l_ip,l_city,d_model) base left join (select count(distinct u_mid) umid_count_same_ip,l_ip from dm.dm_app_pageview_info p where p.u_mid in 
                 (",umid_sql,") and dt <= '",specDate,"' and path = 'z'",system,"group by l_ip) iptb on base.l_ip = iptb.l_ip")
  }
  else if(mode == "umidmove"){
    #a umid may shows up in more than one city
    sql = paste0("select u_mid,count(distinct l_city) mobile_city_count,count(l_gps) mobile_gps_count,",specDate," from dm.dm_app_pageview_info p 
                 where p.u_mid in (",umid_sql,") and dt <= '",specDate,"' and path = 'z'",system,"group by u_mid")
  }
  base = read_data_impala_general(sql)
  return(base)
}

#' Title
#'
#' @param umid_sql_list 
#' @param datelist 
#' @param mode 
#' @param mobile 
#'
#' @return
#' @export
#'
#' @examples
get_base_with_restrict_union = function(umid_sql_list,datelist,mode = "base",mobile = TRUE){
  sql = vector(mode = "character",length = 0)
  system = " "
  if(mobile){
    system = " and platf_lv1='APP' "
  }
  for(i in 1:length(umid_sql_list)){
    if(mode == "base"){
        # sql[i] = paste0("select base.*,umid_count_same_ip,'",datelist[i],"' as dt from 
        #          (select u_mid,l_ip,l_city,d_model,max(cast(p_stay_time as int)) max_stay,min(cast(p_stay_time as int)) min_stay,avg(cast(p_stay_time as int)) avg_stay,count(*) pv 
        #          from dm.dm_app_pageview_info p where p.u_mid in (",umid_sql_list[i],") and dt <= '",datelist[i],"' and path = 'z' and cast(p_stay_time as int) < 1000*3600*6 and cast(p_stay_time as int) > 0",system,
        #          "group by u_mid,l_ip,l_city,d_model) base left join (select count(distinct u_mid) umid_count_same_ip,l_ip from dm.dm_app_pageview_info p where p.u_mid in 
        #          (",umid_sql_list[i],") and dt <= '",datelist[i],"' and path = 'z'",system,"group by l_ip) iptb on base.l_ip = iptb.l_ip")
        sql[i] = paste0("select base.*,umid_count_same_ip from 
                 (select u_mid,l_ip,l_city,d_model,max(cast(p_stay_time as int)) max_stay,min(cast(p_stay_time as int)) min_stay,avg(cast(p_stay_time as int)) avg_stay,count(*) pv,max(cast(dt as int)) dt 
                        from dm.dm_app_pageview_info p where p.u_mid in (",umid_sql_list[i],") and dt <= '",datelist[i],"' and path = 'z' and cast(p_stay_time as int) < 1000*3600*6 and cast(p_stay_time as int) > 0",system,
                        "group by u_mid,l_ip,l_city,d_model) base left join (select count(distinct u_mid) umid_count_same_ip,l_ip from dm.dm_app_pageview_info p where p.u_mid in 
                        (",umid_sql_list[i],") and dt <= '",datelist[i],"' and path = 'z'",system,"group by l_ip) iptb on base.l_ip = iptb.l_ip")
                         }
    else if(mode == "umidmove"){
    #a umid may shows up in more than one city
        sql[i] = paste0("select u_mid,count(distinct l_city) mobile_city_count,count(l_gps) mobile_gps_count from dm.dm_app_pageview_info p 
                 where p.u_mid in (",umid_sql_list[i],") and dt <= '",datelist[i],"' and path = 'z'",system,"group by u_mid")
               }
  }
  if(length(umid_sql_list)>1){
    sql = paste(sql,collapse =  " union ")
  # sql = paste0("select * from (",sql,")")
  }
  base = read_data_impala_general(sql)
  return(base)
  }

#With some CONSTANT SQL, We exchange the parameter to get large range of assisting data set
#' Title
#'
#' @param SQL 
#' @param specDate 
#' @param specCity 
#'
#' @return
#' @export
#'
#' @examples
run_flexible_sql = function(SQL,specDate = last.date.str,specCity = city_name){
  SQL = str_replace_all(SQL,'date_to_be_replaced',specDate)
  SQL = str_replace_all(SQL,'city_to_be_replaced',specCity)
  result = read_data_impala_general(SQL)
  return(result)
}

#The function give the final test and train set
#first using get_base_with_restrict,then replace by get_base_with_restrict_union 2018118
#complexity indicate if we need to include the neg case,which is used to produce training
#while the pure positive or negative case is used for test
#' Title
#'
#' @param umid_sql 
#' @param neg_umid_sql 
#' @param specDate 
#' @param negspecDate 
#' @param renew 
#' @param complexity 
#' @param sign 
#'
#' @return
#' @export
#'
#' @examples
get_advbase_set = function(umid_sql,neg_umid_sql,specDate,negspecDate = specDate,renew = FALSE,complexity = FALSE,sign = 1){
  if(!complexity){
    move = get_base_with_restrict_union(umid_sql,specDate,mode = "umidmove")
    base = get_base_with_restrict_union(umid_sql,specDate)
    move$sign = sign
    }
  else{
    positive_base = get_base_with_restrict_union(umid_sql,specDate)
    negative_base = get_base_with_restrict_union(neg_umid_sql,negspecDate)
    #for positive and negative case, if duplicates occurs, we remove duplicates
    positive_base = positive_base[!duplicated(positive_base[,c("u_mid","l_city","l_ip","d_model")]),]
    negative_base = negative_base[!duplicated(negative_base[,c("u_mid","l_city","l_ip","d_model")]),]
    base = rbind(positive_base,negative_base)
    base = data.table(base)
    base[,num:=.N,by = c("u_mid","l_city","l_ip","d_model")]
    base = base[num<2,]
    base = as.data.frame(base)
    positive_move = get_base_with_restrict_union(umid_sql,specDate,mode = "umidmove")
    negative_move = get_base_with_restrict_union(neg_umid_sql,negspecDate,mode = "umidmove")
    positive_move$sign = 1
    negative_move$sign = 0
    move = rbind(positive_move,negative_move)
    move = move[!duplicated(move$u_mid),]
  }
  #I didn't set the time because we always use
  if(!exists("model")|renew){
    assign("model",run_flexible_sql(MODEL_SQL),pos = .GlobalEnv)
  }
  if(!exists("city")|renew){
    assign("city",run_flexible_sql(CITY_SQL),pos = .GlobalEnv)
  }
  if(!exists("page_city_dt")|renew){
    assign("page_city_dt",run_flexible_sql(PAGE_CITY_DT_FIRST_SQL),pos = .GlobalEnv)
  }
  #base umid with city attributes on a given day
  #unionDate = union(specDate,negspecDate)
  #base_city = merge(base,city[city$dt %in% unionDate,],by.x = c("l_city","dt"),by.y = c("l_city","dt"),all.x = TRUE)
  base_city = merge(base,city,by.x = c("l_city","dt"),by.y = c("l_city","dt"),all.x = TRUE)
  #base umid with city and model attributes on a given day
  base_city_model = merge(base_city,model,by.x = c("d_model","dt"),by.y = c("d_model","dt"),all.x = TRUE)
  # base_city_model_pgperc = merge(base_city_model,page_city_dt[page_city_dt$dt %in% unionDate,],by.x = c("l_city","dt"),by.y = c("l_city","dt"),all.x = TRUE)
  # base_city_model_pgperc = data.table(base_city_model_pgperc)
  #duplicated umid should be take care of
  base_city_model = data.table(base_city_model)
  base_city_model_summary = base_city_model[,c(lapply(.SD[,sum_col,with=FALSE],sum,na.rm = TRUE),lapply(.SD[,avg_col,with=FALSE],mean,na.rm = TRUE),lapply(.SD[,min_col,with = FALSE],min,na.rm = TRUE),lapply(.SD[,max_col,with = FALSE],max,na.rm = TRUE)),by = "u_mid"]
  base_city_model_summary = as.data.frame(base_city_model_summary)
  base_city_model_move = merge(base_city_model_summary,move,by.x = "u_mid",by.y = "u_mid",all.x = TRUE,all.y = FALSE)
  return(base_city_model_move)
}

#' Title
#'
#' @param city 
#' @param specDate 
#' @param func_umid_sql 
#' @param factordf 
#' @param renew 
#'
#' @return
#' @export
#'
#' @examples
get_test_sample = function(city,specDate,func_umid_sql =1,factordf = 1,renew = FALSE){
  if(func_umid_sql == 1){
    func_umid_sql = paste0("select u_mid from dm.dm_app_pageview_info where l_city like '",city,"%' and dt = '",specDate,"' ")
  }
  base = get_advbase_set(func_umid_sql,specDate = specDate,renew = renew)
  if(any(class(factordf) == 'data.frame')){
    base = merge(base,factordf,by.x = "u_mid",by.y = "u_mid")
  }
  else{
    base$sign = factor(factordf,levels = c('0','1'))
  }
  base_set = base[,pickedColums]
  return(base_set)
}

#' Title
#'
#' @param city 
#' @param specDate 
#' @param func_umid_sql 
#' @param factordf 
#' @param test_model 
#'
#' @return
#' @export
#'
#' @examples
get_test_sample_result = function(city,specDate,func_umid_sql =1,factordf = 1,test_model){
  base_set = get_test_sample(city,specDate,func_umid_sql,factordf)
  result_nb = predict(test_model,base_set[complete.cases(base_set),!(names(base_set) %in% "sign")])
  result = cbind(base_set[complete.cases(base_set),!(names(base_set) %in% "sign")],result_nb)
  nb_list = calf1(base_set[complete.cases(base_set),"sign"],result_nb)
  return(list(nb_list,result))
}

#' Title
#'
#' @param name_para_sets 
#' @param factordf 
#' @param renew 
#'
#' @return
#' @export
#'
#' @examples
collect_test_set = function(name_para_sets,factordf = 1,renew = TRUE){
  name_para_sets[["test_set_name"]] = paste0(name_para_sets[["test_set_name"]],str_sub(name_para_sets[["date"]],5,8))
  for(k in 1:nrow(name_para_sets)){
    city = name_para_sets[[k,"city"]]
    specDate = name_para_sets[[k,"date"]]
    test_set_name = name_para_sets[[k,"test_set_name"]]
    if(k > 1){
      renew = FALSE
    }
    temp = get_test_sample(city = city,specDate = specDate,factordf = factordf,renew = renew)
    assign(test_set_name,temp,envir = .GlobalEnv)
      }
}

# go_around_test_result = function(test_set){
#   for(k in 1:nrow(test_set)){
#     city = test_set[k,"city"]
#     specDate = test_set[k,"date"]
#   }
# }

#' Title
#'
#' @param test_set_names 
#' @param model_names 
#'
#' @return
#' @export
#'
#' @examples
test_around_set = function(test_set_names, model_names) {
  result_cal = data.frame()
  for (test_set_name in test_set_names) {
    for (model_name in model_names) {
      test_model = get(model_name)
      test_set = get(test_set_name)
      result_cal[test_set_name,model_name] = cal_model_accuracy(
        model = test_model,
        newset = test_set,
        keycol = "sign",
        calf = calAR,
        caltype = "accuracy"
      )
    }
  }
  return(result_cal)
}

#' Title
#'
#' @param original_set 
#'
#' @return train_base
#' @export
#'
#' @examples
preprocess_train_data = function(original_set){
  train_set = original_set[,pickedColums]
  train_base = train_set[complete.cases(train_set),]
  train_base = as.data.frame(train_base)
  # x = train_base[,!(names(train_base) %in% "sign")]
  train_base$sign = as.factor(train_base$sign)
  # y = train_base$sign
  return(train_base)
}

#' Title
#'
#' @param oringin_set 
#' @param keycol 
#' @param posdiv 
#' @param negdiv 
#' @param randnum 
#'
#' @return
#' @export
#'
#' @examples
get_var_sample <- function(oringin_set,keycol = "sign",posdiv = 0.2,negdiv = 1,randnum = 999) {
  set.seed(randnum)
  posIndex = which(oringin_set[[keycol]] == 1)
  negIndex = which(oringin_set[[keycol]] == 0)
  samposIndex = sample(x = posIndex,size = length(posIndex)*posdiv,replace = FALSE)
  samnegIndex = sample(x = negIndex,size = length(negIndex)*negdiv,replace = FALSE)
  result_set = oringin_set[c(samnegIndex,samposIndex),]
  return(result_set)
}

#' Title
#'
#' @param n 
#' @param train_set 
#'
#' @return
#' @export
#'
#' @examples
manually_repeat_nb_ml = function(n,train_set){
  for(k in 1:n){
    temp_model = naive_bayes(sign~.,train_set[complete.cases(train_set),])
    assign(paste0("model",k),temp_model,pos = .GlobalEnv)
  }
  result = test_around_set(test_set_names = test_set_names,model_names = paste0("model",1:n))
}

#' Title
#'
#' @param dateset 
#' @param umid_sql 
#' @param renew 
#'
#' @return
#' @export
#'
#' @examples
get_complex_train_set = function(dateset,umid_sql,renew = FALSE){
  neg_umid_sql = c(order_umid_sql,last_long_umid_sql)
  general_pn = get_advbase_set(umid_sql,neg_umid_sql,dateset,renew = renew,complexity = TRUE)
  train_pn = preprocess_train_data(general_pn)
  return(train_pn)
}

#' Title
#'
#' @param training_model 
#' @param specDate 
#' @param renew 
#'
#' @return
#' @export
#'
#' @examples
pred_day_result <- function(training_model,specDate = last.date.str,renew = TRUE) {
  #get everyday's feature set
  umid_sql = "select distinct u_mid from dm.dm_app_pageview_info where  dt = 'date_to_be_replaced' and path = 'z'"
  umid_sql = str_replace_all(umid_sql,'date_to_be_replaced',specDate)
  general_today = get_advbase_set(umid_sql,specDate = specDate,renew = renew)
  general_today = general_today[complete.cases(general_today),]
  train_today = preprocess_train_data(general_today)
  temp = predict(training_model,newdata = train_today,type = 'prob')
  sign = ifelse(temp[[1]]>temp[[2]],0,1)
  names(temp) = c("normal_prob","abnorm_prob")
  today_result = data.frame(u_mid = general_today[,"u_mid"],sign,temp,dt = specDate)
  return(today_result[complete.cases(today_result),])
}