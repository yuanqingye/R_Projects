library(tabulizer)
library(tesseract)
library(readxl)
library(stringr)
library(data.table)
synt = c('′','、','`')
malls = extract_tables("C://Users/qingye.yuan/Downloads/mall.pdf",encoding = "UTF-8")
#租金大多是16年的
#杭州古镇无表示没有
data_process = function(url = "C://Users/qingye.yuan/Downloads/mall.pdf"){
  out = list()
  result = list()
  for(i in 1:19){
    out[[i]] = extract_tables(url,encoding = "UTF-8",pages = i)
  }
  for(j in 9:15){
    temp_out = out[[j]][[1]]
    temp_out1 = temp_out[1,-1]
    temp_out2 = temp_out[2,-1]
    temp_out3 = temp_out[c(-1,-2),c(-ncol(temp_out))]
    matrix_out = rbind(temp_out1,temp_out2,temp_out3)
    matrix_out = t(matrix_out)
    colnames(matrix_out) = matrix_out[1,]
    matrix_out = matrix_out[-1,]
    result[[j-8]] = matrix_out
  }
  fresult = do.call(rbind,result)
  fresult = fresult[c(-20,-24,-28),]
  mall = c(rep("苏州园区",4),rep("杭州古墩",4),rep("南京卡子门",4),rep("重庆江北",4),rep("天津河西",3),rep("天津红桥",3),rep("深圳香蜜湖",3))
  fresult = cbind(fresult,"mall" = mall)
  changsha = t(out[[16]][[1]])
  beijing_xisi_origin = out[[8]][[1]]
  beijing_xisi = rbind(beijing_xisi_origin[,c(1,2)],beijing_xisi_origin[,c(3,4)])
  beijing_xisi_new = beijing_xisi
  beijing_xisi_new[!grepl('[^@]+',beijing_xisi_new)] = NA
  line = beijing_xisi_new[1,1]
  beijing_xisi_new = beijing_xisi_new[complete.cases(beijing_xisi_new),]
  words = unlist(strsplit(line,'于'))
  beijing_xisi_new = rbind(beijing_xisi_new,c('name',words[1]))
  beijing_xisi_new = rbind(beijing_xisi_new,c('年份',substr(words[2],1,9)))
  beijing_north5 = out[[5]][[1]]
  beijing_north5[2:5,c(1,2,3,4)] = beijing_north5[2:5,c(2,3,4,1)]
  beijing_north5 = rbind(beijing_north5[,c(1,2)],beijing_north5[,c(3,4)])
  beijing_north5[!grepl('[^@]+',beijing_north5)] = NA
  beijing_north5 = beijing_north5[complete.cases(beijing_north5),]
  rm(beijing_xisi)
  rm(beijing_xisi_origin)
  shanghai1 = extract_tables(url,encoding = "UTF-8",pages = 2,area = list(c(0,0,125,350)))
  shanghai2 = extract_tables(url,encoding = "UTF-8",pages = 2,area = list(c(125,0,250,350)))
  shanghai3 = extract_tables(url,encoding = "UTF-8",pages = 2,area = list(c(225,0,320,350)))
  shanghai4 = extract_areas(url,encoding = "UTF-8",pages = 2)
  shanghai5 = extract_tables(url,encoding = "UTF-8",pages = 2,area = list(c(0,370,180,700)))
  shanghai6 = extract_tables(url,encoding = "UTF-8",pages = 2,area = list(c(170,370,270,700)))  
  shanghai7 = extract_tables(url,encoding = "UTF-8",pages = 2,area = list(c(270,370,450,700)))
  }
  chinese = tesseract("chi_sim")
  beijing_north4 = ocr("C://Users/qingye.yuan/Desktop/工作需要/租金定价/北京北四.png",engine = chinese)
  beijing_north4 = gsub('′','',beijing_north4)
  beijing_north4 = gsub('、','',beijing_north4)
  beijing_north4 = gsub('`','',beijing_north4)
  beijing_north4_parts = unlist(strsplit(beijing_north4,split = '\\n\\n'))
  beijing_north4_parts = gsub('竟争椿屠  ','',beijing_north4_parts)
  beijing_north4_parts = gsub('竟争椿屠 ','',beijing_north4_parts)
  cat(beijing_north4_parts[1],file = 'test_table_1')
  cat(beijing_north4_parts[2],file = 'test_table_2')
  beijing_north4_1 = read.table(file = "test_table_1",header = T,stringsAsFactors = F)
  beijing_north4_2 = read.table(file = "test_table_2",header = T,stringsAsFactors = F)
  
  beijing_east4 = read_xlsx("C://Users/qingye.yuan/Desktop/工作需要/租金定价/数据挖掘.xlsx",sheet = "Sheet2")
  fresult_copy = fresult
  fresult[,"area"] = gsub('([\\s]*m2|方)','',fresult[,"area"],perl = TRUE)
  temp = result[,"area"]
  str_extract(temp,'.+(?=万)')
  rep = as.numeric(str_extract(temp,'.+(?=万)'))*10000
  result[,"area"] = str_replace(temp,'.+万',as.character(rep))
  temp1 = fresult[,"rent"]
  rep1 = str_extract(temp1,'.+(?=万)')
  rep1 = as.character(10000*as.numeric(rep1))
  rep1 = str_replace(temp1,".+万",rep1)
  rep2 = str_replace(rep1,'亿','')
  rep2 = grep('-',rep2,value = T)
  rep2 = str_split(rep2,'-')
  rep2 = lapply(rep2,as.numeric)
  rep2 = lapply(rep2,mean)
  rep2 = as.character(unlist(rep2)*100000000)
  rep2 = str_replace(rep1,'.+亿',rep2)
  fresult[,'rent'] = rep2
  temp = fresult[,"2016sale"]
  rep = ifelse(grepl('亿',temp),as.numeric(str_extract(temp,'.+(?=亿)'))*100000000,
               ifelse(grepl('万',temp),as.numeric(str_extract(temp,'.+(?=万)'))*10000,temp))
  fresult[,"2016sale"] = rep
  temp = fresult[,"rent_rate"]
  rep = str_replace(temp,'%','')
  rep = as.numeric(rep)/100
  fresult[,"rent_rate"] = rep
  assign("temp",fresult[,'gain_loss_rate'])
  temp = gsub('[\u4e00-\u9fa5]+','',temp)
  temp = gsub('\\r','',temp)
  temp = gsub("^:","",temp)
  temp = gsub("%","",temp)
  temp = gsub("/","",temp)
  temp = gsub(";",":",temp)
  fresult[,'gain_loss_rate'] = temp
  fresult[5,c('willing_ranking','ranking')] = 1
  fresult[7,c('willing_ranking','ranking')] = 2
  fresult[c(20,21,22),c('willing_ranking','ranking')] = c(1,2,3,1,3,2)
  temp = fresult[,c("furniture_perc","constr_perc")]
  temp = gsub('%','',temp)
  temp = gsub('无','',temp)
  temp = gsub('(-|无)',NA,temp)
  temp = gsub('^$',0,temp)
  rep = temp
  fresult[,c("furniture_perc","constr_perc")] = rep
  fresult[17:19,c('willing_ranking','ranking')] = c(1.5,1.5,3,1.5,1.5,3)
  fresult = data.table(fresult)
  fresult[,city:=substring(mall_circle,1,2)]
  fresult$ifredstar = ifelse(str_detect(fresult$mall_name,"(红星|商场)"),1,0)
  colnames(changsha) = changsha[1,]
  changsha = changsha[-1,]
  changsha_origin = changsha
  changsha = data.table(changsha)
  colnames(changsha) = c("mall_name","nearby_business_center","area","age","type",
                         "win_cate_furni","win_cate_cons","win_cate_soft","pop_brand",
                         "avg_rent","contract_type","contract_benifit","avg_rent_after_benifit",
                         "common_factor","floors")
  changsha[,city:="长沙"]
  changsha_origin = changsha
  changsha[,winning_category:=paste(win_cate_furni,win_cate_cons,win_cate_soft,sep =',')]
  changsha = changsha[,winning_category:=gsub('(无,)+','',winning_category)]
  changsha = changsha[,winning_category:=gsub('(,无)+','',winning_category)]
  changsha$ifredstar = ifelse(str_detect(changsha$mall_name,"红星"),1,0)
  beijing_north4_all = t(beijing_north4_all)
  beijing_north4_all = data.table(beijing_north4_all)
  beijing_north4_all$mall_name = c("","红星门店","北四坏居然之蒙","大忡寺簧秉丽蒙")
  colnames(beijing_north4_all)[-ncol(beijing_north4_all)] = unlist(beijing_north4_all[1,-12])
  setkey(beijing_north4_all,'mall_name')
  beijing_north4_all = beijing_north4_all[!""]
  beijing_north4_all[,`2016sale`:=as.numeric(`2016sale`)*100000000]
  setcolorder(beijing_north4_all,c(colnames(beijing_north4_all)[ncol(beijing_north4_all)],colnames(beijing_north4_all)[-ncol(beijing_north4_all)]))
  beijing_north4_all$city = "北京"
  beijing_north4_all$ifredstar = c(0,0,1)
  beijing_north5_origin = beijing_north5
  beijing_north5 = gsub('%','',beijing_north5)
  beijing_north5 = t(beijing_north5)
  colnames(beijing_north5) = unlist(beijing_north5[1,])
  beijing_north5 = beijing_north5[-1,]
  colnames(beijing_north5) = c("area","range","targeting","own_status","background","avg_rent",
                               "furniture_perc","constr_perc","pop_brand","status_analysis",
                               "gain_loss_rate","furniture_perc2","constr_perc2")
  beijing_north5$winning_category = "卫浴,整体定制"
  beijing_north5$city = "北京"
  beijing_north5[,c("range","own_status","background","pop_brand","status_analysis",
                    "furniture_perc2","constr_perc2"):=NULL]
  beijing_north5[,':='(furniture_perc = as.numeric(furniture_perc),constr_perc = as.numeric(constr_perc))]
  beijing_north5[,':='(furniture_perc = furniture_perc/(furniture_perc+constr_perc)*100,constr_perc = constr_perc/(furniture_perc+constr_perc)*100)]
  beijing_north5[,mall_name := "集美家居北苑店",]
  setcolorder(beijing_north5,c(colnames(beijing_north5)[ncol(beijing_north5)],colnames(beijing_north5)[-ncol(beijing_north5)]))
  beijing_north5$ifredstar = 0
  beijing_east4_origin = beijing_east4
  beijing_east4 = t(beijing_east4)
  # library(png)
  # img <- readPNG("~/Rimage/beijing_east5.png")
  # library(grid)
  # grid.raster(img)
  colname = beijing_east4[1,]
  colnames(beijing_east4) = colname
  beijing_east4 = beijing_east4[-1,]
  mall_name = rownames(beijing_east4)
  beijing_east4 = data.table(beijing_east4)
  beijing_east4 = lapply(beijing_east4,as.numeric)
  beijing_east4 = as.data.table(beijing_east4)
  colnames(beijing_east4) = colname
  beijing_east4_copy = beijing_east4
  beijing_east4 = cbind(beijing_east4,mall_name)
  beijing_east4$targetting = c("","中低档","高档")
  beijing_east4$gain_loss_rate = c("","49:29:22","46:27:27")
  beijing_east4$winning_category = c("","沙发,板式","建材,沙发,板式")
  beijing_east4$furniture_perc = c(NA,623,107)
  beijing_east4$constr_perc = c(NA,397,273)
  setcolorder(beijing_east4,c("mall_name",colnames(beijing_east4)[!colnames(beijing_east4) %in% "mall_name"]))
  beijing_east4$city = "北京"
  colnames(beijing_east4)[1:4] = c("mall_name","avg_rent","rent_rate","area")
  beijing_east4 = beijing_east4[,c(-5,-6)]
  beijing_east4$ifredstar = c(1,0,0)
  beijing_xisi_origin = beijing_xisi_new
  beijing_xisi_new[1,] = c("可租面积",100000)
  beijing_xisi_new = beijing_xisi_new[c(1,3,6,9,11,12,13,14,15),]
  beijing_xisi_new[,1] = c("area","avg_rent","winning_category","targeting","gain_loss_rate","furniture_perc","constr_perc","mall_name","age")
  beijing_xisi_new[,2] = c(100000,310,"卫浴,软体","中高端","32:24:44",160,280,"居然之家丽泽店",7)
  beijing_xisi_new = t(beijing_xisi_new)
  colnames(beijing_xisi_new) = beijing_xisi_new[1,]
  beijing_xisi_new = data.table(beijing_xisi_new)
  beijing_xisi_new = beijing_xisi_new[-1,]
  beijing_xisi_new$city = "北京"
  beijing_xisi_new$ifredstar = 0
  shanghai_tables = paste0("shanghai",1:7)
  for(name in shanghai_tables){assign(name,"[["(eval(as.name(name)),1))} #good!
  shanghai1[6,] = c("定位","高端/中高端","中端","中端/中高端","中端/中高端")
  colname = c("mall_name","area","avg_rent","rent_rate","targeting","winning_category","ranking")  
  shanghai1 = shanghai1[,-1]
  for(name in shanghai_tables){assign(name,t("["(eval(as.name(name)),,-1)))}
  for(name in shanghai_tables){assign(name,data.table(eval(as.name(name))))}
  shanghai = rbindlist(lapply(sapply(shanghai_tables,as.name),eval))
  colnames(shanghai) = colname
  shanghai$ifredstar = ifelse(str_detect(shanghai$mall_name,"(欧丽洛雅|商场)"),1,0)
  library(plyr)
  total = rbind.fill(shanghai,beijing_east4,beijing_north4_all,beijing_north5,beijing_xisi_new,fresult,changsha)
  total_original = total
  total = data.table(total)
  total[1:24,'city'] = "上海"
  total[nearby_business_center=='无',nearby_business_center:=NA]
  total[,mall_circle := ifelse(!is.na(mall_circle),mall_circle,nearby_business_center)]
  total_copy = total
  colname = colnames(total)[19:29]
  total[,eval(colname):=NULL]
  total[26:27,'targeting'] = c("中低端","高端")
  total[,targetting:=NULL]
  total[,rent_rate:=str_replace(rent_rate,"%","")]
  total[,furniture_perc:=str_replace(furniture_perc,"%","")]
  total[,constr_perc:=str_replace(constr_perc,"%","")]
  actual_avg_rent = total_original[!is.na(total_original$avg_rent_after_benifit),"avg_rent_after_benifit"]
  total[,age:=str_replace(age,'(\\d+)(?=年).+',"\\1")]
  total[,age:=str_replace(age,'\\d+个月',as.character(2/12))]
  total[,age:=as.numeric(age)+1]
  total$year = 2016
  setnames(total,"2016sale","sale")
  total[,area:=str_replace(area,'(\\d+)(?=万).+','\\1')]
  total[,c("area","avg_rent","rent_rate","furniture_perc","constr_perc","rent","sale","age"):=.(as.numeric(area),as.numeric(avg_rent),as.numeric(rent_rate),as.numeric(furniture_perc),as.numeric(constr_perc),as.numeric(rent),as.numeric(sale),as.numeric(age))]
  total[as.vector(city)=='长沙',area:=area*10000]
  total[as.character(city)=='长沙','rent'] =  as.numeric(actual_avg_rent) * total[as.character(city)=='长沙',area]
  total[,rent:=ifelse(!is.na(rent),rent,avg_rent*area+0.06)]
  total[,rent_rate:=ifelse(rent_rate>1,rent_rate/100,rent_rate)]
  changsha_rent_rate = c(0.9354,0.9327,NA,0.93,0.9590,0.9767,NA,NA,0.85)  
  total[as.vector(city)=="长沙","rent_rate"] = changsha_rent_rate
  total[c(1,3,4,28,29,30,37:40),"targeting"] = c("中高端","中端","中端","高端","高端","高端",rep("中高端",4))
  total[as.vector(city)=="长沙","gain_loss_rate"] = c("28:26.7:45.3","20.8:25:54.2",NA,"30.4:34.4:35.2","46.82:31.84:21.35","28.57:31.05:40.47",NA,NA,"25.97:38.96:32.07")
  total[44,"gain_loss_rate"]= "29.92:44.5:25.58"
  total[as.vector(city) == "长沙","furniture_perc"] = c(61,64,NA,46,42,43,56,NA,38)
  total[as.vector(city) == "长沙","constr_perc"] = 100-c(61,64,NA,46,42,43,56,NA,38)
  total[rent>area*avg_rent*3,c("rent","sale"):=.(rent/12,sale/12)]
  total[49:51,c("ranking","willing_ranking"):=.(c(1.5,1.5,3),c(1.5,1.5,3))]
  total[49:51,"gain_loss_rate"] = c("30:40:30","30:40:30",NA)
  total[c(52,53),"targeting"] = "中高端"
  total_backup = total
  
  #以下开始进行缺失值处理
  total[] <- lapply(total, function(x) if(is.character(x)) as.factor(x) else x)  
  total$gain_loss_rate = as.vector(total$gain_loss_rate)
  total$mall_name = as.vector(total$mall_name)
  total$targeting = as.vector(total$targeting)
  total$targeting = as.factor(total$targeting)
  # #考虑基础决策树插值 误差明显大于 随机森林 不使用
  # library(party)
  # targeting.model = ctree(targeting~.,data=total[!is.na(targeting),c(-1,-9)])
  # targeting.pred = predict(targeting.model,total[is.na(targeting),])
  # #check correct rate
  # targeting.train.pred = predict(targeting.model,total[!is.na(targeting),])
  # actual = total[!is.na(targeting),targeting]
  # mean(targeting.train.pred==actual)
  
  # md.pattern(total_backup_df)
  #考虑随机森林插值
  library(party)
  total$targeting = as.factor(total$targeting)
  targeting.model.rf = cforest(targeting~.,data=total[!is.na(targeting),c(2:5,8,10,11,13,14)])
  targeting.pred.rf = predict(targeting.model.rf,total[is.na(targeting),],OOB = TRUE)
  targeting.train.pred.rf = predict(targeting.model.rf,total[!is.na(targeting),],OOB = TRUE)
  actual = total[!is.na(targeting),targeting]
  mean(targeting.train.pred.rf==actual)
  #插进！
  total[is.na(targeting),"targeting"] = targeting.pred.rf
  
  regionnum = 0
  total$regionnum = 0
  for(i in 1:nrow(total)){
    if(total[i,]$ifredstar==1){regionnum = regionnum +1}
    total[i,]$regionnum = regionnum
  }
  total_df[28:30,"regionnum"] = 22
  total_df[32,"regionnum"] = 23
  total_df[55:57,"regionnum"] = 24
  total_df[58:66,"regionnum"] = 25
  total_df[c(29,28),c("furniture_perc","constr_perc")] = c(200,260,350,200)
  total_df[25,c("furniture_perc","constr_perc")] = c(189,239)
  total_df[51,"gain_loss_rate"] = "30:40:30"
  total = data.table(total_df)
  total[city=="重庆",c("furniture_perc","constr_perc") :=NA]
  total_df = data.frame(total)
  total_df$rent = total$rent
  total$regionnum = total_df$regionnum
  total_backup$regionnum = total$regionnum
  total[,willing_ranking:=NULL]
  total_df = data.frame(total)
  #使用随机森林完备版 后续尝试
  library(mice)
  total_df = data.frame(total)
  miceMod <- mice(total_df[, c(2,5,9:19)], method="rf")  # perform mice imputation, based on random forests.
  miceOutput2 <- complete(miceMod)  # generate the completed data.

  #使用knn方法 对rent_rate进行插值
  library(DMwR)
  knnOutput <- knnImputation(total_df[, c("area","avg_rent","rent_rate","targeting","ifredstar",
                                          "city","rent","sale")])  # perform knn imputation.
  index = which(is.na(total_df$rent_rate))
  knnOutput$rent_rate[index]
  #插入!
  total_df$rent_rate = knnOutput$rent_rate
  
  total_df_dt = data.table(total_df)
  setcolorder(total_df_dt,c(1,5,6,9,12,15,17,18,2:4,7,8,10,11,13,14,16))
  total_df = data.frame(total_df_dt)
  
  set.seed(7)
  library(mlbench)
  library(caret)
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  model <- train(rent~., data=total_df, method="rf", preProcess="scale", trControl=control,
                 importance = T)
  importance <- varImp(model, scale=FALSE)
  print(importance)
  plot(importance)
  
  set.seed(7)
  library(mlbench)
  library(caret)
  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  results <- rfe(total_df[,c(2:15,17,18)], total_df[,16], sizes=c(1:16), rfeControl=control)
  print(results)
  predictors(results)
  plot(results, type=c("g", "o"))
  
  library(e1071)
  svm.model = svm(rent~.-mall_name-year-gain_loss_rate,data = total_df,cost = 10,gamma = 0.0078125,cross = 5)
  
  library(baidumap)
  library(rvest)
  library(data.table)
  options(baidumap.key = '4SY3dY8GrhfU5ChOeqFMvhcaf9DWo7dc')
  
  shops = total[,.(city=city,mall_name = mall_name)]
  shops[as.vector(mall_name)=="北四坏居然之蒙",mall_name := "北四环居然之家"]
  shops[as.vector(mall_name)=="大忡寺簧秉丽蒙",mall_name:="大钟寺蓝景丽家"]
  shops[as.vector(mall_name)=="城外城",mall_name:="城外诚"]
  total[as.vector(mall_name)=="北四坏居然之蒙",mall_name := "北四环居然之家"]
  total[as.vector(mall_name)=="大忡寺簧秉丽蒙",mall_name:="大钟寺蓝景丽家"]
  total[as.vector(mall_name)=="城外城",mall_name:="城外诚"]
  
  
  shops = shops[,location := paste0(city,mall_name)]
  shops[,location:=str_replace_all(location,"\\s","")]
  shops = shops[!duplicated(shops$location),]
  shops = shops[c(-8,-9),]
  shops_df = data.frame(shops_df)
  # shops[,c("lon","lat")] = getCord(shops$location)[,c("lon","lat")] #百度地图API
  city = result_dt3$city
  result_dt4 = result_dt3
  result_dt4$city = NULL
  result_dt4$city = city
  
  library(ggmap)
  shops_cord_ch = lapply(shops$location,geocode) #the correct coordinate from google
  shops_cord_ch_df = rbindlist(shops_cord_ch)
  shops_df$lon = shops_cord_ch_df$lon
  shops_df$lat = shops_cord_ch_df$lat
  shops_df$coordinates = NULL
  shops_df[11,c("lon","lat")] = c(120.2027,30.2601)
  library(readxl)
  ?read_xlsx
  shop_parts = read_xlsx('~/data/map_manual_work.xlsx',sheet = "Sheet3")
  shop_parts = shop_parts[!duplicated(shop_parts$name),]
  shop_parts = shop_parts[c(-8,-9),]
  shop_parts$name[c(14,15)] = c("上海同福易佳丽建材馆","上海同福易佳丽灯具馆")
  shops_df = merge(shops_df,shop_parts,by.x = "location",by.y = "name",all.x = TRUE)
  shops_df[shops_df$location=="上海同福易佳丽金山",c("lon","lat")] = c(121.3272705619,30.7329496385)
  shops_cluster = kmeans(shops_df[,c("lon","lat")],centers = 20,iter.max = 100)
  shops_df = cbind(shops_df,shops_cluster)
  shanghai_cluster = kmeans(shops_df[shops_df$city=="上海",c("lon","lat")],iter.max = 50,center = 5)
  shanghai_cluster_df = data.frame("location" = shops_df[shops_df$city == "上海",]$location,"region" = shops_df[shops_df$city == "上海",]$region,"cluster" = shanghai_cluster$cluster)
  View(shanghai_cluster_df[order(shanghai_cluster_df$region),])
  shanghai_cluster_df = merge(shanghai_cluster_df,shops_df[,c("location","lon","lat")],by = "location",all.x = TRUE)
  library(ggmap)
  library(magrittr)
  shanghai_map <- get_map("shanghai,China", zoom=10)
  p = ggmap(shanghai_map)
  p + geom_point(data = shanghai_cluster_df[shanghai_cluster_df$cluster==1,c("lon","lat")],aes(x=lon, y=lat), color="red", size=3, alpha=0.5)+geom_point(data = shanghai_cluster_df[shanghai_cluster_df$cluster==2,c("lon","lat")],aes(x=lon, y=lat), color="yellow", size=3, alpha=0.5)+geom_point(data = shanghai_cluster_df[shanghai_cluster_df$cluster==3,c("lon","lat")],aes(x=lon, y=lat), color="green", size=3, alpha=0.5)+geom_point(data = shanghai_cluster_df[shanghai_cluster_df$cluster==4,c("lon","lat")],aes(x=lon, y=lat), color="blue", size=3, alpha=0.5)+geom_point(data = shanghai_cluster_df[shanghai_cluster_df$cluster==5,c("lon","lat")],aes(x=lon, y=lat), color="white", size=3, alpha=0.5)
  # shanghai_map %>% geom_point(data = shanghai_cluster_df[shanghai_cluster_df$cluster==1,
  # c("lon","lat")],aes(x=lon, y=lat), color="red", size=3, alpha=0.5)%>%geom_point(data = shanghai_cluster_df[shanghai_cluster_df$cluster==2,c("lon","lat")],aes(x=lon, y=lat), color="yellow", size=3, alpha=0.5)%>%geom_point(data = shanghai_cluster_df[shanghai_cluster_df$cluster==3,c("lon","lat")],aes(x=lon, y=lat), color="green", size=3, alpha=0.5)%>%geom_point(data = shanghai_cluster_df[shanghai_cluster_df$cluster==4,c("lon","lat")],aes(x=lon, y=lat), color="blue", size=3, alpha=0.5)%>%geom_point(data = shanghai_cluster_df[shanghai_cluster_df$cluster==5,c("lon","lat")],aes(x=lon, y=lat), color="white", size=3, alpha=0.5)
  shops_df = cbind(shops_df,shops_cluster)
  colnames(shops_df)[10] = "cluster"
  
  shanghai_cluster_df[shanghai_cluster_df$location=="上海建配龙逸仙路店","cluster"] = 3
  shanghai_factor = factor(shanghai_cluster_df$cluster)
  levels(shanghai_factor) = c("上海浦东南","上海浦东北","上海浦西北","上海浦西南","上海宝山")
  shops_df[shops_df$city=="上海","region"] = as.vector(shanghai_factor)
  
  shops_df[shops_df$cluster == 16,"region"] = "东四环"
  shops_df[6,"region"] = "北四环"
  shops_df[59,"region"] = "重庆远江北"
  shops_df$cluster = NULL
  shops_df[44,c("lat","lon")] = c(39.1001140000,117.2089660000) #天津市河西加宜家居博览中心
  #苏州月星家得乐
  #深圳租金 200737000
  
  total2_temp_list =str_split_fixed(total2$gain_loss_rate,":",3)
  total2_temp_list[total2_temp_list[,1] == "",] = NA
  colnames(total2_temp_list) = c("win","deuce","loss")
  shops_df = cbind(shops_df,total2_temp_list)
  total2_copy = cbind(total2,total2_temp_list)
  total2 = total2_copy
  total2$willing_ranking = NULL
  total2_copy = total2
  
  beijing_map <- get_map("beijing,China", zoom=11)
  p = ggmap(beijing_map)
  p + geom_point(data = shops_df[shops_df$cluster==15,c("lon","lat")],aes(x=lon, y=lat), color="red", size=3, alpha=0.5)+geom_point(data = shops_df[shops_df$cluster==16,c("lon","lat")],aes(x=lon, y=lat), color="blue", size=3, alpha=0.5)

  chongqing_map <- get_map("chongqing,China", zoom=10)
  p = ggmap(chongqing_map)
  p + geom_point(data = shops_df[shops_df$cluster==1,c("lon","lat")],aes(x=lon, y=lat), color="red", size=3, alpha=0.5)+geom_point(data = shops_df[shops_df$cluster==2,c("lon","lat")],aes(x=lon, y=lat), color="blue", size=3, alpha=0.5)+geom_point(data = shops_df[shops_df$cluster==20,c("lon","lat")],aes(x=lon, y=lat), color="purple", size=3, alpha=0.5)
  
  shops_year = readxl::read_xlsx("~/data/opponent.xlsx",sheet = "Sheet1")
  shops_year$year = as.Date(shops_year$year_open,origin = "1900/01/01")
  shops_year$year_diff = as.numeric(difftime(as.Date("2016-12-31"), shops_year$year, unit="weeks"))/52.25
  shops_year[,c("year_open","year")] = NULL
  total2 = merge(total2,shops_year,by = "location",all.x = TRUE)
  new_age = ifelse(is.na(total2$age),total2$year_diff,total2$age)
  total2$age = new_age
  
  shops_winloss = readxl::read_xlsx("~/data/opponent.xlsx",sheet = "Sheet1")
  shops_winloss$year_open = NULL
  total2_copy = total2
  total2 = merge(total2,shops_winloss,by = "location")
  win = ifelse(is.na(total2$win.x),total2$win.y,total2$win.x)
  deuce = ifelse(is.na(total2$deuce.x),total2$deuce.y,total2$deuce.x)
  loss = ifelse(is.na(total2$loss.x),total2$loss.y,total2$loss.x)
  total2$win = win
  total2$deuce = deuce
  total2$loss = loss
  total2[c(1,2,3,5,6,10,11),"age"] = c(6.7313739,21.9630895,14.4661654,7.1305537,6.3185236,9.1073137,8.3800410)
  total2[10,c("win","deuce","loss")] = data.frame(58,21,21)
  total2[11,c("win","deuce","loss")] = data.frame(52,28,20)
  shops_str = readxl::read_xlsx("~/data/opponent.xlsx",sheet = "Sheet2")
  setDT(shops_str)
  shops_str[(furniture_perc>0||constr_perc>0),`:=`(F=furniture_perc/(furniture_perc+constr_perc)*100,C=constr_perc/(furniture_perc+constr_perc)*100)]
  total2$furniture_perc = shops_str$F
  total2$constr_perc = shops_str$C
  total2[5,"rent_rate"] = 0.95
  total2[,c("win.x","win.y","deuce.x","deuce.y","loss.x","loss.y","v1","mall_circle","gain_loss_rate"):=NULL]
  total2$location[c(9,10,11)] = c("上海同福易佳丽金山","上海同福易佳丽建材馆","上海同福易佳丽灯具馆")
  
  total3 = merge(total2,shops_df,by = "location")
  setnames(total3,"distance_commerce_in_2500","num_business_within_2500")
  setnames(total3,"min_distance_commerce","dist_from_business_district")
  setnames(total3,"subway_distance","dist_from_subway")
  setnames(total3,"distance_from_road","dist_from_big_road")
  setnames(total3,"distance_from_highway","dist_from_highway")
  setnames(total3,"location","mall_name")
  total3[c(1:3,6,25,26,34,36),"brand_num"] = c(460,1020,650,440,380,428,293,378) #won't work,keep info
  total3[13,c("dist_from_big_road","dist_from_highway")] = data.frame(200,12000)
  total3[19,c("dist_from_big_road","dist_from_highway")] = data.frame(100,8300)
  total3[24,c("dist_from_big_road","dist_from_highway")] = data.frame(350,3400)
  total3 = as.data.frame(total3)
  
  total4 = total3[total3$ifredstar ==0,]
  
  library(party)
  setDT(total3)
  total3$targeting = as.factor(total3$targeting)
  targeting = total3$targeting
  
  total3$win = as.numeric(total3$win)
  total3$deuce = as.numeric(total3$deuce)
  total3$loss = as.numeric(total3$loss)
  target_level = levels(total3$targeting)
  total3$ranking = as.numeric(total3$ranking)
  targeting.model.rf = cforest(targeting~.,data=total3[!is.na(targeting),c(2:8,10,12:14,21,22,23)])
  targeting.pred.rf = predict(targeting.model.rf,total3[is.na(targeting),],OOB = TRUE)
  targeting.train.pred.rf = predict(targeting.model.rf,total3[!is.na(targeting),],OOB = TRUE)
  actual = total3[!is.na(targeting),targeting]
  mean(targeting.train.pred.rf==actual)
  #插进！
  total3[is.na(targeting),"targeting"] = targeting.pred.rf
  
  #使用knn方法 对rent_rate进行插值
  library(DMwR)
  knnOutput <- knnImputation(total3[, c("area","rent_rate","targeting","ifredstar",
                                          "rent","sale")])  # perform knn imputation.
  index = which(is.na(total3$rent_rate))
  knnOutput$rent_rate[index]
  #插入!
  total3$rent_rate = knnOutput$rent_rate
  
  library(mice)
  total3 = as.data.frame(total3)
  miceMod <- mice(total3[, c(-1,-9,-18)], method="rf")  # perform mice imputation, based on random forests.
  miceOutput2 <- complete(miceMod)  # generate the completed data.
  total4 = miceOutput2
  
  total4$mall_name = total3$mall_name
  total4$city = total3$city
  total4$region = total3$region
  total4$constr_perc = 100 - total4$furniture_perc
  total5 = total4[total4$ifredstar == 0,]
  total5 = plyr::rbind.fill(total5,redstar_combine)
  total5$city = str_sub(total5$mall_name,1,2)
  
  miceMod3 <- mice(total5, method="rf")  # perform mice imputation, based on random forests.
  miceOutput3 <- complete(miceMod3)  # generate the completed data.
  total6 = miceOutput3
  
  source("~/R_Projects/SVM/Rfile/test_svm.R")
  total6$city = as.numeric(as.factor(total6$city))
  total6$region = as.numeric(as.factor(total6$region))
  total6$targeting = as.numeric(as.factor(total6$targeting))
  test_set = total6[total6$year == 2017,c(-21)]
  train_set = total6[total6$year != 2017,c(-21)]
  lll = svm_para_percent(train_set,test_set)
  
  svm.model = svm(rent~.,data = train_set,cost = 10,gamma = 3.12500e-02,cross = 5)
  predict.model = predict(svm.model,test_set[,-20])
  View(cbind(predict.model,rent = test_set[,20],mall_name = total6[total6$year==2017,]$mall_name))
  result_compare2 = cbind(predict.model,rent = test_set[,20],mall_name = total6[total6$year==2017,]$mall_name)
  openxlsx::write.xlsx(result_compare2,"./Rfile/result_compare2.xlsx")
  
  set.seed(7)
  library(mlbench)
  library(caret)
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  model <- train(rent~., data=total6[,-21], method="rf", preProcess="scale", trControl=control,
                 importance = T)
  importance <- varImp(model, scale=FALSE)
  print(importance)
  plot(importance)
  
  result_dt4$loc
  pv = paste0(result_dt4$city,result_dt4$region)
  result_dt4$location = pv
  commerce_cord_ch = lapply(result_dt4$location,geocode)
  commerce_cord_ch_list = rbindlist(commerce_cord_ch)
  result_dt4 = cbind(result_dt4,commerce_cord_ch_list)
  
  dt4_sample = result_dt4[,.SD[sample(.N,5)], by=city]
  result_dt5 = result_dt4[!is.na(lon),]
  result_dt5$latitude = NULL
  result_dt5$longitude = NULL
  shop_commerce = merge(shops_df,result_dt5,by = "city",allow.cartesian = TRUE)
  shop_commerce = data.table(shop_commerce)
  shop_commerce$coordinates.x = NULL
  shop_commerce$coordinates.y = NULL
  library(geosphere)
  load(file = '~/data/result_dt3_modified.RData')
  
  obj_distance = distHaversine(shop_commerce[,c('lon.x','lat.x')],shop_commerce[,c('lon.y','lat.y')])
  shop_commerce[,distance := obj_distance]
  
  shop_commerce_count = shop_commerce[distance<2500,.N,by = location.x]
  shops_df = merge(shops_df,shop_commerce_count,by.x = "location",by.y = "location.x",all.x = TRUE)
  setnames(shops_df,"N","distance_commerce_in_2500")
  shop_commerce_distance = shop_commerce[,min(distance,na.rm = TRUE),by = location.x]  
  # false_row = shop_commerce[distance ==0,c("lon","lat","location.x","location.y")]
  shops_df = merge(shops_df,shop_commerce_distance,by.x = "location",by.y = "location.x",all.x = TRUE)
  setnames(shops_df,"V1","min_distance_commerce")
  shops_df = cbind(shops_df,subway_distance)
  
  library(googleway)
  library(RCurl)
  proxy_url <- "http://127.0.0.1:61387/"
  Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)
  
  url.exists("www.google.com")
  key = "AIzaSyCW6btatRC9nXwaMI06bgV5iHZbsX4H034"
  place_key = "AIzaSyCW6btatRC9nXwaMI06bgV5iHZbsX4H034"
  test_subway = google_places(key = key,place_type = "subway_station",location = c(31.22941,121.3787),radius = 1000,rankby = "distance")
  test_subway = test_subway$results
  
  test_nearestway = google_nearestRoads(df_points = data.frame(latitude = 31.22941,longitude = 121.3787),key = road_key)

  df_path <- read.table(text = "lat lon
-35.27801 149.12958
-35.28032 149.12907
-35.28099 149.12929
-35.28144 149.12984
-35.28194 149.13003
-35.28282 149.12956
-35.28302 149.12881
-35.28473 149.12836", header = T)
  
  google_snapToRoads(df_path, key = road_key, interpolate = TRUE, simplify = TRUE)
  # google_place_details()
  
  df_points = read.table(text = "lat lon
                         31.2429860000 121.3927610000
                         31.2399950000 121.3972420000
                         31.2308790000 121.4060810000",header = T)
  road_key = "AIzaSyB3Hz_xqngONaTyfpWfNBdtQfJx-Ay9p0o"
  map_key = "AIzaSyAQwxqx2skmYVbgffhTfyKixuxAgU9PTbs"
  # google places key AIzaSyCW6btatRC9nXwaMI06bgV5iHZbsX4H034
  # google map key may be AIzaSyAQwxqx2skmYVbgffhTfyKixuxAgU9PTbs
  # res <- find_closest_roads(df_points,"lat","lon",key = road_key)
  res <- google_nearestRoads(df_points, key = road_key)
  res$snappedPoints = res$snappedPoints[c(1,3,5),]
  df_points$snapped_lat <- res$snappedPoints$location$latitude
  df_points$snapped_lon <- res$snappedPoints$location$longitude
  df_points$colour <- 'green'
  
  library(leaflet)
  library(XML)
  library(leafletCN)
  dat = data.frame(name = regionNames("world"),
                   value = runif(length(regionNames("world"))))
  geojsonMap(dat,"world")
  
  leaflet() %>%
    amap() %>%
    addMarkers(lng = 116.3125774825, lat = 39.9707249401,
               popup = "The birthplace of COS")
  leaflet() %>%
    amap() %>%
    addMarkers(data = df_points, lat = ~lat,lng = ~lon) %>%
    addMarkers(data = df_points, lat = ~snapped_lat,lng = ~snapped_lon)
  
  # google_map(key = map_key) %>%
  #   add_markers(data = df_points, lat = 'lat', lon = 'lon') %>%
  #   add_markers(data = df_points, lat = 'snapped_lat', lon = 'snapped_lon')
  
  # https://roads.googleapis.com/v1/nearestRoads?points=60.170880,24.942795|60.170879,24.942796|60.170877,24.942796&key=YOUR_API_KEY
  find_closest_roads = function(location, latname, lonname, key) {
    library(rjson)
    library(RCurl)
    weburl = "https://roads.googleapis.com/v1/nearestRoads"
    loc_string = paste(location[[latname]], location[[lonname]], sep = ",", collapse = "|")
    sendurl = paste0(weburl,
                     "?points=", loc_string,
                     "&key=", key)
    result = getURL(sendurl)
    result = fromJSON(result)$snappedPoints
    return(result)
  }
  
  getCord = function(v) {
    dt = data.table(location = v)
    dt[, coordinates := getCoordinate(location, output = 'xml')]
    xmls = lapply(dt$coordinates, read_xml)
    lonpart = lapply(xmls, xml_node, 'lng')
    latpart = lapply(xmls, xml_node, 'lat')
    xml_text_v = Vectorize(xml_text)
    longitude1 = xml_text_v(lonpart)
    latitude1 = sapply(latpart, xml_text)
    dt$lon = as.numeric(longitude1)
    dt$lat = as.numeric(latitude1)
    return(dt)
  }
  
  find_closest_subways = function(coordinate_df,place_key){
    coordinate_df$lat_lon <- do.call(Map, c(f= c, unname(coordinate_df)))
    i = 1
    result_df = data.frame(matrix(,nrow=nrow(coordinate_df), ncol=2))
    for(v in coordinate_df$lat_lon){
      df = google_places(key = place_key,place_type = "subway_station",location = v,radius = 5000,rankby = "distance")
      if(!is.na(df$results$geometry$location$lat) && length(df$results$geometry$location$lat)>0){
      result_df[i,] = c(df$results$geometry$location$lat[1],df$results$geometry$location$lng[1])
      }
      i = i+1
    }
    return(result_df)
    #apply(DF[, c("height", "weight")], 1, f)
  }
  
  closest_subway = find_closest_subways(shops_cord_ch_df[,c("lat","lon")],place_key)
  sindex = sample(1:60,10,replace = FALSE)
  sample_subway = cbind(shops_df$mall_name,shops_df$city,closest_subway)[sindex,]
  library(geosphere)
  subway_distance = distHaversine(shops_cord_ch_df[,c('lon','lat')],closest_subway[,c('X2','X1')])
  
  
  find_closest_places = function(search_string,location,types,name,radius){
    library(RCurl)
    library(rjson)
    key = "AIzaSyCW6btatRC9nXwaMI06bgV5iHZbsX4H034"
    location = paste0(location,collapse = ",")
    web = getURL(
      paste(
        "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=", location,
        "&key=", key,
        "&types=",types,
        "&name=",name,
        "&radius=",radius,
        sep = ""
      )
    )
    return(fromJSON(web))
  }
  
  
  function(){
    # shops[,coordinates := getCoordinate(location,output = 'xml')]
    # xmls = lapply(shops$coordinates,read_xml)
    # lonpart = lapply(xmls,xml_node,'lng')
    # latpart = lapply(xmls,xml_node,'lat')
    # xml_text_v = Vectorize(xml_text)
    # longitude1 = xml_text_v(lonpart)
    # latitude1 = sapply(latpart,xml_text)
    # shops$lon = as.numeric(longitude1)
    # shops$lat = as.numeric(latitude1)
    # new_vector = list() 
    # for(i in 1:nrow(shops)){
    #   new_vector[[i]] = c(shops$longitude[[i]],shops$latitude[[i]])
    # }
    # zz = Map(f= c, unname(shops[,c("longitude","latitude")]))
    # shops$test <- do.call(Map, c(f= c, unname(shops[,c("longitude","latitude")])))
    # # new_location2 = apply(shops[, c("longitude", "latitude")], 1, getLocation,output = "xml")
    # 
    # new_location = lapply(new_vector,getLocation,output = 'xml')
    # names(new_location) = NULL
    # new_location_xmls = lapply(new_location,read_xml)
    # address = lapply(new_location_xmls,xml_node,'formatted_address')
    # address1 = sapply(address,xml_text)
    # shops$new_location = address1
    # commerce_cord = getCord(commerce_loc)
    # commerce_loc_en = lapply(commerce_loc,transnew)
    # commerce_cord_ch = lapply(commerce_loc,geocode)
    # address_correct = cbind(commerce_loc,commerce_cord_ch_df)
    # result_dt4 = merge(result_dt3,address_correct,by.x = "location",by.y = "commerce_loc",all.x = TRUE)
    # result_dt4[,':='(longitude=ifelse(!is.na(lon),lon,longitude),latitude=ifelse(!is.na(lat),lat,latitude))]
    # shop_commerce = merge(shops,result_dt4,by = "city", allow.cartesian = TRUE)
    # obj_distance = distHaversine(shop_commerce[,c('lon','lat')],shop_commerce[,c('longitude','latitude')])
    # shop_commerce[,distance := obj_distance]
    # shop_commerce[distance<2500,.N,by = mall_name]
    
  }