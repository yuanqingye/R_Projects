library(readxl)
big_general_info_original = read_xlsx("~/data/rental_edit.xlsx",na = "?")
big_general_info_original = data.table(big_general_info_original)
big_general_info = big_general_info_original[!is.na(rental_prd) & rental_prd != 0,]
big_general_info[,area_avg := median(RENT_AREA_WA,na.rm = TRUE),by = c("MALL_NAME")]
big_general_info[is.na(RENT_AREA_WA),RENT_AREA_WA := area_avg]
big_general_info[AVERAGE_PRICE == 0, AVERAGE_PRICE := rental_prd/RENT_AREA_WA]

big_general_info[,GROUP_NUM := 1:.N,by = "MALL_NAME"] #Get seq num inside each mall
big_general_info = big_general_info[!(GROUP_NUM<10 & rental_prd<10000),] #remove the first no rental
big_general_info = as.data.frame(big_general_info)

need_fix_area_index = which(big_general_info$RENT_AREA_WA<0.5*big_general_info$area_avg|big_general_info$RENT_AREA_WA>2*big_general_info$area_avg)
# View(big_general_info[!complete.cases(big_general_info),])
# View(big_general_info[big_general_info$RENT_AREA_WA<0.5*big_general_info$area_avg|big_general_info$RENT_AREA_WA>2*big_general_info$area_avg,c("MALL_NAME","DATE_ID","rental_prd","RENT_AREA_WA","area_avg","GROUP_NUM")])
unusual_names = unique(big_general_info[big_general_info$RENT_AREA_WA<0.5*big_general_info$area_avg|big_general_info$RENT_AREA_WA>2*big_general_info$area_avg,]$MALL_NAME)
big_general_info = data.table(big_general_info)
lm_model_part_table = big_general_info[(RENT_AREA_WA>=0.5*area_avg&RENT_AREA_WA<=2*area_avg)&(MALL_NAME %in% unusual_names),]
lm_model = by(lm_model_part_table,lm_model_part_table$MALL_NAME,function(m){y = m$RENT_AREA_WA;x = m$rental_prd;lm(y~x)})
need_fix_part_table = big_general_info[(RENT_AREA_WA < 0.5*area_avg|RENT_AREA_WA > 2*area_avg)&(MALL_NAME %in% unusual_names),]
ls_need_fix = split(need_fix_part_table$rental_prd,need_fix_part_table$MALL_NAME)
ls_need_fix_table = lapply(ls_need_fix,as.data.frame)
ls_need_fix_table = lapply(ls_need_fix_table,"colnames<-",value = "x")
pred_area = mapply(predict,lm_model,ls_need_fix_table)
big_general_info[(RENT_AREA_WA<0.5*area_avg|RENT_AREA_WA>2*area_avg) & MALL_NAME == "上海浦江商场",]$RENT_AREA_WA[-1] = pred_area[[1]][-1]
big_general_info[(RENT_AREA_WA<0.5*area_avg|RENT_AREA_WA>2*area_avg) & MALL_NAME == "上海吴中路商场",]$RENT_AREA_WA = pred_area[[2]]

big_general_info[need_fix_area_index, AVERAGE_PRICE := rental_prd/RENT_AREA_WA]
# View(big_general_info[is.na(big_general_info$AREA_JIAJU),])
# View(big_general_info[is.na(big_general_info$AREA_XINYETAI)&is.na(big_general_info$FINALPRICE_XINYETAI),])

# source("~/Rfile/R_hana.R")
# sql = "select *
# from BIGBI.DIM_BRAND_ENTER_INFO
# where begin_date <= '2015-9-30'
# and finish_date >= '2015-9-1'
# and mall_code='10063'
# and booth_type='0090'"
# # and category_name_1='新业态'"
# hana_data = read_data_from_hana(sql)
# read_data_from_hana

# some special cases
big_general_info[big_general_info$MALL_NAME=="上海汶水商场" & big_general_info$DATE_ID == 201409,"XINYETAI_NUM"] = 0.5
big_general_info[big_general_info$MALL_NAME=="杭州古墩商场" & big_general_info$DATE_ID == 201410,"XINYETAI_NUM"] = 6/31
big_general_info[big_general_info$MALL_NAME=="杭州古墩商场" & big_general_info$DATE_ID == 201410,"FINALPRICE_XINYETAI"] = 39.9 * 155
big_general_info = data.table(big_general_info)
big_general_info[is.na(big_general_info$FINALPRICE_XINYETAI) & big_general_info$MALL_NAME == "上海浦江商场",FINALPRICE_XINYETAI := 42*AREA_XINYETAI]
big_general_info[is.na(big_general_info$AREA_XINYETAI)&is.na(big_general_info$FINALPRICE_XINYETAI),c("AREA_XINYETAI","FINALPRICE_XINYETAI")] = 0

big_general_info[is.na(big_general_info$FINALPRICE_XINYETAI)&big_general_info$DATE_ID == 201404,"FINALPRICE_XINYETAI"] = 4650-28*166+48*83
big_general_info[is.na(big_general_info$FINALPRICE_XINYETAI)&big_general_info$DATE_ID == 201405,"FINALPRICE_XINYETAI"] = 12000-(1-0.323)*28*115

#租金有一定差距
# big_general_info[abs(big_general_info$rental_prd-(big_general_info$FINALPRICE_JIAJU+big_general_info$FINALPRICE_JIANCAI+big_general_info$FINALPRICE_RUANZHUANG+big_general_info$FINALPRICE_JINKOU+big_general_info$FINALPRICE_XINYETAI))>1,rental_prd - (FINALPRICE_JIAJU+FINALPRICE_JIANCAI+FINALPRICE_RUANZHUANG+FINALPRICE_JINKOU+FINALPRICE_XINYETAI)]

#因数据错误很多且纠正成本高，故排除面积数据
big_general_info = data.frame(big_general_info)
big_general_info = big_general_info[,!str_detect(names(big_general_info),"^AREA_")]
big_general_info[big_general_info$DATE_ID==201509&big_general_info$MALL_NAME=="上海浦江商场",c("JIAJU_NUM","JIANCAI_NUM","RUANZHUANG_NUM","JINKOU_NUM","XINYETAI_NUM","BRAND_NUM")] = 1/3*big_general_info[big_general_info$DATE_ID==201510&big_general_info$MALL_NAME=="上海浦江商场",c("JIAJU_NUM","JIANCAI_NUM","RUANZHUANG_NUM","JINKOU_NUM","XINYETAI_NUM","BRAND_NUM")]

getAdjacentNum = function(x,lowerbound = 1,upperbound = x+2){
  start = x - 2
  if (x - lowerbound == 1)
    start = x - 1
  if (x == lowerbound)
    start = x
  end = x + 2
  if (upperbound - x == 1)
    end = x + 1
  if (upperbound == x)
    end = x
  result = seq(from = start,to = end,by = 1)
  return(result)
}

getAdjacentNumV = Vectorize(getAdjacentNum,SIMPLIFY = FALSE)

getAdjacentIndex = function(index){
  unique(unlist(getAdjacentNumV(index)))
}


big_general_info = big_general_info[complete.cases(big_general_info),]

# #考虑采用聚类的方式来使得问题被解决
# library(DMwR)
# big_general_info = as.data.frame(big_general_info)
# knnOutput <- new_knn_imputation(big_general_info[,c("MALL_CODE","FINALPRICE_XINYETAI","AREA_XINYETAI","XINYETAI_NUM")])  # perform knn imputation.
# View(big_general_info[complete.cases(big_general_info[, c("AREA_XINYETAI","FINALPRICE_XINYETAI")]),])
# View(big_general_info[big_general_info$MALL_NAME %in% big_general_info[is.na(big_general_info$FINALPRICE_XINYETAI),"MALL_NAME"],c("FINALPRICE_XINYETAI","AREA_XINYETAI","XINYETAI_NUM","MALL_NAME","DATE_ID","MALL_CODE")])
# 
# library(mice)
# #check missing value pattern
# big_general_info = as.data.frame(big_general_info)
# md.pattern(big_general_info[,colnames(big_general_info) != "MALL_NAME"])# pattern or missing values in data.
# miceMod <- mice(big_general_info[, !names(big_general_info) %in% "MALL_NAME"], method="rf")  # perform mice imputation, based on random forests.
# miceOutput <- complete(miceMod)  # generate the completed data.

big_general_info = big_general_info[,!(names(big_general_info) %in% c("area_avg","GROUP_NUM"))]
big_general_info = data.table(big_general_info)
big_general_info[,rent := c(rental_prd[-1:-3],rep(NA,3)),by = "MALL_NAME"]
big_general_info = as.data.frame(big_general_info)
geo_info = new_general_info[new_general_info$year == 2017,c(1,2,12:23,25)]
big_general_info$MALL_NAME =  str_replace(big_general_info$MALL_NAME,"北京至尊Mall","北京东四环商场")
geo_info = data.frame(geo_info)

big_general_info_copy = big_general_info
big_general_info = merge(big_general_info,geo_info,by.x = "MALL_NAME",by.y = "mall_name",all.x = TRUE)
names(big_general_info)[names(big_general_info) == "rental_prd"] = "FINALPRICE"

train_df = big_general_info[!(big_general_info$DATE_ID %in% 201704:201709),]
dest_df = big_general_info[big_general_info$DATE_ID %in% c(201707,201708,201709),]
test_df = big_general_info[big_general_info$DATE_ID %in% c(201704,201705,201706),]

train_df = train_df[complete.cases(train_df),]
dest_df = dest_df[complete.cases(dest_df[,!(names(dest_df) %in% "rent")]),]
test_df = test_df[complete.cases(test_df),]

test_mall_names = test_df[,"MALL_NAME"]
train_rent = train_df[,!(names(big_general_info) %in% c("MALL_NAME","MALL_CODE","DATE_ID"))]
dest_rent = dest_df[,!(names(big_general_info) %in% c("MALL_NAME","MALL_CODE","DATE_ID"))]
test_rent = test_df[,!(names(big_general_info) %in% c("MALL_NAME","MALL_CODE","DATE_ID"))]

library(openxlsx)
ws = createWorkbook()
addWorksheet(ws,"train_rent")
addWorksheet(ws,"test_rent")
addWorksheet(ws,"dest_rent")
writeData(ws,"train_rent",train_df)
writeData(ws,"test_rent",test_df)
writeData(ws,"dest_rent",dest_df)
saveWorkbook(ws,"~/data/modified_second_edit_rental_data.xlsx")
