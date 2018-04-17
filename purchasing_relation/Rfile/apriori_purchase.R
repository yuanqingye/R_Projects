source('~/Rfile/R_impala.R')
source('~/Rfile/toolfunc.R')
source2('~/Rfile/R_hive.R',7,17)
library(data.table)
library(xlsx)
if(!require(arules)){
  install.packages('arules')
  library(arules)
}

# order_list = read_data_impala_general(query)

dates = 20170501:20170531
dates_str = as.character(dates)
dates = as.Date(dates_str,'%Y%m%d')
dates_fm = format(dates,'%Y-%m-%d')

assignorderbydates = function(dates){
  for(date in dates){
  query_temp = "select mobile,prod_name,cont_cat1_name,cont_cat2_name,cont_cat3_name,to_date(ordr_date) from dl.fct_ordr d
  where ((d.ordr_status not in ('1','7','19') and d.sale_type<>'oms') or (d.ordr_status ='Y' and d.sale_type='oms')) and to_date(ordr_date) = '"
  query = paste0(query_temp,date,"'")
  result = read_data_impala_general(query)
  assign(paste0('order',date),result,envir = .GlobalEnv)
  }}

assignorderbydates(dates_fm)
ll = paste0('order',dates_fm)
lll = lapply(ll,'as.name')
ln = lapply(lll,'eval')
data_hive = rbindlist(ln)
data_hive = data.table(data_hive)
write_xlsx(data_hive,"~/data/order_data_may.xlsx")
order.list.nona = data_hive[!is.na(cont_cat3_name),]
order.list.nodup = unique(order.list.nona, by=c("mobile", "cont_cat3_name"))

prepare.list = aggregate(cont_cat3_name~mobile,data = order.list.nodup,paste)

#refer to http://michael.hahsler.net/research/arules_RUG_2015/demo/
frequentsets=eclat(prepare.list$cont_cat3_name,parameter=list(support=0.001,maxlen=20))  #求频繁项集  
rules <- apriori(prepare.list$cont_cat3_name, parameter = list(supp=0.0001,confidence=0.1, minlen = 2))

inspect(head(sort(rules), n=10))
quality(rules)
# quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift")
inspect(head(sort(rules, by = "lift"), n=200))

plot(head(sort(rules, by = "lift"), n=50), method = "graph", control=list(cex=1.2))

plot(rules)

itemFrequencyPlot(items(rules), topN=30, cex.names=.6,col = "purple")

inspect(head(sort(rules, by = "support"), n=200))

diaoding = subset(rules, subset = (rhs%pin%"吊顶"))

plot(head(sort(diaoding,by = 'lift'),n=30),method = 'graph',control = list(cex=.8))

plot(rules, method="grouped")
plot(rules, measure = c("support", "lift"), shading = "confidence")