library(readxl)

booth_info = read_xlsx("~/data/booth_info.xlsx")

booth_mean_floor = booth_info[,.(avg_price = mean(调整后展位单价)),by = "楼层名称"]
booth_mean_type = booth_info[,.(avg_price = mean(调整后展位单价)),by = "调整后大类"]
booth_mean_level = booth_info[,.(avg_price = mean(调整后展位单价)),by = "展位等级"]
booth_mean_mall_type = booth_info[,.(avg_price = mean(调整后展位单价)),by = "商场性质"]
booth_mean_district = booth_info[,.(avg_price = mean(调整后展位单价)),by = 小区名称][order(avg_price),]
booth_mean_mall_name = booth_info[,.(avg_price = mean(调整后展位单价)),by = 商场名称][order(avg_price),]


booth_mean_comprehensive = booth_info[展位类型!="非标准展位"&展位类型!="广告位",.(avg_price = mean(调整后展位单价),num = .N),by = c("调整后大类","楼层名称","展位等级")][order(avg_price,decreasing = T ),]
