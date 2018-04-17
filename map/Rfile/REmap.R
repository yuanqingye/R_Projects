# install.packages('REmap')
# library(githubinstall)
# githubinstall('REmap')
library(REmap)
options(baidumap.key = '4SY3dY8GrhfU5ChOeqFMvhcaf9DWo7dc')

city_vec = c("beijing","Shanghai","guangzhou")
get_city_coord("Shanghai")
get_geo_position(city_vec)
set.seed(125)
origin = rep('beijing',10)
destination = c('shanghai','guangzhou','dalian','nanjing','fuzhou',
                'wuhan','chengdu','hangzhou','shenzhen','chongqing')
dat = data.frame(origin,destination)
out = remap(dat,title = "REmap",subtitle = "theme:Dark")
plot(out)

# xxx<-remapB(markPointData = data.frame(lat$city),zoom=5,
#             markPointTheme = markPointControl(symbol = "pin",
#                                               effect=T,
#                                               symbolSize = 10,
#                                               color="red"),
#             geoData = lat)
# 
# plot(xxx)

library(ggmap)
map <- get_map(location = 'Zhejiang', zoom = 10, maptype = 'roadmap')
ggmap(map)
#这是个问题
