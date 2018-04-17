library(rvest)
library(data.table)
library(magrittr)

get_douban_book_list = function(){
web<-read_html("https://book.douban.com/top250?icn=index-book250-all",encoding="UTF-8")
book_list = web%>%html_nodes('p.pl')%>%html_text()
month = 2
year = 2015
url = paste0('https://www.timeanddate.com/weather/china/suzhou/historic?month=',month,'&year=',year)
web2 = read_html(url,encoding = 'UTF-8')
return(book_list)
}

get_ateam_example = function(){
ateam = read_html("http://www.boxofficemojo.com/movies/?id=ateam.htm",encoding = 'UTF-8')
v1 = ateam%>%html_nodes('center')%>%html_nodes('td')
v2 = ateam%>%html_nodes('center')%>%html_nodes('font')
library(magrittr)
imgs = ateam%>%html_nodes('table')%>%extract2(1)%>%html_nodes('img')
}

get_3A_hospitals = function(){
  url = "http://www.fudanmed.com/institute/news2015-2.aspx"
  web = read_html(url,encoding = 'UTF-8')
  table = web%>%html_nodes('table#table10')%>%html_nodes('table')
  hospital_list = web%>%html_nodes('table#table10')%>%html_nodes('table')%>%html_nodes('tr')%>%html_text()
  hospital_list_final = gsub('\\s','',hospital_list)
}

get_3A_hospitals2 = function(){
  url = 'http://www.fudanmed.com/institute/news2015-2.aspx'
  web = readLines(url,encoding = 'UTF-8')
  tables = readHTMLTable(web, as.data.frame = TRUE, stringsAsFactors = FALSE)
}

# get_business_region = function(url = 'https://www.dianping.com/beijing'){
#   web = read_html(url,encoding = 'UTF-8')
# # business_region = web%>%html_nodes('ul#hotregion')%>%html_nodes('li')%>%html_text()
#   business_region = web%>%html_nodes('ul#hotregion')%>%html_nodes('li>a')%>%html_attr('title')
#   business_link = web%>%html_nodes('ul#hotregion')%>%html_nodes('li>a')%>%html_attr('href')
#   business_link = paste0('https://www.dianping.com',business_link)
#   dt = data.table(region = business_region,link = business_link)
#   return(dt)
# }

get_business_shop_info = function(url = 'https://www.dianping.com/search/category/2/0/r1467'){
  web = read_html(url,encoding = 'UTF-8')
# business_shop_name = web %>% html_nodes('div#shop-all-list') %>% html_nodes('div.tit>a')%>%extract2(1)%>%html_attr('title')
# business_shop_name = web %>% html_nodes('div#shop-all-list') %>% html_nodes('div.tit')%>%html_nodes('a')%>%extract2(1)%>%html_attr('title')
  business_shop_name = web %>% html_nodes('div#shop-all-list') %>% html_nodes('div.tit')%>%html_node('a')%>%html_attr('title')
  business_shop_addr = web %>% html_nodes('div#shop-all-list') %>% html_nodes('div.tag-addr>span.addr') %>%html_text()
  dt = data.table(name = business_shop_name,address = business_shop_addr)
  return(dt)
}

dianping_scrapper = function(city_list){
  v = vector(mode = 'character',length = 0)
  result_dt = data.table(city=v,region = v,name = v,address = v)
  for(city in city_list){
    city_url = paste0('https://www.dianping.com/',city)
    region_dt = get_business_region(city_url)
    for(i in 1:nrow(region_dt)){
      region_link = region_dt[[i,"link"]]
      region = region_dt[[i,"region"]]
      shop_dt = get_business_shop_info(region_link)
      shop_dt[,c('city','region') := list(city,region)]
      result_dt = rbind(result_dt,shop_dt)
    }
  }
  return(result_dt)
}

library(compiler)
c_dianping_scrapper = cmpfun(dianping_scrapper)

get_business_region2 = function(url = 'https://www.dianping.com/shopall/2/0#BDBlock'){
 web = read_html(url,encoding = 'UTF-8')
 business_region2 = web%>%html_nodes('div .box.shopallCate')%>%extract2(2)%>%html_nodes('li>a.B')%>%html_text()
 return(business_region2)
}

dianping_scrapper2 = function(city_list){
  v = vector(mode = 'character',length = 0)
  result_dt = data.table(city=v,region = v)
  i = 1
  for(city in city_list){
    city_url = paste0('https://www.dianping.com/',city)
    web = read_html(city_url,encoding = 'UTF-8')
    # business_groups_link = web%>%html_nodes('ul#hotregion')%>%html_nodes('li>a.more')%>%html_attr('href')
    business_groups_link = web%>%html_nodes('div.cata-hot-detail.cata-hot-area')%>%html_nodes('div.hot-title')%>%html_nodes('a.hot-title-link')%>%html_attr('href')
    business_groups_link = paste0('https:',business_groups_link)
    business_region2 = get_business_region2(business_groups_link)
    temp_dt = data.table(city = city,region = business_region2)
    result_dt = rbind(result_dt,temp_dt)
    print(i)
    i = i+1
    }
  return(result_dt)
}

fang_scrapper = function(city_list){
  v = vector(mode = 'character',length = 0)
  result_dt = data.table(name=v,city=v,district = v,location=v,onsaledate = v,unitprice = v)
  for(city in city_list){
    for(i in 1:10){
    url = paste0("https://xf.fangdd.com/",city,"/loupan/s5-pg",i)
    web = read_htmls(url,encoding = 'UTF-8')
    name = web%>%html_nodes('ul#houslist>span.hsname')%>%html_text()
    district = web%>%html_nodes('ul#houslist>span.hsname')%>%html_text()
    }
  }
}

temp = dirname(rstudioapi::getActiveDocumentContext()$path)
# test <- read_html("http://www.privacyrights.org/data-breach/new?title=")
# test %>% html_table(html_nodes("table.data-breach-table")[[1]])

# book_list_df = read.table(text = book_list,sep = "/")
# book_list_df = data.table(book_list)
# 
# str_split(book_list,pattern = "/")
# apply(book_list_df,1,str_split,"/")
# 
# url = "http://www.360doc.com/content/15/0605/00/22099567_475741839.shtml"
# web = read_html(url)
# table = web%>%html_nodes("table")%>%`[[`(3)%>%html_table(header = TRUE)
