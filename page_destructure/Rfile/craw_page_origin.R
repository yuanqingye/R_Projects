library(XML)
library(RCurl)
library(rjson)

url = 'https://www.timeanddate.com/scripts/cityajax.php?n=china/suzhou&mode=historic&hd=20160302&month=3&year=2016&json=1'
year = '2015'
month = '2'
crawler = function(year,month){
  url = paste0('https://www.timeanddate.com/weather/china/suzhou/historic?month=',month,'&year=',year)
  page = readLines(url,encoding = 'utf-8')
  tables = readHTMLTable(page, as.data.frame = TRUE, stringsAsFactors = FALSE)
  weather_table = tables[[2]]
  weather_table = data.table(weather_table)
  weather_table[,Temp:=gsub('\\s','',Temp)]
  weather_table[,Visibility:=gsub('\\s','',Visibility)]
  return(weather_table)
}

crawler_json = function(){
  web_url = 'https://www.timeanddate.com/scripts/cityajax.php?n=china/suzhou&mode=historic&hd=20160302&month=3&year=2016&json=1'
  content = getURL(web_url)
  str_content = fromJSON(content)
  }
# readHTMLTable(page, as.data.frame = TRUE, stringsAsFactors = FALSE)