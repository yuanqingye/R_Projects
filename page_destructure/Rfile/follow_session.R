library(rvest)

united <- html_session("http://www.united.com/")

tmp <- gurl %>% html_session %>% 
  read_html(encoding="utf-8") %>% 
  html_nodes("div.f-main-list>div>div")

baidu_url = "https://www.baidu.com/"             
baidu = html_session("https://www.baidu.com/")
next_baidu = baidu%>%follow_link("新闻")
baidu_page = read_html(baidu_url)
baidu_page%>%html_node("a.mnav")%>%html_text()

search_form_fill = baidu_page%>%html_node("#form")%>%html_form()%>%set_values("nobel prize")
search_results = baidu%>%submit_form(search_form_fill)
search_results%>%follow_link()

sina_url = "https://www.sina.com.cn"
sina = html_session(sina_url)
sina_sport = sina%>%follow_link("体育")%>%html_nodes("li:nth-child(1) h2")%>%html_text()
sina_pl = sina_sport%>%follow_link("英超")

library(httr)
sina_raw = GET(sina_url,user_agent("Mozilla/5.0 (Windows NT 6.1; WOW64; rv:54.0) Gecko/20100101 Firefox/54.0"))
sina = html_session(sina_url,user_agent("Mozilla/5.0 (Windows NT 6.1; WOW64; rv:54.0) Gecko/20100101 Firefox/54.0"))
sina_sport = sina%>%html_nodes(".nav-w:nth-child(2) ul:nth-child(1) b")%>%html_text()


login <- united %>%
  html_node("form[name=LoginForm]") %>%
  html_form() %>%
  set_values(
    MpNumber = "GY797363",
    Password = password
  )

logged_in <- united %>% submit_form(login)

logged_in %>%
  follow_link("View account") %>%
  html_node("#ctl00_ContentInfo_AccountSummary_spanEliteMilesNew") %>%
  html_text() %>%
  readr::parse_number()

rm(list=ls())
library(rvest)

### Trying to sign into a form using email/password 

url       <-"http://www.perfectgame.org/"   ## page to spider
pgsession <-html_session(url)               ## create session
pgform    <-html_form(pgsession)[[1]]       ## pull form from session

#the backstroke means the name
set_values(pgform, `ctl00$Header2$HeaderTop1$tbUsername` = "myemail@gmail.com") 
set_values(pgform, `ctl00$Header2$HeaderTop1$tbPassword` = "mypassword")

submit_form(pgsession,pgform,submit=`ctl00$Header2$HeaderTop1$Button1`)

#corrected cases
url       <-"http://www.perfectgame.org/"   ## page to spider
pgsession <-html_session(url)               ## create session
pgform    <-html_form(pgsession)[[1]]       ## pull form from session

# Note the new variable assignment 


filled_form <- set_values(pgform,
                          `ctl00$Header2$HeaderTop1$tbUsername` = "myemail@gmail.com", 
                          `ctl00$Header2$HeaderTop1$tbPassword` = "mypassword")

submit_form(pgsession,filled_form)