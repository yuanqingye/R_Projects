library(rvest)
sina_url = "https://www.sina.com.cn"
sina = html_session(sina_url)
sina_sport = sina%>%follow_link("体育")%>%html_nodes("li:nth-child(1) h2")%>%html_text()
