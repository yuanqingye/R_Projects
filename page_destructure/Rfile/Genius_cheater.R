url = "https://movie.douban.com/subject/27024903/comments?status=P"
library(rvest)
library(magrittr)
web = read_html(url)
comments = web %>% html_nodes("div#comments p")%>% html_text()

s <- html_session(url)
s2 = s %>% follow_link("后页")

comments2 = s2 %>% html_nodes("div#comments p")%>% html_text()

inits <- html_session(url)
l=list()
l[[1]] = inits
comments.df = data.frame(comments = vector(mode = "character",length = 0))
for(i in 1:50){
  comments = l[[i]] %>% html_nodes("div#comments p")%>% html_text()
  temp_data_frame = data.frame(comments = comments)
  comments.df = rbind(comments.df,temp_data_frame)
  l[[i+1]] =  l[[i]] %>% follow_link("后页")
}

paste(i,"")