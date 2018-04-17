library(rvest)
library(magrittr)
url = "http://www.imdb.com/title/tt2250912/"
web = read_html(url)
cast_star = web%>%html_nodes("#titleCast .itemprop span")%>%html_text()
characters = web%>%html_nodes("#titleCast .character div")%>%html_text()
characters = gsub("\\s+","",characters)
cast_lines = data.frame(actor = cast_star,character = characters)
# reviews =  web%>%html_nodes("table") %>%.[[3]] %>%html_table()
