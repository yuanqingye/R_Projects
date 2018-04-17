library(rvest)
library(magrittr)
url = "https://list.tmall.com/search_product.htm?q=%BA%CB%B5%AF&type=p&vmarket=&spm=875.7931836%2FB.a2227oh.d100&from=mallfp..pc_1_searchbutton"
web = read_html(url)
name = web%>%html_nodes("p.productTitle a")%>%html_attr('title')
price = web%>%html_nodes("p.productPrice em")%>%html_attr('title')
nums = web%>%html_nodes("p.productStatus em")%>%html_text()

tianmao_result = cbind(name,price,nums)
