library(rvest)
url<-"https://www.liepin.com/zhaopin/?init=1"
page<-read_html(url)
baseurl = "https://www.liepin.com"

position<-page%>%html_nodes('h3 a')%>%html_text(trim =TRUE)
position  #查看职位
position<- position[-41]   #删除第41个

link<- page %>% html_nodes('h3 a')%>%html_attrs()
# link<- page %>% html_nodes('ul.sojob_list  div.job-info,h3 a')%>%html_attrs()
#也可以写成'ul.sojob_list div.sojob-item-main div.job-info,h3 a
link[1] #读取数据，规定编码
# position<-web %>% html_nodes("div.pages_content") %>% html_text()

link1<-c(1:length(link))  #初始化一个和link长度相等的link1
for(i in 1:length(link))
  link1[i]<-paste0(baseurl,link[[i]][1])
link1  #查看link1
link2<-link1[-41] #删除最后一行
link2    #查看link2
link<-link2    #将link2重新赋值给link

#薪水
salary <- page %>% html_nodes('span.text-warning') %>% html_text()
salary

#工作地点
place <- page %>% html_nodes('p .area') %>% html_text()
place

#教育学历
edu<-page %>% html_nodes('span.edu') %>% html_text()
edu

#个人经历
experience = page %>% html_nodes("p.condition span:nth-child(4)") %>% html_text()
experience

alldata = cbind(position,salary,place,edu,experience,link)

