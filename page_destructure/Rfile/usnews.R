library(rvest)

#login
# url <- "https://secure.usnews.com/member/login?ref=https%3A%2F%2Fpremium.usnews.com%2Fbest-graduate-schools%2Ftop-medical-schools%2Fresearch-rankings"
# session <- html_session(url)
# 
# form <- html_form(read_html(url))[[1]]
# 
# filled_form <- set_values(form,
#                           username = "notmyrealemail",
#                           password = "notmyrealpassword")
# 
# submit_form(session, filled_form)

login_url = "https://secure.indeed.com/account/login?service=my&hl=zh_CN&co=CN&continue=https%3A%2F%2Fcn.indeed.com%2F"
session = html_session(login_url)
form = html_form(session)[[1]]
filled_form = set_values(form,username = "memory_717@126.com",password = "testscrape")
submit_form(session,filled_form)

query = "data science"
loc = "beijing"
session <- html_session("http://cn.indeed.com")
form <- html_form(web)[[1]]
form <- set_values(form, q = query, l = loc)
success_search = submit_form(session,form)
job_name = success_search%>%html_nodes("a[data-tn-element=jobTitle]")%>%html_text()
