setClass('employee',representation(name = 'character',salary = 'numeric',union = 'logical'))

setMethod("show","employee",function(object){
  inorout = ifelse(object@union,"is","is not")
  cat(object@name,"has a salary of",object@salary,"and",inorout,"in the union","\n")
})

j = list(name = "joe",salary = 55000,union = T)
class(j) = "employee"
attributes(j)

print.employee = function(wrk){
  cat(wrk$name,"\n")
  cat("salary",wrk$salary,"\n")
  cat("union member",wrk$union,"\n")
}

k = list(name = "Kate",salary = 68000,union = F,hrsthismonth = 2)
class(k) = c("hrlyemployee","employee")

ls()

ls(pattern="jo")

z = rnorm(10000)
hz = hist(z)
plot(hz)

save(hz,'hzfile.RData')

exists('hz')

x = c(1,2,3)
y = c(1,3,8)
lmout = lm(y~x)

stu = list(name="Lily", age=20, GPA=3.6)

class(stu) = "student"

print.student <- function(obj) {
  cat(obj$name, "\n")
  cat(obj$age, "years old\n")
  cat("GPA:", obj$GPA, "\n")
}

s2<- list(name="John", age=21, GPA=3.5, country="France")
class(s2) = c("InternationalStudent","student")

print.InternationalStudent = function(obj){
  cat(obj$name, "is from", obj$country, "\n")
}
