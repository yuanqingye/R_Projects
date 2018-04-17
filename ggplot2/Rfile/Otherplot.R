
proxy_url <- "http://127.0.0.1:16823/"
Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)

?funnel.plot

### Read and format the data (binary)
data = read.csv(url("http://www.statistica.it/gianluca/bmeta/Data-bin.csv"))

### List data for binary outcome 
data.list <- list(y0=data$y0,y1=data$y1,n0=data$n0,n1=data$n1) 

### Select random-effects meta-analysis with t-distribution prior for binary
### data
x <- bmeta(data.list, outcome="bin", model="std.dt", type="ran")

### using output from bmeta to produce funnel plot 
funnel.plot(x)

### using output from bmeta and specify title of the plot 
funnel.plot(x,title="funnel plot")

### using output from bmeta and specify the limit of x-axis and title
funnel.plot(x,title="funnel plot",xlim=c(-2,1))

install.packages("meta")
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c,
                 data=Olkin95, subset=c(41,47,51,59),
                 studlab=paste(author, year),
                 sm="RR", method="I")
#
# Radial plot
#
radial(meta1, level=0.95)
oldpar <- par(mfrow=c(2, 2))
#
# Funnel plots
#
funnel(meta1)
funnel(meta1$TE, meta1$seTE, sm="RR",
       comb.fixed=TRUE, level=0.95)
#
# Funnel plot with confidence intervals,
# fixed effect estimate and contours
#
cc <- funnel(meta1, comb.fixed=TRUE,
             level=0.95, contour=c(0.9, 0.95, 0.99))$col.contour
legend(0.05, 0.05,
       c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"), fill=cc)
#
# Contour-enhanced funnel plot with user-chosen colours
#
funnel(meta1, comb.fixed=TRUE,
       level=0.95, contour=c(0.9, 0.95, 0.99),
       col.contour=c("darkgreen", "green", "lightgreen"),
       lwd=2, cex=2, pch=16, studlab=TRUE, cex.studlab=1.25)
legend(0.05, 0.05,
       c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
       fill=c("darkgreen", "green", "lightgreen"))
par(oldpar)

# install.packages("treemap")
library(dplyr)
library(treemap)
data("business")
str(business)
treemap(business,index=c("NACE1"),vSize = "turnover",title = "树图")
a<-business%>%group_by(NACE1)%>%summarise(sum1=sum(turnover))%>%arrange(-sum1)
# write.csv(a,"C:\\Users\\Administrator\\Desktop\\第二课\\a.csv")
?treemap()
treemap(business,index=c("NACE1","NACE2"),vSize = "turnover",vColor="NACE3",title = "树图")
treemap(business,index=c("NACE1","NACE2","NACE3"),vSize = "turnover",title = "树图")
treemap(business,index=c("NACE1","NACE2"),vSize = "turnover",vColor="NACE3",title = "树图")
treemap(business,index=c("NACE1","NACE2"),vSize = "turnover",type="index",title = "树图")

# install.packages("heatmap3")
x  <- as.matrix(mtcars)
rc <- rainbow(nrow(x), start = 0, end = .3)
cc <- rainbow(ncol(x), start = 0, end = .3)
hv <- heatmap(x, col = cm.colors(256), scale = "column",
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "specification variables", ylab =  "Car Models",
              main = "heatmap(<Mtcars data>, ..., scale = \"column\")")


library(githubinstall)
githubinstall("heatmap")
library(heatmap)
data=matrix(1:25,nrow=5,ncol=5,Rrow=FALSE,dimnames=list(c('A1','A2','A3','A4','A5'),c('B1','B2','B3','B4','B5')))
heatmap(data,xlab='test1',ylab='test2',main='test',col=abs(data/3))