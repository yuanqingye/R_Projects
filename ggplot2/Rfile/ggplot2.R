library(ggplot2)
library(reshape2)
library(plyr)
require(scales)
p = ggplot(data = mpg, mapping = aes(x = cty,y = hwy))
p + geom_point()

summary(p)
summary(p+geom_point())

p = ggplot(data = mpg, mapping = aes(x = cty,y = hwy,colour = factor(year)))
p + geom_point()

p + geom_point() + stat_smooth()

p = ggplot(data = mpg, mapping = aes(x = cty, y = hwy))
p + geom_point(aes(colour = factor(year))) + stat_smooth()

p + geom_point(aes(colour = factor(year),size = displ)) + 
    stat_smooth() + scale_color_manual(values = c("blue2","red4"))

p + geom_point(aes(colour = factor(year),size = displ),alpha = 0.5,position = "jitter")+ 
  stat_smooth() + scale_color_manual(values = c("green","purple")) + scale_size_continuous(range = c(2,5))

p + geom_point(aes(colour = factor(year),size = displ),alpha = 0.5,position = "jitter")+
  stat_smooth() + scale_color_manual(values = c("orange","blue")) + scale_size_continuous(range = c(2,5))+
  coord_cartesian(xlim = c(15,25),ylim = c(15,40))

p + geom_point(aes(colour=class,size = displ),alpha=0.5,position = "jitter") + stat_smooth()+
  scale_size_continuous(range(2,5)) + facet_wrap(~year,ncol = 1)

p + geom_point(aes(colour=class,size = displ),alpha=0.5,position = "jitter") + stat_smooth()+
  scale_size_continuous(range(2,5)) + facet_wrap(~year,ncol = 1) + 
  labs(y = "每加仑高速公路行驶距离","每加仑城市公路行驶距离")+guides(size = guide_legend(title = "排量"),
  colour = guide_legend(title = "车型",override.aes = list(size = 5)))

p = ggplot(mpg,aes(x = hwy))
p + geom_histogram()

p = ggplot(mpg,aes(x = cty))
p + geom_histogram()

p + geom_histogram(aes(fill = factor(year),y = ..density..),alpha = 0.3,colour = "purple") +
  stat_density(geom = "line",position = "identity",size = 1.5,aes(colour = factor(year)))+
  facet_wrap(~year,ncol = 1)

p = ggplot(data = mpg,mapping = aes(x = class))
p + geom_bar()
p + geom_bar(aes(fill = class))

class2 = mpg$class
class2 = reorder(class2,class2,length)
mpg$class2 = class2
p = ggplot(mpg,aes(x = class2))
p + geom_bar(aes(fill = class2))

#重合图
p = ggplot(mpg,aes(class2,fill = factor(year)))
p + geom_bar(position = "identity",alpha = 0.5)

#并排方式
p + geom_bar(position = "dodge") 

#叠加方式
p + geom_bar(position = "stack")

#察看比例
p + geom_bar(position = "fill")

#分图展示
p + geom_bar(aes(fill = class2)) + facet_wrap(~year)

#饼图展示
p = ggplot(mpg,aes(x= factor(1),fill = factor(class))) + geom_bar(width = 1)
p + coord_polar(theta = "y")

p = ggplot(mpg,aes(class,hwy,fill = class))
p + geom_boxplot()

dir = cut_interval(runif(100,0,360),n = 16)
mag = cut_interval(rgamma(100,15),4)
sample = data.frame(dir = dir,mag = mag)
p = ggplot(sample,aes(x = dir,y =..count..,fill = mag))
p + geom_bar() + coord_polar()

x = seq(from =0,to = 2*pi, by = 0.01)
y = sin(x)
p = ggplot(data.frame(x,y),aes(x,y))
p + geom_area(fill = "blue") + geom_text(parse = T,aes(x = pi/2,y = 0.3,label = 'integral(sin(x)*dx,0,pi)'))+geom_line()

library(quantmod)
getSymbols('^SSEC',src = "yahoo",from = "1997-01-01")
close = (Cl(SSEC))
time = index(close)
value = as.vector(close)
yrng = range(value)
xrng = range(time)
data = data.frame(start = as.Date(c("1997-01-01","2003-01-01")),end = as.Date(c("2002-12-30","2012-01-20")),core = c("jiang","hu"))
timepoint <- as.Date(c('1999-07-02','2001-07-26','2005-04-29','2008-01-10','2010-03-31'))
events <- c('证券法实施','国有股减持','股权分置改革','次贷危机爆发','融资融券试
点') 
data2 = data.frame(timepoint,events,stock = value[time %in% timepoint])

p = ggplot(data.frame(time,value),aes(time,value))
p + geom_line(size = 1,colour = 'turquoise4')+
  geom_rect(alpha = 0.2,aes(NULL,NULL,xmin = start,xmax = end,fill = core),ymin = yrng[1],ymax = yrng[2],data = data)+
  scale_fill_manual(values = c('blue','red'))+ 
  geom_text(aes(timepoint, stock, label = events),data = data2,vjust = -2,size = 5)+ 
  geom_point(aes(timepoint, stock),data = data2,size = 5,colour = 'red',alpha=0.5) 

p + geom_line(size=1,colour='turquoise4')+ 
  geom_rect(alpha=0.2,aes(x=NULL, y = NULL,xmin = start, xmax = end, fill = core),ymin = 
              yrng[1],ymax=yrng[2],data = data)+ 
  scale_fill_manual(values = c('blue','red'))

nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
nba$Name <- with(nba, reorder(Name, PTS))
nba.m <- melt(nba)
nba.m <- ddply(nba.m, .(variable), transform,rescale = rescale(value))
(p <- ggplot(nba.m, aes(variable, Name)) + geom_tile(aes(fill = rescale),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue"))


#漏斗图
library(dplyr) #数据转换包
library(tidyr) #数据转换包
library(splines) #数据差值包
set.seed(123) #设定随机种子，保证做的图和这里的一样
df <- data.frame(
  var=LETTERS[1:10], #字母A-J
  id=1:10, #数字1-10
  a=runif(10), #10个随机数
  b=runif(10), #10个随机数
  c=runif(10), #10个随机数
  stringsAsFactors = F #不转换为因子
)
df_tmp4<-df %>% select(1:3) %>% 
  arrange(a) %>% 
  mutate(new_id=1:10,
         ymin = (1-a)/2,
         ymax = a+(1-a)/2,
         mid = 0.5)
ggplot(df_tmp4,aes(new_id,mid))+
  geom_linerange(aes(ymin=ymin,ymax=ymax,
                     colour=factor(new_id)),
                 size=10,
                 alpha=1,show.legend = F)+
  scale_x_continuous(breaks = 1:10,
                     labels = df_tmp4$var)+
  coord_flip()

ggplot(df_tmp4,aes(new_id,mid))+
  #  geom_step(colour="grey50")+
  geom_crossbar(aes(ymin=ymin,ymax=ymax),
                size=0,
                fill="skyblue",
                colour="grey50",
                width=1)+
  scale_x_continuous(breaks = 1:10,
                     labels = df_tmp4$var)+
  coord_flip()

ggplot(df_tmp4,aes(new_id,mid))+
  #  geom_step(colour="grey50")+
  geom_crossbar(aes(ymin=ymin,ymax=ymax),
                size=0,
                fill="skyblue",
                colour="grey50",
                width=1)+
  scale_x_continuous(breaks = 1:10,
                     labels = df_tmp4$var)+geom_text(aes(x=new_id,y=mid,label=paste0(round(a*100),"%")))+
  coord_flip()


df <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

p <- ggplot(df, aes(trt, resp, colour = group))
p + geom_linerange(aes(ymin = lower, ymax = upper))
p + geom_pointrange(aes(ymin = lower, ymax = upper))
p + geom_crossbar(aes(ymin = lower, ymax = upper), width = 0.2)
p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)

ggplot() +
  layer(
    data = diamonds, mapping = aes(x = carat, y = price),
    geom = "area", stat = "identity", position = "identity"
  ) +
  scale_y_continuous() +
  scale_x_continuous() +
  coord_cartesian()

ggplot(data = diamonds, aes(x = carat, y = price)) + 
  layer(
    geom = "point", stat = "identity", position = "identity"
  )
