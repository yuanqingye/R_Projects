spending <- data.frame(c("Social protection","Health","Education","Other","Defence","Debt interest","Transport","Housing\n & environment","Public order","Social\n services","Industry"),
                       c(245,149,102,50,48,46,37,36,34,32,23),
                       c(802,802,802,802,802,802,802,802,802,802,802))
names(spending) <- c("Expenditure","Spending","Total")
income <- data.frame(c("Income tax","VAT","National Insurance", "Other taxes","Borrowing","Other (non taxes)","Corporation Tax","Excise duties","Council tax","Business\n rates"),
                     c(175,143,130,80,58,54,52,48,32,30),
                     c(802,802,802,802,802,802,802,802,802,802))
names(income) <- c("Source","Income","Total")

# install.packages("packcircles")
# install.packages("extrafont")
# install.packages("extrafontdb")
library("packcircles")
library("ggplot2")
options(scipen = 999)
library(extrafont)
library(extrafontdb)
loadfonts()
fonts()

t <- theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.title=element_blank())

# circle areas
areas <- income$Income 
areas2 <- spending$Spending
# income
packing1 <- circleProgressiveLayout(areas)
dat1 <- circleLayoutVertices(packing1)
# spending
packing2 <- circleProgressiveLayout(areas2) 
dat2 <- circleLayoutVertices(packing2)

dat <- rbind(
  cbind(dat1, set = 1),
  cbind(dat2, set = 2) )
p <- ggplot(data = dat, aes(x, y)) + 
  geom_polygon(aes(group = id, fill = -set), 
               colour = "black", show.legend = FALSE) + theme(text = element_text(family="Browallia New", size = 50)) +
  coord_equal() + ggtitle("Income and expenditure for the UK Government") +
  scale_colour_manual(values = c("white","White")) +
  facet_grid(~set, 
             labeller = as_labeller(
               c('1' = "Income", '2' = "Expenditure")))

p

