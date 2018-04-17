# Load the party package. It will automatically load other dependent packages.
library(party)

# Create the input data frame.
input.dat <- trainset

# Create the tree.
output.tree <- ctree(
  rent ~ ., 
  data =  input.dat)

output.tree.2 = ctree(
  rent ~ city_level+area+num_ordered_custom+dist_from_highway+dist_from_big_road+position+dist_from_subway+dist_from_business_district+num_business_within_2500,
  data =  input.dat
  )

output.tree.3 = ctree(
  rent ~ city_level+dist_from_highway+dist_from_big_road+position+dist_from_subway+dist_from_business_district+num_business_within_2500,
  data =  input.dat
)


# Plot the tree.
plot(output.tree)
plot(output.tree.2)
plot(output.tree.3)

library(rpart)

model<-rpart(rent~.,
             data=input.dat,method="class",control=5,
             parms=list(split = "information"))



plot(model,margin=0.2)
text(model,use.n=T,all=T,cex=0.9)

plot(as.party(model), type="simple")
plot(as.party(model))

rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01, 
              maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
              surrogatestyle = 0, maxdepth = 30, ...)

dtm<-rpart(rent~., input.dat, method="class", minsplit=2, minbucket=1)
printcp(dtm)
plot(dtm)
text(dtm,use.n=T,all=T,cex=0.9)

# dtm<-rpart(sign~total_amount+province_id+serial_name, modified_order_522, method="class", minsplit=2, minbucket=1)

library(rpart.plot)
rpart.plot(dtm)

png(filename = '~/Rimage/dtm.png',width = 1100, height = 800)
prp(dtm)
dev.off()

library(partykit)
rpk <- as.party(dtm)
plot.party(rpk)
