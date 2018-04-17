dim(USArrests)
dimnames(USArrests)
apply(USArrests,2,mean)
apply(USArrests,2,var)
pca.out<-prcomp(USArrests,scale=TRUE)
pca.out
summary(pca.out)
names(pca.out)
biplot(pca.out,scale = 0, cex=0.65)
