library(readxl)
booth_data = read_xlsx("./data/booth_rental_shanghai.xlsx")
booth_dt = booth_data[,c("floor_name","booth_grade","rentable_area","ctg3","brand_score")]
booth_dt$floor_name = as.numeric(as.factor(booth_dt$floor_name))
booth_dt$ctg3 = as.numeric(as.factor(booth_dt$ctg3))
pca.out<-prcomp(booth_dt,scale=TRUE)
pca.out
summary(pca.out)
names(pca.out)
biplot(pca.out,scale = 0, cex=0.65)
