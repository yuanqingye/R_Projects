library(readxl)
library(lattice)
library(e1071)
library(data.table)

shanghai2016 = read_xlsx("~/data/shanghai.xlsx",sheet = "Sheet5")
shanghai2015 = read_xlsx("~/data/shanghai.xlsx",sheet = "Sheet4")

trainset.2015 = shanghai2015[,-1]
trainset.2015 = na.omit(trainset.2015)
names(trainset.2015) = c('area','rent','avg_rent','partial','p_density','avg_gdp','pred_avg_rent','pred_rent','bias')
trainset = trainset.2015[,c(1,2,4,5,6)]
svm.model.1 <- svm(rent ~ ., data = trainset)
svm.model.100 <- svm(rent ~ ., data = trainset, cost = 100, gamma = 1)
svm.model.1000 <- svm(rent ~ ., data = trainset, cost = 1000, gamma = 1)

testset.2016 = shanghai2016[,-1]
testset.2016 = na.omit(testset.2016)
names(testset.2016) = c('area','rent','avg_rent','partial','p_density','avg_gdp','pred_avg_rent','pred_rent','bias')
testset = testset.2016[,c(1,2,4,5,6)]
testset = data.table(testset)
svm.pred <- predict(svm.model, testset[,-2])
cbind(svm.pred,testset$rent)
crossprod(svm.pred - unlist(testset[,2])) / nrow(testset)

svm.pred.1 <- predict(svm.model.1, testset[,-2])
svm.pred.100 <- predict(svm.model.100, testset[,-2])
svm.pred.1000 <- predict(svm.model.1000, testset[,-2])

m_1 = cbind(svm.pred.1,testset$rent)
m_100 = cbind(svm.pred.100,testset$rent)
m_1000 = cbind(svm.pred.1000,testset$rent)

error_1 = crossprod(svm.pred.1 - unlist(testset[,2])) / nrow(testset)
error_100 = crossprod(svm.pred.100 - unlist(testset[,2])) / nrow(testset)
error_1000 = crossprod(svm.pred.100 - unlist(testset[,2])) / nrow(testset)

plot(svm.model.1,testset,area~p_density)


plot(trainset$partial,trainset$rent)
text(trainset$partial,trainset$rent-0.025*(range(trainset$rent)[[2]]-range(trainset$rent)[[1]]),labels = paste0(shanghai2015$名字[1:6],'2015'))

points(testset$partial,testset$rent,pch = '+')
text(testset$partial,testset$rent-0.025*(range(testset$rent)[[2]]-range(testset$rent)[[1]]),labels = paste0(shanghai2016$名字[1:7],'2016'))

plot(trainset$p_density,trainset$rent)
text(trainset$p_density,trainset$rent-0.025*(range(trainset$rent)[[2]]-range(trainset$rent)[[1]]),labels = paste0(shanghai2015$名字[1:6],'2015'))

persp(trainset$p_density, trainset$partial, trainset$rent, theta = 30, phi = 30, expand = 0.5, col = drapecol(z))

library(scatterplot3d)
rent3d = scatterplot3d(trainset$partial,trainset$p_density,trainset$rent,pch = "+")
rent3d$points3d(testset$partial,testset$p_density,testset$rent)

trainset = cbind(trainset,2015)
colnames(trainset)[ncol(trainset)] = "year"
testset = cbind(testset,2016)
colnames(testset)[ncol(testset)] = "year"
plot3dset = rbind(trainset,testset)

library(plotly)
plot_ly(plot3dset, x = ~partial, y = ~p_density, z = ~rent, color = ~year, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'percentage'),
                      yaxis = list(title = 'people density'),
                      zaxis = list(title = 'rent')))

plot_ly(plot3dset, x = ~area, y = ~avg_gdp, z = ~rent, color = ~year, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'area'),
                      yaxis = list(title = 'people income'),
                      zaxis = list(title = 'rent')))
