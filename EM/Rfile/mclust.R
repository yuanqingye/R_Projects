library(mclust)
?mclustModelNames
#clustering
data(diabetes)
class <- diabetes$class
table(class)
X <- diabetes[,-1]
head(X)
clPairs(X, class)

BIC <- mclustBIC(X)
plot(BIC)

summary(BIC)
mod1 <- Mclust(X, x = BIC)
summary(mod1, parameters = TRUE)
mod1$parameters$mean
mod1$z
mod1$uncertainty
mod1$classification
plot(mod1, what = "classification")

#class <- diabetes$class
table(class, mod1$classification)
adjustedRandIndex(class, mod1$classification)
mod1$classification

par(mfrow = c(2,2))
plot(mod1, what = "uncertainty", dimens = c(1,1), main = "")
plot(mod1, what = "uncertainty", dimens = c(1,2), main = "")
plot(mod1, what = "uncertainty", dimens = c(2,1), main = "")
par(mfrow = c(1,1))

ICL = mclustICL(X)
summary(ICL)
## Best ICL values:
##              VVV,3       EVE,6       EVE,7
## ICL      -4770.169 -4797.38232 -4797.50566
## ICL diff     0.000   -27.21342   -27.33677
plot(ICL)

LRT = mclustBootstrapLRT(X, modelName = "VVV")
LRT
plot(LRT)

data(iris)
class <- iris$Species
table(class)
X <- iris[,1:4]
head(X)

mod2 <- MclustDA(X, class, modelType = "EDDA")
summary(mod2)
plot(mod2, what = "classification")
plot(mod2, what = "scatterplot")

unlist(cvMclustDA(mod2, nfold = 10)[2:3])


data(banknote)
class <- banknote$Status
table(class)
## class
## counterfeit     genuine 
##         100         100
X <- banknote[,-1]
head(X)

mod3 <- MclustDA(X, class)
summary(mod3)

data(acidity)
mod4 <- densityMclust(acidity)
summary(mod4)

plot(mod4, what = "BIC")
plot(mod4, what = "density", data = acidity, breaks = 15)

plot(mod4, what = "diagnostic", type = "cdf")

data(faithful)
mod5 <- densityMclust(faithful)
summary(mod5)

plot(mod5, what = "density")
plot(mod5, what = "density", type = "image", col = "dodgerblue3", grid = 100)
plot(mod5, what = "density", type = "persp")

mod1dr <- MclustDR(mod1)
summary(mod1dr)
plot(mod1dr, what = "pairs")
plot(mod1dr, what = "boundaries", ngrid = 200)

msEst <- mstep(modelName = "EEE", data = iris[,-5], 
               z = unmap(iris[,5]))
esEst = estep(modelName = "EEE", data = iris[,-5], 
              z = unmap(iris[,5]))
result_mem_iris = em(modelName = msEst$modelName, data = iris[,-5],
                    parameters = msEst$parameters)
result_eme_iris = me(modelName = esEst$modelName, data = iris[,-5],
                     z = esEst$z)
result_mme_iris = me(modelName = msEst$modelName, data = iris[,-5],
                     z = msEst$z)
result_eem_iris = em(modelName = esEst$modelName, data = iris[,-5],
                     parameters = esEst$parameters)
result_me_iris = me(modelName = "EEE", data = iris[,-5],
                     z = unmap(iris[,5]))
source2("~/R_Projects/abnormal_activity_analysis/Rfile/outdecider.R",6,23)
mem_result = ifelse(result_mem_iris$z[,1]-result_mem_iris$z[,2]>0,0,1)
mem_list = calf1(iris[,5],mem_result)
mem_AUC = calAUC(iris[,5],mem_result)

eme_result = ifelse(result_eme_iris$z[,1]-result_eme_iris$z[,2]>0,0,1)
eme_list = calf1(iris[,5],eme_result)
eme_AUC = calAUC(iris[,5],eme_result)

mme_result = ifelse(result_mme_iris$z[,1]-result_mme_iris$z[,2]>0,0,1)
mme_list = calf1(iris[,5],mme_result)
mme_AUC = calAUC(iris[,5],mme_result)

eem_result = ifelse(result_eem_iris$z[,1]-result_eem_iris$z[,2]>0,0,1)
eem_list = calf1(iris[,5],eem_result)
eem_AUC = calAUC(iris[,5],eem_result)

me_result = ifelse(result_me_iris$z[,1]-result_me_iris$z[,2]>0,0,1)
me_list = calf1(iris[,5],me_result)
me_AUC = calAUC(iris[,5],me_result)

irisMclust <- Mclust(iris[,1:4])
summary(irisMclust)
ll = logLik(irisMclust)

faithfulModel <- Mclust(faithful)
Dens <- dens(modelName = faithfulModel$modelName, data = faithful,
             parameters = faithfulModel$parameters)
Dens
sum(log(Dens))
logLik(faithfulModel)
