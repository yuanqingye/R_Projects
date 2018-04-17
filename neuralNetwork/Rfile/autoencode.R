library(autoencoder)

autoencode.object = autoencode(as.matrix(train.scaled), as.matrix(test.scaled), nl = 5, 
                               N.hidden =c(15,15,8), unit.type = c("logistic", "tanh"),
           lambda = 0.0002, beta = 6, rho = 0.01, epsilon = 0.001, optim.method = c("BFGS", "L-BFGS-B", "CG"),
           rel.tol=sqrt(.Machine$double.eps), max.iterations = 2000,
           rescale.flag = F, rescaling.offset = 0.001)

cat("autoencode(): mean squared error for training set: ",round(autoencode.object$mean.error.training.set,3),"\n")

X.output <- predict(autoencode.object, X.input=as.matrix(train.scaled), hidden.output=FALSE)$X.output
View(rbind(X.output[1,],train.scaled[1,]))
data('autoencoder_Ninput=100_Nhidden=100_rho=1e-2')
# visualize.hidden.units(autoencoder.object,Nx.patch=100,Ny.patch=100)
# visualize.hidden.units(autoencode.object,Nx.patch = 22,Ny.patch=22)
