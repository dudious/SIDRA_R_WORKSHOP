### load package
library(glmnet)

### load data
myData<-read.csv('./Data/EY2.csv', header = TRUE, row.names = 1)

### check first rows/columns & dimension
myData[1:5,1:3]
dim(myData)

### create outcome variables (sex and cmv) and predictors (genes-standardize)
sex = myData[2,]
cmv = myData[3,]
genes = as.matrix(scale(myData[5:8004,]))

### run cv.glmnet & plot results; default gaussian, deviance, 10-fold cv, alpha = 1
t <- data.frame(repetition = numeric(),
                Lambda = numeric()
                )
for (i in 1:20) {
  fit1 <- cv.glmnet(x = t(genes), y = t(sex), family = 'binomial', type = 'class')
  t[i,1]<- i
  t[i,2]<- fit1$lambda.min
}
class(t$Lambda)
summary (t$Lambda)

plot(fit1)

### check object attributes
attributes(fit1)

### check lambda and lambda.min
fit1$lambda
fit1$lambda.min

### find features and position in the matrix selected at lambda min, by observing s 
which((fit1$glmnet.fit$beta[,20]!=0),20)

### find beta at lambda min by observing s --> get beta of all features at s = 20 that is different than zero 
fit1$glmnet.fit$beta[which(fit1$glmnet.fit$beta[,20]!=0),20]

### get features and beta at lambda min 
fit1$glmnet.fit$beta[which(fit1$glmnet.fit$beta[,which(fit1$lambda==fit1$lambda.min)]!=0),which(fit1$lambda==fit1$lambda.min)]

### change alpha towards ridge
fit2 <- cv.glmnet(x = t(genes), y = t(sex), family = 'binomial', type = 'auc', alpha = 0.2,nfolds=3)
plot(fit2)

### change nfolds
fit3 <- cv.glmnet(x = t(genes), y = t(sex), family = 'binomial', type = 'class', alpha = 0.2, nfold = 5)
plot(fit3)

### change alpha and type to AUC
fit4 <- cv.glmnet(x = t(genes), y = t(sex), family = 'binomial', type = 'auc', alpha = 0.5, nfold = 5)
plot(fit4)

### Predict CMV positivity and change penaly factor
### alpha = 1 for first 100 features, alpha = 0.1 for all others 
fit5 <- cv.glmnet(x = t(genes), y = t(cmv), family = 'binomial', type = 'class', nfold = 5,
                  penalty.factor = c(rep(1, 100), rep(0.1, 7900)))
plot(fit5)

### RUN regression and get signifcance of betas by FDR
### load functions
source('./R_scripts/regressNPermute.R')
source('./R_scripts/estimateFDRbyPerm.R')
source('./R_scripts/utility_fxns.R')

### load the neu data file
neuData<-read.csv('./Data/gabNeut.csv', header = TRUE, row.names = 1)

### create outcome variables (ys) and predictors (xs)
xNeu <- neuData[1:2,]
yNeu <- neuData[3:244,]

### run linear regression with 500 permutation on the outcomes
fit_reg<-regressNPermuteFast(t(xNeu), t(yNeu), numPerms = 500)

### compute q-values by analzying p-values estimated from permuted data
fdr_fit_reg<-fdrByPerm(LMcoef=fit_reg$rndCoef,realPv=fit_reg$r$pv,thres=DefThes)

### observe p-values from the regression , select the very  significant ones 
fit_reg$r$pv
fit_reg$r$pv[fit_reg$r$pv[,3]<0.005,]

### observe q-values from the permutation procedure
fdr_fit_reg$q

### plot FDR as a function of # features and add dashed line at FDR = 10%
plot(sort(fdr_fit_reg$q[,3]*100), type = "n", xlab = "# features called significant", ylab = "FDR")
lines(sort(fdr_fit_reg$q[,3]*100))
abline(h=10,col=4,lty=2)

### find features and q-value from all features at column 3 that are less or equal to 0.1 (FDR <=10%)
fdr_fit_reg$q[which(fdr_fit_reg$q[,3]<=0.1),3]

### plot FDR as a function of the regression coefficients add dashed line at FDR = 10%
plot(fdr_fit_reg$q[,3]*100~fit_reg$r$beta[,3], xlab = "Regression cofficient", ylab = "FDR")
abline(h=10,col=4,lty=2)

### create table with betas (1st column) and q-values (second column)
beta_q <- cbind(fit_reg$r$beta[,3], fdr_fit_reg$q[,3])
