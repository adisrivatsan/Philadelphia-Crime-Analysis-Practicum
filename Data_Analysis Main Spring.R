setwd(dir='Documents/Penn/SeniorFall/Practicum Project/Philadelphia-Crime-Analysis-Practicum')
#################### INSTALLATION #############################################
install.packages('logOfGamma')
library(logOfGamma)
library(MASS)
library(rpart)
library(dplyr)
library(olsrr)
library(glmnet)
library(dplyr)

crimeDf = read.csv(file = 'ProcessedDataSets/TotalInfoDF_03_12_2019.csv')
weatherDf = read.csv(file='ProcessedDataSets/weatherPhiladelphiaData20062015.csv')

totalDf = merge(crimeDf,weatherDf,by.x = c('Year','Month'), by.y = c('Year','Month'))

## Filtering columns
nonNumericCols = c('YearTract','violent','nonviolent','Total_y','vice','X.y','X.x','count_inf',
                   'hispanic','culture_recreation')
totalFilteredDf = totalDf[,!(names(totalDf) %in% nonNumericCols)]

tractNum = length(unique(totalDf$tract))

XtrainTract = totalFilteredDf[totalDf$Year %in% 2006:2013,]
XtestTract = totalFilteredDf[totalDf$Year %in% 2014:2015,]

Xtrain = XtrainTract[,!(names(XtrainTract) %in% c('tract'))]
Xtest = XtestTract[,!(names(XtestTract) %in% c('tract'))]

colnames(Xtrain)
hist(Xtrain$Y,main = 'Histogram of Y variable',xlab='crime counts per tract per month')
hist(log(Xtrain$Y))
############################### Linear Model  #######################################
fullLinearModelAT = lm(Y ~ ., data = Xtrain)
summary(fullLinearModelAT)
#plot(fullLinearModelAT)
##  Eval  
inSampLin = mean((Xtrain$Y - predict(fullLinearModelAT))^2)
outSampLin = mean((Xtest$Y - predict(fullLinearModelAT,newdata = Xtest))^2)
c(inSampLin,outSampLin)
############################### Poisson Model  #######################################
fullPoisModelAT = glm(Y ~ ., data=Xtrain, family=poisson())

nbdMod = glm.nb(Y ~ ., data=Xtrain)
summary(nbdMod)
summary(fullPoisModelAT)
inSampPois = mean((Xtrain$Y - exp(predict(fullPoisModelAT)))^2)
outSampPois = mean((Xtest$Y - exp(predict(fullPoisModelAT,newdata = Xtest)))^2)
valDf = cbind(valDf,evaluateErrPois(nbdMod,Xtrain,Xtest))
valNames = c('linear','Poisson','Decision Tree','Lasso','Log-Linear','NBD','Random Forest','BART')
names(valDf) = valNames
row.names(valDf) = c('In-sample','Out-Sample')
############################### DT Model  #######################################
install.packages('rpart.plot')
library(rpart.plot)

fullDTModelAT = rpart(Y ~ .,data=Xtrain,method='anova',
                      control =rpart.control(minsplit = 5,maxdepth = 25) )
summary(fullDTModelAT)
inSampDT = mean((Xtrain$Y - predict(fullDTModelAT))^2)
outSampDT = mean((Xtest$Y - predict(fullDTModelAT,newdata = Xtest))^2)
rpart.plot(fullDTModelAT)

############################### Error Plot  #######################################

plot(c(inSampLin,inSampPois,inSampDT))
plot(c(outSampLin,outSampPois,outSampDT))
valMat = rbind(c(inSampLin,inSampPois,inSampDT),c(outSampLin,outSampPois,outSampDT))
barplot(valMat,main = 'Figure 1: MSE Plot Basic Models',names.arg = c('Linear','Poisson','DT'),
        xlab = 'Model Type',ylab = 'Prediction Accuracy',col=c('red','blue'))
legend("topleft", c('in-samp','out-samp'), cex = 0.7, fill = c('red','blue'))

valDf = data.frame(valMat)
names(valDf) = c('linear','Poisson','DT')
valDf
############################### Visualizing Linear Model #######################################


fullLinearModelAT = lm(Y ~ ., data = Xtrain[,!(names(Xtrain) %in% 'tract')])
summary(fullLinearModelAT)
#plot(fullLinearModelAT)
##  Eval  
inSampLin = mean((Xtrain$Y - predict(fullLinearModelAT))^2)
outSampLin = mean((Xtest$Y - predict(fullLinearModelAT,newdata = Xtest))^2)

evaluateError = function(model, trainData, testData) {
  inSampErr = mean((trainData$Y - predict(model))^2)
  outSampErr = mean((testData$Y - predict(model,newdata = testData))^2)
  c(inSampErr,outSampErr)
}
evaluateErrPois = function(model,trainData,testData) {
  inSampErr = mean((trainData$Y - exp(predict(model)))^2)
  outSampErr = mean((testData$Y - exp(predict(model,newdata = testData)))^2)
  c(inSampErr,outSampErr)
}
### Looking at 2013, aggregate all tracts

visualizeModelYear = function(model,trainData,year,titleName) {
  Xvisual = trainData
  Xvisual['pred'] = predict(model)
  XYear = Xvisual[Xvisual$Year == year,]
  XYearAgg = XYear %>% group_by(tract) %>% summarise_all(funs(sum))
  
  hist(XYearAgg$pred,col='blue',main=titleName,xlab = 'Predicted (blue) vs. Actual(red)')
  hist(XYearAgg$Y,col='red',add=T)
  
}
visualizeModelOutSampYear = function(model,testData,year,titleName) {
  Xvisual = testData
  Xvisual['pred'] = predict(model,newdata = testData)
  XYear = Xvisual[Xvisual$Year == year,]
  XYearAgg = XYear %>% group_by(tract) %>% summarise_all(funs(sum))
  
  hist(XYearAgg$pred,col='blue',main=titleName,xlab = 'Predicted (blue) vs. Actual(red)')
  hist(XYearAgg$Y,col='red',add=T)
  
}
visualizeModelDataYear = function(predData,trainData,year) {
  Xvisual = trainData
  Xvisual['pred'] = predData
  XYear = Xvisual[Xvisual$Year == year,]
  XYearAgg = XYear %>% group_by(tract) %>% summarise_all(funs(sum))
  
  hist(XYearAgg$pred,col='blue')
  hist(XYearAgg$Y,col='red',add=T)
  
}
### Looking at tract 
visualizeModelTract = function(model,trainData,tract,title) {
  Xvisual = trainData
  Xvisual['pred'] = predict(model)
  Xtract = Xvisual[Xvisual$tract == tract,]
  plot(Xtract$Y,col='red',main = title,xlab = 'Time',ylab = 'Crime Counts')
  lines(Xtract$pred,col='blue',type='line')
}

par(mfrow=c(2,2))
visualizeModelYear(fullLinearModelAT,XtrainTract,2013,titleName = '2013 Aggregate')
visualizeModelYear(fullLinearModelAT,XtrainTract,2012,titleName = '2012 Aggregate')
visualizeModelYear(fullLinearModelAT,XtrainTract,2011,titleName = '2011 Aggregate')
visualizeModelYear(fullLinearModelAT,XtrainTract,2010,titleName = '2010 Aggregate')

par(mfrow = c(1,1))
visualizeModelOutSampYear(fullLinearModelAT,XtestTract,2014,titleName = '2014 Aggregate')
visualizeModelTract(fullLinearModelAT,XtrainTract,29000,'Linear Model for Census Tract 2900 ')
############################### Covariate Testing #######################################

## baseline-
evaluateError(fullLinearModelAT,Xtrain,Xtest)
summary(fullLinearModelAT)

log0 = function(data) {
  data[data <=0] = 1
  log (data)
}
XTrC = Xtrain 
XtestC = Xtest
XTrC$Income = log0(XTrC$Income)
XtestC$Income = log0(XtestC$Income)
LogIncomeModel = lm(Y ~., data = XTrC)
evaluateError(fullLinearModelAT,XTrC,XtestC)



stepWiseModelPVal = ols_step_forward_p(fullLinearModelAT)

steWiseModelAIC = ols_step_forward_aic(fullLinearModelAT)

#### Ridge Regression

genMatirx = as.matrix(Xtrain %>% select(-one_of(c('Y'))))
crossval <-  cv.glmnet(x = genMatirx, y = Xtrain[,'Y'])
lamMin = crossval$lambda.min
ridgeSampleModel = glmnet(genMatirx, 
                          Xtrain[,'Y'], alpha = 0,lambda = lamMin)
summary(ridgeSampleModel)
inSampRidge = predict(ridgeSampleModel,newx = genMatirx)
outSampRidge = predict(ridgeSampleModel,newx = as.matrix(
  XtestSamp[,monthPred]))

#### Lasso Regression ####


lassoTotalModel = glmnet(genMatirx,alpha = 1, Xtrain[,'Y'], lambda = median(crossval$lambda))
summary(lassoTotalModel)
coeffLasso = coefficients(lassoTotalModel)[,1]
coeffLasso = coeffLasso[coeffLasso !=0]
coeffLasso = coeffLasso[-11]
coeffLasso
lassoFilterColumns = names(coeffLasso)



inSampLas = mean((Xtrain$Y - predict(lassoTotalModel,newx = genMatirx))^2)
outSampLas = mean((Xtest$Y - predict(lassoTotalModel,
                                     newx = as.matrix(Xtest %>% 
                                       dplyr::select(-one_of(c('Y'))))))^2)
c(inSampLas,outSampLas)
plot(lassoTotalModel)
#####Income Explanation
##RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS
### This is the number of people who have ratio 0.5,1,1.5,1.85,and above 2. 

############################### Bayesian Linear Regression on Filtered Covariates #######################################


## Normal Linear Regression.
XLassFilterTrain = Xtrain[,names(Xtrain) %in% c(names(coeffLasso),'Y')]
XLassFilterTest = Xtest[,names(Xtest) %in% c(names(coeffLasso),'Y')]
linModLassFilter = lm(Y ~ ., data = XLassFilterTrain)
summary(linModLassFilter)
LassoErr = evaluateError(linModLassFilter,XLassFilterTrain,XLassFilterTest)

valDf = cbind(valDf,LassoErr)


visualizeModelYear(linModLassFilter,XtrainTract,2013,titleName = '2013 Lasso Regression Aggregate Data')

### Log Linear Regression 
linModLassLogFilter = lm(log(Y) ~ ., data = XLassFilterTrain)
summary(linModLassLogFilter )
mean((exp(predict(linModLassLogFilter)) - XLassFilterTrain$Y)^2)
mean((exp(predict(linModLassLogFilter,newdata=XLassFilterTest)) - XLassFilterTest$Y)^2)

valDf = cbind(valDf,evaluateErrPois(linModLassLogFilter,XLassFilterTrain,XLassFilterTest))

names(valDf) = valNames
visualizeModelYearPois(linModLassLogFilter,XtrainTract,2013,titleMain = '2012 Aggregate Data')



## Bayesian Linear Regression 
linearBayesianAnalysis = function(linModelTract, XtrainTract){
  linearBetaHat = linModelTract$coefficients
  params = length(linearBetaHat)
  linSigmaSq = summary(linModelTract)$sigma^2
  linCovMat = summary(linModelTract)$cov.unscaled
  linDF = nrow(XtrainTract) - params
  numpsamp = 1000
  linBeta.samp = matrix(NA,nrow = numpsamp,ncol=params)
  linSigma.samp = rep(NA,numpsamp)
  for (i in 1:numpsamp) {
    linSigma.samp[i] = 1 / rgamma(1,shape = linDF/2,rate = (linDF *linSigmaSq)/2)
    covarMat = linSigma.samp[i] * linCovMat
    linBeta.samp[i,] = mvrnorm(1,linearBetaHat,covarMat)
  }
  linBeta.samp
}

### Visualize individual variables ##### 
lassFilterBetaSamp = linearBayesianAnalysis(linModLassFilter,XLassFilterTrain)
dim(lassFilterBetaSamp)
#names(lassFilterBetaSamp) = names(coeffLasso)
par(mfrow=c(2,2))
for (i in 12:15) {
  tempTitle = paste('Distribution',names(coeffLasso)[i],'Coefficient')
  hist(lassFilterBetaSamp[,i],xlab = 'Coefficient Distribution',main = tempTitle)
}



############################### Bayesian Linear Regression Visualization #######################################

bayesianModelAccuracy = function() {
  trainHolder = matrix(NA,nrow = dim(XLassFilterTrain)[1],ncol=dim(lassFilterBetaSamp)[1])
  testHolder = matrix(NA,nrow = dim(XLassFilterTest)[1],ncol=dim(lassFilterBetaSamp)[1])
  for (i in 1:dim(XLassFilterTrain)[1]) {
    rowMati = cbind(1,XLassFilterTrain[i,!(names(XLassFilterTest) %in% c('Y'))])
    rowMatrix = matrix(rowMati,nrow = dim(lassFilterBetaSamp)[1], ncol = dim(lassFilterBetaSamp)[2],byrow = T)
    trainHolder[i,] = rowSums(apply(rowMatrix,2, as.numeric) * lassFilterBetaSamp)
    
    # for (j in 1:dim(lassFilterBetaSamp)[1]) {
    #   trainHolder[i,j] = sum(XLassFilterTrain[i,!(names(XLassFilterTest) %in% c('Y'))]  * 
    #                              lassFilterBetaSamp[j,-1]) + lassFilterBetaSamp[j,1] 
    # }
  }
  print('finished first')
  for (i in 1:dim(XLassFilterTest)[1]) {
    rowMati = cbind(1,XLassFilterTest[i,!(names(XLassFilterTest) %in% c('Y'))])
    rowMatrix = matrix(rowMati,nrow = dim(lassFilterBetaSamp)[1], ncol = dim(lassFilterBetaSamp)[2],byrow = T)
    testHolder[i,] = rowSums(apply(rowMatrix,2, as.numeric) * lassFilterBetaSamp) 
  }
  list(trainHolder,testHolder)
  
}

totalSampData = bayesianModelAccuracy()

trainSampData = totalSampData[[1]]
testSampData = totalSampData[[2]]

convertToDF = function(sampData,Xfill) {
  sampDf = data.frame(sampData)
  sampDf$Year = Xfill$Year
  sampDf$tract = Xfill$tract
  sampDf
}
trainSampDf = convertToDF(trainSampData,XtrainTract)
testSampDf = convertToDF(testSampData,XtestTract)

### visualize hist:
yearSelect = 2015

#### In sample
bayesHistVisualization = function(Xtract,XSamp,yearSelect,titleName) {
  XYOrig = Xtract[Xtract$Year == yearSelect,] %>% group_by(tract)  %>% summarise_all(funs(sum)) %>% 
    dplyr::select(Y)
  XTSamp = XSamp[XSamp$Year == yearSelect,] %>% group_by(tract)  %>% summarise_all(funs(sum)) %>% 
    dplyr::select(-tract,-Year)
  print(dim(XYOrig))
  print(dim(XTSamp) )
  hist(XTSamp %>% pull(1),col='blue',main=titleName,xlab='Prediction(blue) vs. Actual (Red)')
  for (j in 1:(dim(XTSamp)[2] -2)) {
    hist(XTSamp %>% pull(j),col='blue',add=T)
  }
  hist(XYOrig$Y,col='red',add=T)
  
}

par(mfrow=c(2,2))
bayesHistVisualization(XtrainTract,trainSampDf,2013,'2013 Posterior Histogram')
bayesHistVisualization(XtrainTract,trainSampDf,2012,'2012 Posterior Histogram')
bayesHistVisualization(XtrainTract,trainSampDf,2011,'2011 Posterior Histogram')
bayesHistVisualization(XtrainTract,trainSampDf,2010,'2010 Posterior Histogram')
bayesHistVisualization(XtestTract,testSampDf,2014)

bayesTractVisualization = function(Xtract,Xsamp,tractNum,titleMain) {
  XYOrig = Xtract %>% filter (tract == tractNum)  %>% dplyr::select(Y)
  XTSamp = Xsamp %>% filter (tract == tractNum)  %>% dplyr::select(-tract,-Year)
  print(dim(XYOrig))
  print(dim(XTSamp))
  plot(XYOrig$Y,col='red',main=titleMain,xlab = 'Time',ylab = 'Crime Counts')
  for (j in 1:(dim(XTSamp)[2] -2)) {
    lines(XTSamp %>% pull(j),col='blue')
  }
}
par(mfrow=c(1,1))
bayesTractVisualization(XtrainTract,trainSampDf,29000,'Bayesian Linear Reg Prediction Tract 29000')


############################### Bayesian Poisson Regression #######################################

fullPoisModelAT = glm(Y ~ ., data=Xtrain, family=poisson())


summary(fullPoisModelAT)
inSampPois = mean((Xtrain$Y - exp(predict(fullPoisModelAT)))^2)
outSampPois = mean((Xtest$Y - exp(predict(fullPoisModelAT,newdata = Xtest)))^2)

visualizeModelYearPois = function(model,trainData,year,titleMain) {
  Xvisual = trainData
  Xvisual['pred'] = exp(predict(model))
  XYear = Xvisual[Xvisual$Year == year,]
  XYearAgg = XYear %>% group_by(tract) %>% summarise_all(funs(sum))
  
  hist(XYearAgg$pred,col='blue',,breaks = 50,main = titleMain,xlab = 'Prediction (blue) vs. Actual (red)')
  hist(XYearAgg$Y,col='red',breaks = 50,add=T)
  
}
visualizeModelTractPois = function(model,trainData,tractNum,titleMain) {
  Xvisual = trainData
  Xvisual['pred'] = exp(predict(model))
  
  plot(Xvisual[Xvisual$tract == tractNum,'Y'],col='red',main = titleMain,xlab = 'Prediction (blue) vs. Actual (red)',ylab='Crime Counts')
  lines(Xvisual[Xvisual$tract == tractNum,'pred'],col='blue')
  
}
visualizeDistYearPois = function(probVector,trainData,year) {
  Xvisual = trainData
  Xvisual['pred'] = probVector
  XYear = Xvisual[Xvisual$Year == year,]
  XYearAgg = XYear %>% group_by(tract) %>% summarise_all(funs(sum))
  
  hist(XYearAgg$pred,col='blue',,breaks = 50)
  hist(XYearAgg$Y,col='red',breaks = 50,add=T)
  
}
visualizeModelYearPois(fullPoisModelAT,XtrainTract,2013)
visualizeDistYearPois(rpois(length(XtrainTract$y),lambda),XtrainTract,2013)

####### POIS FILITER PARAM ########

PoisModelLassFilter = glm(Y ~ ., data=XLassFilterTrain, family=poisson())
summary(PoisModelLassFilter)
inSampPois = mean((Xtrain$Y - exp(predict(PoisModelLassFilter)))^2)
outSampPois = mean((Xtest$Y - exp(predict(PoisModelLassFilter,newdata = XLassFilterTest)))^2)
par(mfrow=c(1,1))
visualizeModelYearPois(PoisModelLassFilter,XtrainTract,2013,'Poisson Model Agggregate 2013')
visualizeModelYearPois(PoisModelLassFilter,XtrainTract,2012,'Poisson Model Agggregate 2012')
visualizeModelYearPois(PoisModelLassFilter,XtrainTract,2011,'Poisson Model Agggregate 2011')
visualizeModelYearPois(PoisModelLassFilter,XtrainTract,2010,'Poisson Model Agggregate 2010')

visualizeModelTractPois(PoisModelLassFilter,XtrainTract,29000,'Poisson Model Prediction Tract 29000')

######### NBD Filter Param and Posterior#################

nbdModFilter = glm.nb(Y ~ ., data=XLassFilterTrain)
summary(nbdModFilter)
theta = 9.1872
inSampNBD = mean((XLassFilterTrain$Y - exp(predict(nbdMod)))^2)
outSampNBD = mean((XLassFilterTest$Y - exp(predict(nbdMod,newdata = XLassFilterTest)))^2)
visualizeModelYearPois(nbdModFilter,XtrainTract,2013,'NBD Model Aggregate 2013')
visualizeModelYearPois(nbdModFilter,XtrainTract,2012,'NBD Model Aggregate 2012')
visualizeModelYearPois(nbdModFilter,XtrainTract,2011,'NBD Model Aggregate 2011')
visualizeModelYearPois(nbdModFilter,XtrainTract,2010,'NBD Model Aggregate 2010')

#### Method of moments NBD no covariates###### 

XLassFilterTrainNY = XLassFilterTrain %>% dplyr::select(-Y)
exp(sum(nbdMod$coefficients * cbind(1,XLassFilterTrainNY[1,])))
dim(XLassFilterTrain)
muSamp = mean(XLassFilterTrain$Y)
muSamp2 = 89
varSamp = var(XLassFilterTrain$Y)
r = muSamp^2/(varSamp-muSamp)
alpha = muSamp/(varSamp-muSamp)
r2 = muSamp2^2/(varSamp-muSamp2)
alpha2 = muSamp2/(varSamp-muSamp2)
c(r2,alpha2)
rgamma(100,shape=r,rate = alpha)
exp(rgamma(1 ,shape=r,rate = alpha) + sum(nbdModFilter$coefficients[-1] * XLassFilterTrainNY[10,]))
nbdMod$coefficients[1]

nbdPredict = function( r, alpha) {
  x = seq(1,300)
  covConstant = 1
  probVector = gammaln(r + x) - gammaln(r) - gammaln(x+1) + r* log(alpha)-r * log(alpha + covConstant) +
    x * log(covConstant) - x * log(alpha + covConstant)
  exp(probVector )
  
  
  
}

visualizeDistYearPois(probVector = sample(x = seq(1,300), 
                                          prob = nbdPredict(r,alpha),size = length(XtrainTract$y),replace = T),
                      trainData = XtrainTract,year = 2013 )





##### Method of moments Poisson Regression ########
exp(predict(PoisModelLassFilter))[1]

exp(sum(PoisModelLassFilter$coefficients * cbind(1,XLassFilterTrainNY[1,])))
exp(rgamma(100,shape=r2,rate=alpha2) + sum(PoisModelLassFilter$coefficients[-1] * XLassFilterTrainNY[1,]))


sampNumber=100

PoisModelCoeffMatrix = cbind(rgamma(sampNumber,shape=r2,rate=alpha2), matrix(PoisModelLassFilter$coefficients[-1],
                             nrow=sampNumber,ncol=length(PoisModelLassFilter$coefficients)-1,byrow = T))


bayesianModelPoissonAccuracy = function(sampNumber) {
  trainHolder = matrix(NA,nrow = dim(XLassFilterTrain)[1],ncol=sampNumber)
  testHolder = matrix(NA,nrow = dim(XLassFilterTest)[1],ncol=sampNumber)
  for (i in 1:dim(XLassFilterTrain)[1]) {
    rowMati = cbind(1,XLassFilterTrain[i,!(names(XLassFilterTest) %in% c('Y'))])
    rowMatrix = matrix(rowMati,nrow = sampNumber, ncol = length(rowMati),byrow = T)
    trainHolder[i,] = rowSums(apply(rowMatrix,2, as.numeric) * PoisModelCoeffMatrix)
  }
  print('finished first')
  for (i in 1:dim(XLassFilterTest)[1]) {
    rowMati = cbind(1,XLassFilterTest[i,!(names(XLassFilterTest) %in% c('Y'))])
    rowMatrix = matrix(rowMati,nrow = sampNumber, ncol = length(rowMati),byrow = T)
    testHolder[i,] = rowSums(apply(rowMatrix,2, as.numeric) * PoisModelCoeffMatrix) 
  }
  list(trainHolder,testHolder)
  
}

totalPois = bayesianModelPoissonAccuracy(100)

trainPois = totalPois[1]

fitdistr(XLassFilterTrain$Y, "poisson")


















###### MCMC Poisson ######
install.packages('MCMCpack')
library(MCMCpack)

poisRegFilter = MCMCpoisson(Y~.,data=XLassFilterTrain,mcmc = 1000)
exp(sum(poisRegFilter[1,] * cbind(1,XLassFilterTrainNY[1,])))

bayesianPoissonModelCalc = function() {
  trainHolder = matrix(NA,nrow = dim(XLassFilterTrain)[1],ncol=dim(poisRegFilter)[1])
  testHolder = matrix(NA,nrow = dim(XLassFilterTest)[1],ncol=dim(poisRegFilter)[1])
  for (i in 1:dim(XLassFilterTrain)[1]) {
    rowMati = cbind(1,XLassFilterTrain[i,!(names(XLassFilterTest) %in% c('Y'))])
    rowMatrix = matrix(rowMati,nrow = dim(poisRegFilter)[1], ncol = dim(poisRegFilter)[2],byrow = T)
    trainHolder[i,] = exp(rowSums(apply(rowMatrix,2, as.numeric) * poisRegFilter))
  }
  print('finished first')
  for (i in 1:dim(XLassFilterTest)[1]) {
    rowMati = cbind(1,XLassFilterTest[i,!(names(XLassFilterTest) %in% c('Y'))])
    rowMatrix = matrix(rowMati,nrow = dim(poisRegFilter)[1], ncol = dim(poisRegFilter)[2],byrow = T)
    testHolder[i,] = exp(rowSums(apply(rowMatrix,2, as.numeric) * poisRegFilter)) 
  }
  list(trainHolder,testHolder)
  
}

totalPoisHolder = bayesianPoissonModelCalc()
trainPoisSampData = totalPoisHolder[[1]]
testPoisSampData = totalPoisHolder[[2]]

trainPoisSampDf = convertToDF(trainPoisSampData,XtrainTract)
testPoisSampDf = convertToDF(testPoisSampData,XtestTract)

par(mfrow=c(1,1))
bayesHistVisualization(XtrainTract,trainPoisSampDf,2013,'Poisson Reg Samples 2013')
bayesHistVisualization(XtrainTract,trainPoisSampDf,2012,'Poisson Reg Samples 2012')
bayesHistVisualization(XtrainTract,trainPoisSampDf,2011,'Poisson Reg Samples 2011')
bayesHistVisualization(XtrainTract,trainPoisSampDf,2010,'Poisson Reg Samples 2010')

bayesHistVisualization(XtestTract,testPoisSampDf,2014)
bayesTractVisualization(XtrainTract,trainPoisSampDf,29000)


############################### Bayesian Decision Tree Analysis #######################################

visualizeModelYear(model = fullDTModelAT, XtrainTract,2012,'Decision Tree Model 2012')
### Filtered DT model

filteredDTModelAT = rpart(Y ~ .,data=XLassFilterTrain,method='anova')
summary(filteredDTModelAT)
par(mfrow=c(2,2))
rpart.plot(filteredDTModelAT)

visualizeModelYear(model = filteredDTModelAT, XtrainTract,2013,'Decision Tree Model 2013 Agg')
visualizeModelYear(model = filteredDTModelAT, XtrainTract,2012,'Decision Tree Model 2012 Agg')
visualizeModelYear(model = filteredDTModelAT, XtrainTract,2011,'Decision Tree Model 2011 Agg')
visualizeModelYear(model = filteredDTModelAT, XtrainTract,2010,'Decision Tree Model 2010 Agg')

#### Random Forest

library(randomForest)

rfModel = randomForest(Y~.,data=XLassFilterTrain,ntrees = 1000, nodesize=10,maxnodes = 100 )
summary(rfModel)

visualizeModelYear(model = rfModel, XtrainTract,2013,'RF Model 2013 Agg')
visualizeModelYear(model = rfModel, XtrainTract,2012,'RF Model 2012 Agg')
visualizeModelYear(model = rfModel, XtrainTract,2011,'RF Model 2011 Agg')
visualizeModelYear(model = rfModel, XtrainTract,2010,'RF Model 2010 Agg')

valDf = cbind(valDf, evaluateError(rfModel,Xtrain,Xtest))

#### BART 
install.packages('BART')
library(BART)
XLassFilterTrainNY = XLassFilterTrain %>% dplyr::select(-Y)
XLassFilterTestNY = XLassFilterTest %>% dplyr::select(-Y)
bartModel = wbart(x.train= XLassFilterTrainNY,y.train = XLassFilterTrain$Y,x.test =XLassFilterTestNY  ,
                  ntree = 1000,numcut=20,ndpost=100,nskip=5)

inSampBart = mean((bartModel$yhat.train.mean - Xtrain$Y)^2)
outSampBart = mean((bartModel$yhat.test.mean - Xtest$Y)^2)

valDf = cbind(valDf,c(inSampBart,outSampBart))

visualizeModelDataYear(bartModel$yhat.train.mean, XtrainTract,2011)
trainBartSampDf = convertToDF(t(bartModel$yhat.train),XtrainTract)
testBartSampDf = convertToDF(t(bartModel$yhat.test),XtestTract)


bayesHistVisualization(XtrainTract,trainBartSampDf,2013)
bayesHistVisualization(XtestTract,testBartSampDf,2014,'BART Out Sample 2014')
bayesTractVisualization(XtrainTract,trainBartSampDf,29000,titleMain = 'BART prediction 29000')


par(mfrow=c(1,1))
bayesHistVisualization(XtrainTract,trainBartSampDf,2013,'BART Samples 2013')
bayesHistVisualization(XtrainTract,trainBartSampDf,2012,'BART Samples 2012')
bayesHistVisualization(XtrainTract,trainBartSampDf,2011,'BART Samples 2011')
bayesHistVisualization(XtrainTract,trainBartSampDf,2010,'BART Samples 2010')

##################################### Analysis for Violent Crimes ############################################

#### Does Poisson Model Work Better for just violent crimes???


#### Get violent Crime DF 
## Filtering columns
nonNumericVCols = c('YearTract','violent','nonviolent','Total_y','vice','X.y','X.x','count_inf',
                   'hispanic','culture_recreation','Y')
totalVDf = totalDf
totalVDf$Yv = totalVDf$violent

totalVFilteredDf = totalVDf[,!(names(totalVDf) %in% nonNumericVCols)]

tractNum = length(unique(totalDf$tract))

XVtrainTract = totalVFilteredDf[totalDf$Year %in% 2006:2013,]
XVtestTract = totalVFilteredDf[totalDf$Year %in% 2014:2015,]

XVtrain = XVtrainTract[,!(names(XtrainTract) %in% c('tract'))]
XVtest = XVtestTract[,!(names(XtestTract) %in% c('tract'))]


### Apply Lasso Regression

## Baseline 
LinModV = lm(Yv ~ ., data = XVtrain)
summary(LinModV)
inSampVLin = mean((XVtrain$Yv - predict(LinModV))^2)
outSampVLin = mean((XVtest$Yv - predict(LinModV,newdata = XVtest))^2)
c(inSampVLin,outSampVLin)

## Lasso
genVMatirx = as.matrix(XVtrain %>% dplyr::select(-one_of(c('Yv'))))
crossval <-  cv.glmnet(x = genVMatirx, y = XVtrain[,'Yv'])
lamVMin = crossval$lambda.min

lassoVMod = glmnet(genVMatirx,alpha = 1, XVtrain[,'Yv'], lambda = median(crossval$lambda))
summary(lassoVMod)
coeffVLasso = coefficients(lassoVMod)[,1]
coeffVLasso = coeffVLasso[coeffVLasso !=0]
coeffVLasso = coeffVLasso[-14]
coeffVLasso
lassVFilterColumn = names(coeffVLasso)

XlVFTrain = XVtrain[,names(XVtrain) %in% c(names(coeffVLasso),'Yv')]
XlvTest = XVtest[,names(XVtest) %in% c(names(coeffVLasso),'Yv')]
linModLVF = lm(Yv ~ ., data = XlVFTrain)
summary(linModLVF)
inSLV = mean((XVtrain$Yv - predict(linModLVF))^2)
outSLV = mean((XVtest$Yv - predict(linModLVF,newdata = XlvTest))^2)
c(inSampVLin,outSampVLin)

vvisModYear = function(model,trainData,year) {
  Xvisual = trainData
  Xvisual['pred'] = predict(model)
  XYear = Xvisual[Xvisual$Year == year,]
  XYearAgg = XYear %>% group_by(tract) %>% summarise_all(funs(sum))
  
  hist(XYearAgg$pred,col='blue')
  hist(XYearAgg$Yv,col='red',add=T)
  
}
vvisModYearPois = function(model,trainData,year) {
  Xvisual = trainData
  Xvisual['pred'] = exp(predict(model))
  XYear = Xvisual[Xvisual$Year == year,]
  XYearAgg = XYear %>% group_by(tract) %>% summarise_all(funs(sum))
  
  hist(XYearAgg$pred,col='blue')
  hist(XYearAgg$Yv,col='red',add=T)
  
}

vvisModYear(linModLVF,XVtrainTract,2012)



### Apply Poisson Model

PoisVMod = glm(Yv ~ ., data=XlVFTrain, family=poisson())
summary(PoisVMod)
inSampVPois = mean((XVtrain$Yv - exp(predict(PoisVMod)))^2)
outSampVPois = mean((XVtest$Yv - exp(predict(PoisVMod,newdata = XVtest)))^2)
c(inSampVPois,outSampVPois)

vvisModYearPois(PoisVMod,XVtrainTract,2013)

nbdVMod = glm.nb(Yv ~ ., data=XVtrain)
summary(nbdVMod)
inSampVNbd = mean((XVtrain$Yv - exp(predict(nbdVMod)))^2)
outSampVNbd = mean((XVtest$Yv - exp(predict(nbdVMod,newdata = XVtest)))^2)
c(inSampVNbd,outSampVNbd)
vvisModYearPois(nbdVMod,XVtrainTract,2013)







