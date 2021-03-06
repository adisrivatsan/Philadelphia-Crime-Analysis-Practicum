setwd(dir='Documents/Penn/SeniorFall/Practicum Project/')
library(xlsx)

## read in data 

crimeDf = read.csv(file = 'ProcessedDataSets/TotalInfoDF_03_12_2019.csv')
weatherDf = read.csv(file='ProcessedDataSets/weatherPhiladelphiaData20062015.csv')
exampleTract = 1102

totalDf = merge(crimeDf,weatherDf,by.x = c('Year','Month'), by.y = c('Year','Month'))

tractList = unique(totalDf$tract)
nonNumericCols = c('YearTract','violent','nonviolent','Total_y','vice','X.y','X.x')

totalFilteredDf = totalDf[,!(names(totalDf) %in% nonNumericCols)]
colnames(totalFilteredDf)

excelDfStat = totalFilteredDf[,c('Year','Month','tract','Y')]
dim(excelDfStat)
write.csv(x = excelDfStat,file = 'PhillyCrimeNBDModel.csv')
## Split into training and testing

Xtrain = totalFilteredDf[totalDf$Year %in% 2006:2013,]
Xtest = totalFilteredDf[totalDf$Year %in% 2014:2015,]
XtrainSamp = Xtrain[Xtrain$tract == exampleTract,!(names(Xtrain) %in% c('tract'))]
#YtrainSamp = Ytrain[Ytrain$tract == exampleTract,!(names(Ytrain) %in% c('tract'))]
XtestSamp = Xtest[Xtest$tract == exampleTract,!(names(Xtest) %in% c('tract'))]

## Month predictors
monthPred = c('Year','Month','Max','Average','Min','Rain')

### Full Linear Model All tracts

fullLinearModelAT = lm(Y ~ ., data = Xtrain)




### Sample DATA for visualization 
######## Lin Model
samplinModel = lm(log(Y) ~Year + Month + Max + Average + Min + Rain,data=XtrainSamp)
summary(samplinModel)
plot(samplinModel)
######## Pos Model
sampPosModel = glm(Y ~ Year + Month + Max + Average + Min + Rain,
                   data=XtrainSamp, family=poisson())
summary(sampPosModel)
######## Decision Tree Model
library(rpart)

dtSampModel = rpart(Y ~ Year + Month + Max + Average + Min + Rain,
                    data=XtrainSamp,method='anova')
summary(dtSampModel)
printcp(dtSampModel)

#### Ridge Regression
library(glmnet)
genMatirx = as.matrix(XtrainSamp[,monthPred])
crossval <-  cv.glmnet(x = genMatirx, y = XtrainSamp[,'Y'])
lamMin = crossval$lambda.min
ridgeSampleModel = glmnet(genMatirx, 
                          XtrainSamp[,'Y'], alpha = 0,lambda = lamMin)
summary(ridgeSampleModel)
inSampRidge = predict(ridgeSampleModel,newx = genMatirx)
outSampRidge = predict(ridgeSampleModel,newx = as.matrix(
  XtestSamp[,monthPred]))

#### Lasso Regression
library(glmnet)
genMatirx = as.matrix(XtrainSamp[,monthPred])
crossval <-  cv.glmnet(x = genMatirx, y = XtrainSamp[,'Y'])
lamMin = crossval$lambda.min
lassoSampleModel = glmnet(genMatirx, 
                          XtrainSamp[,'Y'], alpha = 1,lambda = lamMin)
summary(lassoSampleModel)
inSampLas = predict(lassoSampleModel,newx = genMatirx)
outSampLas = predict(lassoSampleModel,newx = as.matrix(
  XtestSamp[,monthPred]))

####### Visualization 
######### train
plot(XtrainSamp$Y,main = 'Example Tract Regularized Linear Prediction Models Insample',xlab = 'Month', 
     ylab = 'Crime Rate')
lines(exp(predict(samplinModel)),col='blue')
#lines(predict(sampPosModel,type='response'),col='red')
#lines(predict(dtSampModel),col='green')
lines(inSampRidge,col='red')
lines(inSampLas,col='green')
legend(80, 75, legend=c("Linear", "Ridge","Lasso"),
       col=c("blue", "red","green"),lty=1:3,cex=0.5)

######### test
plot(XtestSamp$Y,main = 'Example Tract Regularized Linear Prediction Models Out-Sample',xlab = 'Month', 
     ylab = 'Crime Rate')
lines(exp(predict(object = samplinModel,newdata = XtestSamp)),col='blue')
#lines(predict(sampPosModel,newdata = XtestSamp,type='response'),col='red')
#lines(predict(dtSampModel,newdata = XtestSamp),col='green')

lines(outSampRidge,col='red')
lines(outSampLas,col='green')
legend(20, 10, legend=c("Linear", "Ridge","Lasso"),
       col=c("blue", "red","green"),lty=1:3,cex=0.5)


#### Error Analysis Basic Models 

meanSqError = function(x,y) {
  sum((x-y)^2) / length(x)
}

modelAccuracy = function(modelType) {
  holder = matrix(NA,nrow = length(tractList),ncol=3)
  for (i in 1:length(tractList)) {
    tractNum = tractList[i]
    print(i)
    XtrainInd = Xtrain[Xtrain$tract == tractNum,!(names(Xtrain) %in% c('tract'))]
    XtestInd = Xtest[Xtest$tract == tractNum,!(names(Xtest) %in% c('tract'))]
    if(modelType == 'lin') {
      indLinModel = lm(log(Y) ~Year + Month + Max + Average + Min + Rain,data=XtrainInd)
      holder[i,1] = meanSqError(exp(predict(indLinModel)),XtrainInd$Y)
      holder[i,2] = meanSqError(exp(predict(indLinModel,newdata=XtestInd)),XtestInd$Y)
      holder[i,3] = var(exp(predict(indLinModel,newdata=XtestInd)))
    } else if (modelType == 'pos'){
      indPosModel = glm(Y ~ Year + Month + Max + Average + Min + Rain,
                        data=XtrainInd, family=poisson())
      holder[i,1] = meanSqError(predict(indPosModel,type='response'),XtrainInd$Y)
      holder[i,2] = meanSqError(predict(indPosModel,newdata=XtestInd,type='response'),XtestInd$Y)
      holder[i,3] = var(predict(indPosModel,newdata=XtestInd,type='response'))
    } else if (modelType=='tree'){
      indTreeModel = rpart(Y ~ Year + Month + Max + Average + Min + Rain,
                          data=XtrainSamp,method='anova')
      holder[i,1] = meanSqError(predict(indTreeModel),XtrainInd$Y)
      holder[i,2] = meanSqError(predict(indTreeModel,newdata=XtestInd),XtestInd$Y)
      holder[i,3] = var(predict(indTreeModel,newdata=XtestInd))
    } else if (modelType == 'ridge') {
      genMatirx = as.matrix(XtrainInd[,monthPred])
      crossval <-  cv.glmnet(x = genMatirx, y = XtrainInd[,'Y'])
      lamMin = crossval$lambda.min
      ridgeSampleModel = glmnet(genMatirx, 
                                XtrainInd[,'Y'], alpha = 0,lambda = lamMin)
      summary(ridgeSampleModel)
      inIndRidge = predict(ridgeSampleModel,newx = genMatirx)
      outIndRidge = predict(ridgeSampleModel,newx = as.matrix(
        XtestInd[,monthPred]))
      holder[i,1] = meanSqError(inIndRidge,XtrainInd$Y)
      holder[i,2] = meanSqError(outIndRidge,XtestInd$Y)
      holder[i,3] = var(outIndRidge)
    }else if (modelType=='lasso') {
      genMatirx = as.matrix(XtrainInd[,monthPred])
      crossval <-  cv.glmnet(x = genMatirx, y = XtrainInd[,'Y'])
      lamMin = crossval$lambda.min
      lassSampleModel = glmnet(genMatirx, 
                                XtrainInd[,'Y'], alpha = 1,lambda = lamMin)
      inIndLas = predict(lassSampleModel,newx = genMatirx)
      outIndLas = predict(lassSampleModel,newx = as.matrix(
        XtestInd[,monthPred]))
      holder[i,1] = meanSqError(inIndLas,XtrainInd$Y)
      holder[i,2] = meanSqError(outIndLas,XtestInd$Y)
      holder[i,3] = var(outIndLas)
      
    }
  }
  holder
}

basicModelResults = matrix(NA,nrow = length(tractList),ncol=15)
basicModelResults[,1:3] = modelAccuracy('lin')
basicModelResults[,4:6] = modelAccuracy('pos')
basicModelResults[,7:9] = modelAccuracy('tree')
basicModelResults[,10:12] = modelAccuracy('ridge')
basicModelResults[,13:15] = modelAccuracy('lasso')


## Visulaizations for Basic Model Results

modNames = c('linear','Poisson','Decision Tree','Ridge','Lasso')
barplot(colSums(basicModelResults)
        [seq(from=1,to=ncol(basicModelResults),by=3)]/
          nrow(basicModelResults),names.arg =modNames,col = 'blue',
        main = 'In-Sample Model Errors over all tracts', ylab = 'Mean Squared Error' )

barplot(colSums(basicModelResults)
        [seq(from=2,to=ncol(basicModelResults),by=3)]/
          nrow(basicModelResults),names.arg =modNames,col = 'blue',
        main = 'Out-Sample Model Errors over all tracts', ylab = 'Mean Squared Error' )

barplot(colSums(basicModelResults)[seq(from=3,to=ncol(basicModelResults),by=3)]/
          nrow(basicModelResults),names.arg =modNames,col = 'blue',
        main = 'Out-Sample Model Variance over all tracts', ylab = 'Variance Error' )

# maxIndex = apply(FUN = function(x) which.max(x),X =
#                    basicModelResults[,seq(from=2,to=ncol(basicModelResults),by=2)],MARGIN = 1) 



## Simple Bayesian

###### Linear 
linearBetaHat = samplinModel$coefficients
params = length(linearBetaHat)
linSigmaSq = summary(samplinModel)$sigma^2
linCovMat = summary(samplinModel)$cov.unscaled
linDF = nrow(XtrainSamp) - params
numberSamples = 1000
linBeta.samp = matrix(NA,nrow = numpsamp,ncol=params)
linSigma.samp = rep(NA,numpsamp)
for (i in 1:numpsamp) {
  linSigma.samp[i] = 1 / rgamma(1,shape = linDF/2,rate = (linDF *linSigmaSq)/2)
  covarMat = linSigma.samp[i] * linCovMat
  linBeta.samp[i,] = mvrnorm(1,linearBetaHat,covarMat)
}

### sample plot lin
plot(XtrainSamp$Y,main = 'Bayesian Linear Model In-Sample',xlab = 'Month', 
     ylab = 'Crime Rate' )
collect = matrix(NA,nrow = nrow(linBeta.samp),ncol = nrow(XtrainSamp))
for (i in 1:nrow(linBeta.samp)) {
  for (j in 1:nrow(XtrainSamp)) {
    collect[i,j] = sum(linBeta.samp[i,] * c(1,as.numeric(XtrainSamp[j,monthPred])))
  }
  lines(exp(collect[i,]),col='blue')
}

plot(XtestSamp$Y,main = 'Bayesian Linear Model Out-Sample',xlab = 'Month', 
     ylab = 'Crime Rate' )
collect = matrix(NA,nrow = nrow(linBeta.samp),ncol = nrow(XtestSamp))
for (i in 1:nrow(linBeta.samp)) {
  for (j in 1:nrow(XtestSamp)) {
    collect[i,j] = sum(linBeta.samp[i,] * c(1,as.numeric(XtestSamp[j,monthPred])))
  }
  lines(exp(collect[i,]),col='blue')
}

### sample plot Pos
plot(XtrainSamp$Y,main = 'Bayesian Poisson Model In-Sample',xlab = 'Month',
     ylab = 'Crime Rate' )
collect = matrix(NA,nrow = nrow(linBeta.samp),ncol = nrow(XtrainSamp))
for (i in 1:nrow(linBeta.samp)) {
  for (j in 1:nrow(XtrainSamp)) {
    collect[i,j] = sum(linBeta.samp[i,] * c(1,as.numeric(XtrainSamp[j,monthPred])))
  }
  lines(rpois(nrow(XtrainSamp),exp(collect[i,])),col='blue')
}


### Evaluation - out of sample error generation 

linearBayesianAnalysis = function(linModelTract, XtrainTract){
  linearBetaHat = linModelTract$coefficients
  params = length(linearBetaHat)
  linSigmaSq = summary(linModelTract)$sigma^2
  linCovMat = summary(linModelTract)$cov.unscaled
  linDF = nrow(XtrainTract) - params
  numberSamples = 1000
  linBeta.samp = matrix(NA,nrow = numpsamp,ncol=params)
  linSigma.samp = rep(NA,numpsamp)
  for (i in 1:numpsamp) {
    linSigma.samp[i] = 1 / rgamma(1,shape = linDF/2,rate = (linDF *linSigmaSq)/2)
    covarMat = linSigma.samp[i] * linCovMat
    linBeta.samp[i,] = mvrnorm(1,linearBetaHat,covarMat)
  }
  linBeta.samp
}

bayesianModelAccuracy = function() {
  holder = matrix(NA,nrow = length(tractList),ncol=4)
  for (i in 1:length(tractList)) {
    tractNum = tractList[i]
    print(i)
    XtrainInd = Xtrain[Xtrain$tract == tractNum,!(names(Xtrain) %in% c('tract'))]
    XtestInd = Xtest[Xtest$tract == tractNum,!(names(Xtest) %in% c('tract'))]
    indLinModel = lm(log(Y) ~Year + Month + Max + Average + Min + Rain,data=XtrainInd)
    betaIndHat.samp = linearBayesianAnalysis(indLinModel,XtrainInd)
    ## test correlation 
    collect = matrix(NA,nrow = nrow(betaIndHat.samp),ncol = nrow(XtestInd))
    corCount = matrix(NA,nrow = nrow(betaIndHat.samp),ncol=2)
    for (k in 1:nrow(betaIndHat.samp)) {
      for (j in 1:nrow(XtestInd)) {
        collect[k,j] = sum(betaIndHat.samp[k,] * c(1,as.numeric(XtestInd[j,monthPred])))
      }
      corCount[k,1] =  cor(exp(collect[k,]), XtestInd$Y)
      corCount[k,2] = var(exp(collect[k,]))
    }
    holder[i,1] = mean(corCount[,1]) 
    holder[i,2] = max(corCount[,1])
    holder[i,3] = mean(corCount[,2]) 
    holder[i,4] = max(corCount[,2]) 
    print(mean(corCount))
    
    
  }
  holder 
    
}

bayesianCorrelationLinearTest = bayesianModelAccuracy()

## Visualizations
hist(bayesianCorrelationLinearTest[,1],main = 'Histogram - Bayesian Linear Regression Avg Cor',xlab = 'Average Correlation')
hist(bayesianCorrelationLinearTest[,4],main = 'Histogram - Bayesian Linear Regression Avg Variance',xlab='Average Variance')

### Variance test data
varTractHolder = rep(NA,length(tractList))
for (i in 1:length(tractList)) {
  tractNum = tractList[i]
  XtestInd = Xtest[Xtest$tract == tractNum,!(names(Xtest) %in% c('tract'))]
  varTractHolder[i] = var(XtestInd$Y)
}
mean(varTractHolder)

### Clustering
# read in from Python
clusterDf = read.csv('KMeanLabelDF.csv')
totalClusterDf = merge(totalDf,clusterDf,by.x = 'tract',by.y = 'tract')

hist(clusterDf$classLabel,col='blue',main='K Mean Class Frequency')


