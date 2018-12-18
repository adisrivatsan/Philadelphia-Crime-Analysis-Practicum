###### Poisson
# posBetaHat = sampPosModel$coefficients
# params = length(poisBetaHat)
# posSigmaSq = mean(XtrainSamp$Y)
# posCovMat = summary(sampPosModel)$cov.unscaled
# posDF = nrow(XtrainSamp) - params
# numberSamples = 1000
# posBeta.samp = matrix(NA,nrow = numpsamp,ncol=params)
# posSigma.samp = rep(NA,numpsamp)
# for (i in 1:numpsamp) {
#   posSigma.samp[i] = 1 / rgamma(1,shape = posDF/2,rate = (posDF *posSigmaSq)/2)
#   covarMat = posSigma.samp[i] * posCovMat
#   posBeta.samp[i,] = mvrnorm(1,posBetaHat,covarMat)
# }
# 
# ### graph
# plot(XtrainSamp$Y,main = 'Bayesian Poisson Model Sample',xlab = 'Month',
#      ylab = 'Crime Rate' )
# collect = matrix(NA,nrow = nrow(posBeta.samp),ncol = nrow(XtrainSamp))
# for (i in 1:nrow(linBeta.samp)) {
#   for (j in 1:nrow(XtrainSamp)) {
#     collect[i,j] = sum(posBeta.samp[i,] * c(1,as.numeric(XtrainSamp[j,monthPred])))
#   }
#   lines(rpois(nrow(XtrainSamp),exp(collect[i,])),col='blue')
# }
# lines(predict(sampPosModel,type = 'response'))