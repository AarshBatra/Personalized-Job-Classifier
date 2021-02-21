# # 
# # n <- names(tmpVarTrainSet.XYPair)
# # f <- as.formula(paste("U ~", paste(n[!n %in% "U"], collapse = " + ")))
# # nn <- neuralnet(f, data = tmpVarTrainSet.XYPair, hidden = c(5,3), linear.output = FALSE)
# 
# # #--training the 'personalizedJobClassifier' (using glm)
# # personalizedJobClassifierGLM <- glm(labels~.,family= binomial(link = "logit"), data = tmpVarTrainSet.XYPair)
# # 
# # #--making predictions on the test set (underlying method - glm)
# #  probabilities <- predict(personalizedJobClassifierGLM, tmpVarTestSet.XOnly, type="response")
# #  predictions <- ifelse(probabilities>0.5, 1, 0)
# #  accuracyPersonalizedJobClassiferGLM <- mean(predictions == (companyDataset[-index,(totalNumOfFeat+1)]))
# #  
# 
#  #-- training the personalizedJobClassifier using nn
#  # names(tmpVarTrainSet.XYPair) <- LETTERS[1:21]
#  # n <- names(tmpVarTrainSet.XYPair)
#  # f <- as.formula(paste("U ~", paste(n[!n %in% "U"], collapse = " + ")))
#  # initWeights <- (rnorm(536,0,1))*10^-4
#  # personalizedJobClassifierNN <- neuralnet(f, data=tmpVarTrainSet.XYPair, hidden = c(5,5), threshold = 0.003, startweights = initWeights,  learningrate = 0.003, algorithm = "backprop", act.fct = "tanh", linear.output = FALSE)
# 
# 
# # #--code for analyzing train/test distributions
# # numOnes <- c(rep(NA, times = totalNumOfComp))
# # for(d in 1:nrow(companyDataset)){
# #   numOnes[d] <- sum(companyDataset[d,])
# # }
# # #hist(numOnes)
# # #plot(density(numOnes))
# # #plot(density(numOnes[trainRows]))
# # #plot(density(numOnes[-trainRows]))
# 
# 
# #--figuring how many number of folds give best accuracy on test set
# 
# # #training personalizedJobClassifier model using cv.glmnet
# # nfoldsVec <- c(10,20,30,40,50,60,70,80,90,100)
# # nTests <- 10
# # resAccMat <- matrix(NA, nTests, length(nfoldsVec))
# # 
# # for(c in 1:nTests){
# # for(d in 1:length(nfoldsVec)){
# #   
# #   #predicting on test set
# #   tp <- predict(personalizedJobClassifier, companyDataset[-trainRows,], s = "lambda.min", type="class", nfolds = nfoldsVec[d], alpha = 0)
# #   resAccMat[c,d] <- mean(tp == myPersonalizedLabels[-trainRows])
# # }
# # }
# 
# 
# # tstAccVec <- c(rep(NA, times = total))
# # for(d in 1:total){
# #   tp <- predict(grandList[[d]], companyDataset[-trainRows,], s = "lambda.min", type = "class")
# #   tstAccVec[d] <- mean(tp == myPersonalizedLabels[)
# # }
# 
# # 
# # avgRecVec <- c(rep(NA, times = nrow(accMat)))
# # varRecVec <- c(rep(NA, times = nrow(accMat)))
# # maxAvg <- 0
# # minVar <- 100
# # maxAvgInd <- 0
# # minVarInd <- 0
# # #--choosing the optimal set of hyperparameters
# # for(id in 1:nrow(accMat)){
# #   avgRecVec[id] <- mean(accMat[id,])
# #   if(avgRecVec[id] > maxAvg){
# #     maxAvg <- avgRecVec[id]
# #     maxAvgInd <- id
# #   }
# #   varRecVec[id] <- var(accMat[id,])
# #   if(varRecVec[id] < minVar){
# #     minVar <- varRecVec[id]
# #     minVarInd <- id
# #   }
# # }
# 
# 
# # #--counting accuracy > 0.70 for each row
# # sumGreaterThanPercent <- c(rep(NA, times = nrow(accMat)))
# # sumGreaterThanPercentind <- c()
# # for(id1 in 1:nrow(accMat)){
# #   sumGreaterThanPercent[id1] <- sum(accMat[id1,] > 0.65)
# #   if(sumGreaterThanPercent[id1] > 19){
# #    sumGreaterThanPercentind <-  append(sumGreaterThanPercentind, id1)
# #   }
# # }
# # 
# # 
# # #--checking how set.seed works
# # x <- matrix(NA, 10,10)
# # y <- matrix(NA, 10, 10)
# # z <- matrix(NA, 10, 10)
# # set.seed(234)
# # d1 <- sample(1:10,3)
# # for(i in 1:10){
# #   x[i,] <- sample(1:10, 10)
# #   y[i,] <- sample(1:10, 10)
# #   z[i,] <- runif(10, 0, 1)
# # }
# # c <- sample(1:10,3)
# # #set.seed(234)
# # d2 <- sample(1:10,3)
# 
# 
# 
# # set.seed(12)
# # x <- sample(1:10, 10)
# # y <- sample(1:10, 10) 

#=============================================================================================================

