#(Imp Note: always initialize lists,vetors, etc. outside all loops to avoid-> reinitializing on each iteration and losing data).



#==========================Logistic Regression (without regularization, problem - 'Perfect seperation')=================================================================
#--training logistic regression model 
# modelsList[[z]] <- glm(labels~., family = binomial(link = "logit"), data = appDatasetListTrainData[[z]])

#--predicting on the test set and getting accuracy of the model, then storing it in the accuracyVec...
#...which keeps a record of accuracy of all models.
# accuracyVec <- c(rep(NA, times = totalNumOfAppDatasets))
# probabilities <- predict(modelsList[[z]], appDatasetListTestData[[z]], type = "response") # type="response" returns probabilities
# predictions <- ifelse(probabilities > 0.5, 1, 0)
# misClassErr <- mean(predictions != appDatasetListTestData[[z]]$labels)
# accuracyVec[z] <- 1-misClassErr

#-- export data to excel 
#xlsx::write.xlsx(appDatasetList[[z]], file = sprintf("ApplicantDatasetForC-%i(Criteria-%i).xlsx", z, criteriaNumber))
#=================================================================================================================================================================


#======================Shuffling a subset of labels to break the problem of 'Perfect Seperation'=================
# Note that this helps sometimes and in general is not recommended as we have available techniques like...
#...regularization (using glmnet, etc) which are significantly better, and more intuitive.
# perturb <- 12
# labelsForTmpAppDataset[1:perturb,] <- sample(labelsForTmpAppDataset[1:perturb,], perturb)


#                     #                    OR                       #                     #


#-- figuring out total number of labels to shuffle (a number chosen randomly b/w 11%(of the nrow in the training set)...
#...and 15%(of the number of row in the training set))
#rngStart <- round(0.20*(nrow(tmpAppDatasetConstructor)))
#rngEnd <- round(0.30*(nrow(tmpAppDatasetConstructor)))
#numTrnExmpShuff <- sample(c(rngStart:rngEnd), 1) #using 12% of the labels for shuffling breaks 'Perfect linearity'.

#-- choosing a random number b/w 1 and nrow(training set)/2, from where onwards shuffle will start
#dataToChooseStPt <- c(1: ((nrow(tmpAppDatasetConstructor))/2)) 
#startShuffFrom <- sample(dataToChooseStPt, 1)

#-- performing shuffling

#labelsForTmpAppDataset[c(startShuffFrom:(startShuffFrom+numTrnExmpShuff)),] <- sample(labelsForTmpAppDataset[c(startShuffFrom:(startShuffFrom+numTrnExmpShuff)),], numTrnExmpShuff+1)
                
#================================================================================================================


#-- training the personalizedJobClassifier using nn
# names(tmpVarTrainSet.XYPair) <- LETTERS[1:21]
# n <- names(tmpVarTrainSet.XYPair)
# f <- as.formula(paste("U ~", paste(n[!n %in% "U"], collapse = " + ")))
# initWeights <- (rnorm(536,0,1))*10^-4
# personalizedJobClassifierNN <- neuralnet(f, data=tmpVarTrainSet.XYPair, hidden = c(5,5), threshold = 0.003, startweights = initWeights,  learningrate = 0.003, algorithm = "backprop", act.fct = "tanh", linear.output = FALSE)

#==============================================================================================================

#--training the 'personalizedJobClassifier' (using 'glm' package)
# trainRows <- sample(c(1:nrow(companyDataset)), round(0.7*(nrow(companyDataset))))
# personalizedJobClassifier <- glm(labels~.,family= binomial(link = "logit"), data = companyDataset[trainRows,])
# prob <- predict(personalizedJobClassifier, companyDataset[-trainRows,(1:totalNumOfFeat)], type = "response")
# predictions <- ifelse(prob>0.5, 1, 0)
# accuracy <- mean(predictions == companyDataset[-trainRows,(totalNumOfFeat+1)])

#==============================================================================================================

# #--training the 'personalizedJobClassifier' (using 'neuralnet' package)
# trainRows <- sample(c(1:nrow(companyDataset)), round(0.7*(nrow(companyDataset))))
# n <- colnames(companyDataset)
# f <- as.formula(paste("labels ~", paste(n[!n %in% "labels"], collapse = " + ")))
# personalizedJobClassifierNN <- neuralnet(f, data = companyDataset[trainRows,], hidden = c(6), linear.output = FALSE, act.fct = "logistic")
# 
# #--predictive accuracy on the training set
# probTrn <- personalizedJobClassifierNN$net.result
# predTrn <- ifelse((probTrn[[1]][,1]>0.5),1,0)
# accuracyPersJobClassModTrnSet <- mean(predTrn == (companyDataset[trainRows,(totalNumOfFeat+1)]))
# 
# #--predictive accuracy on the test set
# tstSetRes <- compute(personalizedJobClassifierNN, companyDataset[-trainRows,(1:totalNumOfFeat)])
# probTst <- tstSetRes$net.result
# predTst <- ifelse((probTst[,1] > 0.5), 1, 0)
# accuracyPersJobClassModTstSet <- mean(predTst == (companyDataset[-trainRows,(totalNumOfFeat+1)]))

#===========================================================================================================

# #--training personalizedJobClassifier model using cv.glmnet
#grandCompModList <- list()
#grandCompDatasetList <- list()
#grandMyPersLabelsList <- list()
#grandTrainRowsList <- list()
#grandTestRowsList <- list()
#accVec <- c(NA, times = total)  #total = 100

# index <- sample(c(1:nrow(companyDataset)), nrow(companyDataset))
# trainRows <- index[1:(0.7*(nrow(companyDataset)))]
# testRows <- index[-trainRows]
# personalizedJobClassifier <- cv.glmnet(companyDataset[trainRows,], myPersonalizedLabels[trainRows], family = "binomial", type.measure="class")
# 
# # #--predicting on test set
# # predTst <- predict(personalizedJobClassifier, companyDataset[-trainRows,], s = "lambda.min", type="class", nfolds = 10) 
# # accuracyPJC <- mean(predTst == myPersonalizedLabels[-trainRows]) 
# 
# #--pred on training set
# tp <- predict(personalizedJobClassifier, companyDataset[trainRows,], s = "lambda.min", type="class")
# 
# accVec[g] <- mean(tp == myPersonalizedLabels[trainRows])
#tstAccVec <- c(rep(NA, times = total))
# grandCompDatasetList[[g]] <- companyDataset 
# grandCompModList[[g]] <- personalizedJobClassifier
# grandMyPersLabelsList[[g]] <- myPersonalizedLabels
# grandTrainRowsList[[g]] <- trainRows
# grandTestRowsList[[g]] <- testRows
# 
# 
# if(g != total){
# print(sprintf("Iteration-%i -> complete...", g), quote = FALSE)
# }
# 
# else{
#   print(sprintf("Iteration-%i -> complete. All Done!", g), quote = FALSE)
# }
# 
# }
# 
# 
# print(".........................................................................")
# print("Now predciting on the test set...")
# 
# for(d in 1:total){
#   ind <- as.vector(grandTestRowsList[[d]])
#   tp <- predict(grandCompModList[[d]], grandCompDatasetList[[d]][ind,] , s = "lambda.min", type = "class")
#   tstAccVec[d] <- mean(tp == grandMyPersLabelsList[[d]][ind])
#   print(sprintf("Iteration-%i/100 complete", d))
#
# } # when uncomment add for(g in 1:total){ at the top
# 
# endTime <-system.time()
# elapsedTime <- endTime - startTime
# print(sprintf("Elapsed Time: %f",elapsedTime), quote = FALSE)
# print("All done!")
# 
# 

#=============================================================================================================



# specific code that might be needed in R markdown file------------------------
  
# was placed after second type of logic correction in part 1.
#-------------------sanity check (distribution of total 1's and total 0's)-----------------#
# sum_vec <- c(rep(NA, times = nrow(tmpAppDatasetConstructor)))
# for(o in 1: nrow(tmpAppDatasetConstructor)){
#   sum_vec[o] <- sum(tmpAppDatasetConstructor[o,])
# }
# hist(sum_vec) # distribution of total 1's (by row)
# hist(20 - sum_vec) # distribution of total 0's (by row)
#------------------------------------------------------------------------------------------#  

#==============================================================================

# code for predicting on a single row in glmnet (Part-1) (the typecasting is important)
#---------------this works!------------------------------------
# x <- as.numeric(c(1,1,1,1,1,1,1,0,0,0,1,1,0,0,1,1,0,1,0,1))
# label <- predict(modelsList[[34]], newx = matrix(x,1,20), s = "lambda.min", type="class")

#===============================================================================


# (Part1)code for extracting the criteria numbers corresponding to the companies in which User1 got the job.
# placed after passing the myAppFeatVec through all of 100 models.

# indexGotTheJob <- c()
# indexNoJob <- c()
# for(r in 1:length(criteriaNumberRecord)){
#   if(myPersonalizedLabels[r] == 1){
#     indexGotTheJob <- append(indexGotTheJob,r)
#   }
#   else{
#     indexNoJob <- append(indexNoJob,r)
#   }
# }
# 
# #plot(hist(criteriaNumberRecord[indexGotTheJob])) #distribution of criteria numbers when User1 got the job
# #plot(hist(criteriaNumberRecord[indexNoJob])) #distribution of criteria numbers where User1 didn't got the job
# #plot(density(criteriaNumberRecord[indexGotTheJob])) #kernel density estimation
# #plot(density(criteriaNumberRecord[indexNoJob])) # kernel density estimation

#============================================================================================================

# (Part2, just before training), used to generate formulas in format y~ x1+x2+...
# n <- colnames(companyDataset)
# f <- as.formula(paste("labels ~", paste(n[!n %in% "labels"], collapse = " + ")))

#============================================================================================================

# Part-2 Top candidates for the Personalozed Job Classifier model
# Top candidates for final models:

#setting 27: alpha = 0.01846625, lambda = 0.9215817
#setting 19: alpha = 0.01385693, lambda = 1.720017 
#setting 62: alpha = 0.1749644, lambda = 0.5262626
#setting 3: alpha = 0.1387897, lambda = 1.830748
#setting 13: alpha = 0.1387897, lambda = 1.720017

#--results on individual runs
#searchGridTwo(S): alpha = 0.01281309, lambda = 1.634585 , 0.66 +- 1
#searchGridTwo(S): alpha = 0.01300321, lambda = 1.793407 , 0.55 +- 5
#searchGridTwo(S): alpha = 0.04709933 lambda = 1.760286 , 0.59 +- 2
#searchGridTwo(S): alpha = 0.04106969, lambda = 0.4932048 , 0.56 +- 12
#searchGridTwo(S): alpha = 0.02462365, lambda = 1.882838 , 0.64 +- 2
#searchGridTwo(S): alpha = 0.03179076, lambda = 1.823945 , 0.64 +- 0.5

#searchGridOneUsable <- expand.grid(alpha = runif(10, 0, 0.4), lambda = sort(runif(10, 0, 2), decreasing = TRUE))
#searchGridTwo <- expand.grid(alpha = runif(10, 0.01, 0.2 ), lambda = sort(runif(10, 0.4, 1.9), decreasing = TRUE))

#==============================================================================================================

