# Personalized Job Classifier: Can you predict which company will hire you?
# =============================================================================

# Metadata---------------------------------------------------------------------
  # Author: Aarsh Batra
  # Date  : October, 1, 2017

# Recommended reading----------------------------------------------------------
  # Please read the paper which I wrote in correspondance to this code. The 
  # paper lays down the theortical foundation and the terminology for the model
  # It is 12 pages and wouldn't take much time. I strongly recommend reading it
  # as that is important in order to understand the code ahead. Moving ahead I
  # will assume that you have read the paper.

# About------------------------------------------------------------------------
  # The Personalized Job Classifier model takes as its input a company feature
  # vector, and output a label (0/1) that would inform it's user whether or not
  # he gets a job in that company. 'Personalized' means that the classifier is
  # trained and tuned for a specific 'user'(I will assume that the user is ca-
  # lled 'User1', same convention is used in the paper) and is rendered unusa
  # -ble or atleast unfit for anyone else to 'directly' use. The code ahead is 
  # a simulation of such a model.

# Purpose----------------------------------------------------------------------
  # The purpose of this simulation is to serve as a guide to anyone wanting to
  # build a Personalized Job Classifier. The code ahead covers the entire pro-
  # -cess of building such a model, which involves but is not limited to buil-
  # -ding datasets from scratch, followed by training, tuning, prediction and 
  #  measuring of accuracy of the models built.

# Notes-------------------------------------------------------------------------
    # Note-1:
      # Both 'Applicant Datasets' and 'Company Dataset' use binary features
      # only. This is done to for simulation purposes and for generation of  
      # datasets from scratch using commands like 'runif'. But, when building a 
      # full fledged implementation of the model, one must use techniques like 
      # web scraping to generate features much richer than the binary ones.

    # Note-2:
      # I have built ths simulation by assuming that User1 is a person whose
      # academic credentials are in the social sciences, data Analytics
      # fields. Given that I have appropriately constructed the features
      # for the applicant dataset and also by affect assumed that companies
      # C-1 to C-100 are the type of companies in which a person from Social
      # Sciences is interested. Other people may build classifiers by assuming
      # different settings.
    
    # Note-3:
      # Finally, when I mean 'company' that does not mean 'company' in the
      # strict sense. It generally refers to any kind of employer User1 
      # might be interested in, e.g. University, NGO, Research Group, etc.
  
# Formal Structure (What is the code doing?)-----------------------------------
  # 1.  Parameter initializations
  # 2.  for each company do
  # 3.   | Build applicant dataset (sans labels column)
  # 4.   | Assign row and column names to applicant dataset
  # 5.   | Perform logic correction on applicant dataset
  # 6.   | Generate labels using criteria number
  # 7.   | Add labels column to the appicant dataset
  # 8.   | Set Controls for model training, e.g train/test split, etc.
  # 9.   | Train model using training set, save model for future ref
  # 10.  | Create User1's applicant feature vector
  # 11.  | Pass AFV through the trained model to get a label.
  # 12.  | Save the label in 'myPersonalizedLabels' vector
  # 13.  | Save other datasets for future reference.
  # 14. end
  # 15. Build company dataset (sans labels column)
  # 16. Assign row and column names to company dataset
  # 17. Perform logic correction on company dataset
  # 18. Add 'myPersonalizedLabels' as the labels column for company dataset
  # 19. Set Controls for training the 'Personalized Job Classifier' model
  # 20. Train model using training set, save model.
  # 21. Done! Use the model on 'new' unseen companies.


# used later to calculate elapsed Time-----------------------------------------
startTime <- proc.time()

# set current working directory------------------------------------------------
# setwd("C:/Users/pc/Dropbox/RA Docs/PersonalizedJobClassifier")

# set seed---------------------------------------------------------------------
set.seed(336)

# load pacakges------------------------------------------------------------
library(knitr)
library(caret)
library(magrittr)  # for using pipes (%>%)

# parameter initialization-----------------------------------------------------
kTotalNumOfApp <- 100
kTotalNumOfAppDatasets <- 100
kTotalNumOfFeat <- 20
kTotalNumOfCriterions <- 10

# lists for storing datasets for debugging-------------------------------------
  # list of all applicant datasets (for companies C-1 to C-100)
  # where appDatasetList[[z]] is the applicant dataset for the 
  # z'th company (C-z). Also, making lists for storing the train  
  # and test subsets of appDatasetList[[z]]
appDatasetList <- list()
appDatasetListTrainData <- list()
appDatasetListTestData <- list()

# list for storing models (M-1 to M-100, corresp to C-1 to C-100)--------------
modelsList <- list()

# more on this in later code---------------------------------------------------
criteriaNumberRecord <- c(rep(NA, times = kTotalNumOfAppDatasets))

# stores labels as returned by running User1's AFV through models M-1 to M-100
myPersonalizedLabels <- c()

# user interface
print(sprintf(">> Simulation Part-1: total iterations = %i", 
              kTotalNumOfAppDatasets), quote = FALSE)

print("     Running...", quote = FALSE)

# Part-1: Build 100 models M-1 to M-100 corresp to C-1 to C-100----------------
for (z in 1:kTotalNumOfAppDatasets){
  
  # filling applicant dataset (tmpAppDatasetConstrutor) of C-z-----------------    
    # Generate 20 random numbers (single row in the applicant dataset of a com-
    # -pany) for each of the total num of app (100) = raw applicant dataset
    # for C-z
  tmpAppDatasetConstructor <- matrix(NA, nrow = kTotalNumOfApp, 
                                     ncol = kTotalNumOfFeat)
  
  for (i in 1:kTotalNumOfApp){
    temp <- runif(kTotalNumOfFeat, 0, 1)
    temp <- temp > (runif(1, 0, 1))
    tmpAppDatasetConstructor[i, ] <- temp
  }
   
  # converting raw appDataset for C-z from logical(TRUE/FALSE) to numeric (0/1)
  tmpAppDatasetConstructor <- apply(tmpAppDatasetConstructor, c(1, 2), 
                                    as.integer)
  
  # defining row and column names for tmpAppDatasetConstructor
  tempRowNamesVec <- c(rep(NA, times = nrow(tmpAppDatasetConstructor)))
  for (p in 1:nrow(tmpAppDatasetConstructor)){
    tempRowNamesVec[p] <- sprintf("App-%i", p)
  }
  
  rownames(tmpAppDatasetConstructor) <- tempRowNamesVec
  colnames(tmpAppDatasetConstructor) <- c("hi sch deg?", 
  "hi sch deg highest hns?",	"clg deg in Econ/SocSc/CS?",	
  "highest hns in any cleg deg?",	
  "crsewrk in quant sub e.g Lin Alg, Calculus?",	
  "strong grades (e.g >90%) in any qnt sub?",	"grad sch deg?",	
  "grad sch deg highest hns?", "R/MATLAB/STATA/SAS + fmlr?",	
  "R/MATLAB/..+prty cmfrtble?",	
  "gen purp prg lng (e.g. Python) + fmlr?",	
  "gen purp prg lng (e.g. Python) + prty cmf?",	
  "dtabse mng sys (e.g MySQl ..) + fmlr?",	
  "dtabse mng sys (e.g MySQl ..) + pr cmf?", "Hadoop/Apache Spark+ fmlr?",
  "Hadoop/Apache Spark prty cmfrtable?",	
  "exp estmting eco'ric mod in prg lng's?",	"exp in experimental design?",
  "Prev rsrch exp/ frml empl?",	"rec any awards/fellowships/schlrships?")
  
  
  # logic correction in raw 'tmpAppDatasetConstructor'-------------------------
    # each row of tmpAppDatasetConstructor contains randomly distributed 0's 
    # and 1's. Given the colnames (features) these raw 0's and 1's will have
    # logical errors, each row in turn needs to be checked for possible
    # logical inconsistencies and be corrected if found.
  
  # e.g.of logical mistake corrected: If rec hi sch deg with hns is TRUE(1) 
  # then rec hi sc deg cannot be false(0). Similar to this all the column
  # numbers in 'colNumWithPotClash' has a potential for a clash with other
  # columns. Look at the colnames to learn more.
  
  colNumWithPotClash <- c(2, 4, 6, 8, 10, 12, 14, 20) 
  for (l in 1:nrow(tmpAppDatasetConstructor)){
    for (m in 1:length(colNumWithPotClash)){
      if (tmpAppDatasetConstructor[l, colNumWithPotClash[m]] == 1){
        if (tmpAppDatasetConstructor[l, (colNumWithPotClash[m] - 1)] == 0){
          tmpAppDatasetConstructor[l, (colNumWithPotClash[m] - 1)] <- 1
        }
        else{
          next
        }
      }
      else{
        next
      }
    }
  }
  
  # another type of logical mistake corrected----------------------------------
    # Type of logical mistake corrected (or treat it as an assumption): If grad 
    # sch deg rec =TRUE, then both cleg deg rec and hi sch deg rec must be TRUE
  
  for (n in 1:nrow(tmpAppDatasetConstructor)){
    if (tmpAppDatasetConstructor[n, 7] == 1){
      if ((tmpAppDatasetConstructor[n, 3]) == 1 &&
        (tmpAppDatasetConstructor[n, 1] == 1)){
        next
      }
      else{
        tmpAppDatasetConstructor[n, 3] <- 1
        tmpAppDatasetConstructor[n, 1] <- 1
      }
    }
    else{
      if (tmpAppDatasetConstructor[n, 3] == 1){
        if (tmpAppDatasetConstructor[n, 1] == 0){
          tmpAppDatasetConstructor[n, 1] <- 1
        }
        else{
          next
        }
      }
      else{
        next
      }
    }
  }
  
  # coerce from type 'matrix' to type 'data.frame', for ease in handling
  tmpAppDatasetConstructor <- as.data.frame(tmpAppDatasetConstructor)
  
  # label construction for 'tmpAppDatasetConstructor'--------------------------
    # labels construction for tmpAppDatsetConstructor using a randomly chosen
    # criteria (out of a total of 10 criterions). Each criteria has a 
    # 'degree of selectivity' associated with it (scale ranging from 1 to 10), 
    #  such that criteria-1 is most selective (10/10), criteria-2 (9/10) ... 
    # criteria-10 (1/10, least selective).
    
    # Degree of selectivity is meant to associate a company with a particular 
    # selection criteria. This is done for simulation purposes, to generate  
    # labels for our applicant dataset. The purpose of this criterion system,
    # is to try to simulate the real world differences in selection criteria,
    # between companies and serve as a good representative of the total set
    # of possible criterions out there.
  
  # randomly choose a criteria number for C-z----------------------------------
  criteriaNumber <- sample(c(1:kTotalNumOfCriterions), 1)
  
  # initialize the labels column with NA's
  labelsForTmpAppDataset <- as.data.frame(matrix(NA, 
                                         nrow = nrow(tmpAppDatasetConstructor),
                                         ncol = 1))
  
  # Almost all criterions below (total 10) have a general theme, which they 
  # use to automatically label training data for simulation purposes. So, for 
  # a particular training example (a single row in the applicant dataset for a
  # particular company), a label of 1 is assigned if the number of "1's" in 
  # the row are greater or equal to a particular threshold. Next, a label of 
  # zero is assigned, if the number of "1's" in the row are less than equal to 
  # a particular threshold. For rest of the cases, a 'custom' decision making 
  # mechanism is employed, which if satisfied, then the row is labelled as '1' 
  # , else '0'. Think of '1' as representing 'presence of a quality', and of
  # '0' as 'absence of a quality'. 
  
  switch(criteriaNumber,
         "1" = for (q in 1:nrow(tmpAppDatasetConstructor)){
           if (sum(tmpAppDatasetConstructor[q, ]) >= 18){
             labelsForTmpAppDataset[q, ] <- 1
           }
           else if (sum(tmpAppDatasetConstructor[q, ]) <= 14){
             labelsForTmpAppDataset[q, ] <- 0
           }
           else{
             if ((tmpAppDatasetConstructor[q, 2] == 1) && 
               (tmpAppDatasetConstructor[q, 4] == 1) && 
               (tmpAppDatasetConstructor[q, 6] == 1) && 
               (tmpAppDatasetConstructor[q, 10] == 1) && 
               (tmpAppDatasetConstructor[q, 19] == 1) && 
               (tmpAppDatasetConstructor[q, 12] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
           }
         },
         
         "2" = for (q in 1:nrow(tmpAppDatasetConstructor)){
           if (sum(tmpAppDatasetConstructor[q, ]) >= 17){
             labelsForTmpAppDataset[q, ] <- 1
           }
           else if (sum(tmpAppDatasetConstructor[q, ]) <= 13){
             labelsForTmpAppDataset[q, ] <- 0
           }
           else{
             if ((tmpAppDatasetConstructor[q, 2] == 1) &&
                (tmpAppDatasetConstructor[q, 4] == 1) && 
                (tmpAppDatasetConstructor[q, 6] == 1) && 
                (tmpAppDatasetConstructor[q, 10] == 1) &&
                (tmpAppDatasetConstructor[q, 12] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
           }
         },
         
         "3" = for (q in 1:nrow(tmpAppDatasetConstructor)){
           if (sum(tmpAppDatasetConstructor[q, ]) >= 16){
             labelsForTmpAppDataset[q, ] <- 1
           }
           else if (sum(tmpAppDatasetConstructor[q, ]) <= 12){
             labelsForTmpAppDataset[q, ] <- 0
           }
           else{
             if ((tmpAppDatasetConstructor[q, 2] == 1) && 
                (tmpAppDatasetConstructor[q, 4] == 1) && 
                (tmpAppDatasetConstructor[q, 6] == 1) &&
                (tmpAppDatasetConstructor[q, 10] == 1) && 
                (tmpAppDatasetConstructor[q, 11] == 1) && 
                (tmpAppDatasetConstructor[q, 14] == 1) && 
                (tmpAppDatasetConstructor[q, 15] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
           }
         },
         
         "4" = for (q in 1:nrow(tmpAppDatasetConstructor)){
           if (sum(tmpAppDatasetConstructor[q, ]) >= 15){
             labelsForTmpAppDataset[q, ] <- 1
           }
           else if (sum(tmpAppDatasetConstructor[q, ]) <= 11){
             labelsForTmpAppDataset[q, ] <- 0
           }
           else{
             if ((tmpAppDatasetConstructor[q, 2] == 1) && 
                (tmpAppDatasetConstructor[q, 4] == 1) && 
                (tmpAppDatasetConstructor[q, 6] == 1) && 
                (tmpAppDatasetConstructor[q, 10] == 1) &&
                (tmpAppDatasetConstructor[q, 11] == 1) &&
                (tmpAppDatasetConstructor[q, 14] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
           }
         },
         
         "5" = for (q in 1:nrow(tmpAppDatasetConstructor)){
           if (sum(tmpAppDatasetConstructor[q, ]) >= 14){
             labelsForTmpAppDataset[q, ] <- 1
           }
           else if (sum(tmpAppDatasetConstructor[q, ]) <= 10){
             labelsForTmpAppDataset[q, ] <- 0
           }
           else{
             if ((tmpAppDatasetConstructor[q, 2] == 1) && 
                (tmpAppDatasetConstructor[q, 4] == 1) && 
                (tmpAppDatasetConstructor[q, 6] == 1) && 
                (tmpAppDatasetConstructor[q, 10] == 1) && 
                (tmpAppDatasetConstructor[q, 11] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
           }
         },
         
         "6" = for (q in 1:nrow(tmpAppDatasetConstructor)){
           if (sum(tmpAppDatasetConstructor[q, ]) >= 13){
             if ((tmpAppDatasetConstructor[q, 4] == 1) && 
                (tmpAppDatasetConstructor[q, 6] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
             
           }
           else if (sum(tmpAppDatasetConstructor[q, ]) <= 9){
             labelsForTmpAppDataset[q, ] <- 0
           }
           else{
             if ((tmpAppDatasetConstructor[q, 4] == 1) && 
                (tmpAppDatasetConstructor[q, 6] == 1) && 
                (tmpAppDatasetConstructor[q, 9] == 1) && 
                (tmpAppDatasetConstructor[q, 17] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
           }
         },
         
         "7" = for (q in 1:nrow(tmpAppDatasetConstructor)){
           if (sum(tmpAppDatasetConstructor[q, ]) >= 12){
             if ((tmpAppDatasetConstructor[q, 3] == 1) && 
                (tmpAppDatasetConstructor[q, 6] == 1) && 
                (tmpAppDatasetConstructor[q, 9] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
             
           }
           else if (sum(tmpAppDatasetConstructor[q, ]) <= 8){
             labelsForTmpAppDataset[q, ] <- 0
           }
           else{
             if ((tmpAppDatasetConstructor[q, 3] == 1) &&
                (tmpAppDatasetConstructor[q, 6] == 1) &&
                (tmpAppDatasetConstructor[q, 10] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
           }
         },
         
         "8" = for (q in 1:nrow(tmpAppDatasetConstructor)){
           if (sum(tmpAppDatasetConstructor[q, ]) >= 11){
             if ((tmpAppDatasetConstructor[q, 3] == 1) &&
                (tmpAppDatasetConstructor[q, 6] == 1) &&
                (tmpAppDatasetConstructor[q, 12] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
             
           }
           else if (sum(tmpAppDatasetConstructor[q, ]) <= 7){
             labelsForTmpAppDataset[q, ] <- 0
           }
           else{
             if ((tmpAppDatasetConstructor[q, 3] == 1) &&
                (tmpAppDatasetConstructor[q, 6] == 1) &&
                (tmpAppDatasetConstructor[q, 12] == 1) &&
                (tmpAppDatasetConstructor[q, 16] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
           }
         },
         
         "9" = for (q in 1:nrow(tmpAppDatasetConstructor)){
           if (sum(tmpAppDatasetConstructor[q, ]) >= 10){
             if ((tmpAppDatasetConstructor[q, 4] == 1) &&
                (tmpAppDatasetConstructor[q, 6] == 1) &&
                (tmpAppDatasetConstructor[q, 17] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
             
           }
           else if (sum(tmpAppDatasetConstructor[q, ]) <= 6){
             labelsForTmpAppDataset[q, ] <- 0
           }
           else{
             if ((tmpAppDatasetConstructor[q, 4] == 1) &&
                (tmpAppDatasetConstructor[q, 6] == 1) &&
                (tmpAppDatasetConstructor[q, 10] == 1) &&
                (tmpAppDatasetConstructor[q, 17] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
           }
         },
         
         "10" = for (q in 1:nrow(tmpAppDatasetConstructor)){
           if (sum(tmpAppDatasetConstructor[q, ]) >= 9){
             if ((tmpAppDatasetConstructor[q, 3] == 1) &&
                (tmpAppDatasetConstructor[q,6] == 1) && 
                (tmpAppDatasetConstructor[q, 12] == 1) &&
                (tmpAppDatasetConstructor[q, 16] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1  
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
             
           }
           else if (sum(tmpAppDatasetConstructor[q, ]) <= 5){
             labelsForTmpAppDataset[q, ] <- 0
           }
           else{
             if ((tmpAppDatasetConstructor[q, 3] == 1) &&
                (tmpAppDatasetConstructor[q, 6] == 1) &&
                (tmpAppDatasetConstructor[q, 12] == 1) &&
                (tmpAppDatasetConstructor[q, 14] == 1) &&
                (tmpAppDatasetConstructor[q, 16] == 1) &&
                (tmpAppDatasetConstructor[q, 19] == 1)){
               
               labelsForTmpAppDataset[q, ] <- 1
             }
             else{
               labelsForTmpAppDataset[q, ] <- 0
             }
           }
         })  # switch statement ends here
  
  # storing criteria number of C-z for later reference-------------------------
  criteriaNumberRecord[z] <- criteriaNumber 
  
  # stacking 'labels' column at the end of tmpAppDatasetConstructor------------
    # It turns out that labels generated from the above process leads to 
    # 'perfect separation' in data. This happens because in all of the 
    # criterions we use some sort of thresholding (above which label is 1, 
    #  below which label is 0) which leads to such an effect. So to deal with 
    # this issue we add regularization to our model via the 'caret' package 
    # using the method glmnet. This will help to reduce the extent of 
    # overfitting in the model and help to make the model more generalizable 
    # and also intuitive for simulation purposes.
  tmpAppDatasetConstructor[, (kTotalNumOfFeat+1)] <- (labelsForTmpAppDataset
                                                      %>% as.data.frame())
                                                     
  colnames(tmpAppDatasetConstructor)[kTotalNumOfFeat+1] <- "labels"
  
  # setting various controls for training of the model-------------------------
  indexTrainTmp <- createDataPartition(tmpAppDatasetConstructor$labels,
                                       p = 0.70, list = FALSE)
  trainingSetTmp <- tmpAppDatasetConstructor[indexTrainTmp, ]  # training set
  testSetTmp <- tmpAppDatasetConstructor[-indexTrainTmp, ]  # test set
  controlsTmp <- trainControl(method = "repeatedcv", repeats = 5)
  
  # setting up a search grid for k fold cross validation-----------------------
    # below search grid contains only one value each for 'alpha' and 'lambda',
    # this is because I have already trained and tuned the model several times
    # and the below values are the optimal ones as they performed best on avg
    # in k fold cross validation. Note alpha = 1 (lasso penalty), alpha = 0,
    # (ridge penalty). See ?glmnet for more details.
  searchGridTmp <- expand.grid(alpha = 0.07206841, lambda = 0.03755844) 
  
  # training using caret package's 'glmnet' method-----------------------------
  modelsList[[z]] <- train(x = trainingSetTmp[, 1:kTotalNumOfFeat], 
                           y = as.factor(
                               trainingSetTmp[, (kTotalNumOfFeat+1)]), 
                           method = "glmnet", 
                           metric = "Accuracy",
                           trControl = controlsTmp,
                           tuneGrid = searchGridTmp
                          )
  
  # constructing User1's applicant feature vector------------------------------
    # 'myAppFeatVec' would then be feeded to model M-z (corresponding to compa-
    # -ny C-z) to get a label L-z, which will then be stored in 
    # 'myPersonalizedLabels' vector.
  myAppFeatVec <- as.numeric(c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 
                               1, 1, 0, 0, 1, 1, 0, 1, 0, 1))
  myAppFeatVec <- as.data.frame(matrix(myAppFeatVec, 1, kTotalNumOfFeat))
  colnames(myAppFeatVec) <- (colnames(tmpAppDatasetConstructor)[
    1:kTotalNumOfFeat])
  lbl <- predict.train(modelsList[[z]], newdata = myAppFeatVec, type = "raw")
  lbl <- as.numeric(lbl)
  myPersonalizedLabels <- append(myPersonalizedLabels, lbl)

  # store datasets for future reference----------------------------------------
    # transfer 'tmpAppDatasetConstructor' to appDatasetList[[z]] (corresponding 
    # to company C-z) and also storing in two seperate lists the corresponding
    # train and test sets for each applicant dataset.
  appDatasetList[[z]] <- tmpAppDatasetConstructor
  appDatasetListTrainData[[z]] <- tmpAppDatasetConstructor[indexTrainTmp, ]
  appDatasetListTestData[[z]] <-  tmpAppDatasetConstructor[-indexTrainTmp, ]
  
  # user interface
  if (z != kTotalNumOfAppDatasets){
    print(sprintf("  Iteration %i/%i: Complete...", z, kTotalNumOfAppDatasets),
          quote = FALSE)
  }else{
    print(sprintf("  Iteration %i/%i: Complete", z, kTotalNumOfAppDatasets))
  }
    
}  # top most for loop for Part 1 ends here.

# accuracy stats of the 100 models trained above-------------------------------
accuracyModelsList <- lapply(modelsList, function(x) x$results$Accuracy)
minAccuracyModelsList <- min(as.numeric(accuracyModelsList))
maxAccuracyModelsList <- max(as.numeric(accuracyModelsList))

# Part-1 ends here-------------------------------------------------------------

# Part-2: Build Company Dataset and the 'Personalized Job Classifier model'----

# company dataset described in words
  # What does row-1 in company dataset represent?: First comes the Company Name
  # (C-1), that is followed by a 'n' dimensional company feature vector (V-1),  
  # which defines C-1 in terms of 'n' features, after that comes a label 
  # (L-1), this label L-1 comes from running User-1's applicant feature vector
  # through the model M-1 (remember, M-1 took as its input an applicant feature
  # vector and outputted whether or not that  applicant got the job in company
  # C-1) which outputs a label (let's assume that label was 1, i.e. User1 got 
  # the job). So, an informal way to read row-1 of the table is as follows,
  # "As per M-1, User1 got the job in company C-1 which has features V-1"
  #
  # We will follow a similar procedure for row-2 to row-100, while keeping in
  # mind that just like L-1, each label L-2 to L-n comes from running User-1's
  # applicant feature vector in turn through models M-2 to M-n (this is what
  # makes the model personalized to User-1 as all labels L-1 to L-n in our
  # entire company dataset correspond to User-1).
  #
  # For more information read the corresponding paper.

# user interface---------------------------------------------------------------
print("-----------------------------------------------", quote = FALSE)
print(">> Simulation Part-2", quote = FALSE)
print("     Running...", quote = FALSE)

# initializing companyDataset matrix (sans labels)----------------------------- 
kTotalNumOfComp <- kTotalNumOfAppDatasets
companyDataset <- matrix(NA, kTotalNumOfComp, kTotalNumOfFeat)

# filling company dataset------------------------------------------------------
  # Generate 20 random numbers (single row in the company dataset or in other
  # words a feature vector for a single company) for each of 100 companies.
  # This will give us the 'raw' companyDataset.
companyDataset <- apply(companyDataset, 1, 
                        function(x) x <- runif(kTotalNumOfFeat, 0, 1))
companyDataset <- apply(companyDataset, 1, function(x) x <- (x > runif(1, 0, 1)))

# converting company dataset from logical(T/F) to numeric(0/1)-----------------
companyDataset <- apply(companyDataset, c(1, 2), as.integer)

# constructing rownames for company dataset------------------------------------
tempRowNamesVec <- c(rep(NA, times = kTotalNumOfComp))
for (b in 1:nrow(companyDataset)){
  tempRowNamesVec[b] <- sprintf("Company-%i", b)
}

# assigning names to rows and columns of the companyDataset-------------------
rownames(companyDataset) <- tempRowNamesVec
colnames(companyDataset) <- c("emplyrUniversity", "emplyrPrivateComp", 
"emplyrNGO",	"emplyrListedOnAStockExchange",	"emplyrWorkTypeResearch",
"emplyrISO9001OrOtherEqvlentCompliant",	"emplyrLessThan1YrOldInItsSec",
"emplyrGreaterThan10YrsOldInItsSec", "NumOfEmployeesLessThan100",
"NumOfEmployeesGreaterThan1000", "emplyrCountAsOneOfTop50InItsSector",
"emplyrInternationallyRecognizedViaItsWork",	
"emplyrInternationalPhysicalPresence", "emplyrRecipientOfAwardsInRecogOfWork",
"emplyrHireOnlyLocallyLessCulturalDiversity",	
"emplyrPerformPoorOnSocialRespIndexLikeCSR",	
"emplyrHasSignificantOnlinePresence", "emplyrSignifSpendingInRandD",	
"emplyrSignifSpendOnGrowingTechLikeAI", "emplyrAnyControversialHistory")

# logic corrections in the company Dataset-------------------------------------
  # I will correct 1 mistake at a time for better interpretability of the code.
  # This comes at the cost of using more  for loops then needed.

# type of logic correction: employer for a single row in the company dataset 
# can be only 'one' of the various types (see column names above for more info)
for (v in 1:nrow(companyDataset)){
  if (sum(companyDataset[v, 1], companyDataset[v, 2], 
         companyDataset[v, 3]) != 1){
    if (sum(companyDataset[v, 1], companyDataset[v, 2], 
           companyDataset[v, 3]) == 0){
      next
    }
    else{
      tmpVar <- sample(c(1, 2, 3), 3)
      companyDataset[v, tmpVar[1]] <- 1
      companyDataset[v, tmpVar[2]] <- 0
      companyDataset[v, tmpVar[3]] <- 0
      
    }
  }
  else{
    next
  }
}

# type of logic correction: If 'emplyr listed on a stock exchange == TRUE',
# then emplyr cannot be a University OR NGO.
for (w in 1:nrow(companyDataset)){
  if (companyDataset[w, 4] == 1){
    if ((companyDataset[w, 1] == 1) || (companyDataset[w, 3] == 1)){
      companyDataset[w, 1] <- 0
      companyDataset[w, 3] <- 0
    }
    else{
      next
    }
  }
  else{
    next
  }
}

# type of logic correction: employer cannot be both (less than 1yr old) AND 
# (greater than 10 yrs old)
for (x in 1:nrow(companyDataset)){
  if ((companyDataset[x, 7] == 1) && (companyDataset[x, 8] == 1)){
    tmpVar <- sample(c(7, 8), 2)
    companyDataset[x, tmpVar[1]] <- 1
    companyDataset[x, tmpVar[2]] <- 0
  }
  else{
    next
  }
}

# type of logical correction: If employer has international physical presence,
# then, it is internationally recognized via its work.
for (y in 1:nrow(companyDataset)){
  if (companyDataset[y, 13] == 1){
    if (companyDataset[y, 12] == 0){
      companyDataset[y, 12] <- 1
    }
    else{
      next
    }
  }
  else{
    next
  }
}

# type of logical mistake corrected: total number of employees cannot be both
# (less than 100) AND (greater than 1000)
for (a in 1:nrow(companyDataset)){
  if ((companyDataset[a, 9] == 1) && (companyDataset[a,10] == 1)){
    tmpVar <- sample(c(9, 10), 2)
    companyDataset[a, tmpVar[1]] <- 1
    companyDataset[a, tmpVar[2]] <- 0
  }
  else{
    next
  }
}

# coercing company dataset into type data.frame--------------------------------
companyDataset <- as.data.frame(companyDataset)

# stacking 'myPersonalizedLabels' (generated in 'Part-1') column at the end of
# company dataset.
companyDataset[, (ncol(companyDataset)+1)] <- (myPersonalizedLabels %>% 
                                                as.data.frame()) 
colnames(companyDataset)[kTotalNumOfFeat+1] <- "labels"

# setting various controls for training of the model---------------------------
indexTrain <- createDataPartition(companyDataset$labels, p = 0.70,
                                  list = FALSE)
trainingSet <- companyDataset[indexTrain, ]
testSet <- companyDataset[-indexTrain, ]
controls <- trainControl(method = "repeatedcv", 
                         repeats = 5) 
                        
# setting up a search grid for k fold cross validation-------------------------
# below search grid contains only one value each for 'alpha' and 'lambda',
# this is because I have already trained and tuned the model several times
# and the below values are the optimal ones as they performed best on avg
# in k fold cross validation. Note alpha = 1 (lasso penalty), alpha = 0,
# (ridge penalty). See ?glmnet for more details.

searchGrid <- expand.grid(alpha = 0.01385693, lambda = 1.720017)

# user interface---------------------------------------------------------------
print("     Training Personalized Job Classifier...", quote = FALSE)

# training PersonalizedJobClassifier using caret package's 'glmnet' method-----
personalizedJobClassifier <- train(x = trainingSet[, 1:kTotalNumOfFeat], 
                                   y = as.factor(
                                     trainingSet[, (kTotalNumOfFeat+1)]), 
                                   method = "glmnet", 
                                   metric = "Accuracy",
                                   trControl = controls,
                                   tuneGrid = searchGrid
                                   )

# total time elapsed-----------------------------------------------------------
elapsedTime <- proc.time() - startTime
elapsedTime <- as.vector(elapsedTime)

# user interface---------------------------------------------------------------
print(">> Done!", quote = FALSE)
print(sprintf(">> Elapsed Time (in min): %f", ((elapsedTime[3])/60)),
      quote = FALSE)

# save entire workspace to current working directory---------------------------
  # restoreWorkspace <- save.image()
  # save(restoreWorkspace, file = "restoreWorkspace.RData")

# end of code------------------------------------------------------------------