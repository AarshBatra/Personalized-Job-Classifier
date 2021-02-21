# Personalized Job Classifier Model--------------------------------------------
# =============================================================================

# Metadata---------------------------------------------------------------------
# Author: Aarsh Batra
# Date  : October, 1, 2017

# Recommended reading----------------------------------------------------------
# Please read the paper which I wrote in correspondance to this app. The 
# paper lays down the theortical foundation, terminology for the model.
# Besides that it provides a coded simulation for th model which you can run
# in downloadable as a .Rmd (R Markdown) file which means that you can run it
# in R studio to see the code work for yourself.

# Purpose----------------------------------------------------------------------
# This app's purpose is to give a user interface to the code presented in the
# paper to further better understand the model and develop a strong intuition
# for each step in the model building process. Moving ahead I will assume that 
# you have read the paper. More information on viewing the paper can be found 
# in the "About" section of the app.

# About------------------------------------------------------------------------
# The Personalized Job Classifier model takes as its input a company feature
# vector, and output a label (0/1) that would inform it's user whether or not
# he gets a job in that company. 'Personalized' means that the classifier is
# trained and tuned for a specific 'user'(I will assume that the user is ca-
# lled 'User1', same convention is used in the paper) and is rendered unusa
# -ble or atleast unfit for anyone else to 'directly' use.

# loading libraries------------------------------------------------------------
library(shiny)
library(shinythemes)
# library(tidyverse)
library(magrittr)
library(caret)
library(knitr)
library(glmnet)
library(e1071)

# importing complete environment of 'FullProcessSimulation.R' file-------------
  # save(list = ls(all.names = TRUE), file = "completeEnvOfFullProcessSimulationFile.RData", envir = .GlobalEnv)
load("completeEnvOfFullProcessSimulationFile.RData")
load("shinyAppFileFullEnvironment.RData")

# parameter initialization
  # appUserAppFeatVec <- c()
appUserPersLab <- c()
appUserNewCompDataset <<- data.frame()
afvFullQuestionsVec <- c("Do you have a high school degree?",
"Do you have a high school degree with highest honors?",
"Do you have a college degree in Social Sciences/CS?",
"Do you have a college degree with highest honors?",
"Have you completed coursework in Quantitative subjects, e.g. Linear Algebra, Calculus?",
"Strong grades (e.g >90%) in any quantitative subject?",
"Do you have a grad school degree?",
"Do you have a grad school degree with highest honors?",
"Are you familiar with programming languages like R/MATLAB/STATA/SAS?",
"Are you pretty comfortable working with programming languages like R/MATLAB/STATA/SAS?",
"Are you familiar with general purpose programming languages like Python?",
"Are you pretty comfortable working with with general purpose programming languages like Python?",
"Are you familiar with database management systems e.g. MySQL, Excel?",
"Are you pretty comfortable working with database management systems e.g. MySQL, Excel?",
"Are you familiar with Big Data frameworks e.g. Hadoop, Apache Spark, etc?",
"Are you pretty comfortable working with Big Data frameworks e.g. Hadoop, Apache Spark, etc?", 
"Do you have experience in estimating econometric models in programming languages?",
"Do you have experience in experimental design?",
"Do you have previous research experience/formal Employment?",
"Have you been the recipient of any awards/scholarships/fellowships?")

cfvFullQuestionsVec <- c("Is the employer a University?",
"Is the employer a Private Company?",
"Is the employer an NGO?",
"Is the employer listed on a stock exchange?",
"Is the employer's work type research?",
"Is the employer ISO 9001 (or other equivalent) compliant?",
"Is the employer less than one year old in its sector?",
"Is the employer greater than ten years old in its sector?",
"Are the number of employees employed by the employer less than 100?",
"Are the number of employees employed by the employer greater than 1000?",
"Does employer count as one of top 50 in its sector?",
"Is employer internationally recognized via it's work?",
"Does the employer have international physical presence?",
"Has employer recieved any awards in recognition of work?",
"Does the employer hire only locally (less cultural diversity)?",
"Does employer perform poor on social responsibility index (e.g. CSR)?",
"Does employer has significant online presence?",
"Does employer significantly spend on R&D?",
"Does employer significantly spend on growing tech like AI?",
"Does employer has any controversial history?")

# user interface
ui <- fluidPage(
  theme = shinytheme("flatly"),
  includeCSS("styles.css"),
  navbarPage(
    "Personalized Job Classifier",
    tabPanel(
     "Welcome",
     h1("Can you Predict which company will hire you?", style = "margin-top: 270px; margin-left: 270px;")
    ),
    tabPanel(
      "About",
      p(tags$em("This app is in its early development stages. I am sharing this early version here so that those who have read
                    my corresponding paper and the coded simulation, can start building an even stronger intuition as to what's going on
                    in each step of the model building process. I recommend reading the corresponding paper (and the coded simulation
                    , which is in the paper) before using this app. As I mentioned, this app is in its early development stages
                    such that it has potential bugs, still I have put forward the entire model building process in this
                    app divided in a few sections, each section is fully functional (though, there is a potential for bugs) such
                    that you can try it out (click buttons for training, building labels, explore datasets etc) and get a more
                    better intuition of what I mean by different parts of the model. What you should take away from this early
                    version is hopefully a better intuition of the entire model building process which will come by trying things 
                    out in the app. ")),
      br(),
      p(tags$em("Later versions of this app would be less buggy and most probably involve a user login (as it is a
                    'Personalized' Job Classifier). Until then, go experience this version:-)")),
      br(),
      p("The Personalized Job Classifier model aims at building a model personalized to a specific 'User' such that
        when feeded with information of a specific company (in form of a feature vector) the model will output a
        binary label (0/1) that would inform its 'User' of his job prospects in that company ('0' = didn't get the job;
        '1' = got the job). Personalized means that the classifier is specifically trained and tuned to a specific User
        and is rendered unusable or at least unfit for anyone else to use."),
      br(),
      p("Please read the paper which I wrote in correspondance to this app. The 
          paper lays down the theoretical foundation, terminology for the model.
          Besides that it provides a coded simulation for the model which 
          is downloadable as a .Rmd (R Markdown) file which means that you can run it
          in R studio to see the code work for yourself.", a("Download Paper.", 
          href = "https://www.dropbox.com/s/jmocezoyhm5f28p/PersonalizedJobClassifierPaperAndSim.pdf?dl=0")),
      br(),
      p("This app's purpose is to give a user interface to the code presented in the
         paper to further better understand the model and develop a strong intuition
         for each step in the model building process. Moving ahead I will assume that 
         you have read the paper.", tags$strong("Please use this app starting from the 'Build your AFV' section
         and moving ahead to the next section and continuing this until you reach the 'Predict on new companies'
         section. This is important as later parts of the app are dependent on the previous parts. So, please move
        ahead one section at a time starting from 'Build your AFV'.")),
      br(),
      p("Please note (this will make more sense to those who have read the paper) I have built this app by assuming
          that User1 is a person whose academic credentials lie in fields like Social Sciences/Data Analytics/Computer Science. 
        (You can think of User1 as an Economics Graduate or a CS graduate). Given that, I have appropriately
          constructed the features (columns) for the applicant dataset on which to fill in
          information for various applicants. Also I have assumed that companies (by companies, I mean employers)", "C", tags$sub("1"),
           "to  ","C",tags$sub("100"),"  are the type of companies which are looking for applicants with credentials like that of User1
          (for e.g. some consultancy firm, Banks, etc). You may build classifiers by assuming different settings."),
      br(),
      br()
    ),
    tabPanel(
      "Process",
      p("Here's what we will do:"),
      tags$div(
        tags$ol(
          tags$li("Build your Applicant feature vector."),
          tags$li("Pass the AFV through models  ", "M", tags$sub("1"), "  to  ", "M", tags$sub("100"), "  to get labels  ",
                  "L", tags$sub("1"), "  to  ", "L", tags$sub("100"), "  (Your Personalized Labels)"),
          tags$li("Use the personalized labels generated in Step 2 in as the 'labels' column of the company Dataset"),
          tags$li("Train model using company dataset generated in Step 3"),
          tags$li("Predict labels on new unseen companies")
        )
      ),
      br(),
      p("Please note that I have already trained models  ", "M", tags$sub("1"), "  to  ", "M",tags$sub("100"), "  and they will be used here
        to generate your personalized labels. This is possible because if User1 builds a model 
       for himself (which we have in the coded simulation), then a lot of User1's model infrastructure (models,  ", "M", tags$sub("1"), "  to  ", "M", tags$sub("100"), 
        "  can be directly utilized by other users if those users belong to the same 'group' of people as User1 does
        (in our case someone with academic credentials in Data Analytics/CS/Social Science fields). This will make more
        sense once you have read the corresponding paper."),
      br(),
        p("So, given we have already trained the models  ", "M", tags$sub("1"), "  to  ", "M",tags$sub("100"), "  (corresponding to companies  ", "C",tags$sub("1"), "  to  ", "C", tags$sub("100"), "  for User1",  
        "it is reasonable to assume that models  ", "M", tags$sub("1"), "  to  ", "M",tags$sub("100") , "  are re-usable by other 'similar' (other people with credentials 
       in fields like Data Analytics/CS/Social Science) users because they may themselves apply for a Job in companies  ", "C", tags$sub("1"), "  to  ", "C", tags$sub("100"), "  as they
      have similar academic credentials. This further means that the entire company dataset we built for User1 is re-usable
          except for the 'labels' column. The 'labels' column in the Company Dataset currently contains User1's personalized
          labels. But, as we are traning the model for you, we will have to build your own personalized labels and use that
          as the new 'labels' column of the company dataset (the personalized labels that we fill in as the new 'labels' column
          of the Company dataset is what makes the model personalized to you). The company dataset will then be feeded to the 
          learning algorithm for training and the resulting model will serve as your Personalized Job Classifier which you can use
          to predict labels on new companies."),
      br(),
      p("Next, Let's start by building your Applicant Feature Vector (AFV)."),
      br(),
      br()
    ),
    tabPanel(
      "Build your AFV",
      fluidRow(
      column(6,  
      h3("Construct your AFV by filling out the below form."),
      br(),
      br(),
      hr(),
      uiOutput("afvForm"),
      br(),
      br()
      ),
      column(6,
      p("Here's what your AFV looks like:"),
      textOutput("afvDisp")
      )
    )
    ),
    tabPanel(
      "Get your Personalized Labels",
      p("With your applicant feature vector built, we will now construct your Persoanlized Labels."),
      br(),
      p("Remember, we already have models  ", "M",tags$sub("1"),"  to  ", "M",tags$sub("100"), "  that we trained at the time of building a model for 
        User1 in the simulation provided in the paper. I have loaded them here for further calculations.
        Remember that models  ", "M",tags$sub("1"), "  to  ", "M", tags$sub("100"), "  correspond to companies", "C", tags$sub("1"), "  to  ", "C",tags$sub("100"), "  where model  ", "M",tags$sub("i"), "  takes in
        as its input an 'Applicant Feature Vector' and outputs a label which informs the applicant whether
        or not he got a job in  ", "C", tags$sub("i"),"."), 
      br(),
      p("Now we will pass your applicant feature vector that you constructed in the previous step in turn 
        through the models  ", "M",tags$sub("1"), "  to  ", "M", tags$sub("100"), "  to get your 'personalized' labels  ", "L", tags$sub("1"), "  to  ", "L", tags$sub("100."),
      br(),
      p("Once your personalized labels are built they will be used as the new 'labels' column of the company dataset. This is what makes the model 
        personalized to you. You can view your company dataset in the next section. You will find that labels built here, as the last 
        column of that dataset. Explore the dataset if you like, then go on to train your 'Personalized Job Classifier' model."),
      br(),
      p("Click on the button below to build your personalized labels. Remember you may edit your applicant feature vector
         as many times as you like, but every time you do, you have to rebuild the labels by clicking the button below."),
      hr(),
      actionButton("butBuildPersLab", "Build Personalized Labels"),
      br(),
      br(),
      textOutput("persLabBuildComp"),
      br(),
      br()
    )
    ),
    tabPanel(
      "Build your PJC",
      tabsetPanel(
      tabPanel(
       "View/Explore dataset",
       br(),
       p("Below is the company dataset that we talked about in the previous section. This dataset is personalized to
         you as its labels column is exactly the personalized labels that you generated in the previous section. This
         is the dataset that we will feed to the learning algorithm for training. Explore the dataset below and then 
         go on to train the model."),
       br(),
       tags$small("Note: In case you are wondering, I previously mentioned that '0' means 'did not get a job' and '1'
          means 'got a job'. Please don't worry about it, a binary variable may be represented as 0/1,
          1/2, 3/4, etc. What matters is that there are two possible states, not how you represent them. It turns out
                  that the 'glmnet' method in the caret package (the package I used to train the model) represents
                  binary labels as (1/2)  rather than (0/1)."),
       hr(),
       dataTableOutput("appUserCompDatasetOutput")
      ),
      tabPanel(
        "Train your model",
        br(),
        p("Click on the button below to train your model (it will take 30-45 seconds to train). The model will be trained using the dataset you viewed in the 
        previous section. Once the model is trained, please go on to the next section in which you can predict whether
          or not you will get a job in a new company by first creating the company feature vector for the new company
          (by filling a 'features' form) and then feeding that to your trained Personalized Job Classifier to get a label for 
          your self."),
        br(),
        p("Please note that when you click the train button the underlying caret package uses its train function to 
          to perform 5 fold cross validation (repeated 2 times) to search over a grid of parameter sets and choose the set that performs
          best on average on these resampling attempts. But, I have set a single search grid. This app will be used by more than
          one user and each of them will have a different dataset which will be feeded to the model and would need to be separately
          tuned. The current search grid might not be efficient for you. But, here the objective is to step you through
          the entire process of making a Personalized Job Classifier such that , once you understand that you can yourself tune your classifier
          to your needs. I will show the accuracy for the model for your reference. (Or maybe I will add an another section in future version of the app that will let you tune
          your model and download the results.)"),
        hr(),
        actionButton("trainPJC", "Train"),
        br(),
        br(),
        textOutput("appUserTrainPJC"),
        br(),
        tags$small("Note: If you are getting a message", tags$strong("'Error: Stopping'"), "It is most likely
                   happening due to the fact that all of your personalized labels are either a '1' (did not get a job), or
                   a '2' (got a job). In other words, the 'labels' column of the company dataset that  is either all 1's or 
                   all 2's. This will lead to stopping the underlying classification algorithm as there is only 'one' class
                   of data available in the training set."),
        br(),
        br(),
        tags$small("This kind of issue will arise if your applicant feature vector either contains all 1's (or almost all)
                   or all 0's (or almost all). When such an applicant feature vector is passed through models  ",
                   "M", tags$sub("1"), "  to  ", "M", tags$sub("100"), "  all of these models output a label of '1' (did not get a job) for an applicant feature vector 
                   of all zeros and a label of '2' (got a job) for an applicant feature vector of all ones. This leads to 
                   all personalized labels to be either all '1's' or all '2's' such that the dataset we feed for training 
                   to our classification algorithm has only one class. This leads to the abrupt stopping as there is nothing to 
                   learn unless we have training examples from both classes."),
        br(),
        br(),
        tags$small("(The following text will make more sense to those who have read the corredponding paper) Models  ", "M",tags$sub("1"),
                   "  to  ", "M", tags$sub("100"), "  were built using the simulated appplicant datasets", "D", tags$sub("1"), "  to  ", "D",tags$sub("100"), "  which correspond to companies", 
                   "C",tags$sub("1"), "to", "C", tags$sub("100"), "These simulated datasets were inturn build from scratch using a particular 'criteria Number'
                   such that they will be on average less 'random' than there corresponding real world counterparts. Given that,
                   when we feed these models 'extreme' applicant feature vectors (which either contains all 1's or all 0's), these
                   simulated models are more likely to output same labels than their real world counterparts."),
        br(),
        br(),
        tags$small("Moreover, If you have an applicant feature vector which contains all 0's 
                   (or almost all 0's) it is likely that you are not a suitable candidate for the model as the model is built
                   for someone with academic credentials in fields like Economics/CS such that the model does not recognize you
                   as that type of person, hence resulting in an error. On the other hand if you have an applicant feature 
                   vector with all 1's (or almost all 1's) than you surely are a suitable candidate and your personalized labels
                   will be '2' (you will get a job in all companies in your company dataset) such that for any new company you
                   will get a job."),
        br(),
        br(),
        tags$small(tags$strong("What can we do to fix it?"), "When working with real world datasets with a
                   large number of training examples, such problems will not arise as the datasets are less 
                   deterministic than there simulated counterparts.", tags$strong("For this simulation this
                    type of error can be fixed by making the datasets more random than they currently are.
                    The datasets in the simulation were made using 'criteria numbers' so as to make them
                    more intuitive, making them more random will come at the cost of losing some intuition.
                    I have not done that here (I plan to make a seperate section in this app for trying that 
                    more random version) but for now if you are getting an error and you want to still go 
                    on and try the whole process, you should rebuild the applicant feature vector to make 
                    it less extreme (i.e not all 1's neither all 0's, but somewhere in between)")),
        br(),
        br()
      )
      )
    ),
    tabPanel(
      "Predict on New Companies",
      tabsetPanel(
        tabPanel(
          "Build CFV",
          fluidRow(
            column(6,
            h3("Construct your CFV by filling out the below form."),
            br(),
            br(),
            hr(),
            uiOutput("cfvForm")
            ),
            column(6,
            p("Here's what your CFV looks like:"),
            textOutput("cfvDisp")
            )
          )
        ),
        tabPanel(
          "Get Your Label (0/1)",
          br(),
          p("You have constructed the form for the new company ('new' as in previously unseen/outside of train and test sets).
            Click on the below button to get a label for yourself. A '1' will mean that you didn't get a job in the company.
            A '2' means that you got a job in the company."),
          hr(),
          actionButton("getYourLabel", "Get Label"),
          br(),
          br(),
          textOutput("yourLabelOnNewComp")
        ),
        tabPanel(
          "Takeaways",
          br(),
          p("The simulated features and the datasets that we earlier built tried to capture some real world processes that
            go into deciding who gets the job and who does not. Of Course that is not all of it. In the corresponding paper
            I discuss, how one can use techniques like web scraping to collect much more interesting features (lots of them
            ) than the binary one's we had here. So, when building a full fledged model we will include lots of these new
            type of features alongside are binary features. Also, the datasets would be from actual companies not like the
            ones we simulated here."),
          br(),
          p("I hope this app has strengthened your intuition for the model. Later versions of this app will fix potential
            bugs. If you have any questions about this model you can write to me at [aarshbatra.in@gmail.com] and I will
            answer them.")
        )
      )
    )
  )
)


# server script----------------------------------------------------------------
server <- function(input, output, session){
  
# applicant feature vector form construction
output$afvForm <- renderUI({
  tmpList <- list()
  for (cbox in 1 : kTotalNumOfFeat){
    tmpId <- sprintf("af%i", cbox)
    tmpList[[cbox]] <- checkboxInput(inputId = tmpId, afvFullQuestionsVec[cbox])
  }
  tmpList
})

# applicant feature vector display  
afvDat <- reactive({
as.integer(c(input$af1, input$af2, input$af3, input$af4, input$af5,
input$af6, input$af7, input$af8, input$af9, input$af10, input$af11, input$af12,
input$af13, input$af14, input$af15, input$af16, input$af17, input$af18, input$af19,
input$af20))
})

output$afvDisp <- renderText({
  paste(afvDat())
})

# build personalized labels
persLabData <- eventReactive(input$butBuildPersLab, {
  afvTmp <- paste(afvDat())
  afvTmp <- as.integer(afvTmp)
  afvTmp <- matrix(data = afvTmp, nrow = 1, ncol = 20)
  colnames(afvTmp) <- colnames(appDatasetList[[1]])[1:kTotalNumOfFeat]
  withProgress(message = "Building ",{
    for (modInd in 1 : kTotalNumOfAppDatasets){
      afvTmp
      appUserPersLab[modInd] <- predict.train(modelsList[[modInd]], newdata = afvTmp, type = "raw")
      incProgress(1/kTotalNumOfAppDatasets, detail = paste("L-", modInd))
    }
    appUserPersLab
})
  
})

output$persLabBuildComp <- renderText({
  afvDat()
  persLabData()
  paste("Status: Personalized Labels build complete.")
})


# building company dataset for training 
newCompDat <- reactive({
  afvDat()
  persLabData()
  newCompDat <- companyDataset[, 1:kTotalNumOfFeat]
  newCompDat[, (kTotalNumOfFeat+1)] <- persLabData()
  colnames(newCompDat) <- colnames(companyDataset)
  newCompDat
})


output$appUserCompDatasetOutput <- renderDataTable({
  newCompDat()
})

# train Personalized Job Classifier model
trainResPJC <- eventReactive(input$trainPJC, {
  datasetAppUser <- newCompDat()
  # indexTrainAppUser <- createDataPartition(datasetAppUser$labels, p = 0.80, list = FALSE)
  indexTrainAppUser <- sample(1:100, 70, replace = FALSE)
  trainingSetAppUser <- datasetAppUser[indexTrainAppUser, ]
  testSetAppUser <- datasetAppUser[-indexTrainAppUser, ]
  controlsAppUser <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
  
  # searchGridAppUser <- expand.grid(alpha = 0.01385693, lambda = 1.720017)
  searchGridAppUser <- expand.grid(alpha = runif(10, 0.01, 0.2 ), lambda = sort(runif(10, 0.8, 1.9), decreasing = TRUE))
  
  appUserPJC <- list()
  withProgress(message = "Status:" , value = 0,{
    incProgress(1/2, detail = paste("Training"))
    appUserPJC <- train(x = trainingSetAppUser[, 1:kTotalNumOfFeat],
                        y = as.factor(trainingSetAppUser[, kTotalNumOfFeat+1]),
                        method = "glmnet",
                        metric  = "Accuracy",
                        trControl = controlsAppUser,
                        tuneGrid = searchGridAppUser)

    incProgress(1/2, detail = "Trained")
  })
  appUserPJC
 })

output$appUserTrainPJC <- renderText({
  trainResPJC()
  paste(sprintf("Model trained, Accuracy = %f percent", (trainResPJC()$results$Accuracy[1])*100))
})


# company feature vector form construction
output$cfvForm <- renderUI({
  tmpList <- list()
  for (cbox in 1 : kTotalNumOfFeat){
    tmpId <- sprintf("cf%i", cbox)
    tmpList[[cbox]] <- checkboxInput(tmpId, cfvFullQuestionsVec[cbox])
  }
  tmpList
})

# company feature vector display
cfvDat <- reactive({
as.integer(c(input$cf1, input$cf2, input$cf3, input$cf4, input$cf5,
input$cf6, input$cf7, input$cf8, input$cf9, input$cf10, input$cf11, input$cf12,
input$cf13, input$cf14, input$cf15, input$cf16, input$cf17, input$cf18, input$cf19,
input$cf20))
})

output$cfvDisp <- renderText({
  paste(cfvDat())
})

# get your label for new (previously unseen company)
yourLab <- eventReactive(input$getYourLabel, {
  withProgress(message = "Status:", value = 0, {
    incProgress(1/2, detail = paste("Getting Label"))
    cfvTmp <- cfvDat()
    cfvTmp <- as.integer(cfvTmp)
    cfvTmp <- matrix(data = cfvTmp, nrow = 1, ncol = 20)
    colnames(cfvTmp) <- colnames(companyDataset)[1:kTotalNumOfFeat]
    trainResPJC()
    labelAppUser <- predict.train(trainResPJC(), newdata = cfvTmp, type = "raw")
    incProgress(1/2, detail = paste("Done!"))
    labelAppUser
    
  })
})

output$yourLabelOnNewComp <- renderText({
  yourLab()
  if (yourLab() == 1){
  paste(sprintf("Your label is: %i; Your PJC model predicts that you will not get the Job!", yourLab()))
  }
  else if (yourLab() == 2) {
    paste(sprintf("Your label is: %i; Your PJC model predicts that you will get the Job!", yourLab()))
  }
})
}

# create shiny app object
shinyApp(ui = ui, server = server)


