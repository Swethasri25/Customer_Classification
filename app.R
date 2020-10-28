library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
credit<-read.csv("C:/Users/hp/Documents/practise2/germancredit.csv")
cred1=data.frame(credit)

#Define UI
ui<-dashboardPage(skin = "purple",
                  dashboardHeader(title = "CUSTOMER CLASSIFICATION",titleWidth = 340,disable = F),
                  
                  dashboardSidebar(
                    width = 340,
                    sidebarMenu(
                      menuItem(tags$strong("DATASET",style="color:orange"),tabName = "Obs"),
                      menuItem(tags$strong("CLASSIFICATION MODELLING",style="color:orange"),tabName = "Split")
                    )#end of the sidebarMenu
                  ),#end of the dashboardSidebar
                  
                  dashboardBody( 
                    
                    tags$strong("CLASSIFICATION MODELING",style="color:#0a25d3",size="300px"),
                    # tags$h3(tags$i("Customer Classification")),
                    
                    tabItems(
                      tabItem(tabName = "Obs",
                              fluidPage(
                                tags$p("CUSTOMER DATA ",size="200px"),
                                theme=shinytheme(theme = "united"),
                                box("The Dataset is about the customers eligible for the loan or not.If He/She eligible for the loan then the Default variable will be 1", "or otherwise 0,here will have 20 variables,Default is my dependent variable and all other  are the independent variables.Select the number of observations that you want to see in the dataset ",background="black",width=12),
                                fluidRow(
                                  box(title = "CUSTOMER DATA",status = "primary",solidHeader = TRUE,width = 12,
                                      DT::dataTableOutput("view")),#end of the box
                                  box(title ="What is classification?",width = 12,status="primary",background="navy",
                                      "An algorithm that implements classification, especially in a concrete implementation, is known as a classifier. The term classifier sometimes also refers to the mathematical function, implemented by a classification algorithm, that maps input data to a category.It 
                        assign a new variable to any one of the groups based on the matched properties.
                        Here we use Bank  Historical Customers Data,Our goal is to classify the customer whether eligible for the loan or not eligible for the loan in future and for new loans,
                        For this purpose we split the dataset into trail and test sets,So it comes under the classification Model. In this data set will have Default variable as dependent variable having 0 and 1. 0 for not eligible for the loan and 1 for eligible for the Loan.
                       " )#end of the box
                                  #box(title = "Summary over The Dataset",status = "primary",solidHeader = TRUE, verbatimTextOutput("summary"),width = 8,background = "black")
                                )#end of the row
                              )#end of fulidPage
                      ),#end of the tabItem
                      tabItem(tabName = "Split",
                              
                              fluidPage(
                                box(height = 150,
                                    title = "Algorithm Name",
                                    status = "primary", width = 6,
                                    solidHeader = T,
                                    selectInput("algorithm", "Choose a Classification algorithm:", 
                                                choices = c("Decision Tree", "RandomForest","LogisticRegression")),
                                    br()),# end of box)
                                box(background = "black",title = "Spliting Data(Observations)",height = 150,status = "primary",solidHeader = T,sliderInput("trainsetsplit",
                                                                                                                                                           label = NULL,
                                                                                                                                                           min = 0, max = 1, value = 0.6, step = 0.1)),
                                box( width = 12,title = "OUTPUT",status = "warning",solidHeader = T,background = "black", 
                                     verbatimTextOutput("results"))#end of the box
                                
                                
                              ))#end of the tabItem
                     
                      
                    )#end of the tabItems
                  )#end of the dashboard
)#end of the dashboardPage
                                

#Define server
server<-server<-function(input,output){
  
  output$summary <- renderPrint({
    summary(cred1)
  })
  output$view<- DT::renderDataTable({
    cr
  },options=list(scrollX=T
  ))
  algorithmInput <- reactive(input$algorithm)
  output$results <- renderPrint({
    
    # split in training testing datasets
    trainIndex <- sample(nrow(cred1), size= nrow(cred1)*input$trainsetsplit)
    training = cred1[trainIndex,]
    testing = cred1[-trainIndex,]
    
    # apply selected classification algorithm
    if(algorithmInput()=="Decision Tree") {
      
      # print classification parameters
      print("Algorithm selected: Decision Tree")
      print(paste("Training set: ", input$trainsetsplit*100, "%", sep = ""))
      print(paste("Testing set: ", (1-input$trainsetsplit)*100, "%", sep = ""))
      
      # build rpart model
      library(tree)
      model <- tree(Default ~ . , data= training)
      
      # test rpart model
      pred <- predict(model, testing[,-1], type  = "vector")
      fitted.results <- ifelse(pred > 0.5,1,0)
      
      DT.misclasi<-mean(fitted.results !=testing$Default)
      DT.accuracy<-1-DT.misclasi
      print(paste("DecisionTree Accuracy",DT.accuracy*100))
      print(table(predicted = fitted.results, reference = testing$Default))
      print(sort(fitted.results))
      
    } else if(algorithmInput()=="RandomForest") {
      
      # print classification parameters
      print("Algorithm selected: randomForest")
      print(paste("Training set: ", input$trainsetsplit*100, "%", sep = ""))
      print(paste("Testing set: ", (1-input$trainsetsplit)*100, "%", sep = ""))
      # build randomForest model
      library(randomForest)
      model <- randomForest(Default ~ . , data= training)
      
      # test randomForest model
      pred <- predict(model, testing, type  = "class")
      fitted.results <- ifelse(pred > 0.5,1,0)
      
      RF.misclasi<-mean(fitted.results !=testing$Default)
      RF.accuracy<-1-RF.misclasi 
      print(paste("RandomForest Accuracy",RF.accuracy*100))
      print(table(predicted =fitted.results, reference = testing$Default))
      print(sort(fitted.results))
      
    } else if(algorithmInput()=="LogisticRegression") {
      print("Algorithm selected:LogisticRegression")
      print(paste("Training set: ", input$trainsetsplit*100, "%", sep = ""))
      print(paste("Testing set: ", (1-input$trainsetsplit)*100, "%", sep = ""))
      
      #train logit model
      model <- glm(Default ~ ., data = training, family = binomial(link = logit))
      #test logit model
      pred1 = predict(model,testing, type = "response")
      fitted.results <- ifelse(pred1 > 0.5,1,0)
      
      LOGIT.misclasi<-mean(fitted.results !=testing$Default)
      LOGIT.accuracy<-1-LOGIT.misclasi
      
      
      print(paste("LOGIT Accuracy",LOGIT.accuracy*100))
      print(table(predicted =fitted.results, reference = testing$Default))
      print(sort(fitted.results))
      
    } 
    else{
      print("Error no Algorithm selected")
    }
    
  })
  
}

#Create shiny object
shinyApp(ui = ui,server = server)