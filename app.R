#ST558 Final Project
#Shaoyu Wang

#Required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(DT)
library(caret)
library(randomForest)
library(tree)

#Read in the data
setwd("~/Desktop/558Final")
data <- read_csv("Breast_cancer_data.csv", show_col_types = FALSE)
data$diagnosis <- as.factor(data$diagnosis)
data <- data %>% 
  rename("Radius" = "mean_radius",
         "Texture" = "mean_texture",
         "Perimeter" = "mean_perimeter",
         "Area" = "mean_area",
         "Smoothness" = "mean_smoothness",
         "Diagnosis" = "diagnosis")

################################# Define UI ####################################
ui <- dashboardPage(skin="red",
                    
                    #add title
                    dashboardHeader(title="Breast Cancer",titleWidth=200),
                    
                    #define sidebar items
                    dashboardSidebar(sidebarMenu(
                      menuItem("About", tabName = "about"),
                      menuItem("Data Exploration", tabName = "exploration"),
                      menuItem("Modeling", tabName = "model"),
                      menuItem("Data", tabName = "data")
                    )),
                    
                    #define the body of the app
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "about",
                                fluidRow(
                                  
                                  #Describe the purpose of the app
                                  h3("Purpose of the application"),
                                  #box to contain description
                                  box(width=12,background="red",
                                      h4("The purpose of this application is to explore the breast cancer data and fit three supervised learning models for it.")
                                  ),
                                  
                                  #Briefly discuss the data and its source
                                  h3("Data info and source"),
                                  #box to contain description
                                  box(width=12,background="red",
                                      h4("Breast cancer is the most common type of cancer in women and the second highest in terms of mortality rates. Diagnosis of breast cancer is performed when an abnormal lump is found (from self-examination or x-ray) or a tiny speck of calcium is seen (on an x-ray). This dataset contains six variables: radius, texture, perimeter, area, smoothness, and diagnosis.","If you would like to read more about the dataset, it comes from",a(href="https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction","here"),".")
                                  ),
                                  
                                  #Tell the user the purpose of each tab
                                  h3("Description of each tab"),
                                  #box to contain description
                                  box(width=12,background="red",
                                      h4(strong("About page:"),"This page can help the user to know the general information about the app and the dataset"),
                                      h4(strong("Data Exploration page:"), "This page can allow the user to get the numerical and graphical summaries, as well as the plots. The user can change the variable and filter the rows to get the data used in summaries and plots."),
                                      h4(strong("Modeling page:"), "The user can fit three supervised learning models. There are three tabs in this page, including Modeling Info, Model Fitting, and Prediction."),
                                      h4(strong("Data page:"), "The user can scroll through the data set and subset this data set, as well as saving the data set as a file.")
                                  ),
                                  
                                  #A picture related to the data
                                  h3("Picture"),
                                  imageOutput("image")
                                )
                        ),
                        
                        #actual app layout
                        tabItem(tabName = "exploration",
                                fluidRow(
                                  column(width=3,
                                         #Numerical Summaries
                                         box(width=12,background="red",
                                             #Type of summary
                                             h4("Numerical Summaries"),
                                             radioButtons(inputId = "sum_type",
                                                          label = "Type of Summary",
                                                          choices = c("Quantitative", "Categorical")),
                                             conditionalPanel(condition = "input.sum_type == 'Quantitative'",
                                                              selectInput(inputId = "select_quant",
                                                                          label = "Select variable",
                                                                          choices = c("Radius",
                                                                                      "Texture",
                                                                                      "Perimeter",
                                                                                      "Area",
                                                                                      "Smoothness"),
                                                                          selected = "Radius"),
                                                              checkboxInput("filter_diag", label = "Filter by Diagnosis"),
                                                              conditionalPanel(condition = "input.filter_diag",
                                                                               radioButtons(inputId = "filter_row",
                                                                                            label = "Choose",
                                                                                            choiceNames = c("No Cancer", "Cancer"),
                                                                                            choiceValues = c("0", "1")))
                                                              ),
                                             conditionalPanel(condition = "input.sum_type == 'Categorical'")
                                             ),
                                         #Graphical Summaries
                                         box(width=12,background="red",
                                             h4("Graphical Summaries"),
                                             radioButtons(inputId = "plot_type",
                                                          label = "Type of Plot",
                                                          choices = c("Histogram", "Boxplot")),
                                             conditionalPanel(condition = "input.plot_type == 'Histogram'",
                                                              selectInput(inputId = "histVar",
                                                                          label = "Select variable for histogram",
                                                                          choices = c("Radius",
                                                                                      "Texture",
                                                                                      "Perimeter",
                                                                                      "Area",
                                                                                      "Smoothness"),
                                                                          selected = "Radius")),
                                             conditionalPanel(condition = "input.plot_type == 'Boxplot'",
                                                              selectInput(inputId = "boxVar",
                                                                          label = "Select variable for boxplot",
                                                                          choices = c("Radius",
                                                                                      "Texture",
                                                                                      "Perimeter",
                                                                                      "Area",
                                                                                      "Smoothness"),
                                                                          selected = "Radius"))
                                             )
                                         ),
                                  column(width=9,
                                         box(width=12,
                                             h4("Numerical Summaries"),
                                             dataTableOutput("dataTable")),
                                         box(width=12,
                                             h4("Graphical Summaries"),
                                             plotOutput("dataPlot"))
                                  )
                                )
                        ),
                        tabItem(tabName = "model",
                                withMathJax(),
                                tabsetPanel(
                                  #Modeling Info
                                  tabPanel("Modeling Info",
                                           fluidRow(
                                             h3("Logistic Regression"),
                                             box(width=12, background="red",
                                                 h4("Logistic regression model is a very common Generalized Linear Model (GLM). Logistic regression models a relationship between predictor variables and a categorical response variable. Logistic regression helps us estimate a probability of falling into a certain level of the categorical response given a set of predictors. It can easily extend to multiple classes(multinomial regression) and a natural probabilistic view of class predictions. The major limitation of Logistic Regression is the assumption of linearity between the dependent variable and the independent variables."),
                                                 h4("The logit function is used as following:"),
                                                 p("$$ln(\\frac{p}{1-p})$$")
                                                 ),
                                             h3("Classification Tree"),
                                             box(width=12, background="red",
                                                 h4("Classfication tree analysis is when the response is the class (discrete), and the goal is to classify (predict) group membership. For a given region, it usually use most prevalent class as prediction. It is simple to understand and interpret, and can handle both numerical and categorical data. It can also perform well with the large datasets. But it still has limitations. Trees can be very non-robust. A small change in the training data can result in a large change in the tree and consquently the final predictions."),
                                                 h4("For the classification tree, the Gini index or deviance is usually used as following"),
                                                 p("$$Gini: 2p(1-p)$$"),
                                                 p("$$Deviance: -2plog(p)-2(1-p)log(1-p)$$")
                                                 ),
                                             h3("Random Forest"),
                                             box(width=12, background="red",
                                                 h4("Random forest Model is an ensemble learning method for classification, regression, and other tasks that operates by constructing a multitude of decision trees at training set. Random forest works well with both categorical and continuous variables and can be used to solve both classification and regression problems. Random forest can also automatically handle missing values. The disadvantage of random forest is that it requires much more time to train as compared to decision trees as it generates a lot of trees.")
                                                 )
                                             )
                                           ),
                                  #Model Fitting
                                  tabPanel("Model Fitting",
                                           fluidRow(
                                             column(width=3,
                                                    box(width=12, background="red",
                                                        radioButtons(inputId = "choose_prop",
                                                                     label = "Choose proportion of data",
                                                                     choices = c("0.8", "0.7")
                                                                     ),
                                                        checkboxGroupInput(inputId = "select_pred",
                                                                           label = "Select predictors for model",
                                                                           choices = c("Radius",
                                                                                       "Texture",
                                                                                       "Perimeter",
                                                                                       "Area",
                                                                                       "Smoothness"),
                                                                           selected = c("Radius",
                                                                                        "Texture",
                                                                                        "Perimeter",
                                                                                        "Area",
                                                                                        "Smoothness")
                                                                           ),
                                                        actionButton("run_mod", "Run Model")
                                                        )
                                                    ),
                                             column(width=9, 
                                                    tabsetPanel(type = "tabs",
                                                                tabPanel("Logistic Regression", 
                                                                         h4("Fit statistics"),
                                                                         box(width=12,
                                                                             verbatimTextOutput(outputId = "log_res")),
                                                                         h4("Summaries about the model"),
                                                                         box(width=12,
                                                                             verbatimTextOutput(outputId = "log_sum")),
                                                                         h4("Accuracy for prediction"),
                                                                         box(width=12,
                                                                             verbatimTextOutput(outputId = "log_acc"))
                                                                         ),
                                                                tabPanel("Classification Tree",
                                                                         h4("A plot for decision tree"),
                                                                         box(width=12,
                                                                             plotOutput(outputId = "class_res")),
                                                                         h4("Summaries about the model"),
                                                                         box(width=12,
                                                                             verbatimTextOutput(outputId = "class_sum")),
                                                                         h4("Accuracy for prediction"),
                                                                         box(width=12,
                                                                             verbatimTextOutput(outputId = "class_acc"))
                                                                         ),
                                                                tabPanel("Random Forest",
                                                                         h4("A plot showing the variable importance"),
                                                                         box(width=12,
                                                                             plotOutput(outputId = "rf_res")),
                                                                         h4("Summaries about the model"),
                                                                         box(width=12,
                                                                             verbatimTextOutput(outputId = "rf_sum")),
                                                                         h4("Accuracy for prediction"),
                                                                         box(width=12,
                                                                             verbatimTextOutput(outputId = "rf_acc")))
                                                                         )
                                                    )
                                             )
                                           ),
                                  #Prediction
                                  tabPanel("Prediction",
                                           fluidRow(
                                             column(width = 3,
                                                    box(width = 12, background = "red",
                                                        numericInput(inputId = "radius", 
                                                                     label = "Radius",
                                                                     value = 10, 
                                                                     min = 0, max = 30, step=0.01),
                                                        numericInput(inputId = "texture", 
                                                                     label = "Texture",
                                                                     value = 20, 
                                                                     min = 0, max = 40, step=0.01),
                                                        numericInput(inputId = "perimeter", 
                                                                     label = "Perimeter",
                                                                     value = 100, 
                                                                     min = 0, max = 200, step=0.01),
                                                        numericInput(inputId = "area", 
                                                                     label = "Area",
                                                                     value = 800, 
                                                                     min = 0, max = 3000, step=0.01),
                                                        numericInput(inputId = "smoothness", 
                                                                     label = "Smoothness",
                                                                     value = 0.1, 
                                                                     min = 0, max = 0.20, step=0.01),
                                                        actionButton("run_pred", "Run Prediction")
                                                    )),
                                             column(width = 9,
                                                    box(width = 12,
                                                        textOutput("pred_result")
                                                        )
                                                    )
                                             )
                                           )
                                  )
                                ),
                        
                        tabItem(tabName = "data",
                                fluidRow(
                                  column(width=3,
                                         box(width=12, background="red",
                                             
                                             #Subset this data set
                                             h4("Subset the data set"),
                                             sliderInput(inputId = "subset_row",
                                                         label = "Select rows",
                                                         min = 0,
                                                         max = 569,
                                                         value = 50),
                                             checkboxGroupInput(inputId = "subset_col",
                                                                label = "Select columns",
                                                                choices = c("Radius", "Texture", "Perimeter", "Area", "Smoothness", "Diagnosis"),
                                                                selected = c("Radius", "Texture", "Perimeter", "Area", "Smoothness", "Diagnosis")
                                                                ),
                                             
                                             #Save the (possibly subsetted) data as a file
                                             downloadButton(outputId = "save_data", 
                                                            label = "Download the data")
                                             )
                                         ),
                                  column(width=9,
                                         box(width=12,
                                             dataTableOutput("data_sub")))
                                  )
                                )
                      )
                    )
)

################################ Define Server #################################
server <- shinyServer(function(input, output) {
  #About
  output$image <- renderImage({
    list(src = "www/image.png", height = 200, width = 200)
  }, deleteFile=FALSE)
  
  #Data Exploration
  output$dataTable <- DT::renderDataTable({
    
    if(input$sum_type == "Quantitative" & input$filter_diag){
      if(input$filter_row == "0"){
        quant_var <- input$select_quant
        data %>% 
          filter(Diagnosis == 0) %>% 
          summarize(min=round(min(get(quant_var)),2),
                    max=round(max(get(quant_var)),2),
                    mean = round(mean(get(quant_var)),2), 
                    sd = round(sd(get(quant_var)),2))
      }else if(input$filter_row == "1"){
        quant_var <- input$select_quant
        data %>% 
          filter(Diagnosis == 1) %>% 
          summarize(min=round(min(get(quant_var)),2),
                    max=round(max(get(quant_var)),2),
                    mean = round(mean(get(quant_var)),2), 
                    sd = round(sd(get(quant_var)),2))
      }
    }else if(input$sum_type == "Quantitative"){
      quant_var <- input$select_quant
      data %>% 
        summarize(min=round(min(get(quant_var)),2),
                  max=round(max(get(quant_var)),2),
                  mean = round(mean(get(quant_var)),2), 
                  sd = round(sd(get(quant_var)),2))
    }else if(input$sum_type == "Categorical"){
      tab <- table(factor(data$Diagnosis, levels = c(0,1), labels = c("No Cancer", "Cancer")))
      tab <- as.data.frame(tab)
      colnames(tab) = c("Diagnosis", "Count")
      tab
    }
  })
  
  output$dataPlot <- renderPlot({
    
    if(input$plot_type == "Histogram"){
      ggplot(data=data, aes_string(x=input$histVar)) +
        geom_histogram(aes(fill = Diagnosis), position = "dodge", bins = 30)
    }else if(input$plot_type == "Boxplot"){
      ggplot(data=data, aes_string(y=input$boxVar)) +
        geom_boxplot(aes(fill = Diagnosis), position = "dodge")
    }
  })
  
  #Modeling
  ##Model Fitting
  observeEvent(input$run_mod,{
    
    #split the data into a training and test set
    set.seed(123)
    mod_pred <- input$select_pred
    prop_value <- as.numeric(input$choose_prop)
    data_new <- data %>% select(mod_pred, Diagnosis)
    split <- createDataPartition(data_new$Diagnosis, p = prop_value, list = FALSE)
    train <- data_new[split, ]
    test <- data_new[-split, ]
    
    #Logistic regression
    fit_log <- train(Diagnosis ~ ., data = train,
                     method = "glm",
                     family = "binomial",
                     preProcess = c("center", "scale"),
                     trControl = trainControl(method = "cv", number = 5))
    
    output$log_res <- renderPrint(fit_log)
    output$log_sum <- renderPrint(summary(fit_log))
    output$log_acc <- renderPrint(confusionMatrix(fit_log, newdata=test))
    
    #Classification tree
    fit_class <- train(Diagnosis ~ ., data = train,
                       method = "rpart",
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "cv", number = 5))
    
    output$class_res <- renderPlot({plot(fit_class$finalModel);text(fit_class$finalModel, cex=0.8)})
    output$class_sum <- renderPrint(fit_class)
    output$class_acc <- renderPrint(confusionMatrix(fit_class, newdata=test))
    
    #Random Forest
    fit_rf <- train(Diagnosis ~ ., data = train,
                    method = "rf",
                    importance = TRUE,
                    preProcess = c("center", "scale"),
                    trControl = trainControl(method = "cv", number = 5))
    
    output$rf_res <- renderPlot(varImpPlot(fit_rf$finalModel))
    output$rf_sum <- renderPrint(fit_rf)
    output$rf_acc <- renderPrint(confusionMatrix(fit_rf, newdata=test))
  })
  
  ##Prediction
  observeEvent(input$run_pred,{
    output$pred_result <- renderText({
      pred_df <- data.frame(Radius = input$radius,
                            Texture = input$texture,
                            Perimeter = input$perimeter,
                            Area = input$area,
                            Smoothness = input$smoothness)
      pred_class <- tree(Diagnosis ~ ., data = data)
      predict_result <- predict(pred_class, newdata = pred_df, type = "class")
      result <- paste0("The prediction for diagonsis is ",predict_result)
      result
    })
  })
  
  #Data
  getData <- reactive({
    subData <- data[1:input$subset_row,input$subset_col]
  })
  
  output$data_sub <- DT::renderDataTable({
    getData()
  })
  
  output$save_data <- downloadHandler(
    filename = "BreastCancerData.csv",
    content = function(file){
      write.csv(getData(), file)
    }
  )
  
})

shinyApp(ui = ui, server = server)
