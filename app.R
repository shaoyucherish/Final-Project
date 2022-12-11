#Required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(ggplot2)

#Read in the data
data <- read_csv("Breast_cancer_data.csv", show_col_types = FALSE)
data <- data %>% rename("Radius" = "mean_radius",
                        "Texture" = "mean_texture",
                        "Perimeter" = "mean_perimeter",
                        "Area" = "mean_area",
                        "Smoothness" = "mean_smoothness",
                        "Diagnosis" = "diagnosis")
data$Diagnosis <- as.factor(data$Diagnosis)

################################# Define UI ####################################
ui <- dashboardPage(skin="red",
                    
                    #add title
                    dashboardHeader(title="Breast Cancer Data",titleWidth=1000),
                    
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
                                  #add in latex functionality if needed
                                  withMathJax(),
                                  
                                  #Describe the purpose of the app
                                  h3("Purpose of the application"),
                                  #box to contain description
                                  box(background="red",width=12,
                                      h4("The purpose of this application is to explore the breast cancer data and fit three supervised learning models for it.")
                                  ),
                                  
                                  #Briefly discuss the data and its source
                                  h3("Data info and source"),
                                  #box to contain description
                                  box(background="red",width=12,
                                      h4("Breast cancer is the most common type of cancer in women and the second highest in terms of mortality rates. Diagnosis of breast cancer is performed when an abnormal lump is found (from self-examination or x-ray) or a tiny speck of calcium is seen (on an x-ray). This dataset contains 6 variables: radius, texture, perimeter, area, smoothness, and diagnosis.","If you would like to read more about the dataset, it comes from",a(href="https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction","here"))
                                  ),
                                  
                                  #Tell the user the purpose of each tab
                                  h3("Description of each tab"),
                                  #box to contain description
                                  box(background="red",width=12,
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
                                         box(width=12,background="red",
                                             
                                             #Numerical Summaries
                                             h4("Numerical Summaries"),
                                             selectInput(inputId = "select_variable",
                                                         label = "Select Variable",
                                                         choices = c("Radius", 
                                                                     "Texture", 
                                                                     "Perimeter", 
                                                                     "Area", 
                                                                     "Smoothness"),
                                                         selected = "Radius"),
                                             
                                             #Filter the rows
                                             checkboxInput("con", label = "Filter by Diagnosis"),
                                             conditionalPanel(condition = "input.con",
                                             radioButtons(inputId = "filter_row",
                                                          label = "Choose",
                                                          choiceNames = c("1: Cancer", "0: No Cancer"),
                                                          choiceValues = c("1", "0"))
                                             ),
                                             
                                             #Type of summary
                                             radioButtons(inputId = "sum_type",
                                                          label = "Type of Summary",
                                                          choiceNames = c("Categorical", "Quantitative"),
                                                          choiceValues = c("cat", "quan"))),
                                         
                                         box(width=12,background="red",
                                             #Graphical Summaries
                                             h4("Graphical Summaries"),
                                             radioButtons(inputId = "plot_type",
                                                          label = "Select the Plot Type",
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
                                                                          selected = "Radius")))),
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
                        tabItem(tabName = "model"),
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

################################# Define Server ################################
server <- shinyServer(function(input, output) {
  #About
  output$image <- renderImage({
    list(src = "www/image.png", height = 200, width = 200)
  }, deleteFile=FALSE)
  
  #Data Exploration
  output$dataTable <- DT::renderDataTable({
    var <- input$select_variable
    #base table
    data %>% 
      select(var) %>%
      summarize(mean = round(mean(get(var)),2), std = round(sd(get(var)),2))
    
#    if(input$con & input$filter_row){
#      if(input$filter_row == "1"){
#        data %>% 
#          select(Diagnosis,var) %>% 
#          filter(Diagnosis == 1) %>% 
#          summarize(mean = round(mean(get(var)),2), std = round(sd(get(var)),2))
#      }else{
#        data %>% 
#          select(Diagnosis,var) %>% 
#          filter(Diagnosis == 0) %>% 
#          summarize(mean = round(mean(get(var)),2), std = round(sd(get(var)),2))
#      }
#    }
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
