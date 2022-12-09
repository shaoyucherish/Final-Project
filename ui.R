library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

dashboardPage(
  
  # add title
  dashboardHeader (title = "Admission in the University", titleWidth = 300),
  
  # define sidebar
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName="about"),
    menuItem("Data Exploration", tabName="exploration"),
    menuItem("Modeling", tabName="model"),
    menuItem("Data", tabName="data")
  )),
  
  # define body
  dashboardBody(
    tabItems(
        # first tab: About page
        tabItem(tabName = "about",
                box(width=12,
                    h3("Purpose of the app"),
                    h4("The purpose of this app is to explore the data for Admission in the University and fit three supervised learning models for it.")),
                box(width=12,
                    h3("Data info and source"),
                    h4("This dataset includes GRE score, TOFEL score, university rating, Statement of Purpose, Letter of Recommendation, CGPA, research and chance of admit. In this dataset, 400 entries are included.",a(href="https://www.kaggle.com/datasets/akshaydattatraykhare/data-for-admission-in-the-university","Here"), "is more information about this dataset.")),
                box(width=12,
                    h3("Description of each tab"),
                    h4("About page: This page can help the user to know the general information about the app and the dataset"),
                    h4("Data Exploration page: This page can allow the user to get the numerical and graphical summaries, as well as the plots. The user can change the variable and filter the rows to get the data used in summaries and plots."),
                    h4("Modeling page: The user can fit three supervised learning models. There are three tabs in this page, including Modeling Info, Model Fitting, and Prediction."),
                    h4("Data page: The user can scroll through the data set and subset this data set, as well as saving the data set as a file.")),
                box(width=12,
                    h3("A picture related to the data"),
                    img(src=""))),
        # second tab: Data Exploration page
        tabItem(tabName = "exploration"),
        # third tab: Modeling
        tabItem(tabName = "model"),
        # fourth tab: Data
        tabItem(tabName = "data"))
  )
)
