library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

dashboardPage(
  
  # add title
  dashboardHeader (title = ""),
  
  # define sidebar
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName="tab1"),
    menuItem("Data Exploration", tabName="tab2"),
    menuItem("Modeling", tabName="tab3"),
    menuItem("Data", tabName="tab4")
  )),
  
  # define body
  dashboardBody(
    tabItems(
      # first item
      tabItem(tabName = "item1",
              #Describe the purpose of the app
              h1("Purpose of the app"),
              box(h4("The purpose of this app is to explora the data for Admission in the University and fit three supervised learning models.")))
    )
  )
)
