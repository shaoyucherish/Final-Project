# Final Project
Shaoyu Wang  
2022-12-12

## Brief description of the app 
This R Shiny app was created as a final project for the ST 558 - Data Science for Statisticians course. The purpose of this app is to explore the breast cancer data and fit three supervised learning models for it. There are four tabs for this app. The first tab of About presents the general information about the dataset and the introduction of app. The second tab of Data Exploration includes the numerical summaries and graphical summaries. The third tab of Modeling is used to fit three supervised learning models. The fourth tab of Data can subset the dataset and download it. 

## A list of packages
- shiny
- shinydashboard
- tidyverse
- ggplot2
- DT
- caret
- randomForest
- tree

## A line of code for installing all the packages used
```
install.packages(c("shiny", "shinydashboard", "tidyverse", "ggplot2", "DT", "caret", "randomForest", "tree"))
```

## The `shiny::runGitHub()` code for running the app
```
shiny::runGitHub(repo = "Final-Project",username = "shaoyucherish",ref="main")
```
