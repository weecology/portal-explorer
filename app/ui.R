#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
library(shiny)
library(rgl)
library(shinythemes)

#Define thumbnail dir
#Source additional pages

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("readable"),
                  
                  #Navbar to each page
                  navbarPage("Portal Explorer",
                             tabPanel("Time Series",uiOutput('landing')),
                             tabPanel("Covariates",uiOutput('covariates')),
                             tabPanel("Population Dynamics",uiOutput('population_dynamics')),
                             tabPanel("Seasonal Dynamics",uiOutput('seasonal_dynamics')),
                             tabPanel("Model Explorer",uiOutput('model_explorer')),
                             tabPanel("About",uiOutput('about'))
                  )))
