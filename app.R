#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(portalr)
library(plotly)

# Data setup
#download_observations()
portal_data <- load_data()
abundances <- abundance(shape = "long", clean = FALSE) %>% 
    inner_join(portal_data$species_table, by = "species")
species_list <- unique(abundances$scientificname)
species_list <- sort(species_list)
min_period <- min(abundances$period)
max_period <- max(abundances$period)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Portal Rodent Species Dynamics"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("species",
                        "Species",
                        species_list,
                        multiple = TRUE),
            sliderInput("periods",
                        "Periods",
                        min = min_period,
                        max = max_period,
                        value = c(min_period, max_period)),
            checkboxGroupInput("display", 
                               h3("Display"), 
                               choices = list("Line" = 1, 
                                              "Points" = 2, 
                                              "Smoother" = 3),
                               selected = 1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$distPlot <- renderPlot({
        filtered_abundances <- abundances %>% 
            filter(scientificname %in% input$species) %>% 
            filter(period >= input$periods[1], period <= input$periods[2])
        p <- ggplot(filtered_abundances, aes(x = period, y = abundance, color = scientificname))
        if (1 %in% input$display){
            p <- p + geom_line()
        }
        if (2 %in% input$display){
            p <- p + geom_point()
        }
        if (3 %in% input$display){
            p <- p + geom_smooth()
        }
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

