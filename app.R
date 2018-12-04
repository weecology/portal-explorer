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

# Data setup
portal_data <- load_data()
abundances <- abundance(shape = "long", time = "date", clean = FALSE) %>% 
    inner_join(portal_data$species_table, by = "species")
species_list <- unique(abundances$scientificname)
species_list <- sort(species_list)
min_date <- min(abundances$censusdate)
max_date <- max(abundances$censusdate)

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
            sliderInput("dates",
                        "Date Range",
                        min = min_date,
                        max = max_date,
                        value = c(min_date, max_date)),
            checkboxInput("smoother", 
                         "Smoother")
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
            filter(censusdate >= input$dates[1], censusdate <= input$dates[2])
        p <- ggplot(filtered_abundances, aes(x = censusdate, y = abundance, color = scientificname)) +
            geom_line()
        if (input$smoother){
            p <- p + geom_smooth()
        }
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

