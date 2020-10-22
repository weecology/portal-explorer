seasonal_dynamics_page <- function(data){
  
  species_list <- sort(unique(data$scientificname))
  min_date <- min(data$date)
  max_date <- max(data$date)
  renderUI({
    fluidPage(
      titlePanel(h4("Explore rodent patterns over time, with seasonal covariates")),
      sidebarLayout(
        sidebarPanel(
          selectInput("seas_species",
                      "Species",
                      species_list),
          selectInput("seasonal",
                      "Seasonal Covariates",
                      c("precipitation","mintemp","maxtemp","meantemp","ndvi"), 
                      multiple = TRUE),
          selectInput("seasons",
                      "Seasons",
                      c(2,4)),
          selectInput("seas_lag",
                      "Lag",
                      c(0,1,2,4)),
          sliderInput("seas_dates",
                      "Date Range",
                      min = min_date,
                      max = max_date,
                      value = c(min_date, max_date)),
          width = 3),
        
        # Show output
        mainPanel(
          plotOutput(outputId = "seasonal_plot", height = 600),
          textOutput("seasonal_model", container = pre)
        )
      )
    )})
}
