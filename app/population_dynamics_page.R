population_dynamics_page <- function(data){
  
  species_list <- sort(unique(data$scientificname))
  min_date <- min(data$date)
  max_date <- max(data$date)
  renderUI({
    fluidPage(
      titlePanel(h4("Explore rodent patterns over time, with covariates")),
      sidebarLayout(
        sidebarPanel(
          selectInput("pop_species",
                      "Species",
                      species_list),
          selectInput("covariates",
                      "Covariates",
                      c("precipitation","mintemp","maxtemp","meantemp","ndvi", 
                        "warm_days", "warm_precip", "cool_precip"), 
                      multiple = TRUE),
          selectInput("lag",
                      "Lag",
                      c(0:12)),
          sliderInput("pop_dates",
                      "Date Range",
                      min = min_date,
                      max = max_date,
                      value = c(min_date, max_date)),
          width = 3),
        mainPanel(
          plotOutput(outputId = "pop_plot", height = 600),
          textOutput("pop_model", container = pre)))
    )})
}

