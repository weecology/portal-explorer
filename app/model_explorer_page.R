model_explorer_page <- function(data){
  #Selector options
  species_list <- sort(c("All", unique(data$scientificname)))
  min_date <- min(data$date)
  max_date <- max(data$date)
  
  renderUI({
    fluidPage(
      titlePanel(h4("Design models using the portalcasting package")),
      sidebarLayout(
        sidebarPanel(
          selectInput("species",
                      "Species",
                      species_list),
          selectInput("seasonal",
                      "Seasonal Covariates",
                      c("precipitation","mintemp","maxtemp","meantemp","ndvi"), 
                      multiple = TRUE),
          selectInput("seasons",
                      "Seasons",
                      c(2,4)),
          selectInput("lag",
                      "Lag",
                      c(0,1,2,4)),
          sliderInput("dates",
                      "Date Range",
                      min = min_date,
                      max = max_date,
                      value = c(min_date, max_date)),
          width = 3),
        mainPanel())
    )})
}