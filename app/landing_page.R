landing_page<-function(data){
  
  species_list <- sort(c("All", unique(data$scientificname)))
  
  min_date <- min(data$censusdate)
  max_date <- max(data$censusdate)
  
  renderUI({
    fluidPage(
      titlePanel(h4("Primary data time series")),
      sidebarLayout(
        sidebarPanel(
          selectInput("species",
                      "Species",
                      species_list,
                      multiple = TRUE, selected = "All"),
          sliderInput("dates",
                      "Date Range",
                      min = min_date,
                      max = max_date,
                      value = c(min_date, max_date)),
          checkboxInput("smoother", 
                        "Smoother")
        ),
      mainPanel(h4("Rodents"),
                plotOutput(outputId = "species_time_plot", width = "100%"),
                h4("Temperature (C)"),
                plotOutput(outputId = "temperature_plot", width = "100%"),
                h4("Precipitation (mm)"),
                plotOutput(outputId = "ppt_plot", width = "100%"),
                h4("NDVI (Landsat)"),
                plotOutput(outputId = "plot_ndvi_plot", width = "100%")
      ))
    )})
}
