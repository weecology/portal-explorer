covariates_page<-function(data){
  min_date <- min(data$date)
  max_date <- max(data$date)
  
  renderUI({
    fluidPage(
      titlePanel(h4("Plot relationships between temperature, precipitation and ndvi")),
      sidebarLayout(
        sidebarPanel(
          selectInput("x1",
                      "X1",
                      c("meantemp","mintemp","maxtemp","precipitation","ndvi", 
                        "warm_days", "warm_precip", "cool_precip")),
          selectInput("lag1",
                      "Lag X1",
                      c(0:12)),
          selectInput("x2",
                      "X2",
                      c(" ","precipitation","mintemp","maxtemp","meantemp","ndvi", 
                        "warm_days", "warm_precip", "cool_precip"), 
                      selected = " ", selectize = TRUE),
          selectInput("lag2",
                      "Lag X2",
                      c(0:12)),
          selectInput("y",
                      "Y",
                      c("ndvi","mintemp","maxtemp","meantemp","precipitation", 
                        "warm_days", "warm_precip", "cool_precip"),
                      multiple = FALSE),
          sliderInput("cov_dates",
                      "Date Range",
                      min = min_date,
                      max = max_date,
                      value = c(min_date, max_date)),
          checkboxInput("interaction", 
                        "Interaction"),
          width = 3),
      mainPanel(
        plotOutput(outputId = "cov_plot", height = 600),
        textOutput("cov_glm", container = pre)
        ))
    )})
}
