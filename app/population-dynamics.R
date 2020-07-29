output$pageStub <- renderUI(fluidPage(
  
  # Application title
  titlePanel("Population Dynamics"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("species",
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
      sliderInput("dates",
                  "Date Range",
                  min = min_date,
                  max = max_date,
                  value = c(min_date, max_date)),
      width = 3),
    
    # Show output
    mainPanel(
      plotOutput(outputId = "main_plot", height = 600),
      textOutput("model", container = pre)
    )
  )
))


output$main_plot <- renderPlot({
  
  filtered_data <- full_dat %>% 
    filter(scientificname %in% input$species) %>% 
    filter(date >= input$dates[1], date <= input$dates[2])
  
  if(length(input$covariates)==0){
    
    fit <- auto.arima(filtered_data$abundance)
    
  } else {
    cov_dat <- filtered_data %>%
      select(input$covariates) %>%
      mutate_all(lag, as.integer(input$lag)) %>%
      as.matrix()
    
    fit <- auto.arima(filtered_data$abundance, xreg = cov_dat)
  }
  
  model_data <- filtered_data %>%
    mutate(fitted = fitted(fit),
           lower = pmax(fitted(fit) - 1.96*sqrt(fit$sigma2),0),
           upper = fitted(fit) + 1.96*sqrt(fit$sigma2))
    
  
  p <- ggplot(model_data, aes(x = date, y = abundance)) +
      theme_set(theme_minimal()) +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
            legend.text=element_text(size=14), legend.title=element_text(size=14)) +
      geom_line() +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .15) +
      geom_line(aes(y = fitted), size = 1, linetype = 2, color = "blue") +
      labs(x = "date", y = "abundance")
    
    p
})

output$model <- renderPrint({
  filtered_data <- full_dat %>% 
    filter(scientificname %in% input$species) %>% 
    filter(date >= input$dates[1], date <= input$dates[2])
  
  if(length(input$covariates)==0){
    
    fit <- auto.arima(filtered_data$abundance)
    
  } else {
    cov_dat <- filtered_data %>%
      select(input$covariates) %>%
      mutate_all(lag, as.integer(input$lag)) %>%
      as.matrix()
    
    fit <- auto.arima(filtered_data$abundance, xreg = cov_dat)
  }
  summary(fit)
})

