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
                  c("precipitation","mintemp","maxtemp","meantemp","ndvi"), 
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
    
    # Show a plot of the generated distributions
    mainPanel(
      plotOutput(outputId = "main_plot", height = 600),
      textOutput("model", container = pre)
    )
  )
))


output$main_plot <- renderPlot({
  
  filtered_data <- full_dat %>% 
    filter(scientificname %in% input$species) %>% 
    filter(censusdate >= input$dates[1], censusdate <= input$dates[2]) %>%
    group_by(month) %>% 
    mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    ungroup()
  
  if(length(input$covariates)==0){
    fit = tsglm(filtered_data$abundance, model = list(past_obs = 1, past_mean = 12), distr = "poisson",
                link = "log")
  } else {
    cov_dat = filtered_data %>%
      select(input$covariates) %>%
      mutate_all(lag, as.integer(input$lag)) %>%
      as.matrix()
    
    filtered_data <- filtered_data[complete.cases(cov_dat), ]
    cov_dat <- cov_dat[complete.cases(cov_dat), ]
    
    fit = tsglm(filtered_data$abundance, model = list(past_obs = 1, past_mean = 12), 
                distr = "poisson",
                xreg = cov_dat, link = "log")
  }
  
  model_data <- filtered_data %>%
    mutate(fitted = fitted(fit))
  
  p <- ggplot(model_data, aes(x = censusdate, y = abundance)) +
      theme_set(theme_minimal()) +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
            legend.text=element_text(size=14), legend.title=element_text(size=14)) +
      geom_line() +
#      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .15) +
      geom_line(aes(y = fitted), size = 1, linetype = 2, color = "blue") +
      labs(x = "date", y = "abundance")
    
    p
})

output$model <- renderPrint({
  filtered_data <- full_dat %>% 
    filter(scientificname %in% input$species) %>% 
    filter(censusdate >= input$dates[1], censusdate <= input$dates[2]) %>%
    group_by(month) %>% 
    mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    ungroup()
  
  if(length(input$covariates)==0){
    fit = tsglm(filtered_data$abundance, model = list(past_obs = 1, past_mean = 12), distr = "poisson",
                link = "log")
  } else {
    cov_dat = filtered_data %>%
      select(input$covariates) %>%
      mutate_all(lag, as.integer(input$lag)) %>%
      as.matrix()
    
    filtered_data <- filtered_data[complete.cases(cov_dat), ]
    cov_dat <- cov_dat[complete.cases(cov_dat), ]
    
    fit = tsglm(filtered_data$abundance, model = list(past_obs = 1, past_mean = 12), 
                distr = "poisson",
                xreg = cov_dat, link = "log")
  }
  summary(fit)
})

