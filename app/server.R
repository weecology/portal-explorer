#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.

library(shiny)
library(shinyWidgets)
library(htmltools)
library(yaml)

#Source page UIs
source("landing_page.R")
source("covariates_page.R")
source("population_dynamics_page.R")
source("seasonal_dynamics_page.R")
source("model_explorer_page.R")
source("about_page.R")
source("functions.R")

shinyServer(function(input, output, session) {
   
  #Load data
  raw_data <- all_data()
  
  #Create pages
  output$landing <- landing_page(raw_data$abundances)
  output$covariates <- covariates_page(raw_data$weath_dat)
  output$population_dynamics <- population_dynamics_page(raw_data$full_dat)
  output$seasonal_dynamics <- seasonal_dynamics_page(raw_data$full_dat)
  output$model_explorer <- model_explorer_page(raw_data$full_dat)
  output$about <- about_page()
  
  ####Landing page###
  
  landing_filter <- reactive({
    #filter based on selection
    if("All" %in% input$species){
      to_plot <- raw_data$abundances %>% 
        filter(censusdate >= input$dates[1], censusdate <= input$dates[2]) 
    } else {
    to_plot <- raw_data$abundances %>% 
      filter(scientificname %in% input$species) %>% 
      filter(censusdate >= input$dates[1], censusdate <= input$dates[2]) }
    return(to_plot)
  }) 
  
  observe({
    output$species_time_plot<-renderPlot(
      if (input$smoother) {
      species_time(data=landing_filter()) + geom_smooth()
    } else { species_time(data=landing_filter()) })
  })

  time_series_filter <- reactive({
    #filter based on selection
    to_plot <- raw_data$weath_dat %>% 
      filter(date >= input$dates[1], date <= input$dates[2])
    return(to_plot)
  }) 
  
  observe({
    output$temperature_plot <- renderPlot(
      if (input$smoother) {
        temperature(data=time_series_filter()) +
          geom_smooth(aes(y = mintemp, color = "mintemp")) +
          geom_smooth(aes(y = maxtemp, color = "maxtemp"))
      } else { temperature(data=time_series_filter()) })
  })
    
  observe({
    output$ppt_plot <- renderPlot(
      if (input$smoother) {
          ppt(data=time_series_filter()) +
            geom_smooth(color = "lightblue")
        } else { ppt(data=time_series_filter()) })
    })
 
  observe({
    output$plot_ndvi_plot  <- renderPlot(
      if (input$smoother) {
        plot_ndvi(data=time_series_filter()) +
          geom_smooth(color = "green")
      } else { plot_ndvi(data=time_series_filter()) })
  })
  
  ###Covariates page###
  covariates_filter <- reactive({
    #filter based on selection
    to_plot <- raw_data$weath_dat %>% 
      filter(date >= input$cov_dates[1], date <= input$cov_dates[2])
    return(to_plot)
  }) 
  observe({
    output$cov_plot <- renderPlot({
      
      if(input$x2==" ") {
        ggplot(covariates_filter(), aes(x = lag(get(input$x1), as.integer(input$lag1)), y = get(input$y))) +
          theme_set(theme_minimal()) +
          theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
                legend.text=element_text(size=14), legend.title=element_text(size=14)) +
          geom_point() +
          geom_smooth() +
          labs(x = paste(input$x1, ", lag =", input$lag1), y = input$y)
        
      } else {
        
        ggplot(covariates_filter(), aes(x = lag(get(input$x1), as.integer(input$lag1)), y = get(input$y), 
                                        size = lag(get(input$x2), as.integer(input$lag2)), 
                                        color = lag(get(input$x2), as.integer(input$lag2)))) +
          theme_set(theme_minimal()) +
          theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
                legend.text=element_text(size=14), legend.title=element_text(size=14)) +
          geom_point() +
          geom_smooth() +
          labs(x = paste(input$x1, ", lag =", input$lag1), y = input$y, 
               color = paste(input$x2, ", lag =", input$lag2),
               size = paste(input$x2, ", lag =", input$lag2)) +
          theme(legend.position = "bottom")
      }
    })
  })
  observe({
    output$cov_glm <- renderPrint({
      if(input$x2==" ") {
        fit=glm(get(input$y)~lag(get(input$x1),as.numeric(input$lag1)),data=covariates_filter())
        names(fit$coefficients) = c("Intercept",
                                    paste(input$x1, ", lag",input$lag1))
      } else {
        if(input$interaction) {
          fit=glm(get(input$y)~lag(get(input$x1),as.integer(input$lag1))*
                    lag(get(input$x2),as.integer(input$lag2)),
                  data=covariates_filter())
          names(fit$coefficients) = c("Intercept",
                                      paste(input$x1, ", lag",input$lag1), 
                                      paste(input$x2, "' lag", input$lag2),
                                      paste(input$x1,"*",input$x2))
        } else {
          fit=glm(get(input$y)~lag(get(input$x1),as.integer(input$lag1))+
                    lag(get(input$x2),as.integer(input$lag2))
                  ,data=covariates_filter())
          names(fit$coefficients) = c("Intercept",
                                      paste(input$x1, ", lag",input$lag1), 
                                      paste(input$x2, ", lag", input$lag2))
        }}
      print(summary(fit))
    })
  })
  
  ####Population Dynamics Page#####
  dynamics_filter <- reactive({
    #filter based on selection
    filtered_data <- raw_data$full_dat %>% 
      filter(scientificname %in% input$pop_species) %>% 
      filter(date >= input$pop_dates[1], date <= input$pop_dates[2])
    
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
    return(list(model_data=model_data,fit=fit))
  })
  
  observe({
    output$pop_plot <- renderPlot({
      pop_dynamics(dynamics_filter()$model_data)
    })})
  observe({  
    output$pop_model <- renderPrint({
      summary(dynamics_filter()$fit)
    })})
  
  ###Seasonal Dynamics Page###
  seasonal_filter <- reactive({
    #filter based on selection
    filtered_data <- raw_data$full_dat %>% 
      filter(scientificname %in% input$seas_species) %>% 
      filter(date >= input$seas_dates[1], date <= input$seas_dates[2]) %>%
      add_seasons(season_level = input$seasons, summary_funs = "mean") %>%
      arrange(year,season) %>%
      ungroup()
    if(!isTruthy(input$seasonal)){
      
      fit <- auto.arima(filtered_data$abundance)
      
    } else {
      cov_dat <- filtered_data %>%
        select(input$seasonal) %>%
        mutate_all(lag, as.numeric(input$seas_lag)) %>%
        as.matrix()
      
      fit <- auto.arima(filtered_data$abundance, xreg = cov_dat)
    }
    
    model_data <- filtered_data %>%
      mutate(fitted = fitted(fit),
             lower = pmax(fitted(fit) - 1.96*sqrt(fit$sigma2),0),
             upper = fitted(fit) + 1.96*sqrt(fit$sigma2))
    return(list(model_data=model_data,fit=fit))
  })
  observe({
    output$seasonal_plot <- renderPlot({
    plot_seasonal(seasonal_filter()$model_data)
  })})
  observe({
    output$seasonal_model <- renderPrint({
      summary(seasonal_filter()$fit)
  })})

  ###Model Explorer Page###
  model_filter <- reactive({
    model_det <- read_yaml("~/simple/data/metadata.yaml")
    moons <- as.integer(model_det$start_moon:model_det$end_moon)
    if(input$interp){dataset <- paste(input$dataset,"_interp", sep = "")} else {dataset <- input$dataset}
    if(input$model == "pevGARCH") {
    fit <- pevGARCH(main = "~/simple", 
                    data_set = dataset, lag = as.integer(input$lag))$model_fits[[input$species]] }
    else{
      f <- match.fun(input$model)
      fit <- f(main = "~/simple", data_set = dataset)$model_fits[[input$species]] }
    
    abund <- fit$x
    model_data <- abund %>%
      mutate(fitted = fitted(fit),
             lower = pmax(fitted(fit) - 1.96*sqrt(as.numeric(fit$sigma2)),0),
             upper = fitted(fit) + 1.96*sqrt(as.numeric(fit$sigma2)),
             moon = model_det$start_moon + row_number() - 1)
  return(list(model_data=model_data,fit=summary(fit)))
  })
  observe({
    output$model_plot <- renderPlot({
      plot_model(model_filter()$model_data)
    })})
  observe({
    output$model_summary <- renderPrint({
      model_filter()$fit
    })})
})