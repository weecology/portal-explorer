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
  model_fit_cache <- reactiveVal(list())
  model_fit_data <- reactive({
    req(input$model, input$dataset, input$mod_species, input$lag)
    req(!is.null(input$interp))

    slow_models <- grep("^jags_", portalcasting::prefab_models(), value = TRUE)
    validate(need(!(input$model %in% slow_models),
                  "This model is expensive to refit in the app. Please choose a non-JAGS model."))

    main_dir <- path.expand("~/simple")
    metadata_path <- file.path(main_dir, "data", "metadata.yaml")
    validate(need(file.exists(metadata_path), "Model metadata not found. Initialize ~/simple first."))

    model_det <- read_yaml(metadata_path)
    moon_start <- dplyr::coalesce(model_det$start_moon,
                                  model_det$time$historic_start_newmoonnumber)
    validate(need(!is.null(moon_start), "Model metadata is incomplete. Rebuild directory data."))
    species_key <- if (identical(input$mod_species, "Total")) "total" else input$mod_species

    if(input$interp){dataset <- paste(input$dataset,"_interp", sep = "")} else {dataset <- input$dataset}
    cache_key <- paste(input$model, dataset, species_key, as.integer(input$lag), sep = "::")
    cache <- model_fit_cache()
    if (!is.null(cache[[cache_key]])) {
      return(cache[[cache_key]])
    }

    model_controls <- portalcasting::models_controls(main = main_dir, models = input$model)[[input$model]]
    validate(need(!is.null(model_controls), "Model controls not found for selected model."))

    # Match portalcasting::cast evaluation context for model control expressions.
    main <- main_dir
    model <- input$model
    species <- species_key
    settings <- portalcasting::read_directory_settings(main = main_dir)
    abundance <- portalcasting::prepare_abundance(main    = main_dir,
                                                  dataset = dataset,
                                                  species = species_key,
                                                  model   = input$model)
    metadata <- portalcasting::read_metadata(main = main_dir)
    newmoons <- portalcasting::read_newmoons(main = main_dir)
    covariates <- portalcasting::read_covariates(main = main_dir)

    fit_args <- vector(mode = "list", length = length(model_controls$fit$args))
    names(fit_args) <- names(model_controls$fit$args)
    for (i in seq_along(fit_args)) {
      fit_args[[i]] <- eval(parse(text = model_controls$fit$args[i]))
    }
    if ("lag" %in% names(fit_args)) {
      fit_args[["lag"]] <- as.integer(input$lag)
    }

    fit <- do.call(what = model_controls$fit$fun, args = fit_args)

    validate(need(!is.null(fit), "No fit found for this species/model/data combination."))
    fitted_vals <- tryCatch(
      as.vector(fitted(fit)),
      error = function(e) {
        if (!is.null(fit_args$response)) {
          tryCatch(as.vector(fitted(fit, response = fit_args$response)),
                   error = function(e2) numeric(0))
        } else {
          numeric(0)
        }
      }
    )
    validate(need(length(fitted_vals) > 0, "Model does not provide fitted values for this selection."))
    observed_vals <- if (!is.null(fit_args$response)) as.vector(fit_args$response) else as.vector(abundance)
    if (length(observed_vals) != length(fitted_vals)) {
      observed_vals <- as.vector(abundance)
    }
    if (length(observed_vals) != length(fitted_vals) && !is.null(fit$x)) {
      observed_vals <- as.vector(fit$x)
    }
    validate(need(length(observed_vals) == length(fitted_vals),
                  "Selected model returned incompatible output lengths for plotting."))
    sigma2_val <- suppressWarnings(as.numeric(fit$sigma2))
    if (length(sigma2_val) == 0 || is.na(sigma2_val[1])) {
      sigma2_val <- stats::var(observed_vals - fitted_vals, na.rm = TRUE)
      if (is.na(sigma2_val) || !is.finite(sigma2_val)) {
        sigma2_val <- 0
      }
    } else {
      sigma2_val <- sigma2_val[1]
    }
    
    model_data <- bind_cols(abundance = observed_vals, fitted = fitted_vals) %>%
             mutate(lower = pmax(fitted - 1.96*sqrt(sigma2_val),0),
             upper = fitted + 1.96*sqrt(sigma2_val),
             moon = as.integer(moon_start) + row_number() - 1) %>%
             left_join(newmoons %>% select(newmoonnumber, censusdate),
                       by = c("moon" = "newmoonnumber")) %>%
             mutate(date = as.Date(censusdate))

    out <- list(model_data=model_data,fit=fit)
    cache[[cache_key]] <- out
    model_fit_cache(cache)
    return(out)
  })
  model_filter <- reactive({
    req(input$mod_dates)
    fit_data <- model_fit_data()
    model_data <- fit_data$model_data %>%
      filter(date >= input$mod_dates[1], date <= input$mod_dates[2])
    validate(need(nrow(model_data) > 0, "No model data in selected date range."))
    return(list(model_data=model_data, fit=fit_data$fit))
  })
  observe({
    output$model_plot <- renderPlot({
      plot_model(model_filter()$model_data)
    })})
  observe({
    output$model_summary <- renderPrint({
      fit <- model_filter()$fit
      tryCatch(
        summary(fit),
        error = function(e) {
          cat("Model summary is unavailable for this fit at moment.\n")
          cat("Reason:", conditionMessage(e), "\n\n")
          cat("Model class:\n")
          print(class(fit))
        }
      )
    })})
})