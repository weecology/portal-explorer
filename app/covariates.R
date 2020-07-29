# Plot relationships between temp, precipitation and ndvi

  output$pageStub <- renderUI(fluidPage(
    
    # Application title
    titlePanel("Covariates"),
    
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
        sliderInput("dates",
                    "Date Range",
                    min = min_date,
                    max = max_date,
                    value = c(min_date, max_date)),
        checkboxInput("interaction", 
                      "Interaction"),
      width = 3),
      
      # Show output
      mainPanel(
        plotOutput(outputId = "main_plot", height = 600),
        textOutput("glm", container = pre)
      )
    )
  ))
  
  output$main_plot <- renderPlot({
    filtered_weath <- weath_dat %>% 
      filter(date >= input$dates[1], date <= input$dates[2])
    
    if(input$x2==" ") {
      p <- ggplot(filtered_weath, aes(x = lag(get(input$x1), as.integer(input$lag1)), y = get(input$y))) +
        theme_set(theme_minimal()) +
        theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
              legend.text=element_text(size=14), legend.title=element_text(size=14)) +
        geom_point() +
        geom_smooth() +
        labs(x = paste(input$x1, ", lag =", input$lag1), y = input$y)
      
      p
      
    } else {
    
    p <- ggplot(filtered_weath, aes(x = lag(get(input$x1), as.integer(input$lag1)), y = get(input$y), 
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
    p
    }

  })
  
  output$glm <- renderPrint({
    filtered_weath <- weath_dat %>% 
      filter(date >= input$dates[1], date <= input$dates[2])
    
    if(input$x2==" ") {
    fit=glm(get(input$y)~lag(get(input$x1),as.numeric(input$lag1)),data=filtered_weath)
    names(fit$coefficients) = c("Intercept",
                                paste(input$x1, ", lag",input$lag1))
    } else {
    
    if(input$interaction) {
      fit=glm(get(input$y)~lag(get(input$x1),as.integer(input$lag1))*
                           lag(get(input$x2),as.integer(input$lag2)),
              data=filtered_weath)
      names(fit$coefficients) = c("Intercept",
                                  paste(input$x1, ", lag",input$lag1), 
                                  paste(input$x2, "' lag", input$lag2),
                                  paste(input$x1,"*",input$x2))
    } else {
      fit=glm(get(input$y)~lag(get(input$x1),as.integer(input$lag1))+
                           lag(get(input$x2),as.integer(input$lag2))
              ,data=filtered_weath)
      names(fit$coefficients) = c("Intercept",
                                  paste(input$x1, ", lag",input$lag1), 
                                  paste(input$x2, ", lag", input$lag2))
    }}
    
    print(summary(fit))
})
    
    