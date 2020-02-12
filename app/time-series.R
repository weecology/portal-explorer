output$pageStub <- renderUI(fluidPage(
  
  # Application title
  titlePanel("Portal Time Series"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("species",
                  "Species",
                  species_list,
                  multiple = TRUE),
      sliderInput("dates",
                  "Date Range",
                  min = min_date,
                  max = max_date,
                  value = c(min_date, max_date)),
      checkboxInput("smoother", 
                    "Smoother")
    ),
    
    # Show output
    mainPanel(
      plotOutput(outputId = "distPlot", width = "100%"),
      plotOutput(outputId = "temp", width = "100%"),
      plotOutput(outputId = "ppt", width = "100%"),
      plotOutput(outputId = "ndvi", width = "100%")
    )
  )
))
  
  output$distPlot <- renderPlot({
    filtered_abundances <- abundances %>% 
      filter(scientificname %in% input$species) %>% 
      filter(censusdate >= input$dates[1], censusdate <= input$dates[2])
    p <- ggplot(filtered_abundances, aes(x = censusdate, y = abundance, color = scientificname)) +
      geom_line() +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
            legend.text=element_text(size=14), legend.title=element_text(size=14)) +
      theme(legend.position = "top")
    if (input$smoother){
      p <- p + geom_smooth()
    }
    p
  })

  
  output$temp <- renderPlot({
    filtered_weath <- weath_dat %>% 
      filter(date >= input$dates[1], date <= input$dates[2])
    
    p1 <- ggplot(data = filtered_weath, aes(x = date)) + 
      geom_line(aes(y = meantemp, color = "meantemp")) +
      geom_line(aes(y = mintemp, color = "mintemp")) +
      geom_line(aes(y = maxtemp, color = "maxtemp")) +
      scale_color_manual(name="",
                         values = c("maxtemp"="red","meantemp"="pink","mintemp"="blue")) +
      labs(x = '', y = 'Air Temp (C)') +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
            legend.text=element_text(size=14), legend.title=element_text(size=14)) +
      theme(legend.position = "top") 
    if (input$smoother){
      p1 <- p1 + geom_smooth(aes(y = meantemp, color = "meantemp")) +
        geom_smooth(aes(y = mintemp, color = "mintemp")) +
        geom_smooth(aes(y = maxtemp, color = "maxtemp"))
    }
    p1
  })
    
  output$ppt <- renderPlot({ 
    filtered_weath <- weath_dat %>% 
      filter(date >= input$dates[1], date <= input$dates[2])
    p2 <- ggplot(data = filtered_weath, aes(x = date, y = precipitation)) + 
      geom_line(color = "lightblue") +
      xlab('date') + ylab('Precip (mm)') +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
            legend.text=element_text(size=14), legend.title=element_text(size=14))
    if (input$smoother){
      p2 <- p2 + geom_smooth(color = "lightblue")
    }
    p2
  })
  
  output$ndvi <- renderPlot({  
    filtered_weath <- weath_dat %>% 
      filter(date >= input$dates[1], date <= input$dates[2])
    p3 <- ggplot(data = filtered_weath, aes(x = date, y = ndvi)) + 
      geom_line(color = "green") +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
            legend.text=element_text(size=14), legend.title=element_text(size=14)) +
      xlab('')
    
    if (input$smoother){
      p3 <- p3 + geom_smooth(color = "green")
    }
    p3
  })
