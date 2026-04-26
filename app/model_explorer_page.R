model_explorer_page <- function(data){
  main_dir <- path.expand("~/simple")
  if (!file.exists(file.path(main_dir, "directory_configuration.yaml"))){
  create_dir(main_dir)
  fill_dir(main=main_dir) }
  
  #Selector options
  species_list <- sort(c("Total", unique(data$species)))
  min_date <- min(data$date, na.rm = TRUE)
  max_date <- max(data$date, na.rm = TRUE)
  models <- setdiff(prefab_models(), grep("^jags_", prefab_models(), value = TRUE))
  
  renderUI({
    fluidPage(
      titlePanel(h4("Design models using the portalcasting package")),
      sidebarLayout(
        sidebarPanel(
          selectInput("mod_species",
                      "Species",
                      species_list, selected = "DM"),
          selectInput("model",
                      "Model",
                      models),
          helpText("Interactive explorer only shows non-JAGS models."),
          selectInput("dataset",
                      "Data Set",
                      c("All" = "all", "Controls" = "controls", "Exclosures" = "exclosures")),
          selectInput("covariates",
                      "Covariates",
                      c("precipitation","mintemp","maxtemp","meantemp","ndvi"), 
                      multiple = TRUE),
          selectInput("lag",
                      "Lag",
                      0:12),
          sliderInput("mod_dates",
                      "Date Range",
                      min = min_date,
                      max = max_date,
                      value = c(min_date, max_date)),
          checkboxInput("interp",
                      "Interpolate Missing Data?"),
          width = 3),
        mainPanel(
          plotOutput(outputId = "model_plot", height = 600),
          textOutput("model_summary", container = pre)
        ))
    )})
}