model_explorer_page <- function(data){
  if (!file.exists("~/simple")){
  create_dir("~/simple")
  fill_raw(main="~/simple")
  fill_dir(main="~/simple", downloads = zenodo_downloads("1215988")) }
  
  #Selector options
  species_list <- sort(c("Total", unique(data$species)))
  min_date <- min(data$date)
  max_date <- max(data$date)
  models <- prefab_models()
  
  renderUI({
    fluidPage(
      titlePanel(h4("Design models using the portalcasting package")),
      sidebarLayout(
        sidebarPanel(
          selectInput("species",
                      "Species",
                      species_list, selected = "DM"),
          selectInput("model",
                      "Model",
                      models),
          selectInput("dataset",
                      "Data Set",
                      c("All", "Controls", "Exclosures")),
          selectInput("covariates",
                      "Covariates",
                      c("precipitation","mintemp","maxtemp","meantemp","ndvi"), 
                      multiple = TRUE),
          selectInput("lag",
                      "Lag",
                      0:12),
          sliderInput("dates",
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