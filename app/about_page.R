about_page <- function(){
  renderUI({
    fluidPage(
      titlePanel(""),
      HTML("<p>This app is a visualization of data from the 
      <a href='http://www.github.com/weecology/PortalData'>PortalData repo</a> GitHub repository. 
      Data are managed and updated in real time, and are available free to download. 
      Data are retrieved and organized for visualization via the 
      <a href='http://www.github.com/weecology/portalr'>portalr package</a>, an R package designed to 
           aid in use of the Portal Data.
      Modeling in the Model Explorer is done via the 
      <a href='http://www.github.com/weecology/portalcasting'>portalcasting package</a>, an r 
      package primarily used to create forecasts for the Portal Forecasting project, an ongoing effort 
      to create iterative, automatically updating ecological forecasts. Forecasts are stored in the 
      <a href='http://www.github.com/weecology/portalPredictions'>portalPredictions repo</a> 
      and available at <a href='http://portal.naturecast.org/'>portal.naturecast.org</a>.</p>"),
      h1(".   "),
      h1(".   "),
      h1(".   "),
      h6(HTML("<a href='http://www.weecology.org/'>WEecology Lab - Interdiscliplinary Ecology</a>")),
      p("WEecology is an interdisciplinary ecology research group at the 
         University of Florida. We consist of Morgan Ernest’s lab, which studies how ecological systems 
         change through time and has a strong field work focus, and Ethan White’s lab which uses 
         computational and statistical methods to understand ecological systems using large ecological 
         and environmental datasets. We do cutting edge research, collect and publish large open datasets, 
         develop open source software, build websites, train scientists in computational tools, and 
         generally try to make science a better, more efficient, more diverse and inclusive place.", 
         style = "font-size:10px")
    )})
}