#functions
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(gridExtra)
library(stringr)
library(htmltools)
remotes::install_github("weecology/portalr")
library(portalr)
library(stats)
library(lubridate)
library(forecast)
remotes::install_github("weecology/portalcasting")
library(portalcasting)

#Load data
all_data <- function(){
  portal_data <- load_rodent_data()
  abundances <- abundance(shape = "long", time = "period", clean = FALSE) %>% 
    inner_join(portal_data$species_table, by = "species") %>%
    left_join(portal_data$newmoons_table, by = "period") %>%
    mutate(censusdate = as.Date(censusdate))
  
  ndvi_dat <- ndvi(level = "monthly", fill = FALSE) %>%
    mutate(year = year(date), month = month(date)) 
  
  weath_dat <- weather(level = "monthly", fill = FALSE) %>%
    full_join(ndvi_dat, by = c("year", "month")) %>%
    select(year, month, mintemp, maxtemp, meantemp, precipitation, ndvi, 
           warm_days, cool_precip, warm_precip) %>%
    mutate(date = ymd(paste(year,month,01)))
  
  full_dat <- abundances %>%
    mutate(censusdate = ymd(censusdate)) %>%
    mutate(year = year(censusdate), month = month(censusdate)) %>%
    left_join(weath_dat, by = c("year", "month")) %>%
    select(-"date") %>%
    rename(date = censusdate)
  
  return(list(abundances=abundances, weath_dat=weath_dat, full_dat=full_dat))
}


species_time <- function(data){
  ggplot(data, aes(x = censusdate, y = abundance, color = scientificname)) +
    geom_line() +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
          legend.text=element_text(size=14), legend.title=element_text(size=14)) +
    theme(legend.position = "top")
}

temperature <- function(data){
  ggplot(data, aes(x = date)) + 
    geom_line(aes(y = meantemp, color = "meantemp")) +
    geom_line(aes(y = mintemp, color = "mintemp")) +
    geom_line(aes(y = maxtemp, color = "maxtemp")) +
    scale_color_manual(name="",
                       values = c("maxtemp"="red","meantemp"="pink","mintemp"="blue")) +
    labs(x = '', y = 'Air Temp (C)') +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
          legend.text=element_text(size=14), legend.title=element_text(size=14)) +
    theme(legend.position = "top") 
}

ppt <- function(data){
  ggplot(data, aes(x = date, y = precipitation)) + 
    geom_line(color = "lightblue") +
    xlab('date') + ylab('Precip (mm)') +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
          legend.text=element_text(size=14), legend.title=element_text(size=14))
}

plot_ndvi <- function(data){
  ggplot(data, aes(x = date, y = ndvi)) + 
    geom_line(color = "green") +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
          legend.text=element_text(size=14), legend.title=element_text(size=14)) +
    xlab('') 
}

pop_dynamics <- function(data){
  
  ggplot(data, aes(x = date, y = abundance)) +
    theme_set(theme_minimal()) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
          legend.text=element_text(size=14), legend.title=element_text(size=14)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .15) +
    geom_line(aes(y = fitted), size = 1, linetype = 2, color = "blue") +
    labs(x = "date", y = "abundance")
}

plot_seasonal <- function(data){
  ggplot(data, aes(x = as.numeric(year), y = abundance, color = season)) +
    theme_set(theme_minimal()) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
          legend.text=element_text(size=14), legend.title=element_text(size=14)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .15) +
    geom_line(aes(y = fitted), size = 1, linetype = 2) +
    labs(x = "year", y = "abundance") +
    facet_grid(rows = vars(season))
}

plot_model <- function(model_fit){
  ggplot(model_fit, aes(x = moon, y = abund)) +
    theme_set(theme_minimal()) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
          legend.text=element_text(size=14), legend.title=element_text(size=14)) +
    geom_line(col="blue") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .15) +
    geom_line(aes(y = fitted), size = 1, linetype = 2) +
    labs(x = "moon", y = "abundance")
}
