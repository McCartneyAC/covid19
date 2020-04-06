#Covid Data Setup


# libraries
library(readxl)
library(httr)
library(scales)
library(ggplot2)
library(mccrr)
library(dplyr)
library(ggrepel)
library(tidyr)
library(transformr)
library(patchwork)
library(gganimate)
library(readr)
library(extrafont); loadfonts()


# personal functions
theme_typewriter <- function() {
  ggplot2::theme_light()+
    ggplot2::theme(text = ggplot2::element_text(family = "Special Elite")) 
}


# data import
setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\Covid Data")
  states<-read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  counties<-read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  
  url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
  
  GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
  
  world <- read_excel(tf)
  
  

data <- covid_pull()


covid_clean<-function(data) {
  data %>%
    mutate(date = lubridate::ymd(dateRep),
           countriesAndTerritories = recode(
             countriesAndTerritories,
             "United_States_of_America" = "United States",
             "Czech_Republic" = "Czechia",
             "United_Kingdom" = "United Kingdom",
             "South_Korea"= "South Korea"
           )) %>% 
    select(date, countriesAndTerritories, geoId, cases, deaths) %>%
    drop_na(geoId) %>%
    group_by(geoId) %>%
    arrange(date) %>%
    mutate(cu_cases = cumsum(cases), 
           cu_deaths = cumsum(deaths)) %>%
    filter(cu_cases > 100) %>%
    mutate(days_elapsed = date - min(date),
           end_label = ifelse(date == max(date), countriesAndTerritories, NA))
}  

mark<-function(data) {
  data%>% 
    filter(geoId %in% c("US", "IT")) %>% 
    arrange(dateRep) %>% 
    group_by(geoId) %>% 
    mutate(mark = row_number()) %>% 
    mutate(DateRep_lagged = case_when(
      geoId == "US" ~ (dateRep - 950400),
      geoId == "IT" ~ dateRep
    ) ) %>% 
    #  ungroup %>% 
    arrange(geoId, mark) %>% 
    #group_by(geoId) %>% 
    mutate(Cases_cumsum = cumsum(cases)) %>% 
    mutate(Deaths_cumsum = cumsum(deaths)) 
}


topn <- cov_curve %>%
  group_by(geoId) %>%
  filter(days_elapsed == max(days_elapsed)) %>%
  ungroup() %>%
  top_n(30, cu_cases) %>%
  select(countriesAndTerritories, geoId, cu_cases) %>%
  mutate(
    days_elapsed = 1,
    cu_cases = max(cov_curve$cu_cases) - 1e4,
    countriesAndTerritories = recode(
      countriesAndTerritories,
      "United_States_of_America" = "United States",
      "Czech_Republic" = "Czechia",
      "United_Kingdom" = "United Kingdom",
      "South_Korea"= "South Korea"
    )
  )




topn_bg<- cov_curve %>% 
  select(-countriesAndTerritories) %>% 
  filter(geoId %in% topn$geoId) %>% 
  select(geoId, days_elapsed, cu_cases)


endpoints <- cov_curve %>% 
  filter(geoId %in% topn$geoId) %>% 
  group_by(geoId) %>% 
  filter(days_elapsed ==max(days_elapsed)) %>% 
  select(countriesAndTerritories, 
         geoId, days_elapsed, 
         cu_cases) %>% 
  ungroup()

