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
library(geofacet)
library(robustbase)

# personal functions
theme_typewriter <- function() {
  ggplot2::theme_light()+
    ggplot2::theme(text = ggplot2::element_text(family = "Special Elite")) 
}
`%not_in%` <- purrr::negate(`%in%`)

# data import
setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\Covid Data")
  


pull_states<-function(){
  read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  }

pull_counties<-function(){
  read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
}

pull_world <- function() {
  url <-paste(
      "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
      format(Sys.time(), "%Y-%m-%d"),".xlsx",sep = "")
  
  GET(url,authenticate(":", ":", type = "ntlm"),write_disk(tf <- tempfile(fileext = ".xlsx")))
  
world <- read_excel(tf)
  return(world)
}


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
  #  mutate(dateRep = mdy)
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

cov_curve<-pull_world() %>% 
  covid_clean()

topn <- cov_curve %>% 
  group_by(geoId) %>%
  filter(days_elapsed == max(days_elapsed)) %>%
  ungroup() %>%
  top_n(30, cu_cases) %>%
  arrange(-cu_cases) %>% 
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


topn_factors<-cov_curve %>% 
  group_by(geoId) %>%
  filter(days_elapsed == max(days_elapsed)) %>%
  ungroup() %>%
  top_n(30, cu_cases) %>%
  arrange(-cu_cases) %>% 
  select(countriesAndTerritories)


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



# graph functions:

covidplot<-function(data_marked){
  data_marked %>% 
    mutate(DateRep_lagged = as.Date(DateRep_lagged)) %>% 
    mutate(pop = case_when(
      geoId == "US" ~ 327.2 ,
      geoId == "IT" ~ 60.48
    ) ) %>% 
    mutate(case_normal = Cases_cumsum/pop) %>% 
    filter(DateRep_lagged > "2020-02-21") %>% 
    ggplot(aes(x = DateRep_lagged, y = Cases_cumsum, fill = geoId)) + 
    geom_col(position = "dodge") +
    scale_x_date(breaks = breaks_pretty(5)
    ) + 
    scale_y_continuous(
      breaks = breaks_extended(7), labels = scales::label_number_si()
    ) +
    #scale_y_log10() + 
    theme_typewriter() + 
    scale_fill_manual(values = c( "forestgreen", "navy")) + 
    labs(
      title = "Cumulative Cases - US and Italy", 
      x =  "", 
      y = "", 
      subtitle = "11-day lag determined by first day of 100 cases", 
      #caption = "Data via European Centre for Disease Prevention and Control", 
      fill = "Country"
    ) + 
    theme(legend.position = c(0.1, 0.75)) 
}
covidplot_raw<-function(data_marked){
  data_marked %>% 
    mutate(DateRep_lagged = as.Date(DateRep_lagged)) %>% 
    mutate(pop = case_when(
      geoId == "US" ~ 327.2 ,
      geoId == "IT" ~ 60.48
    ) ) %>% 
    mutate(case_normal = Cases_cumsum/pop) %>% 
    filter(DateRep_lagged > "2020-02-21") %>% 
    ggplot(aes(x = DateRep_lagged, y = cases, fill = geoId)) + 
    geom_col(position = "dodge") +
    scale_x_date(breaks = breaks_pretty(5)
    ) + 
    scale_y_continuous(
      breaks = breaks_extended(7), labels = scales::label_number_si()
    ) +
    #scale_y_log10() + 
    theme_typewriter() + 
    scale_fill_manual(values = c( "forestgreen", "navy")) + 
    labs(
      title = "Daily New Cases", 
      x =  "", 
      y = "", 
      # subtitle = "11-day lag determined by first day of 100 cases", 
      # caption = "Data via European Centre for Disease Prevention and Control", 
      fill = "Country"
    ) + 
    guides(fill = FALSE)
}

covidplot_log<-function(data_marked){
  data_marked %>% 
    mutate(DateRep_lagged = as.Date(DateRep_lagged)) %>% 
    mutate(pop = case_when(
      geoId == "US" ~ 327.2 ,
      geoId == "IT" ~ 60.48
    ) ) %>% 
    mutate(case_normal = Cases_cumsum/pop) %>% 
    filter(DateRep_lagged > "2020-02-21") %>% 
    ggplot(aes(x = DateRep_lagged, y = case_normal, fill = geoId)) + 
    geom_col(position = "dodge") +
    scale_x_date(breaks = breaks_pretty(5))+
    scale_y_log10(labels = scales::label_number_si()) + 
    theme_typewriter() + 
    scale_fill_manual(values = c( "forestgreen", "navy")) + 
    labs(
      title = "Cumulative Cases Per Million", 
      x =  "", 
      y = "", 
      #subtitle = "11-day lag determined by first day of 100 cases", 
      caption = "Data via European Centre for Disease Prevention and Control", 
      fill = "Country"
    ) + 
    guides(fill = FALSE)
}

covidplot_smooth<-function(data_marked){
  data_marked %>% 
    mutate(DateRep_lagged = as.Date(DateRep_lagged)) %>% 
    mutate(pop = case_when(
      geoId == "US" ~ 327.2 ,
      geoId == "IT" ~ 60.48
    ) ) %>% 
    mutate(case_normal = Cases_cumsum/pop) %>% 
    filter(DateRep_lagged > "2020-02-21") %>% 
    filter(geoId== "IT") -> italy
  data_marked %>% 
    mutate(DateRep_lagged = as.Date(DateRep_lagged)) %>% 
    mutate(pop = case_when(
      geoId == "US" ~ 327.2 ,
      geoId == "IT" ~ 60.48
    ) ) %>% 
    mutate(case_normal = Cases_cumsum/pop) %>% 
    filter(DateRep_lagged > "2020-02-21") %>% 
    filter(geoId== "US") -> unitedstates
  
  
  ggplot(data = italy, aes(x = DateRep_lagged, y = case_normal, color = geoId)) + 
    geom_point(data = italy, 
               aes(x = DateRep_lagged, 
                   y = case_normal, 
                   color = geoId)
    )+
    geom_point(data = unitedstates, 
               aes(x = DateRep_lagged, 
                   y = case_normal, 
                   color = geoId)
    )+
    geom_smooth(data = italy, 
                aes(x = DateRep_lagged, 
                    y = case_normal, 
                    color = geoId)
                # method = "lm", 
                # formula = y ~ splines::bs(x, 3), 
                #  orientation = "y"
    )+
    geom_smooth(data = unitedstates, 
                aes(x = DateRep_lagged, 
                    y = case_normal, 
                    color = geoId), 
                method = "lm"
    )+
    #geom_col(position = "dodge") +
    scale_x_date(breaks = breaks_pretty(5))+
    scale_y_log10() + 
    theme_typewriter() + 
    scale_fill_manual(values = c( "forestgreen", "navy")) + 
    labs(
      title = "Comparing COVID-19 Cases: United States and Italy", 
      x =  "Date (with United States on 11-day lag)", 
      y = "Cumulative Cases per million residents", 
      subtitle = "11-day lag determined by first day of 100 cases", 
      caption = "Data via European Centre for Disease Prevention and Control", 
      fill = "Country"
    ) + 
    theme(legend.position = c(0.1, 0.75))
}

cov_curve %>%
  filter(geoId %in% topn$geoId) %>% 
  group_by(geoId) %>% 
  arrange(-cu_cases)

covidplot_small_multiple<-function(cov_curve){
  #https://kieranhealy.org/blog/archives/2020/03/27/a-covid-small-multiple/
  cov_curve %>%
    filter(geoId %in% topn$geoId) %>% 
    group_by(geoId) %>% 
    arrange(cu_cases) %>% 
    ungroup() %>% 
    ggplot(mapping = aes(x = days_elapsed, y = cu_cases)) + 
    geom_line(data =topn_bg,
              aes(group = geoId),
              size = 0.15,
              color = "grey80") +
    geom_line(color = "firebrick", lineend = "round")  +
    geom_point(data = endpoints,
               size = 1.1,
               shape = 21,
               color = "firebrick",
               fill = "firebrick2") +
    geom_text(data=topn,
              mapping = aes(label = countriesAndTerritories),
              vjust = "inward",
              hjust = "inward",
              fontface = "bold",
              color = "firebrick",
              size = 2.1
    ) +
    scale_y_log10(labels = scales::label_number_si()) + 
    scale_x_continuous()+ 
    # reorder(FUN = max) is bananas but reorder(FUN = min) works just fine????
    facet_wrap( ~ reorder(countriesAndTerritories, -cu_cases, min), ncol = 5) + 
    labs(
      x = "Days since 100th confirmed case",
      y = "Log Cumulative Cases",
      title = "Cumulative COVID-19 Cases: Top 30 Countries", 
      subtitle = paste("Data as of", format(max(cov_curve$date), "%A, %B %e, %Y")),
      caption = "Data: https://www.ecdc.europa.eu/"
    )+ 
    theme_typewriter() + 
    theme(strip.text = element_blank())
}

######## Daily New ###########################

covidplot_us_daily <- function(data){
data %>% 
  filter(geoId == "US") %>% 
  select(dateRep, cases, deaths) %>% 
  filter(dateRep >  "2020-02-21") %>% 
  pivot_longer(-c(dateRep), 
               names_to = "Daily",
               values_to = "total") %>% 
  ggplot(aes(
    x = dateRep, 
    y = total, 
    fill = Daily, 
    color = Daily
  )) + 
  geom_col(position = "dodge") +
  scale_x_datetime(breaks = breaks_pretty(5)
  ) + 
  scale_y_continuous(
    breaks = breaks_extended(7), labels = scales::label_number_si()
  ) +
  theme_typewriter() + 
  scale_fill_inova() + 
  scale_color_inova() +
  labs(
    title = "US Daily COVID-19", 
    x =  "", 
    y = "", 
    caption = "Data via European Centre for Disease Prevention and Control"
  ) + 
  guides(color = FALSE)+ 
    theme(legend.position = c(0.1, 0.75))
}

################
covid_tile<-function(states){
  p8<-states %>% 
    filter(state %not_in% c("Puerto Rico", 
                            "Virgin Islands", 
                            "Guam", 
                            "Northern Mariana Islands",
                            "American Samoa")) %>% 
    group_by(state) %>% 
    mutate(uncum_cases= c(0, diff(cases))) %>% 
    ungroup() %>% 
    filter(date > "2020-03-01") %>% 
    ggplot(aes(
      x = date, 
      y = state,
      fill = uncum_cases
    )) +
    scale_x_date(breaks = breaks_pretty(6))+
    scale_fill_viridis_c() +
    geom_tile(color = "white") + 
    theme_light() +
    labs(title = "By State Case Counts", 
         fill = "Daily Cases",
         x = "",
         y = "")  +
    theme_typewriter() 
  
  p7 <- states %>% 
    filter(state %not_in% c("Puerto Rico", 
                            "Virgin Islands", 
                            "Guam", 
                            "Northern Mariana Islands",
                            "American Samoa")) %>% 
    group_by(state) %>% 
    mutate(uncum_cases= c(0, diff(cases))) %>% 
    ungroup() %>% 
    filter(date > "2020-03-01") %>% 
    group_by(date) %>% 
    summarise(uncum_cases = sum(uncum_cases)) %>% 
    ungroup() %>% 
    ggplot(aes(x = date, y = uncum_cases)) +
    geom_col(fill = "black")+ 
    scale_x_date(breaks = breaks_pretty(6))+
    scale_y_continuous(
      breaks = breaks_extended(7), labels = scales::label_number_si()
    ) +
    theme_light() + 
    labs(x = "", 
         y = "") +
    theme_typewriter() 
  
  p8/p7 + plot_layout(heights = c(4, 1))
  
}
