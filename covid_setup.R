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
library(ggridges)
library(forcats)
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

teacher_pay <- read_csv("https://raw.githubusercontent.com/McCartneyAC/teacher_pay/master/teacher_pay.csv")

states <- pull_states()
states <- states %>% 
  left_join(teacher_pay, by = "state") %>% 
  dplyr::select(date, state, fips, cases, deaths, abbreviation, population2018, log_pop)%>% 
  mutate(cases_per = cases / (population2018/1000))


covid_clean<-function(data) {
  data %>%
    mutate(date = lubridate::ymd(dateRep),
           countriesAndTerritories = dplyr::recode(
             countriesAndTerritories,
             "United_States_of_America" = "United States",
             "Czech_Republic" = "Czechia",
             "United_Kingdom" = "United Kingdom",
             "South_Korea"= "South Korea", 
             "Saudi_Arabia" = "Saudi Arabia"
           )
           ) %>% 
    dplyr::select(date, countriesAndTerritories, geoId, cases, deaths) %>%
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


# cov_curve <- world %>% 
#   covid_clean()
topn <- cov_curve %>% 
  group_by(geoId) %>%
  filter(days_elapsed == max(days_elapsed)) %>%
  ungroup() %>%
  top_n(30, cu_cases) %>%
  arrange(-cu_cases) %>% 
  dplyr::select(countriesAndTerritories, geoId, cu_cases) %>%
  mutate(
    days_elapsed = 1,
    cu_cases = max(cov_curve$cu_cases) - 1e4,
    countriesAndTerritories = dplyr::recode(
      countriesAndTerritories,
      "United_States_of_America" = "United States",
      "Czech_Republic" = "Czechia",
      "United_Kingdom" = "United Kingdom",
      "South_Korea"= "South Korea",
      "Saudi_Arabia" = "Saudi Arabia"
    )
  )


topn_factors<-cov_curve %>% 
  group_by(geoId) %>%
  filter(days_elapsed == max(days_elapsed)) %>%
  ungroup() %>%
  top_n(30, cu_cases) %>%
  arrange(-cu_cases) %>% 
  dplyr::select(countriesAndTerritories)


topn_bg<- cov_curve %>% 
  dplyr::select(-countriesAndTerritories) %>% 
  filter(geoId %in% topn$geoId) %>% 
  dplyr::select(geoId, days_elapsed, cu_cases)


endpoints <- cov_curve %>% 
  filter(geoId %in% topn$geoId) %>% 
  group_by(geoId) %>% 
  filter(days_elapsed ==max(days_elapsed)) %>% 
  dplyr::select(countriesAndTerritories, 
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
  data_adj <- data %>% 
    filter(geoId == "US") %>% 
    dplyr::select(dateRep, cases, deaths) %>% 
    filter(dateRep >  "2020-02-21") %>% 
    pivot_longer(-c(dateRep), 
                 names_to = "Daily",
                 values_to = "total") %>% 
    mutate(dateRep = as.Date(dateRep))
  
  three_quarters<- 0.85*max(data_adj$total)
  
  data_adj %>% 
    ggplot(aes(
      x = dateRep, 
      y = total, 
      fill = Daily, 
      color = Daily
    )) + 
    geom_col(position = "dodge") +
    scale_x_date(breaks = breaks_pretty(10)
    ) + 
    scale_y_continuous(
      breaks = breaks_extended(7), labels = scales::label_number_si()
    ) + 
    # geom_vline(xintercept = as.Date("2020-07-16"),linetype = "dashed",color = "red") +
    # annotate("text", label = "Data Shift from CDC to HHS",y = three_quarters,x = as.Date("2020-07-01"),color = "red")+
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
    mutate(uncum_cases= c(1, diff(cases))) %>% 
    ungroup() %>% 
    filter(date > "2020-03-01") %>% 
    mutate(uncum_cases = if_else(uncum_cases<0, 0, uncum_cases)) %>% 
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
    mutate(uncum_cases= c(1, diff(cases))) %>% 
    ungroup() %>% 
    filter(date > "2020-03-01") %>% 
    group_by(date) %>% 
    summarise(uncum_cases = sum(uncum_cases)) %>% 
    ungroup() %>% 
    ggplot(aes(x = date, y = uncum_cases)) +
    geom_col(fill = "black")+ 
    geom_vline(xintercept = as.Date("2020-07-16"),
               linetype = "dashed",
               color = "red") +
    annotate("text", label = "Data Shift 
from CDC to HHS",
             y = 30000,
             x = as.Date("2020-07-07"),
             color = "red")+
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
state_growth<- function(state){
  
  data <-states %>% 
    filter(state == !!state) %>% 
    mutate(uncum_cases= c(0, diff(cases))) %>% 
    filter(date > "2020-03-01")
  
  state_max <- max(data$uncum_cases)
  
  data %>% 
    ggplot(aes(x = date, y = uncum_cases)) +
    geom_col(fill = "black")+ 
    scale_x_date(breaks = breaks_pretty(8))+
    scale_y_continuous(
      breaks = breaks_extended(7), labels = scales::label_number_si(), 
      limits = c(0, state_max)
    ) +
    geom_smooth(color = "orange", span = 0.4) + 
    theme_light() + 
    labs(title = paste0("Statewide cases: ", state),
         x = "", 
         y = "") +
    theme_typewriter() 
}




data_fuckery<-function(data, SE = FALSE) {
  data_adj <- data %>% 
    filter(geoId == "US") %>% 
    dplyr::select(dateRep, cases, deaths) %>% 
    filter(dateRep >  "2020-02-21") %>% 
    pivot_longer(-c(dateRep), 
                 names_to = "Daily",
                 values_to = "total") %>% 
    mutate(dateRep = as.Date(dateRep)) %>% 
    mccrr::center(dateRep, as.Date("2020-07-16")) %>% 
    mutate(dateRep = as.numeric(dateRep))
  
  data_adj %>% 
    filter(dateRep > - 39) %>% 
    filter(Daily == "cases") %>% 
    mutate(reporter = if_else(dateRep > 0, "HHS", "CDC")) %>% 
    ggplot(aes(x = dateRep, y = total, color = reporter)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = SE) + 
    theme_typewriter() + 
    scale_fill_inova() + 
    scale_x_continuous(breaks =breaks_pretty(10)) +
    scale_y_continuous(
      breaks = breaks_extended(7), labels = scales::label_number_si()
    ) + 
    scale_color_inova() +
    labs(
      title = "US Covid cases have stopped rising since the Data Move. ", 
      x =  "Days since the Data Switch", 
      y = "Daily New Cases", 
      caption = "On July 15, Trump ordered that hospitals send future data to HHS rather than the CDC. 
Since then, COVID cases have ceased to rise."
    ) + 
    theme(legend.position = c(0.1, 0.75))
}



#     mutate(uncum_cases = if_else(uncum_cases<0, 0, uncum_cases)) %>% 
########################################
# US Alt



pwhatever <- states %>% 
  arrange(date) %>%   
  filter(date > "2020-02-01") %>% 
  group_by(state) %>% 
  mutate(uncum_cases= c(cases[1], diff(cases)))  %>% 
  filter(uncum_cases > 0) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  summarize(uncum_cases = sum(uncum_cases))  %>% 
  arrange(date) %>% 
  ggplot(aes(x = date, y = uncum_cases, group = date)) +
  geom_col() + 
  scale_x_date(breaks = breaks_pretty(10)
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
    caption = "U.S. Daily New Cases"
  ) + 
  guides(color = FALSE)+ 
  theme(legend.position = c(0.1, 0.75)) 
  # geom_vline(xintercept = as.Date("2020-09-01"), 
  #                                                  linetype = "dashed", 
  #                                                  color = "red") + 
  # annotate("text", label = "Schools Reopen (approx)", family = "Special Elite", 
  #          y = 150000, 
  #          x = as.Date("2020-08-05"), 
  #          color = "red")
  #geom_smooth(se= T, span = 0.13) + 
  # guides(smooth = FALSE) 
  # transition_reveal(date, keep =  TRUE)



decumulate<-function(df, var){
  user_var<-enquo(var)
  varname <- quo_name(user_var)
  firstval<-df %>% 
    # how the hell does get_quo_expr help here?
    dplyr::select(!!user_var) %>% 
    dplyr::filter(row_number()==1)

  mutate(df, `:=`(!!varname, (
    # actual function here
    c(firstval, diff(!!user_var))
  ))) %>% 
    unnest(!!user_var)
}





# 
# 
# 
# states %>% 
#   group_by(state) %>% 
#   arrange(date) %>% 
#   mutate(dead= c(deaths[1], diff(deaths)))  %>%
#   ungroup() %>% 
#   group_by(date) %>% 
#   mutate(winsum = sum(dead)) %>% 
#   ggplot(aes(x = date, y = dead)) + 
#   geom_col() + 
#   geom_smooth(aes(y = winsum), color = "red"#,
#              # span = 0.01
#              ) + 
#   scale_y_continuous(limits = c(0,3000))+
#   scale_x_date(breaks = breaks_pretty(6),
#                limits = c(as.Date("2020-03-01"), 
#                           Sys.Date()
#                           )
#   ) + 
#   theme_typewriter()
#                

# 
# data_adj <- data %>% 
#   filter(geoId == "US") %>% 
#   dplyr::select(dateRep, cases, deaths) %>% 
#   filter(dateRep >  "2020-02-21") %>% 
#   pivot_longer(-c(dateRep), 
#                names_to = "Daily",
#                values_to = "total") %>% 
#   mutate(dateRep = as.Date(dateRep))
# 
# three_quarters<- 0.85*max(data_adj$total)
# 
# data_adj %>%
#   filter(Daily == "deaths") %>% 
#   ggplot(aes(
#     x = dateRep, 
#     y = total#, 
#     #fill = Daily, 
#     #color = Daily
#   )) + 
#   geom_col(position = "dodge") +
#   scale_x_date(breaks = breaks_pretty(5)
#   ) + 
#   scale_y_continuous(
#     breaks = breaks_extended(7), labels = scales::label_number_si()
#   ) + 
#   # geom_vline(xintercept = as.Date("2020-07-16"),linetype = "dashed",color = "red") +
#   # annotate("text", label = "Data Shift from CDC to HHS",y = three_quarters,x = as.Date("2020-07-01"),color = "red")+
#   theme_typewriter() + 
#   scale_fill_inova() + 
#   scale_color_inova() +
#   labs(
#     title = "US Daily COVID-19", 
#     x =  "", 
#     y = "", 
#     caption = "Data via European Centre for Disease Prevention and Control"
#   ) + 
#   guides(color = FALSE)+ 
#   theme(legend.position = c(0.1, 0.75))+ 
#   geom_vline(xintercept = as.Date("2020-09-01"), 
#                                                     linetype = "dashed", 
#                                                     color = "red") + 
#   annotate("text", label = "Schools Reopen (approx)", 
#            y = 1500, 
#            x = as.Date("2020-08-05"), 
#            color = "red")+
#   geom_smooth(se= FALSE, span = 0.13)
