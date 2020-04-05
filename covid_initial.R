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

theme_typewriter <- function() {
  ggplot2::theme_light()+
    ggplot2::theme(text = ggplot2::element_text(family = "Special Elite")) 
}
library(extrafont); loadfonts()
setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\Covid Data")
#create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")


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
      title = "Cumulative Cases", 
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


#########################################
pull_data<-function(){
  url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
  GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
  data <- read_excel(tf)
  return(data)
  
}

data<-pull_data()
data
p1<-data %>% 
  mark() %>% 
  covidplot()

p2<-data %>% 
  mark() %>% 
  covidplot_log()

p3<-data %>% 
  mark() %>% 
  covidplot_raw()
####################################################


data %>% 
 #filter(GeoId == "CN") %>% 
  group_by(geoId) %>% 
  arrange(as.Date(dateRep)) %>% 
  mutate(mark = row_number()) %>% 
  mutate(Cases_cumsum = cumsum(cases)) %>% 
  mutate(Deaths_cumsum = cumsum(deaths)) %>% 
  ggplot(aes(x = mark, y = Cases_cumsum)) +
  geom_line() + 
  guides(color = FALSE)

data %>% 
  arrange(as.Date(DateRep)) %>% 
  group_by(GeoId) %>% 
  mutate(mark = row_number()) %>% 
  arrange(GeoId, mark) %>% 
  #group_by(GeoId) %>% 
  mutate(Cases_cumsum = cumsum(Cases)) %>% 
  mutate(Deaths_cumsum = cumsum(Deaths)) %>% 
  ggplot(aes(x = DateRep, y = Cases_cumsum, color = GeoId, label = GeoId)) +
  geom_line() + 
  guides(color = F) +
  geom_text_repel()

######################################################

#https://kieranhealy.org/blog/archives/2020/03/27/a-covid-small-multiple/


cov_curve <- data %>%
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


p4<-cov_curve %>%
  filter(geoId %in% topn$geoId) %>% 
  ggplot(mapping = aes(x = days_elapsed, y = cu_cases)) + 
  # geom_line(data =topn_bg, 
  #           aes(group = geoId), 
  #               size = 0.15, 
  #               color = "grey80") +
   geom_line(color = "firebrick", lineend = "round")  +
   # geom_point(data = endpoints, 
   #            size = 1.1, 
   #            shape = 21, 
   #            color = "firebrick", 
   #            fill = "firebrick2") + 
  # geom_text(data=topn, 
  #           mapping = aes(label = countriesAndTerritories),
  #           vjust = "inward",
  #           hjust = "inward",
  #           fontface = "bold", 
  #           color = "firebrick", 
  #           size = 2.1
  #           ) + 
 scale_y_log10(labels = scales::label_number_si()) + 
  facet_wrap( ~ reorder(countriesAndTerritories, -cu_cases), ncol = 5) + 
  labs(
    x = "Days since 100th confirmed case",
    y = "Log Cumulative Cases",
    title = "Cumulative COVID-19 Cases: Top 30 Countries", 
    subtitle = paste("Data as of", format(max(cov_curve$date), "%A, %B %e, %Y")),
    caption = "Data: https://www.ecdc.europa.eu/"
  )+ 
  theme_typewriter() + 
  theme(strip.text = element_blank())


(p1/p2/p3) | p4
#p4


topn %>% 
  arrange(geoId) %>% 
  view()
topn_bg %>% 
  group_by(geoId) %>% 
  count() %>% 
  view()
cov_curve %>% 
  filter(geoId %in% topn$geoId) %>% 
  group_by(geoId, countriesAndTerritories) %>% 
  arrange(geoId) 
endpoints %>% 
  arrange(-cu_cases)



p4 +
  transition_time(days_elapsed)




data %>%
  mark %>%
  mutate(DateRep_lagged = as.Date(DateRep_lagged)) %>%
  mutate(pop = case_when(geoId == "US" ~ 327.2 ,
                         geoId == "IT" ~ 60.48)) %>%
  mutate(case_normal = Cases_cumsum / pop) %>%
  filter(DateRep_lagged > "2020-02-21") %>%
  ggplot(aes(x = DateRep_lagged, y = Cases_cumsum, fill = geoId)) +
  geom_col(position = "dodge") +
  scale_x_date(breaks = breaks_pretty(5)) +
  scale_y_continuous(breaks = breaks_extended(7), labels = scales::label_number_si()) +
  #scale_y_log10() +
  theme_typewriter() +
  scale_fill_manual(values = c("forestgreen", "navy")) +
  labs(
    title = "Cumulative Cases",
    x =  "",
    y = "",
    subtitle = "11-day lag determined by first day of 100 cases",
    #caption = "Data via European Centre for Disease Prevention and Control",
    fill = "Country"
  ) +
  theme(legend.position = c(0.1, 0.75)) +
  transition_time(dateRep) +
  shadow_mark() +
  NULL


###################################################

cov_curve
psych::describe(cov_curve)
cov_curve %>% 
dossier(countriesAndTerritories, "Japan")
pal<-c(
  "CN" = "red",
  "FR" = "blue",
  "DE" = "yellow",
  "IR" = "black",
  "IT" = "green",
  "ES" = "orange",
  "US" = "purple", 
  rep("gray", (122-7))
  )
pal
cov_curve
cov_curve %>% 
 # filter(geoId %in% topn$geoId) %>% 
  ggplot(aes(x = days_elapsed, 
             y = cu_cases, 
             color = geoId,
             label = countriesAndTerritories )) +
  geom_line(size = 1)+
  geom_point(size = 1.5) + 
 scale_x_continuous() +
  guides(color = FALSE) +
  scale_y_log10(labels = scales::label_number_si()) + 
  labs(
    x = "Days since 100th confirmed case",
    y = "Log Cumulative Cases",
    title = "Crossnational progression of COVID-19", 
    subtitle = paste("Data as of", format(max(cov_curve$date), "%A, %B %e, %Y")),
    caption = "Data: https://www.ecdc.europa.eu/
@wouldeye125"
  ) + 
#  scale_color_manual(values = pal)
  geom_label_repel(data = subset(cov_curve, geoId %in% 
                                   c("CN" ,"FR" ,"DE" ,"IR" ,"IT" ,"ES" ,"US", "KR", "JP")),
                   na.rm = TRUE, 
                   nudge_x = 1,
                   nudge_y = 0) +
  theme_typewriter() + 
  transition_reveal(date) 


topn <- cov_curve %>%
  group_by(geoId) %>%
  filter(days_elapsed == max(days_elapsed)) %>%
  ungroup() %>%
  top_n(7, cu_cases) %>%
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


topn
