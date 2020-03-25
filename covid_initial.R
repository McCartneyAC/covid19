library(readxl)
library(httr)
library(scales)
library(ggplot)
library(mccrr)
library(dplyr)
theme_typewriter <- function() {
  ggplot2::theme_light()+
    ggplot2::theme(text = ggplot2::element_text(family = "Special Elite")) 
}
library(extrafont); loadfonts()
#create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")


data <- read_excel(tf)

 mark<-function(data) {
   data%>% 
     filter(GeoId %in% c("US", "IT")) %>% 
     arrange(DateRep) %>% 
     group_by(GeoId) %>% 
     mutate(mark = row_number()) %>% 
     mutate(DateRep_lagged = case_when(
       GeoId == "US" ~ (DateRep - 950400),
       GeoId == "IT" ~ DateRep
     ) ) %>% 
   #  ungroup %>% 
     arrange(GeoId, mark) %>% 
     #group_by(GeoId) %>% 
     mutate(Cases_cumsum = cumsum(Cases)) %>% 
     mutate(Deaths_cumsum = cumsum(Deaths)) 
 }

covidplot<-function(data_marked){
  data_marked %>% 
    mutate(DateRep_lagged = as.Date(DateRep_lagged)) %>% 
    mutate(pop = case_when(
        GeoId == "US" ~ 327.2 ,
        GeoId == "IT" ~ 60.48
      ) ) %>% 
    mutate(case_normal = Cases_cumsum/pop) %>% 
    filter(DateRep_lagged > "2020-02-21") %>% 
    ggplot(aes(x = DateRep_lagged, y = Cases_cumsum, fill = GeoId)) + 
    geom_col(position = "dodge") +
    scale_x_date(breaks = breaks_pretty(5)
    ) + 
    scale_y_continuous(
     breaks = breaks_extended(7), labels = label_comma() 
    ) + 
  # scale_y_log10() + 
    theme_typewriter() + 
    scale_fill_manual(values = c( "forestgreen", "navy")) + 
    labs(
      title = "Comparing COVID-19 Cases: United States and Italy", 
      x =  "Date (with United States on 11-day lag)", 
      y = "Cumulative Cases", 
      subtitle = "11-day lag determined first day of 100 cases", 
      caption = "Data via European Centre for Disease Prevention and Control", 
      fill = "Country"
    ) + 
    theme(legend.position = c(0.1, 0.75)) 
}

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
data <- read_excel(tf)
data %>% 
  mark() %>% 
  covidplot()
+