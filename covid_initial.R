
source("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\Covid Data\\covid_setup.R")



#########################################


data<-pull_world()
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
p4 <- data  %>% 
  covid_clean %>%
  covidplot_small_multiple()
####################################################



# dashboard
(p1/p2/p3) | p4







############# Animation Work


# US and Italy again:
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


#worldwide map log scale
data %>% 
  covid_clean %>% 
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





###############################
# states data 
# #############################



states %>% 
  
  filter(date == max(date)) %>% 
  summarise(
    cases = sum(cases), 
    fatalities=sum(deaths)
  )


states %>% 
  ggplot(aes(x = date, y = cases)) + 
  geom_line(color = "#d52b1e", size = 1) + 
  facet_geo(~state, grid = "us_state_grid2") + 
  scale_x_date(breaks = breaks_pretty(3))+
#  theme(legend.position = c(0.9, 0.2)) + 
  labs(
   title = "By State outbreaks of COVID-19" ,
   caption = "data via New York Times
@wouldeye125"
  ) + 
  theme_typewriter() + 
theme(plot.title.position = "plot")
?plot.title.position
