
source("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\Covid Data\\covid_setup.R")



#########################################


data<-pull_world()
data
states<-pull_states()

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
  covid_clean() %>%
  covidplot_small_multiple()

p5 <-data %>% 
  covidplot_us_daily()
####################################################


p4
# dashboard
(p1/p5) | p4
states %>% 
  covid_tile()

#### deaths
data
data %>% 
  #covid_clean() %>% 
  mark() %>% 
  mutate(DateRep_lagged = as.Date(DateRep_lagged)) %>% 
  mutate(pop = case_when(
    geoId == "US" ~ 327.2 ,
    geoId == "IT" ~ 60.48
  ) ) %>% 
  mutate(death_normal = Deaths_cumsum/pop) %>% 
  filter(geoId %in% c("IT", "US")) %>% 
  ggplot(aes(
    x = DateRep_lagged,
    y = death_normal , 
    fill = geoId
  )) + geom_col(position = "dodge") +
  scale_x_date(breaks = breaks_pretty(5), 
               limits = c(as.Date("2020-03-01"),NA)
               ) + 
  scale_y_log10(labels = scales::label_number_si()) + 
  # scale_y_continuous(
  #   breaks = breaks_extended(7), labels = scales::label_number_si()
  # ) +
  #scale_y_log10() + 
  theme_typewriter() + 
  scale_fill_manual(values = c( "forestgreen", "navy")) + 
  labs(
    title = "Cumulative Deaths", 
    x =  "", 
    y = "Log Scale", 
    subtitle = "11-day lag determined by first day of 100 cases", 
    #caption = "Data via European Centre for Disease Prevention and Control", 
    fill = "Country"
  ) + 
  theme(legend.position = c(0.1, 0.75)) 






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
  covid_clean() %>% 
  ggplot(aes(x =  days_elapsed, 
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




#############################
# 
# cov_curve %>% 
#   filter(cu_cases < 1000) %>% 
#   filter(days_elapsed <30) %>% 
#   view()

###############################
# states data 
# #############################


data %>% 
  filter(countriesAndTerritories == "Italy")%>% 
  summarise(
    cases = sum(cases), 
    fatalities=sum(deaths)
  )
data %>% 
  filter(countriesAndTerritories == "United_States_of_America")%>% 
  summarise(
    cases = sum(cases), 
    fatalities=sum(deaths)
  )



states %>% 
  filter(date == max(date)) %>% 
  summarise(
    cases = sum(cases), 
    fatalities=sum(deaths)
  )

statecounts<- states %>% 
  filter(state %not_in% c("Puerto Rico", 
                          "Virgin Islands", 
                          "Guam", 
                          "Northern Mariana Islands",
                          "American Samoa")) %>% 
  select(date, state, cases, deaths) %>% 
  pivot_longer(-c(date, state), 
               names_to = "cumulative",
               values_to = "total") %>% 
  filter(date == max(date)) %>% 
  mutate(date = as.Date("2020-02-20"))







states %>% 
  select(date, state, cases, deaths) %>% 
  pivot_longer(-c(date, state), 
               names_to = "cumulative",
               values_to = "total")
    

states %>% 
  filter(state %not_in% c("Puerto Rico", 
                          "Virgin Islands", 
                          "Guam", 
                          "Northern Mariana Islands",
                          "American Samoa")) %>% 
  select(date, state, cases, deaths) %>% 
  pivot_longer(-c(date, state), 
               names_to = "cumulative",
               values_to = "total") %>% 

  ggplot(aes(x = date, y = total, color = cumulative)) + 
  geom_line(size = 1.5) +
  geom_text(data = statecounts, 
            aes(
              label = as.character(total), 
              color = cumulative
            ),
            fontface = "bold",
          #  hjust = "right",
            size = 3.25
            ) +
  scale_x_date(breaks = breaks_pretty(3))+
 scale_y_log10(labels = scales::label_number_si())+
  scale_color_manual(values = c("#004b8d", "#d52b1e")) + 
  labs(
    title = "By State outbreaks of COVID-19" ,
    subtitle = paste("Data as of", format(max(states$date), "%A, %B %e, %Y")),
    caption = "data via New York Times
@wouldeye125", 
    y = "Cumulative Cases"
  ) + 
  theme_typewriter() + 
  theme(plot.title.position = "plot") + 
  theme(legend.position = c(0.9, 0.25)) +
  facet_geo(~state, grid = "us_state_grid2") 
  #facet_wrap(~state)





states %>% 
  select(date, state, cases, deaths) %>% 
  pivot_longer(-c(date, state), 
               names_to = "cumulative",
               values_to = "total") %>% 
  filter(state %not_in% c("Puerto Rico", 
                          "Virgin Islands", 
                          "Guam", 
                          "Northern Mariana Islands")) %>% 
  view()




















counties<-pull_counties()
counties <- counties %>% 
  filter(county != "Unknown")

counties %>% 
  filter(state == "New York") %>% 
  view()


counties %>% 
  filter(county != "Unknown") %>% 
  group_by(county) %>% 
  mutate(uncum_deaths= c(0, diff(deaths))) %>% 
  ungroup() %>% 
  filter(date > "2020-03-01") %>% 
  ggplot(aes(
    x = date, 
    y = county,
    fill = uncum_deaths
  )) +
  scale_x_date(breaks = breaks_pretty(6))+
  scale_fill_viridis_c() +
  scale_y_discrete(labels = NULL) +
  geom_tile(color = "white") + 
  theme_light() + 
  facet_geo(~state) 




###########################################################
library(forcats)


states %>% 
  filter(state %not_in% c("Puerto Rico", 
                          "Virgin Islands", 
                          "Guam", 
                          "Northern Mariana Islands",
                          "American Samoa")) %>% 
  select(state) %>% 
  unique() %>% 
  arrange(desc(state)) %>% 
  c() -> state_names

state_names










states %>% 
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
  scale_y_discrete(limits = rev(levels(states$state)))+
  scale_x_date(breaks = breaks_pretty(6))+
  scale_fill_viridis_c() +
  geom_tile(color = "white") + 
  theme_light() +
  labs(title = "By State Case Counts", 
       fill = "Daily Cases",
       x = "",
       y = "")  +
  theme_typewriter() 
