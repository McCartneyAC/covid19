
source("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\Covid Data\\covid_setup.R")



#########################################


data<-pull_world()
data<-world
#data <- readxl::read_xlsx("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\Covid Data\\COVID-19-geographic-disbtribution-worldwide-2020-07-25.xlsx")
states<-pull_states(); max(states$date)





p1<-data %>% 
  mark() %>% 
  covidplot()

p2<-data %>% 
  mark() %>% 
  covidplot_log()

p3<-data %>% 
  mark() %>% 
  covidplot_raw()

p4 <- data %>%   
  covid_clean() %>% 
  covidplot_small_multiple()

p5 <-data %>% 
  covidplot_us_daily()

# data %>%
#   data_fuckery(SE = TRUE)
####################################################

p5   + geom_vline(xintercept = as.Date("2020-09-01"), 
                  linetype = "dashed", 
                  color = "red") + 
  annotate("text", label = "Schools Reopen 
(approx)", 
           y = 150000, 
           x = as.Date("2020-08-05"), 
           color = "red")+
  geom_smooth(se= FALSE, span = 0.13) + guides(smooth = FALSE)

pwhatever + geom_vline(xintercept = as.Date("2020-09-01"), 
                         linetype = "dashed", 
                         color = "red") + 
  annotate("text", label = "Schools Reopen \n(approx)", family = "Special Elite", 
           y = 150000, 
           x = as.Date("2020-08-05"), 
           color = "red") +
  geom_smooth(se= T, span = 0.13) 
  transition_reveal(date, keep =  TRUE)


p4
# dashboard
p5 | p4


states %>% 
  covid_tile()
teacher_pay


unknown_pleasures <- states %>% 
  filter(state %not_in% c("Puerto Rico", 
                          "Virgin Islands", 
                          "Guam", 
                          "Northern Mariana Islands",
                          "American Samoa",
                          "District of Columbia")) %>% 
#  left_join(teacher_pay, by = "state") 
  mutate(cases_per = cases / (population2018/1000)) %>% 
  group_by(state) %>% 
  mutate(uncum_cases= c(0, diff(cases_per))) %>% 
  ungroup() %>% 
  mutate(uncum_cases = if_else(uncum_cases<0, 0, uncum_cases)) %>% 
  filter(date > "2020-03-01")  %>% 
  arrange(desc(state), date) %>% 
  mutate(state = factor(state)) %>% 
  ggplot(aes(
    x = date, 
    y = state,
    height = uncum_cases
  )) +
  scale_x_date(breaks = breaks_pretty(12))+
  geom_ridgeline(alpha = 0.5, fill = "#004b8d") + 
  theme_light() +
  labs(title = "By State Case Counts", 
       fill = "Daily Cases",
       subtitle = "Cases per 1,000 Individuals",
       x = "",
       y = "")  +
  theme_typewriter()  
  
unknown_pleasures 



state_growth("Georgia")  


#### deaths

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
    c("CN" ,"IR" ,"IT" ,"ES" ,"US", "KR", "JP", "BR", "IN", "RU", "PE", "MX", "UK")),
                   na.rm = TRUE, 
                   nudge_x = 1,
                   nudge_y = 0) +
  theme_typewriter() + 
  transition_reveal(date) 



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


########################################################################################
teacher_pay <- read_csv("https://raw.githubusercontent.com/McCartneyAC/teacher_pay/master/teacher_pay.csv")

states <- pull_states()
states %>% 
  covid_tile()
teacher_pay <- teacher_pay %>%
  mutate(winner = if_else(clinton_votes_2016 > trump_votes_2016 , "democratic", "republican")) %>% 
  rename(state = State)

teacher_pay
states <- states %>% 
  left_join(teacher_pay, by = "state") %>% 
  dplyr::select(date, state, fips, cases, deaths, population2018, log_pop, winner) %>% 
  mutate(cases_per = cases / (population2018/1000))

states 
p8<-states %>% 
  filter(state %not_in% c("Puerto Rico", 
                          "Virgin Islands", 
                          "Guam", 
                          "Northern Mariana Islands",
                          "American Samoa",
                          "District of Columbia")) %>% 
  group_by(state) %>% 
  mutate(uncum_cases= c(0, diff(cases_per))) %>% 
  ungroup() %>% 
  filter(date > "2020-03-01") %>%
  mutate(uncum_cases = if_else(uncum_cases < 0, 0, uncum_cases)) %>% 
  filter(uncum_cases > 0) %>% 
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
       subtitle = "Cases per 1,000 Individuals",
       x = "",
       y = "")  +
  theme_typewriter() 

p7 <- states %>% 
  filter(state %not_in% c("Puerto Rico", 
                          "Virgin Islands", 
                          "Guam", 
                          "Northern Mariana Islands",
                          "American Samoa", 
                          "District of Columbia")) %>% 
  group_by(state) %>% 
  mutate(uncum_cases= c(0, diff(cases))) %>% 
  ungroup() %>% 
  filter(date > "2020-03-01") %>% 
  group_by(date) %>% 
  summarise(uncum_cases = sum(uncum_cases)) %>% 
  ungroup() %>% 
  filter(uncum_cases>0) %>% 
  ggplot(aes(x = date, y = uncum_cases)) +
  geom_col(fill = "black")+ 
  scale_x_date(breaks = breaks_pretty(6))+
  scale_y_continuous(
    breaks = breaks_extended(7), labels = scales::label_number_si()
  ) +
  geom_smooth(span = 0.35) + 
  theme_light() + 
  labs(x = "", 
       y = "") +
  theme_typewriter() 

p8/p7 + plot_layout(heights = c(4, 1))

states %>% 
  filter(state %not_in% c("Puerto Rico", 
                               "Virgin Islands", 
                               "Guam", 
                               "Northern Mariana Islands",
                               "American Samoa", 
                               "District of Columbia")) %>% 
  group_by(state) %>% 
  mutate(uncum_cases= c(0, diff(cases))) %>% 
  ungroup() %>% 
  group_by(date, winner) %>% 
  mutate(winsum = sum(uncum_cases)) %>% 
  ungroup() %>% 
  filter(date > "2020-03-01") %>% 
  ggplot(aes(x = date, y = winsum, color = winner)) + 
  geom_line(size = 2) + 
  #geom_smooth()+
  university::scale_color_american() + 
  scale_y_continuous(
    breaks = breaks_extended(7), labels = scales::label_number_si()
  ) +
  scale_x_date(breaks = breaks_pretty(6))+
  theme_typewriter() + 
  labs(title = "Republican States Overtake 
Democratic States in Covid19 Cases", 
       y = "Total Daily Cases", 
       x = "", 
       color = "State Party 
affiliation in 
2016 Election")

library(ggvis)
states %>% 
  filter(state %not_in% c("Puerto Rico", 
                          "Virgin Islands", 
                          "Guam", 
                          "Northern Mariana Islands",
                          "American Samoa", 
                          "District of Columbia")) %>% 
  group_by(state) %>% 
  mutate(uncum_cases= c(0, diff(cases))) %>% 
  ungroup() %>% 
  group_by(date, winner) %>% 
  mutate(winsum = sum(uncum_cases)) %>% 
  ungroup() %>% 
  filter(date > "2020-03-01") %>% 
  ggvis(~date, ~winsum) %>% 
  #group_by(winner) %>% 
  layer_points(fill = ~factor(winner))





data <-states %>% 
  filter(state %in% c("New York", "Florida")) %>% 
  group_by(state) %>% 
  mutate(uncum_cases = c(1, diff(cases))) %>% 
  ungroup() %>% 
  filter(date > "2020-03-01")

state_max <- max(data$uncum_cases)

data %>% 
  filter(date > "2020-06-25")
data %>% 
  ggplot(aes(x = date, y = uncum_cases)) +
  geom_point(fill = "black")+ 
  scale_x_date(breaks = breaks_pretty(8))+
  scale_y_continuous(
    breaks = breaks_extended(7), labels = scales::label_number_si(), 
    limits = c(0, state_max)
  ) +
  facet_wrap(~state) + 
  geom_smooth(color = "orange", span = 0.3) + 
  theme_light() + 
  labs(title = paste0("Statewide Cases: "),
       x = "", 
       y = "") +
  theme_typewriter()


################
maxukr <- data %>% 
  filter(countriesAndTerritories == "Ukraine") %>% 
  summarize(max = max(cases))
maxukr
data %>% 
  filter(countriesAndTerritories == "Ukraine") %>% 
  mutate(max1 = max(cases)) %>% 
  ggplot(aes(x = as.Date(dateRep), y = cases)) + 
  geom_col() + 
  theme_light() + 
  geom_smooth() + 
  scale_y_continuous(limits = c(0, 30000))



data %>% 
  filter(countriesAndTerritories == "Bermuda") %>% 
  ggplot(aes(x = dateRep, y = cases)) +
  geom_col() + geom_smooth() + 
  theme_typewriter()+ 
  labs(title = "COVID-19 cases: Bermuda")
