
source("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\Covid Data\\covid_setup.R")

county_politics<-readr::read_csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-16/master/2016_US_County_Level_Presidential_Results.csv")
county_politics <- county_politics %>% 
  rename("id" = X1) %>% 
  mutate(fips = combined_fips)
county_politics

  
  
  # pull covid case counts
  counties<-pull_counties()
  
  # assign new york city to new york county
  counties <- counties %>% 
    filter(county != "Unknown") %>% 
    mutate(fips = as.numeric(fips)) %>% 
    mutate(fips = if_else(
      county == "New York City", 36061, fips))
  
  # kansas city can't be resolved. kill it. 
  counties <- counties %>% 
    filter(!is.na(fips)) 
  
  
  counties<-left_join(counties, county_politics, id = "fips")
  
  
  
  counties <- counties %>% 
    mutate(winner = if_else(votes_dem > votes_gop, "democratic", "republican")) %>% 
    select(date, county, fips, cases, deaths, winner)
  
  # counties %>%
  #   group_by(fips) %>%
  #   mutate(uncum_cases= c(1, diff(cases))) %>%
  #   ungroup() %>%
  #   group_by(date, winner) %>%
  #   mutate(winsum = sum(uncum_cases)) %>%
  #   ungroup() %>%
  #   view()
  # 
  # 
  # 
  # 
  # counties %>% 
  #   group_by(fips) %>% 
  #   mutate(uncum_cases= c(1, diff(cases))) %>%
  #   ungroup() %>% 
  #   group_by(date, winner) %>% 
  #   mutate(winsum = sum(uncum_cases)) %>% 
  #   ungroup() %>% 
  #   ggplot(aes(x = date, y = winsum, color = winner)) + 
  #   geom_line()
  
  
  novafips <- c(51107, 51061, 51600, 51685, 51683, 51153, 51059, 51610,51510, 51013)
  
  
  
  counties %>% 
    filter(fips %in% novafips)  %>% 
    group_by(fips) %>% 
    mutate(uncum_cases= c(1, diff(cases))) %>%
    ungroup() %>% 
    group_by(date) %>% 
    mutate(winsum = sum(uncum_cases)) %>% 
    ggplot(aes(x = date)) +
    geom_col(aes(fill = county, y = uncum_cases)) +
    geom_smooth(method = "loess",
      color = "black",
      aes(y = winsum),
      span = 0.2) +
    scale_y_continuous(limits = c(0, 1250))+
    scale_x_date(breaks = breaks_pretty(12))+
    theme_typewriter() +
    labs(title = "COVID19 cases: Commonwealth of Northern Virginia", 
         x = "",
         y = "Daily New Cases") +
    university::scale_fill_wm() + 
    theme(legend.position = c(0.1, 0.75)) +
    annotate("rect", xmin = min(counties$date), xmax = as.Date("2020-03-15"),
                     ymin = 0, ymax = 1250, alpha = 0.1) + 
    annotate("rect", xmin = as.Date("2020-09-03"), xmax = as.Date("2020-11-23"),
                     ymin = 0, ymax = 1250, alpha = 0.2, fill = "orange") + 
    annotate("text", label = "Status Quo \nAnte", family = "Special Elite", 
             y = 200, 
             x = as.Date("2020-02-05"), 
             color = "black") +
    annotate("text", label = "Hybrid Model", family = "Special Elite", 
             y = 800, 
             x = as.Date("2020-10-15"), 
             color = "black") + 
    annotate("rect", xmin = as.Date("2020-07-06"), xmax = as.Date("2020-07-31"),
             ymin = 0, ymax = 1250, alpha = 0.2, fill = "green") + 
    annotate("text", label = "ESY", family = "Special Elite", 
             y = 450, 
             x = as.Date("2020-07-15"), 
             color = "black") + 
    geom_vline(xintercept = as.Date("2020-03-15")) + 
    annotate("text", label = "Governor \nCloses Schools", family = "Special Elite", 
             x = as.Date("2020-03-15"), y = 600)
  ?annotate
  
  counties %>% 
    filter(fips %in% novafips)  %>% 
    mutate(population = case_when(
      county == "Alexandria city"     ~ 159200,
      county == "Arlington"           ~ 231803,
      county == "Fairfax"             ~ 1147532,
      county == "Fairfax city"        ~ 25398,
      county == "Falls Church city"   ~ 14617,
      county == "Fauquier"            ~ 71222,
      county == "Loudoun"             ~ 413538,
      county == "Manassas city"       ~ 41085	,
      county == "Manassas Park city"  ~ 17478,
      county == "Prince William"      ~470335
    )) %>% 
    mutate(cases_per = cases / (population/1000)) %>% 
    group_by(fips) %>% 
    mutate(uncum_cases= c(0, diff(cases_per))) %>%
    ungroup() %>% 
    group_by(date) %>% 
    mutate(winsum = sum(uncum_cases)) %>% 
    ggplot(aes(
      x = date, 
      y = county,
      height = uncum_cases, 
      fill = county
    )) +
    scale_x_date(breaks = breaks_pretty(12))+
    geom_ridgeline( alpha = 0.5) + 
    theme_light() +
    labs(title = "Commonwealth of Northern Virginia: COVID 19 Cases",
         subtitle = "Cases per 1,000 Individuals",
         x = "",
         y = "")  +
    theme_typewriter() +
    university::scale_fill_wm() +
    guides(fill = FALSE)


counties %>% 
  filter(fips %in% novafips)  %>% 
  mutate(population = case_when(
    county == "Alexandria city"     ~ 159200,
    county == "Arlington"           ~ 231803,
    county == "Fairfax"             ~ 1147532,
    county == "Fairfax city"        ~ 25398,
    county == "Falls Church city"   ~ 14617,
    county == "Fauquier"            ~ 71222,
    county == "Loudoun"             ~ 413538,
    county == "Manassas city"       ~ 41085	,
    county == "Manassas Park city"  ~ 17478,
    county == "Prince William"      ~470335
  )) %>% 
  mutate(cases_per = cases / (population/1000)) %>% 
  group_by(fips) %>% 
  mutate(uncum_cases= c(0, diff(cases_per))) %>% 
  group_by(county) %>% 
  filter(date == min(date))
