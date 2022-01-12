## covid mapping new york state
library(viridis)
library(tidyverse)
library(cowplot)     # for theme_map()
library(colorspace)  # for scale_fill_continuous_sequential()
library(sf)       




theme_map <- function(font_size = 14, font_family = "", line_size = .5,
                      rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14){
  # work based off of theme_cowplot to get font sizing right
  theme_cowplot(font_size = font_size, font_family = font_family, line_size = line_size,
                rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large) %+replace%
    theme(
      line = element_blank(),
      rect = element_blank(),
      
      axis.line =          element_blank(),
      axis.line.x =        NULL,
      axis.line.y =        NULL,
      axis.text =          element_blank(),
      axis.text.x =        NULL,
      axis.text.x.top =    NULL,
      axis.text.y =        NULL,
      axis.text.y.right =  NULL,
      axis.ticks =         element_blank(),
      axis.ticks.length =  unit(0, "pt"),
      axis.title =         element_blank(),
      axis.title.x =       NULL,
      axis.title.x.top =   NULL,
      axis.title.y.right = NULL,
      complete = TRUE
    )
}

# download shape files and import:

ny <- sf::st_read("C://Users//Andrew//Desktop//Statistics and Data Analysis//Covid Data//new_york_shapefiles//NYS_Civil_Boundaries.shp//Counties.shp")
plot(ny)

head(ny)

ny_2<-ny %>% 
  as_tibble() %>% 
  select(NAME, FIPS_CODE, POP2020) 

# pull covid case counts
countiesny<-pull_counties() %>% 
  filter(county != "Unknown") %>% 
  mutate(fips = as.numeric(fips)) %>% 
  filter(state=="New York")
counties %>% 
  group_by(fips,county ) %>% 
  count() %>% 
  view()

# dealing with the five boroughs merge ------------------------------------


ny_2
ny_2  %>% 
  rename(fips = FIPS_CODE) %>% 
  mutate(fips = as.numeric(fips)) %>% 
  full_join(countiesny, by = "fips") %>% 
  group_by(county, fips) %>% 
  count() %>% 
  view()
countiesny
?as_numeric
?left_join
countiesny
# merge the 5 boroughs
nycfips <-c(
  "Bronx" = 36005,
  "Kings" = 36047,
  "New York" = 36061,
  "Queens" = 36081,
  "Richmond" = 36085
)
countiesny <-countiesny %>% 
  mutate(fips = if_else(is.na(fips), 36061, fips)) %>% 
  # de-cumulative-ate
  group_by(fips) %>% 
  mutate(uncum_cases= c(1, diff(cases))) %>%
  ungroup() 


# Merge with data
ny_geo <- ny %>% 
  mutate(FIPS_CODE = as.numeric(FIPS_CODE)) %>% 
  #filter(FIPS_CODE %in% nycfips) %>% 
  mutate(FIPS_CODE = if_else(FIPS_CODE %in% nycfips, 36061, FIPS_CODE )) %>% 
  rename(fips = FIPS_CODE) %>% 
  left_join(countiesny, by = "fips")

countiesny
# mapping static -----------------------------------------------------------------


ny_geo %>% 
  select(NAME, fips, date, cases, deaths) %>% 
  arrange(desc(date))


# dimensions of the graph. 
# required to know where to place annotation around NYC
plot_box <- tibble(xmin = st_bbox(ny_geo)[1],
                   xmax = st_bbox(ny_geo)[3],
                   ymin = st_bbox(ny_geo)[2],
                   ymax = st_bbox(ny_geo)[4],
                   xrange = xmax - xmin,
                   yrange = ymax - ymin)



ny_geo %>%
  filter(date == as.Date("2021-12-19"))  %>%
#  mutate(uncum_per = (uncum_cases / (POP2020/1000))) %>% 
  ggplot() +
  geom_sf(aes(fill = uncum_cases)) +
  #theme_map() +
  theme(
    legend.title.align = 0.5,
    legend.text.align = 0.5,
    legend.justification = c(0, 0),
    legend.position = c(0.05, 0.1)
  )  +
  scale_fill_viridis_c(
    trans = "log10",
    alpha = .4,
    na.value = "grey60",
    name = "Log10 of Cases",
    breaks = trans_breaks("log10", function(x) 10^x),
    guide = guide_colorbar(
      label.theme = element_text(size =10, angle = -25),
      direction = "horizontal",
      label.position = "bottom",
      title.position = "top",
      barwidth = grid::unit(3.0, "in"),
      barheight = grid::unit(0.2, "in"), 
      text = 1
    )) + 
  labs(x = NULL,
       y = NULL,
       title = "New York State Coronavirus Cases by County") +
# annotate("text", -Inf, Inf, label = "ANNOTATION", hjust = 5, vjust = 50)
annotate("text", label ="NOTE: New York City\n cases are aggregated",
         x = 482751.3, y = 4530943)+
 #annotate("text", x = 0.5, y = 0.25, label = "ANNOTATION")
  NULL

ny_geo %>% 
  #filter(date == as.Date("2021-12-19"))  %>%
  filter(fips %in% nycfips) %>%
  group_by(fips, date) %>% 
  summarise(fipspop =sum(POP2020)) %>% 
  ungroup()


ny_geo %>%
  #filter(date == as.Date("2021-12-19"))  %>%
  group_by(fips, date) %>% 
  mutate(fipspop = sum(POP2020)) %>% 
  ungroup() %>% 
  mutate(uncum_per = (uncum_cases / (fipspop/1000))) %>% 
  # select(fips, NAME, POP2020, fipspop, uncum_cases, uncum_per) %>% 
  # arrange(desc(uncum_cases))
  ggplot() +
  geom_sf(aes(fill = uncum_per)) +
  theme_map() +
  theme(
    legend.title.align = 0.5,
    legend.text.align = 0.5,
    legend.justification = c(0, 0),
    legend.position = c(0.05, 0.1)
  )  +
  scale_fill_viridis_c(
    #trans = "log10",
    alpha = .4,
    na.value = "grey60",
    name = "Cases per 1,000",
   # breaks = trans_breaks("log10", function(x) 10^x),
    guide = guide_colorbar(
      label.theme = element_text(size =10, angle = -25),
      direction = "horizontal",
      label.position = "bottom",
      title.position = "top",
      barwidth = grid::unit(3.0, "in"),
      barheight = grid::unit(0.2, "in"), 
      text = 1
    )) + 
  labs(x = NULL,
       y = NULL,
       title = "New York State Coronavirus Cases by County") +
  # annotate("text", -Inf, Inf, label = "ANNOTATION", hjust = 5, vjust = 50)
  annotate("text", label ="NOTE: New York City\n cases are aggregated",
           x = 482751.3, y = 4530943)+
  #annotate("text", x = 0.5, y = 0.25, label = "ANNOTATION")
  NULL
# make it move ----------------------------------
library(gganimate)

ny_geo
# start with just last two weeks

ny_geo %>%
  group_by(fips) %>% 
  mutate(fipspop = sum(POP2020)) %>% 
  ungroup() %>% 
  mutate(uncum_per = (uncum_cases / (fipspop/1000))) %>%
  filter(date > as.Date("2021-12-01")) %>% 
  ggplot() +
  geom_sf(aes(fill = uncum_per)) +
  theme_map() +
  theme(
    legend.title.align = 0.5,
    legend.text.align = 0.5,
    legend.justification = c(0, 0),
    legend.position = c(0.05, 0.1)
  )  +
  # scale_fill_viridis_c(
  #   alpha = .4,
  #   na.value = "grey60",
  #   name = "cases per 1,000",
  #   guide = guide_colorbar(
  #     label.theme = element_text(size =10, angle = -25),
  #     direction = "horizontal",
  #     label.position = "bottom",
  #     title.position = "top",
  #     barwidth = grid::unit(3.0, "in"),
  #     barheight = grid::unit(0.2, "in"),
  #     text = 1
  #   )) +
  labs(x = NULL,
       y = NULL,
       title = "New York State Coronavirus Cases by County",
       subtitle = "date: {frame_time}" ) +
 annotate("text", label ="NOTE: New York City\n cases are aggregated",
          x = 482751.3, y = 4530943)+
  transition_time(date)
