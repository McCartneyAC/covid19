install.packages("rayshader")
library(rayshader)


gg = ggplot(diamonds, aes(x, depth)) +
  stat_density_2d(aes(fill = stat(nlevel)), 
                  geom = "polygon",
                  n = 100,bins = 10,contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A")
gg

plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250)

states %>% 
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
       y = "")   %>% 
  plot_gg(multicore=TRUE,width=10,height=10,scale=250)
