library(sjPlot)
library(psych)
library(dplyr)
library(readr)
library(sjPlot)

pull_states<-function(){
  # don't ask. I have my reasons. 
  read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
}
education <- read_csv("https://raw.githubusercontent.com/McCartneyAC/teacher_pay/master/teacher_pay.csv")
states<-pull_states()


states_max <- states %>% 
  # already counts cumulative sum. 
  filter(date == max(date))


merged_data <- states_max %>% 
  left_join(education, by = "state")
analytic_set <-merged_data %>% 
  filter(!is.na(abbreviation)) %>% 
  mutate(deaths_million = (deaths / (population2018/1000000)))

psych::describe(analytic_set, fast = T)

m1 <- analytic_set %>% 
  lm(data = ., deaths_million ~ actual_pay)

m2 <- analytic_set %>% 
  lm(data = ., deaths_million ~ adjusted_pay )
m3 <- analytic_set %>% 
  lm(data = ., deaths_million ~ adjusted_pay + pct_trump)
m4 <- analytic_set %>% 
  lm(data = ., deaths_million ~ adjusted_pay + pct_trump + log_pop)


m5 <- analytic_set %>% 
  lm(data = ., deaths_million ~ adjusted_pay + pct_trump + log_pop + pct_white_2012)
m5b <- analytic_set %>% 
  lm(data = ., deaths_million ~ actual_pay + adjusted_pay + pct_trump + log_pop + pct_white_2012)
m1b<-analytic_set %>% 
  lm(data = ., deaths_million ~ actual_pay + adjusted_pay )

m6 <- analytic_set %>% 
  lm(data = ., deaths_million ~ adjusted_pay + pct_trump + 
                                log_pop + pct_white_2012 + 
                                percent_union2018 )
m7<- analytic_set %>% 
  lm(data = ., deaths_million ~ adjusted_pay + pct_trump + 
       log_pop + pct_white_2012 + 
       percent_union2018 + factor(div_name))
m7d<- analytic_set %>% 
  lm(data = ., deaths_million ~ adjusted_pay + pct_trump + 
       log_pop + pct_white_2012 + 
        factor(div_name))
m4c<- analytic_set %>% 
  lm(data = ., deaths_million ~ adjusted_pay + log_pop + percent_union2018)

tab_model(m1, m2, m3, m4, m5,m6, m7)
tab_model(m1, m4c, m7, m7d)

