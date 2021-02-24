library(tidyverse)
library(rio)
library(here)
library(janitor)


covid <- import(here("data", "covid-data-01-30-2021.csv")) %>% 
  clean_names() %>% 
  as_tibble() #load covid data

soccap <- import(here("data", "social-capital-project-state-index.xlsx")) %>% 
  clean_names() #load social capital data for 1997


#Plot part 1. Covid cases, deaths, and testing by state
data_plot1 <- covid %>%
  filter(fips <= 56) %>% #choose only US states
  select(province_state, confirmed, deaths, total_test_results) %>% 
  pivot_longer(!province_state, names_to = "type", values_to = "count") 

data_plot1 %>%
  mutate(province_state = fct_reorder(province_state, count))  %>% 
  ggplot(aes(province_state, count/1000)) +
  geom_col(fill = "steelblue1", alpha = 0.7) + 
  facet_wrap(~type) +
  coord_flip() + 
  theme_minimal()

#I tried faceting cases, deaths and testing but the number for testing is too big compared to two other metricss

data_plot2 <- covid %>%
  filter(fips <= 56) %>% #choose only US states
  select(province_state, confirmed, deaths, total_test_results)

data_plot2 %>%
  mutate(province_state = fct_reorder(province_state, total_test_results))  %>% 
  ggplot(aes(province_state, total_test_results / 1000)) +
  geom_col(fill = "steelblue1", alpha = 0.7) + 
  coord_flip() + 
  theme_minimal()

data_plot2 %>%
  mutate(province_state = fct_reorder(province_state, confirmed))  %>% 
  ggplot(aes(province_state, confirmed / 1000)) +
  geom_col(fill = "steelblue1", alpha = 0.7) + 
  coord_flip() + 
  theme_minimal()

data_plot2 %>%
  mutate(province_state = fct_reorder(province_state, deaths))  %>% 
  ggplot(aes(province_state, deaths / 1000)) +
  geom_col(fill = "steelblue1", alpha = 0.7) + 
  coord_flip() + 
  theme_minimal()


plot1_refined




  mutate(screen_name = factor(screen_name),
         screen_name = fct_reorder(screen_name, n))  %>% 
  slice_head(n = 25)
