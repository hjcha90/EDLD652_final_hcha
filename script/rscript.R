library(tidyverse)
library(rio)
library(here)
library(janitor)
library(usmap)
library(ggpubr)


covid <- import(here("data", "covid-data-01-30-2021.csv")) %>% 
  clean_names() %>% 
  as_tibble() #load covid data

soccap <- import(here("data", "social-capital-project-state-index.xlsx")) %>% 
  clean_names() #load social capital data for 1997

soccap_covid <- soccap %>% 
  left_join(covid, by = c("state" = "province_state"))

data_plota <- soccap_covid %>%
  select(state, confirmed, deaths, total_test_results)


plot_usmap(data = soccap_covid, regions = "states", values = "state_level_index", labels = TRUE) +
  scale_fill_continuous(low = "white", 
                        high = "steelblue1", 
  ) + 
  theme(panel.background = element_rect(color = "black", fill = "white")) + 
  theme(legend.position = "none") + 
  labs(title = "2000")


state_covid_test <- data_plota %>%
  mutate(state = fct_reorder(state, total_test_results))  %>% 
  ggplot(aes(state, total_test_results/1000)) +
  geom_col(fill = "deepskyblue3", alpha = 0.7) + 
  coord_flip() + 
  theme_minimal()

state_covid_case <- data_plota %>%
  mutate(state = fct_reorder(state, confirmed))  %>% 
  ggplot(aes(state, confirmed/1000)) +
  geom_col(fill = "darkorange", alpha = 0.7) + 
  coord_flip() + 
  theme_minimal()

state_covid_death <- data_plota %>%
  mutate(state = fct_reorder(state, deaths))  %>% 
  ggplot(aes(state, deaths/1000)) +
  geom_col(fill = "brown1", alpha = 0.7) + 
  coord_flip() + 
  theme_minimal()

covid_plots <- ggarrange(state_covid_test,
                         state_covid_case,
                         state_covid_death,
                         ncol = 3,
                         nrow = 1)
covid_plots

state_covid_altogether <- data_plota %>%
  mutate(state = fct_reorder(state, total_test_results))  %>% 
  pivot_longer(!state, names_to = "type", values_to = "count") %>%
  ggplot(aes(state, count/1000, fill = type)) +
  geom_col(position = "dodge2") + 
  scale_fill_brewer(palette = "Dark2") + 
  coord_flip() + 
  theme_minimal()
state_covid_altogether







#Combine 4 US Religious Social Capital Maps in 1 plot
covid_plots <- ggarrange(state_covid_test,
                          state_covid_case,
                          state_covid_death,
                          ncol = 3,
                          nrow = 1)

covid_plots


plot1_refined




  mutate(screen_name = factor(screen_name),
         screen_name = fct_reorder(screen_name, n))  %>% 
  slice_head(n = 25)
