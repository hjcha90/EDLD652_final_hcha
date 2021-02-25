library(tidyverse)
library(rio)
library(here)
library(janitor)
library(usmap)
library(ggpubr)
library(magrittr)


covid <- import(here("data", "covid-data-01-30-2021.csv")) %>% 
  clean_names() %>% 
  as_tibble() #load covid data

soccap <- import(here("data", "social-capital-project-state-index.xlsx")) %>% 
  clean_names() #load social capital data for 1997

soccap_covid <- soccap %>% 
  left_join(covid, by = c("state" = "province_state"))

data_plota <- soccap_covid %>%
  select(state, confirmed, deaths, total_test_results)



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




soccap_covid_clean <- soccap_covid %>% 
  select(state, fips_code, 
         state_level_index, 
         family_unity, family_interaction, social_support, community_health, institutional_health, collective_efficacy, philanthropic_health,
         total_test_results,
         confirmed,
         deaths
  )


#visualization 2
plot_usmap(data = soccap_covid, values = "state_level_index", labels = TRUE) +
  scale_fill_continuous(low = "white", 
                        high = "steelblue1") + 
  theme(panel.background = element_rect(color = "black", fill = "white")) + 
  theme(legend.position = "none") + 
  labs(title = "State-Level Overall Social Capital Index",
       subtitle = "Thicker color indicates higher levels of social capital index")

soccap_county <- import(here("data", "social-capital-project-county-index.xlsx")) %>% 
  clean_names() %>% 
  as_tibble() %>% 
  rename(fips = fips_code)

plot_usmap(data = soccap_county, include = c("CA", "OR", "WA"), 
           regions = "counties", values = "county_level_index", labels = FALSE) +
  scale_fill_continuous(low = "white", 
                        high = "steelblue1") + 
  theme(panel.background = element_rect(color = "black", fill = "white")) + 
  theme(legend.position = "none") + 
  labs(title = "County-Level Overall Social Capital Index of Western US States",
       subtitle = "Thicker color indicates higher levels of social capital index")

plot_usmap(data = soccap_covid, regions = "states", values = "family_unity", labels = TRUE) +
  scale_fill_continuous(low = "red", 
                        high = "blue") + 
  theme(panel.background = element_rect(color = "black", fill = "white")) + 
  theme(legend.position = "none") + 
  labs(title = "State-Level Family Unity Subindex",
       subtitle = "Thicker color indicates higher levels of social capital index")


plot_usmap(data = soccap_covid, regions = "states", values = "state_level_index", labels = TRUE) +
  scale_fill_continuous(low = "white", 
                        high = "steelblue1", 
  ) + 
  theme(panel.background = element_rect(color = "black", fill = "white")) + 
  theme(legend.position = "none") + 
  labs(title = "2000")


soccap_covid_clean %<>%
  mutate(positivity = confirmed / total_test_results, fatality = deaths / confirmed) 

model1a <- lm(positivity ~ state_level_index, data = soccap_covid_clean)
model1b <- lm(positivity ~ family_unity + family_interaction + social_support + community_health + institutional_health + collective_efficacy + philanthropic_health, data = soccap_covid_clean)
summary(model1a)
summary(model1b)

soccap_covid_clean %>% 
  ggplot(aes(state_level_index, positivity)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "black", se = FALSE) + 
  labs(title = "Relationship between Covid-19 Positivity and State-Level Social Capital Index",
       x = "Social Capital Index",
       y = "Posivitity") + 
  theme(panel.background = element_rect(color = "black", fill = "white")) 


model2a <- lm(fatality ~ state_level_index, data = soccap_covid_clean)
model2b <- lm(fatality ~ family_unity + family_interaction + social_support + community_health + institutional_health + collective_efficacy + philanthropic_health, data = soccap_covid_clean)

soccap_covid_clean %>% 
  ggplot(aes(state_level_index, fatality)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "black", se = FALSE) + 
  labs(title = "Relationship between Covid-19 Fatality Rate and State-Level Social Capital Index",
       x = "Social Capital Index",
       y = "Fatality Rate") + 
  theme(panel.background = element_rect(color = "black", fill = "white")) 




summary(model2a)
summary(model2b)


