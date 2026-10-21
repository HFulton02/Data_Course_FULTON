library(tidyverse)

# Task I: Read in the data
covid = read.csv("cleaned_covid_data.csv")

# Task II: Subset to states starting with "A"
A_states = covid %>%
  filter(grepl("^A", Province_State))

# Task III: Plot Deaths over time, faceted by state
A_states$Last_Update = as.Date(A_states$Last_Update)

ggplot(A_states, aes(x = Last_Update,
                     y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free") +
  labs(title = "Deaths Over Time (States Starting with A)",
       x = "Date", y = "Deaths")

#Task IV: Find peak Case_Fatality_Ratio per state
state_max_fatality_rate = covid %>%
  group_by(Province_State) %>%
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Maximum_Fatality_Ratio))

#Task V: Plot from Task IV's data frame
state_max_fatality_rate$Province_State = factor(state_max_fatality_rate$Province_State,
                                                levels = state_max_fatality_rate$Province_State)
ggplot(state_max_fatality_rate, aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Peak Case Fatality Ratio by State",
       x = "State", y = "Max Case Fatality Ratio")

#Bonus: Cumulative US deaths over time (full dataset)
US_deaths = covid %>%
  group_by(Last_Update) %>%
  summarize(total_deaths = sum(Deaths, na.rm = TRUE)) %>%
  mutate(Last_Update = as.Date(Last_Update))

ggplot(US_deaths, aes(x = Last_Update,
                      y = total_deaths)) +
  geom_line() +
  labs(title = "Cumulative US Deaths Over Time",
       x = "Date",
       y = "Total Deaths")