library(tidyverse)

# Task I: Read in the data
covid = read.csv("cleaned_covid_data.csv")

# Task II: Subset to states starting with "A"
A_states = covid %>%
  filter(startsWith(Province_State, "A"))

# Task III: Plot Deaths over time, faceted by state
A_states$Last_Update = as.Date(A_states$Last_Update)

ggplot(A_states, aes(x = Last_Update,
                     y = Deaths)) +
  geom_line() +
  facet_wrap(~ Province_State) +
  labs(title = "Deaths Over Time (States Starting with A)",
       x = "Date", y = "Deaths")

#Task IV: Find peak Case_Fatality_Ratio per state
state_max_fatality_rate = covid %>%
  group_by(Province_State) %>%
  summarize(max_CFR = max(Case_Fatality_Ratio, na.rm = TRUE))

#Task V: Plot from Task IV's data frame
ggplot(state_max_fatality_rate, aes(x = reorder(Province_State, max_CFR),
                              y = max_CFR)) +
  geom_col() +
  labs(title = "Peak Case Fatality Ratio by State",
       x = "State",
       y = "Max Case Fatality Ratio")

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