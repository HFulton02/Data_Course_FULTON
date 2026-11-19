library(tidyverse)
library(ggplot2)


# 1. Read in the unicef data
unicef = read.csv('unicef-u5mr.csv')

# 2. Get it into tidy format
unicef_tidy = unicef %>%
  pivot_longer(cols = starts_with("U5MR."),
               names_to = "Year",
               names_prefix = "U5MR.",
               values_to = "U5MR") %>%
  mutate(Year = as.numeric(Year)) %>%
  drop_na(U5MR)

# 3. Plot each country's U5MR over time
plot1 = unicef_tidy %>%
  ggplot(aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line() +
  facet_wrap(~Continent) +
  theme_bw() +
  labs(x = "Year", y = "U5MR")

plot1

# 4. Save this plot as LASTNAME_Plot1.PNG
ggsave("FULTON_Plot_1.png", plot1, width = 10, height = 7)

# 5. Mean U5MR for all countries within a given continent, by year
continent_means = unicef_tidy %>%
  group_by(Continent, Year) %>%
  summarize(Mean_U5MR = mean(U5MR), .groups = "drop")

plot2 = continent_means %>%
  ggplot(aes(x = Year, y = Mean_U5MR, color = Continent)) +
  geom_line(linewidth = 1) +
  theme_bw() +
  labs(x = "Year", y = "Mean_U5MR")

plot2

# 6. Save that plot as LASTNAME_Plot_2.png
ggsave("FULTON_Plot_2.png", plot2, width = 8, height = 6)

# 7. Create three models of U5MR
mod1 = lm(U5MR ~ Year, data = unicef_tidy)
mod2 = lm(U5MR ~ Year + Continent, data = unicef_tidy)
mod3 = lm(U5MR ~ Year * Continent, data = unicef_tidy)

# 8. Compare the three models with respect to their performance
# mod1, mod2, mod3 are nested (each adds more terms to the last),
# so annova() can directly test whether each added term significantly 
# improve the model
anova(mod1, mod2, mod3)

# anova() shows a significant p-value for the step from mod 1 -> mod2
# (adding Continent) and again from mod2 -> mod 3 (adding the 
# Year:Continent interaction), meaning each added term significantly
# improves the fit. mod 3 is the best performing model, which makes sense,
# since mortality decline over time clearly happens at different rates
# on different continents (the interaction matters).

# 9. Plot the 3 models' predictions
pred_grid = expand_grid(
  Year = seq(min(unicef_tidy$Year), max(unicef_tidy$Year), by = 1),
  Continent = unique(unicef_tidy$Continent)
)

preds = bind_rows(
  pred_grid %>% mutate(pred = predict(mod1, newdata = pred_grid), Model = "mod1"),
  pred_grid %>% mutate(pred = predict(mod2, newdata = pred_grid), Model = "mod2"),
  pred_grid %>% mutate(pred = predict(mod3, newdata = pred_grid), Model = "mod3")
)

plot3 = preds %>%
  ggplot(aes(x = Year, y = pred, color = Continent)) +
  geom_line(linewidth = 1) +
  facet_wrap(~Model) +
  theme_bw() +
  labs(title = "Model predictions", x = "Year", y = "Predicted U5MR")

plot3

# 10. BONUS, predict U5MR for Ecuador in 2020, compare to real value of 13
ecuador_2020 = tibble(Year = 2020, Continent = "Americas", CountryName = "Ecuador")
ecuador_pred = predict(mod3, newdata = ecuador_2020)

ecuador_result = tibble(
  Model = "mod3",
  Prediction = ecuador_pred,
  Reality = 13,
  Difference = ecuador_pred - 13
)

ecuador_result

# mod3 predicts a negative U5MR for Ecuador in 2020 (impossible in reality, since the 
# rate can't go below 0), well off from the real value of 13. This happens because mod3
# is a straight line extrapolation: it assumes each continent's rate keeps dropping at 
# the same constant pace it did from 1950-2015, but mortality decline naturally flattens 
# out as it approaches 0, so projecting the early steep decline straight out to 2020 
# overshoots below zero. 


























  