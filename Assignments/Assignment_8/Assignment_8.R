library(tidyverse)

mushroom = read.csv("mushroom_growth.csv")
str(mushroom)

ggplot(mushroom, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(mushroom, aes(x = Light, y = GrowthRate, group = Light)) +
  geom_boxplot() +
  theme_bw()

ggplot(mushroom, aes(x = Humidity, y = GrowthRate)) +
  geom_boxplot() +
  theme_bw()

ggplot(mushroom, aes(x = Species, y = GrowthRate)) +
  geom_boxplot() +
  theme_bw()

ggplot(mushroom, aes(x = Temperature, y = GrowthRate, group = Temperature)) +
  geom_boxplot() +
  theme_bw()

mod1 = lm(GrowthRate ~ Nitrogen, data = mushroom)
mod2 = lm(GrowthRate ~ Nitrogen + Light, data = mushroom)
mod3 = lm(GrowthRate ~ Nitrogen * Light, data = mushroom)
mod4 = lm(GrowthRate ~ Nitrogen * Light + Humidity + Species + Temperature, data = mushroom)


mse = function(model, data) {
  mean((data$GrowthRate - predict(model, data))^2)
}

model_comparison = tibble(
  Model = c("mod1 (Nitrogen)", "mod2 (Nitrogen + Light)",
            "mod3 (Nitrogen * Light)",
            "mod4 (Nitrogen * Light + Humidity + Species + Temperature)"),
  MSE = c(mse(mod1, mushroom), mse(mod2, mushroom),
          mse(mod3, mushroom), mse(mod4, mushroom))
) %>%
  arrange(MSE)

model_comparison

best_model = mod4

new_data = tibble(
  Nitrogen = c(0, 20, 45, 60, 80, 100),
  Light = c(0, 10, 20, 25, 30, 40),
  Humidity = "Low",
  Species = "P.ostreotus",
  Temperature = 20
)

new_data$GrowthRate = predict(best_model, newdata = new_data)

new_data


ggplot(mushroom, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point(color = "black") +
  geom_point(data = new_data, aes(x = Nitrogen, y = GrowthRate), color = "red", size = 3) +
  labs(title = "Real Data (black) vs. Predicted Hypothetical Values (red)") +
  theme_bw()

# 1. Are any of your predicted response values from your best model
#    scientifically meaningless? Explain.
#    Yes. The first hypothetical row (Nitrogen = 0, Light = 0) predicts
#    a GrowthRate of about -11.4. A growth rate can't be negative in
#    reality (mushrooms can't shrink below not growing at all), so this
#    prediction is scientifically meaningless -- it's an artifact of
#    extrapolating a linear model to low/boundary input values.

# 2. Do your plots show any non-linear relationships? Link a resource
#    on handling non-linear relationships in R.
#    Yes -- the Nitrogen vs. GrowthRate scatterplot curves rather than
#    following a straight line (growth rate rises quickly then levels
#    off), suggesting a non-linear relationship that a simple straight
#    lm() fit doesn't fully capture.
#    Resource: https://www.statmethods.net/advstats/nonlinear.html

# 3. Write the code to model the data in non_linear_relationship.csv
#    with a linear model.
nonlin = read.csv("non_linear_relationship.csv")

ggplot(nonlin, aes(x = predictor, y = response)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

# a straight lm() already gets a decent R-squared (~0.90), but there's
# visible curvature left in the residuals. Since lm() can still fit a
# curve as long as the curve is linear in its COEFFICIENTS, adding a
# squared term keeps it a "linear model" while fitting the curve:
nonlin_mod = lm(response ~ poly(predictor, 2), data = nonlin)
summary(nonlin_mod)$r.squared
# -> R-squared improves to about 0.98 with the quadratic term added,
#    confirming the relationship is better described as non-linear.


