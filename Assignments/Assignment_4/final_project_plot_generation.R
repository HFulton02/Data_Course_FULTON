# Simulate fake data: soda intake vs. cavities
set.seed(42)
n = 100
fake_data = data.frame(
  person_id = 1:n,
  soda_servings_per_day = round(runif(n, min = 0, max = 5), 1),
  cavities = NA
)

fake_data$cavities = round(fake_data$soda_servings_per_day * 1.2 + rnorm(n, mean = 0, sd = 1.5))
fake_data$cavities[fake_data$cavities < 0] = 0
write.csv(fake_data, "Assignments/Assignment_4/fake_dental_data.csv")

library(ggplot2)

ggplot(fake_data, aes(x = soda_servings_per_day,
                      y = cavities)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Soda Consumption vs. Cavities (Simulated Data)',
       x = "Soda servings per day",
       y = "Number of cavities")

  