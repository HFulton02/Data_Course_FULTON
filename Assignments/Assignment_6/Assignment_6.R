library(tidyverse)
library(gganimate)

## 1. Load the untidy data
dat = read.csv("Data/BioLog_Plate_Data.csv")

## 2. Tidy into long form + add Soil/Water column

dat_tidy = dat %>%
  pivot_longer(cols = c(Hr_24, Hr_48, Hr_144),
               names_to = "Time",
               values_to = "Absorbance") %>%
  separate(Time, into = c("hr", "Time"), sep = "_", convert = TRUE) %>%
  select(-hr) %>%
  mutate(Type = ifelse(Sample.ID == "Soil_1" | Sample.ID == "Soil_2", "Soil", "Water"))
dat_tidy
View(dat_tidy)

## 3. Static plot: all substrates, dilution == 0.1, colored by Type
plot1 = dat_tidy %>%
  filter(Dilution == 0.1) %>%
  ggplot(aes(x = Time, y = Absorbance, color = Type)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~Substrate) +
  ggtitle("Just dilution 0.1") +
  theme_bw()
plot1
ggsave("biolog_substrate_plot.png", plot1, width = 14, height = 10, dpi = 150)

## 4. Animated plot: Itaconic Acid, mean of reps, by Sample.ID, faceted by Dilution
itaconic_summary = dat_tidy %>%
  filter(Substrate == "Itaconic Acid") %>%
  group_by(Sample.ID, Dilution, Time) %>%
  summarize(Mean_absorbance = mean(Absorbance), .groups = "drop")

anim = itaconic_summary %>%
  ggplot(aes(x = Time, y = Mean_absorbance, color = Sample.ID)) +
  geom_line() +
  facet_wrap(~Dilution, nrow = 1) +
  theme_bw() +
  transition_reveal(Time)

animate(anim, width = 480, height = 480)
anim_save("itaconic_acid_animation.gif")
  
