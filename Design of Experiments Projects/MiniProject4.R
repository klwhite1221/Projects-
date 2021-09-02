library(tidyverse)
library(MASS)
library(RColorBrewer)
selfie <- read_csv("MP4.csv")
View(selfie)

ggplot(data = selfie, aes(x = Angle, group = Lighting, y = Difference, color = Lighting)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") + 
  labs(x = "Angle", y = "Difference in Rating", color = "Lighting", title = "Selfie Rating Difference by Lighting and Angle") +
  scale_color_manual(values = c("blue3", "darkviolet"),
                     breaks = c("inside", "outside"),
                     labels = c("Inside", "Outside"))

anova(lm(Difference~Lighting*Angle, data = selfie))


ggplot(data = selfie, aes(x = Angle, y = Difference)) + 
  geom_boxplot() + 
  labs(x = "Angle", y = "Difference in Rating", title = "Selfie Rating Difference by Lighting and Angle") + 
  facet_wrap( ~ Lighting, ncol = 2)
