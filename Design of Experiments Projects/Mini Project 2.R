library(tidyverse)
dat <- read.csv("MP2D.csv", header = TRUE)
mean(dat$Total, na.rm = T)
sd(dat$Total, na.rm = T)

aggregate(dat$Total, by = list(dat$Hear.See.Both), FUN = mean)

aggregate(dat$Total, by = list(dat$Hear.See.Both), FUN = sd)

aggregate(dat$Total, by = list(dat$Hear.See.Both), FUN = fivenum)

anova(lm(Total~Hear.See.Both, data = dat))

ggplot(dat, aes(x = Hear.See.Both, y = Total, fill = Hear.See.Both)) +
  geom_boxplot()+
  geom_point(color="black")+
  scale_fill_brewer(palette="PuRd")+
  labs(title = "Figure 1: Boxplots of List Recall", x = "List Presentation", y = "Total Words Recalled")+
  theme(legend.position="none")
