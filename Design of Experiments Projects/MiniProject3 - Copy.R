MM <- read_csv("MP3.csv")

summary(lm(MM$Total~MM$Type))

ggplot(data = MM, aes(x = Type, y = Total, fill = Type)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues")+
  labs(title = "M&M Count by Type") +
  theme(plot.title = element_text(hjust = 0.5))

aggregate(MM$Total, by = list(MM$Type), FUN = mean)

aggregate(MM$Total, by = list(MM$Type), FUN = sd)

aggregate(MM$Total, by = list(MM$Type), FUN = fivenum)

contr1 <- c(-1/3, -1/3, -1/3, 1)
contr2 <- c(1, -1/2, -1/2, 0)
contr3 <- c(0, 1, -1, 0)

cond.contrasts <- cbind(contr1, contr2, contr3)

MM$Type <- as.factor(MM$Type)
contrasts(MM$Type) <- cond.contrasts

summary(lm(MM$Total~MM$Type)) 


