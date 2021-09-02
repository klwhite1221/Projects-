Music <- read_csv("Music.csv")

fac <- read_csv("Factorial.csv")

anova(lm(Score~Group, data = Music))
summary(lm(Score~Group, data = Music))

anova(lm(Score~Group+Year, data = m))

Music$Group2 <- Music$Group
for(i in 1:length(Music$Group2)){
  if(Music$Group2[i] == "PL"){
    Music$Group2[i] = "Pop/Low"
  }else if(Music$Group2[i] == "PH"){
    Music$Group2[i] = "Pop/High"
  }else if(Music$Group2[i] == "CL"){
    Music$Group2[i] = "Classical/Low"
  }else if(Music$Group2[i] == "CH"){
    Music$Group2[i] = "Classical/High"
  }else{
    Music$Group2[i] = "None"
  }
}


ggplot(data = Music, aes(x = Group2, y = Score, fill = Group)) + 
  geom_boxplot() + 
  scale_fill_brewer(palette="PuRd")+
  labs(x = "Treatment Group", y = "Assessment Score", title = "Assessment Score by Treatment Group")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_text(angle = 45))


anova(lm(Score~Genre*Volume, data = fac))
summary(lm(Score~Genre*Volume, data = fac))

TukeyHSD(aov(lm(Score~Genre*Volume, data = fac)))

ggplot(data = fac, aes(x = Genre, group = Volume, y = Score, color = Volume)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") + 
  labs(x = "Genre", y = "Assessment Score", color = "Volume", title = "Assessment Score by Genre and Volume") +
  scale_color_manual(values = c("blue3", "darkviolet"),
                     breaks = c("High", "Low"),
                     labels = c("High", "Low"))


ggplot(data = fac, aes(x = Genre, y = Score, fill = Genre)) + 
  geom_boxplot() + 
  labs(x = "Music Genre", y = "Assessment Score", title = "Assessment Score by Music Genre")+
  theme(legend.position = "none")
  


