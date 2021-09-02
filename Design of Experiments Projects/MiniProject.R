mp <- Mini_Project_1_Data_Sheet1
t.test(mp$Difference, alternative = "two.sided")
t.test(mp$`Dominant (inches)`, mp$`Non Dominant (inches)`, alternative = "greater", paired=T)

ggplot(data = mp, aes(Difference)) + 
  geom_histogram(binwidth = 10) +
  labs(title = "Histogram of Differences")

x11()
hist(mp$Difference, main="Histogram of Differences", xlab="Difference")
