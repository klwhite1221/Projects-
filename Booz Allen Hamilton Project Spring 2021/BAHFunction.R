library(tidyverse)

create_HRV <- function(num_people = 50, num_week = 10, story1 = 1, story2 = 1, story3 = 1, story4 = 1, story5 = 1, story6 = 1, story7 = 1, story8 = 1, hrv_range = 14:24, hrv_sdd_1_4 = 5:10, hrv_sdd_2_4 = 5:10, 
                       hrv_sdd_3_4 = 5:10, hrv_sdd_4_4 = 5:10, hrv_sdd_5_4 = 5:10, hrv_sdd_6_4 = 6:10, hrv_sdd_7_4 = 7:10, slp_range = 1:3){
  set.seed(10) 
  n <- num_people*num_week
  hrv_id <- 1:n
  hrv_week <- rep(1:num_week, num_people)
  
  #Create random variables
  hrv_hrv <- sample(hrv_range, n, replace=TRUE)
  hrv_sdd_1_4 <- sample(hrv_sdd_1_4, n, replace=TRUE)
  hrv_sdd_2_4 <- sample(hrv_sdd_2_4, n, replace=TRUE)
  hrv_sdd_3_4 <- sample(hrv_sdd_3_4, n, replace=TRUE)
  hrv_sdd_4_4 <- sample(hrv_sdd_4_4, n, replace=TRUE)
  hrv_sdd_5_4 <- sample(hrv_sdd_5_4, n, replace=TRUE)
  hrv_sdd_6_4 <- sample(hrv_sdd_6_4, n, replace=TRUE)
  hrv_sdd_7_4 <- sample(hrv_sdd_7_4, n, replace=TRUE)
  hrv_slp <- sample(slp_range, n, replace=TRUE)
  
  person_id <- c()
  person_id <- rep(1, num_week)
  for(i in 2:num_people){
    person_id <- append(person_id, rep(i, num_week), after = length(person_id))
  }
  Notes <- rep(NA, n)
  
  #Create data frame
  HRV <- data.frame(hrv_id, hrv_week, hrv_hrv, hrv_sdd_1_4, hrv_sdd_2_4, hrv_sdd_3_4, hrv_sdd_4_4, hrv_sdd_5_4, hrv_sdd_6_4, hrv_sdd_7_4, hrv_slp, person_id, Notes)
  
  p <- sample(1:num_people, (story1+story2+story3+story4+story5+story6+story7+story8))
  
  #A mix of consistent sleep hours/quality and inconsistent sleep hours/quality
  if(story1 > 0){
  s1 <- p[1:story1]
  for(i in 1:length(s1)){
  w <- s1[i]*num_week
  w1 <- (w-(num_week-1))
  w2 <- (w1+round((1/4)*num_week))
  w3 <- (w2+round((1/4)*num_week))
  w4 <- (w3+round((1/4)*num_week))
  HRV[w1:w2, 4:10] = sample(7:9, 7*((w2-w1)+1), replace = TRUE) 
  HRV[w1:w2, 11] = 3
  HRV[(w2+1):w3, 4:10] = sample(4:12, 7*(w3-w2), replace = TRUE)
  HRV[(w2+1):w3, 11] = 1
  HRV[(w3+1):w4, 4:10] = sample(8:10, 7*(w4-w3), replace = TRUE)
  HRV[(w3+1):w4, 11] = 3
  HRV[(w4+1):w, 4:10] = sample(3:12, 7*(w-w4), replace = TRUE)
  HRV[(w4+1):w, 11] = 1
  HRV$Notes[w1:w] = "A mix of consistent sleep hours/quality and inconsistent sleep hours/quality"
  }
  }
  
  #A mix of consistent sleep hours/quality and inconsistent sleep hours/quality
  if(story2 > 0){
  s2 <- p[(story1+1):(story1+story2)]
  for(i in 1:length(s2)){
    w <- s2[i]*num_week
    w1 <- (w-(num_week-1))
    w2 <- (w1+round((1/4)*num_week))
    w3 <- (w2+round((1/4)*num_week))
    w4 <- (w3+round((1/4)*num_week))
  HRV[w1:w2, 4:10] = sample(1:12, 7*((w2-w1)+1), replace = TRUE)
  HRV[w1:w2, 11] = 1
  HRV[(w2+1):w3, 4:10] = sample(5:6, 7*(w3-w2), replace = TRUE)
  HRV[(w2+1):w3, 11] = 1
  HRV[(w3+1):w4, 4:10] = sample(3:10, 7*(w4-w3), replace = TRUE)
  HRV[(w3+1):w4, 11] = 1
  HRV[(w4+1):w, 4:10] = sample(5:7, 7*(w-w4), replace = TRUE)
  HRV[(w4+1):w, 11] = 2
  HRV$Notes[w1:w] = "A mix of consistent sleep hours/quality and inconsistent sleep hours/quality"
  }
  }
  
  #Consistent and now inconsistent sleep hours/quality
  if(story3 > 0){
  s3 <- p[(story1+story2+1):(story1+story2+story3)]
  for(i in 1:length(s3)){
    w <- s3[i]*num_week
    w1 <- (w-(num_week-1))
    w2 <- (w1+round((1/2)*num_week))
  HRV[w1:w2, 4:10] = sample(7:9, 7*((w2-w1)+1), replace = TRUE)
  HRV[w1:w2, 11] = 3
  HRV[(w2+1):w, 4:10] = sample(4:11, 7*(w-w2), replace = TRUE)
  HRV[(w2+1):w, 11] = sample(1:2, (w-w2), replace = TRUE)
  HRV$Notes[w1:w] = "Consistent and now inconsistent sleep hours/quality"
  }
  }
  
  #Inconsistent and now consistent sleep hours/quality
  if(story4 > 0){
  s4 <- p[(story1+story2+story3+1):(story1+story2+story3+story4)]
  for(i in 1:length(s4)){
    w <- s4[i]*num_week
    w1 <- (w-(num_week-1))
    w2 <- (w1+round((1/2)*num_week))
  HRV[w1:w2, 4:10] = sample(3:12, 7*((w2-w1)+1), replace = TRUE)
  HRV[w1:w2, 11] = sample(1:3, ((w2-w1)+1), replace = TRUE)
  HRV[(w2+1):w, 4:10] = sample(8:9, 7*(w-w2), replace = TRUE)
  HRV[(w2+1):w, 11] = 2
  HRV$Notes[w1:w] = "Inconsistent and now consistent sleep hours/quality"
  }
  }
  
  #Sleeps well during the week but not on weekends
  if(story5 > 0){
  s5 <- p[(story1+story2+story3+story4+1):(story1+story2+story3+story4+story5)]
  for(i in 1:length(s5)){
    w <- s5[i]*num_week
    w1 <- (w-(num_week-1))
  HRV[w1:w, 4:7] = sample(7:10, 4*num_week, replace = TRUE)
  HRV[w1:w, 8:10] = sample(c(1, 2, 3, 10, 11, 12, 13), 3*num_week, replace = TRUE)
  HRV$Notes[w1:w] = "Sleeps well during the week but not on weekends"
  }
  }
  
  #Sleeps well on weekends but not during the week
  if(story6 > 0){
  s6 <- p[(story1+story2+story3+story4+story5+1):(story1+story2+story3+story4+story5+story6)]
  for(i in 1:length(s6)){
    w <- s6[i]*num_week
    w1 <- (w-(num_week-1))
  HRV[w1:w, 4:7] = sample(1:5, 4*num_week, replace =TRUE)
  HRV[w1:w, 8:10] = sample(7:9, 3*num_week, replace = TRUE)
  HRV$Notes[w1:w] = "Sleeps well on weekends but not during the week"
  }
  }
  
  #Consistent HRV values
  if(story7 > 0){
  s7 <- p[(story1+story2+story3+story4+story5+story6+1):(story1+story2+story3+story4+story5+story6+story7)]
  for(i in 1:length(s7)){
    w <- s7[i]*num_week
    w1 <- (w-(num_week-1))
  HRV[w1:w, 3] = sample(17:19, num_week, replace = TRUE)
  HRV$Notes[w1:w] = "Consistent HRV values"
  }
  }
  
  #Inconsistent HRV values
  if(story8 > 0){
  s8 <- p[(story1+story2+story3+story4+story5+story6+story7+1):(story1+story2+story3+story4+story5+story6+story7+story8)]
  for(i in 1:length(s8)){
    w <- s8[i]*num_week
    w1 <- (w-(num_week-1))
  HRV[w1:w, 3] = sample(0:24, num_week, replace = TRUE)
  HRV$Notes[w1:w] = "Inconsistent HRV values"
  }
  }
  
  return(HRV)
}


library(lubridate)

create_Sleep <- function(num_people = 50, story1 = 1, story2 = 1, story3 = 1, story4 = 1, story5 = 1, story6 = 1, story7 = 1, story8 = 1, story9 = 1, story10 = 1, story11 = 1, start_date = "2019-06-01", end_date = "2019-09-01", breath_range = seq(12, 20, 0.01), restless_range = 0:100, 
                         score_alignment_range = 0:100, score_deep_range = 0:100, score_disturbances_range = 0:100, 
                         score_efficiency_range = 0:100, score_latency_range = 0:100, score_rem_range = 0:100, score_total_range = 0:100, 
                         hours_bedtime_range = 19:23, hours_wake_range = 6:11, tz_range = (-11:11), deepPer_range = seq(0.13, 0.2, 0.01), 
                         lightPer_range = seq(0.45, 0.5, 0.01), REMPer_range = seq(0.2, 0.2, 0.01), hr_range = 75:85, onset_latency_range = 120:3600, temp_delta_range = seq(-1, 1, 0.01)){
set.seed(10)
d <- seq.Date(as.Date(start_date), as.Date(end_date), 1)
d <- length(d)
person_id <- c()
person_id <- rep(1, d)
for(i in 2:num_people){
  person_id <- append(person_id, rep(i, d), after = length(person_id))
}
n <- num_people*d

Notes <- rep(NA, n)

#Create random variables

breath_average <- sample(breath_range, n, replace = TRUE)
restless <- sample(restless_range, n, replace = TRUE)

score <- rep(0, n)
score_alignment <- sample(score_alignment_range, n, replace = TRUE)
score_deep <- sample(score_deep_range, n, replace = TRUE)
score_disturbances <- sample(score_disturbances_range, n, replace = TRUE)
score_efficiency <- sample(score_efficiency_range, n, replace = TRUE)
score_latency <- sample(score_latency_range, n, replace =TRUE)
score_rem <- sample(score_rem_range, n, replace = TRUE)
score_total <- sample(score_total_range, n, replace = TRUE)
#Calculate score
for(i in 1:n){
  score[i] = (0.10*score_alignment[i]+0.10*score_deep[i]+0.10*score_efficiency[i]+0.05*score_latency[i]+0.10*score_rem[i]+0.35*score_total[i]+0.15*score_disturbances[i])/(0.10+0.10+0.10+0.05+0.10+0.35+0.15)
  score[i] <- as.integer(score[i])
}

#consistent scoring
if(story1 > 0){
s1 <- sample(1:num_people, story1)
for(j in 1:length(s1)){
  w <- s1[j]*d
  w1 <- (w-(d-1))
score_alignment[w1:w] <- sample(60:65, d, replace = TRUE)
score_deep[w1:w] <- sample(61:66, d, replace = TRUE)
score_efficiency[w1:w] <- sample(55:60, d, replace = TRUE)
score_latency[w1:w] <- sample(70:75, d, replace =TRUE)
score_rem[w1:w] <- sample(63:68, d, replace = TRUE)
score_total[w1:w] <- sample(56:61, d, replace = TRUE)
for(i in w1:w){
  score[i] = (0.10*score_alignment[i]+0.10*score_deep[i]+0.10*score_efficiency[i]+0.05*score_latency[i]+0.10*score_rem[i]+0.35*score_total[i])/(0.10+0.10+0.10+0.05+0.10+0.35)
  score[i] <- as.integer(score[i])
}
Notes[w1:w] <- "Consistent scoring"
}
}

#Create dates, datetimes, and duration
summary_date <- rep(seq.Date(as.Date(start_date), as.Date(end_date), 1), num_people)
hour_s <- sample(hours_bedtime_range, n, replace = TRUE)
minutes <- sample(0:59, n, replace = TRUE)
seconds <- sample(0:59, n, replace = TRUE)
bedtime_start <- ymd(summary_date)
bedtime_start <- make_datetime(year(bedtime_start), month(bedtime_start), day(bedtime_start), hour_s, minutes, seconds)
bedtime_end <- bedtime_start
for(i in 1:n){
  if(0<=hour(bedtime_start[i]) && hour(bedtime_start[i])<= 4){
    bedtime_end[i] = bedtime_start[i]
  }else{
    bedtime_end[i] = bedtime_start[i]+ddays(1)
  }
}
hour_e <- sample(hours_wake_range, n, replace = TRUE)
minutes <- sample(0:59, n, replace = TRUE)
seconds <- sample(0:59, n, replace = TRUE)
bedtime_end <- make_datetime(year(bedtime_end), month(bedtime_end), day(bedtime_end), hour_e, minutes, seconds)
duration <- as.duration(bedtime_end-bedtime_start)
summary_date <- bedtime_end-ddays(1)
summary_date <- make_date(year(summary_date), month(summary_date), day(summary_date))
tz <- sample(60*tz_range, num_people, replace=TRUE) 
timezone <- rep(tz[1], d)
for(i in 2:num_people){
  timezone <- append(timezone, rep(tz[i], d), after = length(timezone))
}

p <- sample(1:num_people, (story2+story3+story4+story5+story6+story7+story8+story9))

#consistent bedtime
if(story2 > 0){
s2 <- p[1:story2]
for(j in 1:length(s2)){
  w <- s2[j]*d
  w1 <- (w-(d-1))
hours <- sample(20:21, d, replace = TRUE)
hour(bedtime_start[w1:w]) <- hours
duration[w1:w] <- as.duration(bedtime_end[w1:w]-bedtime_start[w1:w])
Notes[w1:w] <- "Consistent bedtime"
}
}

#consistent duration
if(story3 > 0){
s3 <- p[(story2+1):(story2+story3)]
for(j in 1:length(s3)){
  w <- s3[j]*d
  w1 <- (w-(d-1))
hours <- sample(7:8, d, replace = TRUE)
bedtime_end[w1:w] <- bedtime_start[w1:w] + dhours(hours)
duration[w1:w] <- as.duration(bedtime_end[w1:w]-bedtime_start[w1:w])
Notes[w1:w] <- "Consistent duration"
}
}

#consistent bedtime and duration
if(story4 > 0){
s4 <- p[(story2+story3+1):(story2+story3+story4)]
for(j in 1:length(s4)){
  w <- s4[j]*d
  w1 <- (w-(d-1))
hours <- sample(21:22, d, replace = TRUE)
hour(bedtime_start[w1:w]) <- hours
hours2 <- sample(6:7, d, replace = TRUE)
bedtime_end[w1:w] <- bedtime_start[w1:w] + dhours(hours2)
duration[w1:w] <- as.duration(bedtime_end[w1:w]-bedtime_start[w1:w])
Notes[w1:w] <- "Consistent bedtime and duration"
}
}

#Inconsistent bedtime
if(story5 > 0){
s5 <- p[(story2+story3+story4+1):(story2+story3+story4+story5)]
for(j in 1:length(s5)){
  w <- s5[j]*d
  w1 <- (w-(d-1))
hours <- sample(c(19:23, 0:4), d, replace = TRUE)
hour(bedtime_start[w1:w]) <- hours
for(i in 1:d){
  if(hours[i] < 5){
    bedtime_start[i+(w1-1)] <- bedtime_start[i+(w1-1)] + ddays(1)
  }
}
duration[w1:w] <- as.duration(bedtime_end[w1:w]-bedtime_start[w1:w])
Notes[w1:w] <- "Inconsistent bedtime"
}
}

#Inconsistent duration
if(story6 > 0){
s6 <- p[(story2+story3+story4+story5+1):(story2+story3+story4+story5+story6)]
for(j in 1:length(s6)){
  w <- s6[j]*d
  w1 <- (w-(d-1))
hours <- sample(1:12, d, replace = TRUE)
bedtime_end[w1:w] <- bedtime_start[w1:w] + dhours(hours)
duration[w1:w] <- as.duration(bedtime_end[w1:w]-bedtime_start[w1:w])
Notes[w1:w] <- "Inconsistent duration"
}
}

#inconsistent bedtime and inconsistent duration
if(story7 > 0){
s7 <- p[(story2+story3+story4+story5+story6+1):(story2+story3+story4+story5+story6+story7)]
for(j in 1:length(s7)){
  w <- s7[j]*d
  w1 <- (w-(d-1))
hours <- sample(c(19:12, 0:5), d, replace = TRUE)
hour(bedtime_start[w1:w]) <- hours
for(i in 1:d){
  if(hours[i] < 5){
    bedtime_start[i+(w1-1)] <- bedtime_start[i+(w1-1)] + ddays(1)
  }
}
hours2 <- sample(2:13, d, replace = TRUE)
bedtime_end[w1:w] <- bedtime_start[w1:w] + dhours(hours2)
duration[w1:w] <- as.duration(bedtime_end[w1:w]-bedtime_start[w1:w])
Notes[w1:w] <- "Inconsistent bedtime and inconsistent duration"
}
}

#consistent to sporadic duration
if(story8 > 0){
s8 <- p[(story2+story3+story4+story5+story6+story7+1):(story2+story3+story4+story5+story6+story7+story8)]
for(j in 1:length(s8)){
  w <- s8[j]*d
  w1 <- (w-(d-1))
  w2 <- (w1+round((1/2)*d))
hours <- sample(5:7, ((w2-w1)+1), replace = TRUE)
bedtime_end[w1:w2] <- bedtime_start[w1:w2] + dhours(hours)
duration[w1:w2] <- as.duration(bedtime_end[w1:w2]-bedtime_start[w1:w2])
hours <- sample(4:13, (w-w2), replace = TRUE)
bedtime_end[(w2+1):w] <- bedtime_start[(w2+1):w] + dhours(hours)
duration[(w2+1):w] <- as.duration(bedtime_end[(w2+1):w]-bedtime_start[(w2+1):w])
Notes[w1:w] <- "Consistent to sporadic duration"
}
}

#sporadic to consistent wake ups
if(story9 > 0){
s9 <- p[(story2+story3+story4+story5+story6+story7+story8+1):(story2+story3+story4+story5+story6+story7+story8+story9)]
for(j in 1:length(s9)){
  w <- s9[j]*d
  w1 <- (w-(d-1))
  w2 <- (w1+round((1/2)*d))
hours <- sample(4:13, ((w2-w1)+1), replace = TRUE)
hour(bedtime_end[w1:w2]) <- hours
duration[w1:w2] <- as.duration(bedtime_end[w1:w2]-bedtime_start[w1:w2])
hours <- sample(6:8, (w-w2), replace = TRUE)
hour(bedtime_end[(w2+1):w]) <- hours
duration[(w2+1):w] <- as.duration(bedtime_end[(w2+1):w]-bedtime_start[(w2+1):w])
Notes[w1:w] <- "Sporadic to consistent wake ups"
}
}

bedtime_start_delta <- rep(NA, n)
bedtime_end_delta <- rep(NA, n)
for(i in 2:n){
  if(i %% (d+1) == 0){
    bedtime_start_delta[i] <- NA
    bedtime_end_delta[i] <- NA
  }else{
    bedtime_start_delta[i] <- as.duration(bedtime_start[i]-bedtime_start[i-1])
    bedtime_end_delta[i] <- as.duration(bedtime_end[i]-bedtime_end[i-1])
  } 
}
bedtime_start_delta <- as.integer(bedtime_start_delta)
bedtime_end_delta <- as.integer(bedtime_end_delta)

duration <- as.integer(duration)

#Calculate deep, light, rem, awake, and total

deepPer <- sample(deepPer_range, n, replace = TRUE)
deep <- as.integer(deepPer*duration)
lightPer <- sample(lightPer_range, n, replace = TRUE)
light <- as.integer(lightPer*duration)
REMPer <- sample(REMPer_range, n, replace = TRUE)
rem <- as.integer(REMPer*duration)
awakePer <- rep(1, n)-(deepPer+lightPer+REMPer)
awake <- as.integer(awakePer*duration)
total <- deep+light+rem

period_id <- rep(0, n)
for(i in 1:(n-1)){
  if(summary_date[i+1] == summary_date[i]){
    period_id[i+1] = period_id[i] + 1
  }
}

#Use hr_range to create hr_5min, hr_avg, and hr_lowest
hr_5min <- list()
for(i in 1:n){
  a <- sample(hr_range, floor(duration[i]/5), replace = TRUE)
  a <- array(a)
  hr_5min[[i]] <- a
}

p <- sample(1:num_people, story10+story11)

#Consistent Heart Rate
if(story10 > 0){
s10 <- p[1:story10]
for(j in 1:length(s10)){
  w <- s10[j]*d
  w1 <- (w-(d-1))
for(i in w1:w){
  a <- sample(70:75, floor(duration[i]/5), replace = TRUE)
  a <- array(a)
  hr_5min[[i]] <- a
}
Notes[w1:w] <- "Consistent Heart Rate"
}
}

#Inconsistent Heart Rate
if(story11 > 0){
s11 <- p[(story10+1):(story10+story11)]
for(j in 1:length(s11)){
  w <- s11[j]*d
  w1 <- (w-(d-1))
for(i in w1:w){
  a <- sample(70:90, floor(duration[i]/5), replace = TRUE)
  a <- array(a)
  hr_5min[[i]] <- a
}
Notes[w1:w] <- "Inconsistent Heart Rate"
}
}

hr_avg <- rep(NA, n)
for(i in 1:n){
  hr_avg[i] <- round(mean(hr_5min[[i]]))
}

hr_lowest <- rep(NA, n)
for(i in 1:n){
  hr_lowest[i] <- round(min(hr_5min[[i]]))
}

hr_5min <- as.array(hr_5min)

#These columns have NA values
rmssd_5min <- rep(NA, n)
rmssd <- rep(NA, n)

#Create hypnogram_5min from deep, light, rem, and awake
hypnogram_5min <- rep(NA, n)
for(i in 1:n){
  d = rep("1", round(deep[i]/5))
  l = rep("2", round(light[i]/5))
  r = rep("3", round(rem[i]/5))
  a = rep("4", round(awake[i]/5))
  size <- length(d)+length(l)+length(r)+length(a)
  h <- sample(c(d, l, r, a), size = size)
  hypnogram_5min[i] <- paste(h, collapse = '')
}

#Create random variables 
efficiency <- as.integer((total/duration)*100)
onset_latency <- sample(onset_latency_range, n, replace = TRUE)
midpoint_time <- total/2
temperature_delta <- sample(temp_delta_range, n, replace=TRUE)

#These columns have NA values 
is_longest <- rep(NA, n)
midpoint_at_delta <- rep(NA, n)
temperature_deviation <- rep(NA, n)
temperature_trend_deviation <- rep(NA, n)

#Convert dates/datetimes to characters 
bedtime_start <- as.character(bedtime_start)
bedtime_end <- as.character(bedtime_end)
summary_date <- as.character(summary_date)

#create data frame 

Oura_Ring_Sleep <- data.frame(awake, bedtime_end, bedtime_end_delta, bedtime_start, 
                              bedtime_start_delta, breath_average, deep, duration,
                              efficiency, hr_5min, hr_avg, hr_lowest, hypnogram_5min, 
                              is_longest, light, midpoint_at_delta, midpoint_time, 
                              onset_latency, period_id, rem, restless, rmssd, rmssd_5min,
                              score, score_alignment, score_deep, score_disturbances,
                              score_efficiency, score_latency, score_rem, score_total,
                              summary_date, temperature_delta, temperature_deviation,
                              temperature_trend_deviation, timezone, total, person_id, Notes)
return(Oura_Ring_Sleep)
}


