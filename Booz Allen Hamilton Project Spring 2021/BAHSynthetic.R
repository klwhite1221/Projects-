library(tidyverse)

set.seed(10) 
hrv_id <- 1:500
hrv_week <- rep(1:10, 50)
hrv_hrv <- sample(14:24, 500, replace=TRUE)
hrv_sdd_1_4 <- sample(5:10, 500, replace=TRUE)
hrv_sdd_2_4 <- sample(5:10, 500, replace=TRUE)
hrv_sdd_3_4 <- sample(5:10, 500, replace=TRUE)
hrv_sdd_4_4 <- sample(5:10, 500, replace=TRUE)
hrv_sdd_5_4 <- sample(5:10, 500, replace=TRUE)
hrv_sdd_6_4 <- sample(6:10, 500, replace=TRUE)
hrv_sdd_7_4 <- sample(7:10, 500, replace=TRUE)
hrv_slp <- sample(1:3, 500, replace=TRUE)
person_id <- c()
person_id <- rep(1, 10)
for(i in 2:50){
  person_id <- append(person_id, rep(i, 10), after = length(person_id))
}
Notes <- rep(NA, 500)
HRV <- data.frame(hrv_id, hrv_week, hrv_hrv, hrv_sdd_1_4, hrv_sdd_2_4, hrv_sdd_3_4, hrv_sdd_4_4, hrv_sdd_5_4, hrv_sdd_6_4, hrv_sdd_7_4, hrv_slp, person_id, Notes)
head(HRV)


#Person ID 44: A mix of consistent sleep hours/quality and inconsistent sleep hours/quality
HRV[431:432, 4:10] = sample(7:9, 14, replace = TRUE) 
HRV[431:432, 11] = 3
HRV[433:436, 4:10] = sample(4:12, 28, replace = TRUE)
HRV[433:436, 11] = 1
HRV[437:438, 4:10] = sample(8:10, 14, replace = TRUE)
HRV[437:438, 11] = 3
HRV[439:440, 4:10] = sample(3:12, 14, replace = TRUE)
HRV[439:440, 11] = 1
HRV$Notes[431:440] = "A mix of consistent sleep hours/quality and inconsistent sleep hours/quality"

#Person ID 2: A mix of consistent sleep hours/quality and inconsistent sleep hours/quality
HRV[11:13, 4:10] = sample(1:12, 21, replace = TRUE)
HRV[11:13, 11] = 1
HRV[14, 4:10] = sample(5:6, 7, replace = TRUE)
HRV[14, 11] = 1
HRV[15:16, 4:10] = sample(3:10, 14, replace = TRUE)
HRV[15:16, 11] = 1
HRV[16:20, 4:10] = sample(5:7, 35, replace = TRUE)
HRV[16:20, 11] = 2
HRV$Notes[11:20] = "A mix of consistent sleep hours/quality and inconsistent sleep hours/quality"

#Person ID 25: Consistent and now inconsistent sleep hours/quality
HRV[241:246, 4:10] = sample(7:9, 42, replace = TRUE)
HRV[241:246, 11] = 3
HRV[247:250, 4:10] = sample(4:11, 28, replace = TRUE)
HRV[247:250, 11] = sample(1:2, 4, replace = TRUE)
HRV$Notes[241:250] = "Consistent and now inconsistent sleep hours/quality"

#Person ID 12: Inconsistent and now consistent sleep hours/quality
HRV[111:115, 4:10] = sample(3:12, 35, replace = TRUE)
HRV[11:115, 11] = sample(1:3, 5, replace = TRUE)
HRV[116:120, 4:10] = sample(8:9, 35, replace = TRUE)
HRV[116:120, 11] = 2
HRV$Notes[111:115] = "Inconsistent and now consistent sleep hours/quality"

#Person ID 35: Sleeps well during the week but not on weekends
HRV[341:350, 4:7] = sample(7:10, 40, replace = TRUE)
HRV[341:350, 8:10] = sample(c(1, 2, 3, 10, 11, 12, 13), 30, replace = TRUE)
HRV$Notes[341:350] = "Sleeps well during the week but not on weekends"

#Person ID 18: Sleeps well on weekends but not during the week
HRV[171:180, 4:7] = sample(1:5, 40, replace =TRUE)
HRV[171:180, 8:10] = sample(7:9, 30, replace = TRUE)
HRV$Notes[171:180] = "Sleeps well on weekends but not during the week"

#Person ID 48: Consistent HRV values 
HRV[471:480, 3] = sample(17:19, 10, replace = TRUE)
HRV$Notes[471:480] = "Consistent HRV values"

#Person 4: Inconsistent HRV values
HRV[31:40, 3] = sample(0:24, 10, replace = TRUE)
HRV$Notes[31:40] = "Inconsistent HRV values"


View(HRV)

library(lubridate)

person_id <- c()
person_id <- rep(1, 93)
for(i in 2:50){
  person_id <- append(person_id, rep(i, 93), after = length(person_id))
}

Notes <- rep(NA, 4650)

breath_average <- sample(seq(12, 20, 0.01), 4650, replace = TRUE)
restless <- sample(0:100, 4650, replace = TRUE)

score <- rep(0, 4650)
score_alignment <- sample(0:100, 4650, replace = TRUE)
score_deep <- sample(0:100, 4650, replace = TRUE)
score_disturbances <- sample(0:100, 4650, replace = TRUE)
score_efficiency <- sample(0:100, 4650, replace = TRUE)
score_latency <- sample(0:100, 4650, replace =TRUE)
score_rem <- sample(0:100, 4650, replace = TRUE)
score_total <- sample(0:100, 4650, replace = TRUE)
for(i in 1:4650){
  score[i] = (0.10*score_alignment[i]+0.10*score_deep[i]+0.10*score_efficiency[i]+0.05*score_latency[i]+0.10*score_rem[i]+0.35*score_total[i]+0.15*score_disturbances[i])/(0.10+0.10+0.10+0.05+0.10+0.35+0.15)
  score[i] <- as.integer(score[i])
}

#Person ID 31: consistent scoring

score_alignment[2791:2883] <- sample(60:65, 93, replace = TRUE)
score_deep[2791:2883] <- sample(61:66, 93, replace = TRUE)
score_efficiency[2791:2883] <- sample(55:60, 93, replace = TRUE)
score_latency[2791:2883] <- sample(70:75, 93, replace =TRUE)
score_rem[2791:2883] <- sample(63:68, 93, replace = TRUE)
score_total[2791:2883] <- sample(56:61, 93, replace = TRUE)
for(i in 2791:2883){
  score[i] = (0.10*score_alignment[i]+0.10*score_deep[i]+0.10*score_efficiency[i]+0.05*score_latency[i]+0.10*score_rem[i]+0.35*score_total[i])/(0.10+0.10+0.10+0.05+0.10+0.35)
  score[i] <- as.integer(score[i])
}
Notes[2791:2883] <- "Consistent scoring"

summary_date <- rep(seq.Date(as.Date("2019-06-01"), as.Date("2019-09-01"), 1), 50)
hour_s <- sample(19:23, 4650, replace = TRUE)
minutes <- sample(0:59, 4650, replace = TRUE)
seconds <- sample(0:59, 4650, replace = TRUE)
bedtime_start <- ymd(summary_date)
bedtime_start <- make_datetime(year(bedtime_start), month(bedtime_start), day(bedtime_start), hour_s, minutes, seconds)
bedtime_end <- bedtime_start
for(i in 1:4650){
  if(0<=hour(bedtime_start[i]) && hour(bedtime_start[i])<= 4){
    bedtime_end[i] = bedtime_start[i]
  }else{
    bedtime_end[i] = bedtime_start[i]+ddays(1)
  }
}
hour_e <- sample(6:11, 4650, replace = TRUE)
minutes <- sample(0:59, 4650, replace = TRUE)
seconds <- sample(0:59, 4650, replace = TRUE)
bedtime_end <- make_datetime(year(bedtime_end), month(bedtime_end), day(bedtime_end), hour_e, minutes, seconds)
duration <- as.duration(bedtime_end-bedtime_start)
summary_date <- bedtime_end-ddays(1)
summary_date <- make_date(year(summary_date), month(summary_date), day(summary_date))
tz <- sample(60*c(-11:11), 50, replace=TRUE) 
timezone <- rep(tz[1], 93)
for(i in 2:50){
  timezone <- append(timezone, rep(tz[i], 93), after = length(timezone))
}


#Person ID 9: consistent bedtime 
hours <- sample(20:21, 93, replace = TRUE)
hour(bedtime_start[745:837]) <- hours
duration[745:837] <- as.duration(bedtime_end[745:837]-bedtime_start[745:837])
Notes[745:837] <- "Consistent bedtime"

#Person ID 46: consistent duration
hours <- sample(7:8, 93, replace = TRUE)
bedtime_end[4186:4278] <- bedtime_start[4186:4278] + dhours(hours)
duration[4186:4278] <- as.duration(bedtime_end[4186:4278]-bedtime_start[4186:4278])
Notes[4186:4278] <- "Consistent duration"

#Person ID 37: consistent bedtime and duration
hours <- sample(21:22, 93, replace = TRUE)
hour(bedtime_start[3349:3441]) <- hours
hours2 <- sample(6:7, 93, replace = TRUE)
bedtime_end[3349:3441] <- bedtime_start[3349:3441] + dhours(hours2)
duration[3349:3441] <- as.duration(bedtime_end[3349:3441]-bedtime_start[3349:3441])
Notes[3349:3441] <- "Consistent bedtime and duration"

#Person ID 5: Inconsistent bedtime
hours <- sample(c(19:23, 0:4), 93, replace = TRUE)
hour(bedtime_start[373:465]) <- hours
for(i in 1:93){
  if(hours[i] < 5){
    bedtime_start[i+372] <- bedtime_start[i+372] + ddays(1)
  }
}
duration[373:465] <- as.duration(bedtime_end[373:465]-bedtime_start[373:465])
Notes[373:465] <- "Inconsistent bedtime"

#Person ID 4: Inconsistent duration
hours <- sample(1:12, 93, replace = TRUE)
bedtime_end[280:372] <- bedtime_start[280:372] + dhours(hours)
duration[280:372] <- as.duration(bedtime_end[280:372]-bedtime_start[280:372])
Notes[280:372] <- "Inconsistent duration"

#Person ID 39: inconsistent bedtime and inconsistent duration
hours <- sample(c(19:12, 0:5), 93, replace = TRUE)
hour(bedtime_start[3535:3627]) <- hours
for(i in 1:93){
  if(hours[i] < 5){
    bedtime_start[i+3534] <- bedtime_start[i+3534] + ddays(1)
  }
}
hours2 <- sample(2:13, 93, replace = TRUE)
bedtime_end[3535:3627] <- bedtime_start[3535:3627] + dhours(hours2)
duration[3535:3627] <- as.duration(bedtime_end[3535:3627]-bedtime_start[3535:3627])
Notes[3535:3627] <- "Inconsistent bedtime and inconsistent duration"


#Person ID 34: consistent to sporadic duration
hours <- sample(5:7, 53, replace = TRUE)
bedtime_end[3070:3122] <- bedtime_start[3070:3122] + dhours(hours)
duration[3070:3122] <- as.duration(bedtime_end[3070:3122]-bedtime_start[3070:3122])
hours <- sample(4:13, 40, replace = TRUE)
bedtime_end[3123:3162] <- bedtime_start[3123:3162] + dhours(hours)
duration[3123:3162] <- as.duration(bedtime_end[3123:3162]-bedtime_start[3123:3162])
Notes[3070:3162] <- "Consistent to sporadic duration"

#Person ID 24: sporadic to consistent wake ups
hours <- sample(4:13, 54, replace = TRUE)
hour(bedtime_end[2140:2193]) <- hours
duration[2140:2193] <- as.duration(bedtime_end[2140:2193]-bedtime_start[2140:2193])
hours <- sample(6:8, 39, replace = TRUE)
hour(bedtime_end[2194:2232]) <- hours
duration[2194:2232] <- as.duration(bedtime_end[2194:2232]-bedtime_start[2194:2232])
Notes[2140:2232] <- "Sporadic to consistent wake ups"

bedtime_start_delta <- rep(NA, 4650)
bedtime_end_delta <- rep(NA, 4650)
for(i in 2:4650){
  if(i %% 94 == 0){
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

deepPer <- sample(seq(0.13, 0.2, 0.01), 4650, replace = TRUE)
deep <- as.integer(deepPer*duration)
lightPer <- sample(seq(0.45, 0.5, 0.01), 4650, replace = TRUE)
light <- as.integer(lightPer*duration)
REMPer <- sample(seq(0.2, 0.2, 0.01), 4650, replace = TRUE)
rem <- as.integer(REMPer*duration)
awakePer <- rep(1, 4650)-(deepPer+lightPer+REMPer)
awake <- as.integer(awakePer*duration)
total <- deep+light+rem

period_id <- rep(0, 4650)
for(i in 1:4649){
  if(summary_date[i+1] == summary_date[i]){
    period_id[i+1] = period_id[i] + 1
  }
}

hr_5min <- list()
for(i in 1:4650){
  a <- sample(75:85, floor(duration[i]/5), replace = TRUE)
  a <- array(a)
  hr_5min[[i]] <- a
}

#Person ID 46: Consistent Heart Rate 
for(i in 4186:4278){
  a <- sample(70:75, floor(duration[i]/5), replace = TRUE)
  a <- array(a)
  hr_5min[[i]] <- a
}
Notes[4186:4278] <- "Consistent Heart Rate"

#Person ID 22: Inconsistent Heart Rate
for(i in 1954:2046){
  a <- sample(70:90, floor(duration[i]/5), replace = TRUE)
  a <- array(a)
  hr_5min[[i]] <- a
}
Notes[1954:2046] <- "Inconsistent Heart Rate"

hr_avg <- rep(NA, 4650)
for(i in 1:4650){
  hr_avg[i] <- round(mean(hr_5min[[i]]))
}

hr_lowest <- rep(NA, 4650)
for(i in 1:4650){
  hr_lowest[i] <- round(min(hr_5min[[i]]))
}

hr_5min <- as.array(hr_5min)

rmssd_5min <- rep(NA, 4650)
rmssd <- rep(NA, 4650)


hypnogram_5min <- rep(NA, 4650)
for(i in 1:4650){
  d = rep("1", round(deep[i]/5))
  l = rep("2", round(light[i]/5))
  r = rep("3", round(rem[i]/5))
  a = rep("4", round(awake[i]/5))
  size <- length(d)+length(l)+length(r)+length(a)
  h <- sample(c(d, l, r, a), size = size)
  hypnogram_5min[i] <- paste(h, collapse = '')
}
efficiency <- as.integer((total/duration)*100)
onset_latency <- sample(120:3600, 4650, replace = TRUE)
midpoint_time <- total/2
temperature_delta <- sample(seq(-1, 1, 0.01), 4650, replace=TRUE)

is_longest <- rep(NA, 4650)
midpoint_at_delta <- rep(NA, 4650)
temperature_deviation <- rep(NA, 4650)
temperature_trend_deviation <- rep(NA, 4650)

bedtime_start <- as.character(bedtime_start)
bedtime_end <- as.character(bedtime_end)
summary_date <- as.character(summary_date)

Oura_Ring_Sleep <- data.frame(awake, bedtime_end, bedtime_end_delta, bedtime_start, 
                              bedtime_start_delta, breath_average, deep, duration,
                              efficiency, hr_5min, hr_avg, hr_lowest, hypnogram_5min, 
                              is_longest, light, midpoint_at_delta, midpoint_time, 
                              onset_latency, period_id, rem, restless, rmssd, rmssd_5min,
                              score, score_alignment, score_deep, score_disturbances,
                              score_efficiency, score_latency, score_rem, score_total,
                              summary_date, temperature_delta, temperature_deviation,
                              temperature_trend_deviation, timezone, total, person_id, Notes)
View(Oura_Ring_Sleep)
