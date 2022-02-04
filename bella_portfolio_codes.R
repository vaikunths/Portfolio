install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("Tmisc")
install.packages("dplyr")
install.packages("SimDesign")
install.packages("rmarkdown")
install.packages("vitae")
install.packages("dbplyr")
install.packages("RSQLite")
install.packages("sqldf")
install.packages("odbc")
install.packages("DBI")


library(tidyverse)
library(tidyr)
library(tibble)
library(skimr)
library(janitor)
library(Tmisc)
library(dplyr)
library(SimDesign)
library(rmarkdown)
library(vitae)
library(sqldf)
library(odbc)
library(RSQLite)
library(DBI)
library(dbplyr)

View(sleep_activity)
ggplot(data=sleep_activity)+ geom_point(mapping=aes(x=Sleephours, y=Activehours))
ggplot(data=sleep_activity)+ geom_point(mapping=aes(x=Sleephours, y=Activehours, color=Id))
ggplot(data=sleep_activity)+ geom_point(mapping=aes(x=Sleephours, y=Activehours))+facet_wrap(~Id)
hist(sleep_activity$Activehours)
hist(sleep_activity$Activehours, xlim=Sleephours)

sleep_activity %>%
  group_by(Id) %>%
  filter(n()>15)



sleep_activity2 <- sleep_activity %>%
  mutate(totalwakehours=Activehours+Sleephours+Idlehours)
                  
filtered_sleep <-sleep_activity %>%
  group_by(Id) %>%
  filter(n()>15)

ggplot(data=filtered_sleep)+geom_point(mapping=aes(x=Sleephours,y=Activehours))+geom_smooth(mapping=aes(x=Sleephours, y=Activehours))

pattern_check <-sleep_activity2 %>% 
  filter(totalwakehours>22) %>% 
  filter(totalwakehours<25) %>% 
  group_by(Id) %>% 
  filter(n()>15)
  
sqldf("SELECT * FROM heartrate")

## To check work done in excel to be done here

## Joining sleep table with Work intensities

sleepeffectactivity <- sqldf("
                             SELECT * 
                             FROM sleepDay_merged as sleep
                             LEFT JOIN dailyIntensities_merged AS activity 
                             ON 
                             sleep.SleepDay = activity.ActivityDay")
library(lubridate)

sleepDay_merged$SleepDay <- mdy_hms(sleepDay_merged$SleepDay)

sleepDay_merged <- read_csv("sleepDay_merged_date.csv")

sleepDay_merged %>%
  mutate(SleepDate=as_date(SleepDay))
  

sleepeffectactivity


dailyIntensities_merged$ActivityDay <- mdy(dailyIntensities_merged$ActivityDay)
sleepDay_merged$SleepDay <- dmy(sleepDay_merged$SleepDay)
heartrate$Time <- mdy_hms(heartrate$Time)
heartrate <- read_csv("heartrate_seconds_merged.csv")
again_sleepDay <- read_csv("sleepDay_merged.csv")
again_sleepDay$SleepDay <- mdy_hms(again_sleepDay$SleepDay)
again_sleepDay$SleepDay <- as_date(again_sleepDay$SleepDay)
heartrate$Time <- as_date(heartrate$Time)



heartrate_check <-sqldf("
                        SELECT heartrate.Id AS HId, heartrate.Time, heartrate.Value, sleep.TotalMinutesAsleep
                        FROM heartrate
                        JOIN again_sleepDay as sleep
                        ON heartrate.Time = sleep.SleepDay AND
                        heartrate.Id=sleep.Id
                        ")

heartrateperday <- sqldf("
                             SELECT HId,Time, AVG(Value) AS Avg_beat, TotalMinutesAsleep
                             FROM heartrate_check
                            GROUP BY Time
                         ")

ggplot(data=heartrateperday)+geom_point(mapping=aes(x=TotalMinutesAsleep, y=Avg_beat,color=HId))+geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=Avg_beat))


heartrateperday$dt <- mdy(heartrateperday$dt)
sqldf("
  ALTER TABLE again_sleepDay
  ALTER COLUMN SleepDay Date
")

sleepeffectactivity2 <- sqldf("
                             SELECT *
                             FROM again_sleepDay as sleep
                             JOIN dailyIntensities_merged AS activity 
                             ON 
                             sleep.SleepDay = activity.ActivityDay
                              AND
                              sleep.Id=activity.Id")


