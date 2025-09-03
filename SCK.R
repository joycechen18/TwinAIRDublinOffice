# install packages #
# load packages #
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)

# 20240911
# read csv C379A Indoor Room A #
# view file #
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "C379IndoorA.csv")
View(data)
summary(data)

# creat a dataframe
df <- data.frame(data)
df

# cut second to min
# prerequisites
library(data.table)
library(tibble)
library(xts)
# creat a dataframe 
# seperate the date hour min and second
library(lubridate)
library(tidyverse)
#check timezone 
Sys.timezone()
# add new seperated columns
datetime_y <- year(ymd_hms(df$datatime, tz = "Europe/Dublin"))
datetime_m <- month(ymd_hms(df$datatime, tz = "Europe/Dublin"))
datetime_d <- day(ymd_hms(df$datatime, tz = "Europe/Dublin"))
datetime_h <- hour(ymd_hms(df$datatime, tz = "Europe/Dublin"))
datetime_min <- minute(ymd_hms(df$datatime,tz = "Europe/Dublin"))
datetime_sec <- second(ymd_hms(df$datatime,tz = "Europe/Dublin"))

df <- data.frame(data, 
                 datetime_y, 
                 datetime_m, 
                 datetime_d, 
                 datetime_h, 
                 datetime_min, 
                 datetime_sec)
df

# export the dataframe with new columns of seperated date time
write.csv(df,"~/Desktop/16684_schooltesting1r.csv", 
          row.names = TRUE)
###
setwd("~/Desktop/TwinAIR/202311")
data1 = read.csv(file = "ambient and outdoor comparison.csv")
View(data1)
summary(data1)
df <- data.frame(data1)

### library
library(lme4)
library(lmerTest)
library(r2glmm)
library(Hmisc)
library(dplyr)

### stepwise analysis
model1 <- lm(PM2.5 ~ PM.sub.2.5..sub. + Relative.Humidity + Temperature, data = data1)
model2 <- lm(PM2.5 ~ PM.sub.2.5..sub. + Relative.Humidity, data = data1)
model3 <- lm(PM2.5 ~ PM.sub.2.5..sub. + Temperature, data = data1)
model4 <- lm(PM2.5 ~ PM.sub.2.5..sub., data = data1)

model1 <- lm(PM.sub.2.5..sub. ~  PM2.5 + Relative.Humidity + Temperature, data = data1)
model2 <- lm(PM.sub.2.5..sub. ~  PM2.5 + Relative.Humidity, data = data1)
model3 <- lm(PM.sub.2.5..sub. ~  PM2.5 + Temperature, data = data1)
model4 <- lm(PM.sub.2.5..sub. ~  PM2.5, data = data1)


model1 <- lm(PM.sub.10..sub. ~  PM10 + Relative.Humidity + Temperature, data = data1)
model2 <- lm(PM.sub.10..sub. ~  PM10 + Relative.Humidity, data = data1)
model3 <- lm(PM.sub.10..sub. ~  PM10 + Temperature, data = data1)
model4 <- lm(PM.sub.10..sub. ~  PM10, data = data1)


s <- step(model1)
print(s)
summary(model4)

# explore relationships bettween variables
#https://www.r-bloggers.com/2021/08/exploring-relationship-between-variables-scatter-plot/
plot(~ PM2.5 + Relative.Humidity + Temperature, data = df, main = "Scatterplot Matrix")

# scatter plot using ggplot2
library(ggpmisc)
# PM2.5
jpeg("correction with RH.jpg", width = 8, height = 5, units = 'in', res = 500)
ggplot(df, aes(x = PM2.5, y = PM.sub.2.5..sub., color = Relative.Humidity)) + 
  geom_jitter() + 
  scale_fill_gradient(low = "green", high = "red", na.value = "gray50", aesthetics = "color") +
  theme_bw() + 
  scale_x_continuous(name = "Concentration_Low-cost sensor (ug/m3)") +
  scale_y_continuous(name = "Concentration_Ambient site (ug/m3)") +
  labs(title = "Low-cost sensor vs. Reference method PM2.5") + 
  stat_poly_line() +  #library(ggpmisc)
  stat_poly_eq(use_label(c("eq","adj.R2", "p", "n")))
dev.off() 

# PM10
jpeg("correction with RH-PM10.jpg", width = 8, height = 5, units = 'in', res = 500)
ggplot(df, aes(x = PM10, y = PM.sub.10..sub., color = Relative.Humidity)) + 
  geom_jitter() + 
  scale_fill_gradient(low = "lightblue", high = "red", na.value = "gray50", aesthetics = "color") +
  theme_bw() + 
  scale_x_continuous(name = "Concentration_Low-cost sensor (ug/m3)") +
  scale_y_continuous(name = "Concentration_Ambient site (ug/m3)") +
  labs(title = "Low-cost sensor vs. Reference method PM10") + 
  stat_poly_line() +  #library(ggpmisc)
  stat_poly_eq(use_label(c("eq","adj.R2", "p", "n")))
dev.off() 


###
###
setwd("~/Desktop/TwinAIR/202311")
data1 = read.csv(file = "houtdoor_r1_MayJuly.csv")
View(data1)
summary(data1)
df <- data.frame(data1)

### stepwise analysis
model1 <- lm(Ambient.PM2.5 ~ Outdoor.PM2.5 + oMeanRH + oMeanT, data = data1)
model2 <- lm(Ambient.PM2.5 ~ Outdoor.PM2.5 + oMeanRH, data = data1)
model3 <- lm(Ambient.PM2.5 ~ Outdoor.PM2.5  + oMeanT, data = data1)
model4 <- lm(Ambient.PM2.5 ~ Outdoor.PM2.5, data = data1)


model1 <- lm(Ambient.PM10 ~ Outdoor.PM10 + oMeanRH + oMeanT, data = data1)
model2 <- lm(Ambient.PM10 ~ Outdoor.PM10 + oMeanRH, data = data1)
model3 <- lm(Ambient.PM10 ~ Outdoor.PM10 + oMeanT, data = data1)
model4 <- lm(Ambient.PM10 ~ Outdoor.PM10, data = data1)

s <- step(model1)
print(s)
summary(model4)

# scatter plot using ggplot2
library(ggpmisc)
# PM2.5
jpeg("correction with RH_MayJuly.jpg", width = 8, height = 5, units = 'in', res = 500)
ggplot(df, aes(x = Outdoor.PM2.5, y = Ambient.PM2.5, color = oMeanRH)) + 
  geom_jitter() + 
  scale_fill_gradient(low = "green", high = "red", na.value = "gray50", aesthetics = "color") +
  theme_bw() + 
  scale_x_continuous(name = "Concentration_Low-cost sensor (ug/m3)") +
  scale_y_continuous(name = "Concentration_Ambient site (ug/m3)") +
  labs(title = "Low-cost sensor vs. Reference method PM2.5") + 
  stat_poly_line() +  #library(ggpmisc)
  stat_poly_eq(use_label(c("eq","adj.R2", "p", "n")))
dev.off() 

# PM10
jpeg("correction with RH-PM10_MayJuly.jpg", width = 8, height = 5, units = 'in', res = 500)
ggplot(df, aes(x = Outdoor.PM2.5, y = Ambient.PM2.5, color = oMeanRH)) + 
  geom_jitter() + 
  scale_fill_gradient(low = "lightblue", high = "red", na.value = "gray50", aesthetics = "color") +
  theme_bw() + 
  scale_x_continuous(name = "Concentration_Low-cost sensor (ug/m3)") +
  scale_y_continuous(name = "Concentration_Ambient site (ug/m3)") +
  labs(title = "Low-cost sensor vs. Reference method PM10") + 
  stat_poly_line() +  #library(ggpmisc)
  stat_poly_eq(use_label(c("eq","adj.R2", "p", "n")))
dev.off() 


# =================================================================================
# =================================================================================
# install packages #
# load packages #
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)

# 20240911
# read csv C379A Indoor Room A #
# view file #
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "C379IndoorA.csv")
View(data)
summary(data)
df <-data.frame(data)

## new data file 
# visualization
# weekday variation by month
library(tidyverse)
library(lubridate)
library(ggplot2)

# cut second to min
# tutorial: https://posit.cloud/learn/recipes/datetime/DatetimeB
#  https://www.appsilon.com/post/r-lubridate

# cut second to min
# prerequisites
library(data.table)
library(tibble)
library(xts)
# creat a dataframe 
# seperate the date hour min and second
library(lubridate)
library(dplyr)
library(tidyverse)
#check timezone 
Sys.timezone()

# add new seperated columns
datetime_y <- year(ymd_hms(df$timestamp, tz = "Europe/Dublin"))
datetime_m <- month(ymd_hms(df$timestamp, tz = "Europe/Dublin"))
datetime_d <- day(ymd_hms(df$timestamp, tz = "Europe/Dublin"))
datetime_h <- hour(ymd_hms(df$timestamp, tz = "Europe/Dublin"))
datetime_wd <- wday(ymd_hms(df$timestamp, tz = "Europe/Dublin"))
# lab the month
date_m <- lubridate::month(datetime_m, label = TRUE)
# creat a dateframe
length(datetime_wd) = length(data)
# datetime
alldata <-as.POSIXct(format(as.POSIXct(df$timestamp), tz = "UTC"))
dateonly <-as.Date(alldata)
min(dateonly)
head(dateonly)
# lab the weekday using text
wday = wday(dateonly, label=TRUE)
wday

df <- data.frame(data, 
                 datetime_y, 
                 datetime_m, 
                 datetime_d, 
                 datetime_h, 
                 datetime_wd,
                 date_m, 
                 dateonly, 
                 wday)
df
view(df)
# export the dataframe with new columns of seperated date time
write.csv(df,"~/C379IndoorA.csv", 
          row.names = TRUE)
# or save the file to current folder
write.csv(df, file = "C379IndoorA.csv")

# average the second data to hourly concentration
# creat date
# intersect, union
# load packages
# moving average
library(tidyquant)
df_r <- df %>% 
  mutate(Date = as.Date(timestamp))

# hourly average
# load package
library(dplyr)
# hourly PM2.5
hRH <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(RHMean = mean(Relative.Humidity, na.rm = TRUE))
hRH
view(hRH)

hPM2.5 <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(PM2.5Mean = mean(PM.2.5, na.rm = TRUE))
hPM2.5

hPM10 <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(PM10Mean = mean(PM.10, na.rm = TRUE))
hPM10

hTVOCs <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(TVOCsMean = mean(TVOC, na.rm = TRUE))
hTVOCs

heCO2 <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(eCO2Mean = mean(eCO2, na.rm = TRUE))
heCO2

hT <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(TMean = mean(Temperature, na.rm = TRUE))
hT

hPM1 <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(PM1Mean = mean(PM.1, na.rm = TRUE))
hPM1

hnoise <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(noiseMean = mean(Noise, na.rm = TRUE))
hnoise

hP <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(PMean = mean(Pressure, na.rm = TRUE))
hP

# Save multiple objects
hdf <- data.frame(hPM10, hPM2.5, hPM1, hRH, hT, hTVOCs, heCO2, hnoise, hP)
hdf
# Using dplyr
# Remove duplicate rows (all columns)
# hdf1 <- hdf %>% distinct()
hdf1 <- hdf %>% distinct(dateonly, .keep_all = TRUE)
view(hdf1)
write.csv(hdf,"~/Desktop/C379IndoorAh.csv", 
          row.names = TRUE)


# =========
# read csv FADD Indoor Room A #
# view file #
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "FADDIndoorA.csv")
View(data)
summary(data)
df <-data.frame(data)

# add new seperated columns
datetime_y <- year(ymd_hms(df$timestamp, tz = "Europe/Dublin"))
datetime_m <- month(ymd_hms(df$timestamp, tz = "Europe/Dublin"))
datetime_d <- day(ymd_hms(df$timestamp, tz = "Europe/Dublin"))
datetime_h <- hour(ymd_hms(df$timestamp, tz = "Europe/Dublin"))
datetime_wd <- wday(ymd_hms(df$timestamp, tz = "Europe/Dublin"))
# lab the month
date_m <- lubridate::month(datetime_m, label = TRUE)
# creat a dateframe
length(datetime_wd) = length(data)
# datetime
alldata <-as.POSIXct(format(as.POSIXct(df$timestamp), tz = "UTC"))
dateonly <-as.Date(alldata)
min(dateonly)
head(dateonly)
# lab the weekday using text
wday = wday(dateonly, label=TRUE)
wday

df <- data.frame(data, 
                 datetime_y, 
                 datetime_m, 
                 datetime_d, 
                 datetime_h, 
                 datetime_wd,
                 date_m, 
                 dateonly, 
                 wday)
df
view(df)

# or save the file to current folder
write.csv(df, file = "FADDIndoorA.csv",row.names = TRUE)

# average the second data to hourly concentration
# creat date
# intersect, union
# load packages
# moving average
library(tidyquant)
df_r <- df %>% 
  mutate(Date = as.Date(timestamp))

# hourly average
# load package
library(dplyr)
# hourly PM2.5
hRH <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(RHMean = mean(Relative.Humidity, na.rm = TRUE))
hRH
view(hRH)

hPM2.5 <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(PM2.5Mean = mean(PM.2.5, na.rm = TRUE))
hPM2.5

hPM10 <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(PM10Mean = mean(PM.10, na.rm = TRUE))
hPM10

hTVOCs <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(TVOCsMean = mean(TVOC, na.rm = TRUE))
hTVOCs

heCO2 <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(eCO2Mean = mean(eCO2, na.rm = TRUE))
heCO2

hT <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(TMean = mean(Temperature, na.rm = TRUE))
hT

hPM1 <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(PM1Mean = mean(PM.1, na.rm = TRUE))
hPM1

hnoise <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(noiseMean = mean(Noise, na.rm = TRUE))
hnoise

hP <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(PMean = mean(Pressure, na.rm = TRUE))
hP

# Save multiple objects
hdf <- data.frame(hPM10, hPM2.5, hPM1, hRH, hT, hTVOCs, heCO2, hnoise, hP)
hdf
# Using dplyr
# Remove duplicate rows (all columns)
# hdf1 <- hdf %>% distinct()
hdf1 <- hdf %>% distinct(dateonly, .keep_all = TRUE)
view(hdf1)
write.csv(hdf,"~/Desktop/FADDIndoorAh.csv", 
          row.names = TRUE)


# =========
# read csv 036A Indoor Room B #
# view file #
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "036AIndoorB.csv")
View(data)
summary(data)
df <-data.frame(data)

# add new seperated columns
datetime_y <- year(ymd_hms(df$timestamp))
datetime_m <- month(ymd_hms(df$timestamp))
datetime_d <- day(ymd_hms(df$timestamp))
datetime_h <- hour(ymd_hms(df$timestamp))
datetime_wd <- wday(ymd_hms(df$timestamp))
# lab the month
date_m <- lubridate::month(datetime_m, label = TRUE)
# creat a dateframe
length(datetime_h) = length(data)
# datetime
alldata <-as.POSIXct(format(as.POSIXct(df$timestamp), tz = "Europe/Dublin"))
dateonly <-as.Date(alldata)
min(dateonly)
head(dateonly)
# lab the weekday using text
wday = wday(dateonly, label=TRUE)
wday

df <- data.frame(data, 
                 datetime_y, 
                 datetime_m, 
                 datetime_d, 
                 datetime_h, 
                 datetime_wd,
                 date_m, 
                 dateonly, 
                 wday)
df
view(df)

# or save the file to current folder
write.csv(df, file = "036AIndoorB.csv",row.names = TRUE)

# average the second data to hourly concentration
# creat date
# intersect, union
# load packages
# moving average
library(tidyquant)
df_r <- df %>% 
  mutate(Date = as.Date(timestamp))

# hourly average
# load package
library(dplyr)
# hourly PM2.5
hRH <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(RHMean = mean(Relative.Humidity, na.rm = TRUE))
hRH
view(hRH)

hPM2.5 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(PM2.5Mean = mean(PM.2.5, na.rm = TRUE))
hPM2.5

hPM10 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(PM10Mean = mean(PM.10, na.rm = TRUE))
hPM10

hTVOCs <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(TVOCsMean = mean(TVOC, na.rm = TRUE))
hTVOCs

heCO2 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(eCO2Mean = mean(eCO2, na.rm = TRUE))
heCO2

hT <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(TMean = mean(Temperature, na.rm = TRUE))
hT

hPM1 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(PM1Mean = mean(PM.1, na.rm = TRUE))
hPM1

hnoise <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(noiseMean = mean(Noise, na.rm = TRUE))
hnoise

hP <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(PMean = mean(Pressure, na.rm = TRUE))
hP

# Save multiple objects
hdf <- data.frame(hPM10, hPM2.5, hPM1, hRH, hT, hTVOCs, heCO2, hnoise, hP)
hdf
# Using dplyr

write.csv(hdf,"~/Desktop/036AIndoorBh.csv", 
          row.names = TRUE)


# =========
# read csv 2861 Indoor Room B #
# view file #
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "2861IndoorB.csv")
View(data)
summary(data)
df <-data.frame(data)

# add new seperated columns
datetime_y <- year(ymd_hms(df$timestamp))
datetime_m <- month(ymd_hms(df$timestamp))
datetime_d <- day(ymd_hms(df$timestamp))
datetime_h <- hour(ymd_hms(df$timestamp))
datetime_wd <- wday(ymd_hms(df$timestamp))
# lab the month
date_m <- lubridate::month(datetime_m, label = TRUE)
# creat a dateframe
length(datetime_h) = length(data)
# datetime
alldata <-as.POSIXct(format(as.POSIXct(df$timestamp), tz = "UTC"))
dateonly <-as.Date(alldata)
min(dateonly)
head(dateonly)
# lab the weekday using text
wday = wday(dateonly, label=TRUE)
wday

df <- data.frame(data, 
                 datetime_y, 
                 datetime_m, 
                 datetime_d, 
                 datetime_h, 
                 datetime_wd,
                 date_m, 
                 dateonly, 
                 wday)
df
view(df)

# or save the file to current folder
write.csv(df, file = "2861IndoorB.csv",row.names = TRUE)

# average the second data to hourly concentration
# creat date
# intersect, union
# load packages
# moving average
library(tidyquant)
df_r <- df %>% 
  mutate(Date = as.Date(timestamp))

# hourly average
# load package
library(dplyr)
# hourly PM2.5
hRH <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(RHMean = mean(Relative.Humidity, na.rm = TRUE))
hRH
view(hRH)
nrow(hRH)

hPM2.5 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(PM2.5Mean = mean(PM.2.5, na.rm = TRUE))
hPM2.5
nrow(hPM2.5)

hPM10 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(PM10Mean = mean(PM.10, na.rm = TRUE))
hPM10
nrow(hPM10)

hTVOCs <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(TVOCsMean = mean(TVOC, na.rm = TRUE))
hTVOCs
nrow(hTVOCs)

heCO2 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(eCO2Mean = mean(eCO2, na.rm = TRUE))
heCO2
nrow(heCO2)

hT <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(TMean = mean(Temperature, na.rm = TRUE))
hT
nrow(hT)


hPM1 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(PM1Mean = mean(PM.1, na.rm = TRUE))
hPM1
nrow(hPM1)
hPM1 <- data.frame(hPM1)

hPM1r <- hPM1 %>%
  mutate(PM1Mean = replace(PM1Mean, PM1Mean < 1.83, 0.92))
hPM1r

hnoise <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(noiseMean = mean(Noise, na.rm = TRUE))
hnoise


hP <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(PMean = mean(Pressure, na.rm = TRUE))
hP
nrow(hP)

length(hPM1) = length(hPM10)
# Save multiple objects
hdf <- data.frame(hPM10, hPM2.5, hPM1r, hRH, hT, hTVOCs, heCO2, hnoise, hP)
hdf

# Using dplyr
# Remove duplicate rows (all columns)

write.csv(hdf,"~/Desktop/2861IndoorBh.csv", 
          row.names = TRUE)

# =========
# read csv 790D Outdoor #
# view file #
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "790DOutdoor.csv")
View(data)
summary(data)
df <-data.frame(data)

# add new seperated columns
datetime_y <- year(ymd_hms(df$timestamp))
datetime_m <- month(ymd_hms(df$timestamp))
datetime_d <- day(ymd_hms(df$timestamp))
datetime_h <- hour(ymd_hms(df$timestamp))
datetime_wd <- wday(ymd_hms(df$timestamp))
# lab the month
date_m <- lubridate::month(datetime_m, label = TRUE)
# creat a dateframe
length(datetime_h) = length(data)
# datetime
alldata <-as.POSIXct(format(as.POSIXct(df$timestamp), tz = "UTC"))
dateonly <-as.Date(alldata)
min(dateonly)
head(dateonly)
# lab the weekday using text
wday = wday(dateonly, label=TRUE)
wday

df <- data.frame(data, 
                 datetime_y, 
                 datetime_m, 
                 datetime_d, 
                 datetime_h, 
                 datetime_wd,
                 date_m, 
                 dateonly, 
                 wday)
df
view(df)

# or save the file to current folder
write.csv(df, file = "790DOutdoor.csv",row.names = TRUE)

# average the second data to hourly concentration
# creat date
# intersect, union
# load packages
# moving average
library(tidyquant)
df_r <- df %>% 
  mutate(Date = as.Date(timestamp))

# hourly average
# load package
library(dplyr)
# hourly PM2.5
hRH <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(RHMean = mean(Relative.Humidity, na.rm = TRUE))
hRH
view(hRH)

hPM2.5 <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(PM2.5Mean = mean(PM.2.5, na.rm = TRUE))
hPM2.5

hPM10 <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(PM10Mean = mean(PM.10, na.rm = TRUE))
hPM10

hTVOCs <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(TVOCsMean = mean(TVOC, na.rm = TRUE))
hTVOCs

heCO2 <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(eCO2Mean = mean(eCO2, na.rm = TRUE))
heCO2

hT <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(TMean = mean(Temperature, na.rm = TRUE))
hT

hPM1 <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(PM1Mean = mean(PM.1, na.rm = TRUE))
hPM1

hnoise <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(noiseMean = mean(Noise, na.rm = TRUE))
hnoise

hP <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(PMean = mean(Pressure, na.rm = TRUE))
hP

# Save multiple objects
hdf <- data.frame(hPM10, hPM2.5, hPM1, hRH, hT, hTVOCs, heCO2, hnoise, hP)
hdf
# Using dplyr
# Remove duplicate rows (all columns)
# hdf1 <- hdf %>% distinct()
hdf1 <- hdf %>% distinct(dateonly, .keep_all = TRUE)
view(hdf1)
write.csv(hdf,"~/Desktop/790DOutdoorh.csv", 
          row.names = TRUE)

# =========
# read csv CO2 RoomB #
# view file #
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "OBAFCO2.csv")
View(data)
summary(data)
df <-data.frame(data)

# add new seperated columns
datetime_y <- year(ymd_hms(df$timestamp))
datetime_m <- month(ymd_hms(df$timestamp))
datetime_d <- day(ymd_hms(df$timestamp))
datetime_h <- hour(ymd_hms(df$timestamp))
datetime_wd <- wday(ymd_hms(df$timestamp))
# lab the month
date_m <- lubridate::month(datetime_m, label = TRUE)
# creat a dateframe
length(datetime_h) = length(data)
# datetime
alldata <-as.POSIXct(format(as.POSIXct(df$timestamp), tz = "UTC"))
dateonly <-as.Date(alldata)
min(dateonly)
head(dateonly)
# lab the weekday using text
wday = wday(dateonly, label=TRUE)
wday

df <- data.frame(data, 
                 datetime_y, 
                 datetime_m, 
                 datetime_d, 
                 datetime_h, 
                 datetime_wd,
                 date_m, 
                 dateonly, 
                 wday)
df
view(df)

# or save the file to current folder
write.csv(df, file = "OBAFCO2.csv",row.names = TRUE)

# average the second data to hourly concentration
# creat date
# intersect, union
# load packages
# moving average
library(tidyquant)
df_r <- df %>% 
  mutate(Date = as.Date(timestamp))

# hourly average
# load package
library(dplyr)
# hourly PM2.5
hRH <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(RHMean = mean(Relative.Humidity, na.rm = TRUE))
hRH
view(hRH)

hCO2 <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(CO2Mean = mean(CO2, na.rm = TRUE))
hCO2

hT <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(TMean = mean(Temperature, na.rm = TRUE))
hT

hnoise <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(noiseMean = mean(Noise, na.rm = TRUE))
hnoise

hP <- df_r %>% group_by(dateonly, datetime_h) %>%
  summarise(PMean = mean(Pressure, na.rm = TRUE))
hP

# Save multiple objects
hdf <- data.frame(hRH, hT, hCO2, hnoise, hP)
hdf
# Using dplyr
# Remove duplicate rows (all columns)
# hdf1 <- hdf %>% distinct()
hdf1 <- hdf %>% distinct(dateonly, .keep_all = TRUE)
view(hdf1)
write.csv(hdf,"~/Desktop/OBAFCO2h.csv", 
          row.names = TRUE)


# =============================================================================
# view file #
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "036AIndoorBh.csv")
View(data)
summary(data)
df <-data.frame(data)
# load library
# replace PM10 <2.48, PM2.5 <2.04, PM1< 1.83 #
# replace with PM10 = 1.24, PM2.5 = 1.02, PM1= 0.92 #
library(dplyr)
data_new <- data %>%
  mutate(PM10Mean = replace(PM10Mean, PM10Mean < 2.48, 1.24),
         PM2.5Mean = replace(PM2.5Mean, PM2.5Mean < 2.04, 1.02),
         PM1Mean = replace(PM1Mean, PM1Mean < 1.83, 0.92))
data_new
write.csv(data_new,"~/Desktop/036AIndoorBh_r.csv", 
          row.names = TRUE)
#
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "2861IndoorBh.csv")
View(data)
summary(data)
df <-data.frame(data)
# load library
# replace PM10 <2.48, PM2.5 <2.04, PM1< 1.83 #
# replace with PM10 = 1.24, PM2.5 = 1.02, PM1= 0.92 #
library(dplyr)
data_new <- data %>%
  mutate(PM10Mean = replace(PM10Mean, PM10Mean < 2.48, 1.24),
         PM2.5Mean = replace(PM2.5Mean, PM2.5Mean < 2.04, 1.02))
data_new
write.csv(data_new,"~/Desktop/2861IndoorBh_r.csv", 
          row.names = TRUE)

#
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "C379IndoorAh.csv")
View(data)
summary(data)
df <-data.frame(data)
# load library
# replace PM10 <2.48, PM2.5 <2.04, PM1< 1.83 #
# replace with PM10 = 1.24, PM2.5 = 1.02, PM1= 0.92 #
library(dplyr)
data_new <- data %>%
  mutate(PM10Mean = replace(PM10Mean, PM10Mean < 2.48, 1.24),
         PM2.5Mean = replace(PM2.5Mean, PM2.5Mean < 2.04, 1.02),
         PM1Mean = replace(PM1Mean, PM1Mean < 1.83, 0.92))
data_new
write.csv(data_new,"~/Desktop/C379IndoorAh_r.csv", 
          row.names = TRUE)

#
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "FADDIndoorAh.csv")
View(data)
summary(data)
df <-data.frame(data)
# load library
# replace PM10 <2.48, PM2.5 <2.04, PM1< 1.83 #
# replace with PM10 = 1.24, PM2.5 = 1.02, PM1= 0.92 #
library(dplyr)
data_new <- data %>%
  mutate(PM10Mean = replace(PM10Mean, PM10Mean < 2.48, 1.24),
         PM2.5Mean = replace(PM2.5Mean, PM2.5Mean < 2.04, 1.02),
         PM1Mean = replace(PM1Mean, PM1Mean < 1.83, 0.92))
data_new
data_new <- na.omit(data_new)
view(data_new)
write.csv(data_new,"~/Desktop/FADDIndoorAh_r.csv", 
          row.names = TRUE)

#
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "790DOutdoorh.csv")
View(data)
summary(data)
df <-data.frame(data)
# load library
# replace PM10 <2.48, PM2.5 <2.04, PM1< 1.83 #
# replace with PM10 = 1.24, PM2.5 = 1.02, PM1= 0.92 #
library(dplyr)
data_new <- data %>%
  mutate(PM10Mean = replace(PM10Mean, PM10Mean < 2.48, 1.24),
         PM2.5Mean = replace(PM2.5Mean, PM2.5Mean < 2.04, 1.02),
         PM1Mean = replace(PM1Mean, PM1Mean < 1.83, 0.92))
data_new
write.csv(data_new,"~/Desktop/790DOutdoorh_r.csv", 
          row.names = TRUE)

#
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "8278PAOutdoorh.csv")
View(data)
summary(data)
df <-data.frame(data)
# load library
# replace PM10 <2.48, PM2.5 <2.04, PM1< 1.83 #
# replace with PM10 = 1.24, PM2.5 = 1.02, PM1= 0.92 #
library(dplyr)
data_new <- data %>%
  mutate(PM10Mean = replace(pm10.0_cf_1, pm10.0_cf_1 < 2.48, 1.24),
         PM2.5Mean = replace(pm2.5_cf_1, pm2.5_cf_1 < 2.04, 1.02),
         PM1Mean = replace(pm1.0_cf_1, pm1.0_cf_1 < 1.83, 0.92))
data_new
write.csv(data_new,"~/Desktop/8278PAOutdoorh_r.csv", 
          row.names = TRUE)

#
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "F72BPAIndoorAh.csv")
View(data)
summary(data)
df <-data.frame(data)
# load library
# replace PM10 <2.48, PM2.5 <2.04, PM1< 1.83 #
# replace with PM10 = 1.24, PM2.5 = 1.02, PM1= 0.92 #
library(dplyr)
data_new <- data %>%
  mutate(PM10Mean = replace(pm10.0_cf_1, pm10.0_cf_1 < 2.48, 1.24),
         PM2.5Mean = replace(pm2.5_cf_1, pm2.5_cf_1 < 2.04, 1.02),
         PM1Mean = replace(pm1.0_cf_1, pm1.0_cf_1 < 1.83, 0.92))
data_new
write.csv(data_new,"~/Desktop/F72BPAIndoorAh_r.csv", 
          row.names = TRUE)


#
# https://www.r-bloggers.com/2024/06/remove-rows-from-dataframe-based-on-condition-in-r/
# https://bookdown.org/rwnahhas/IntroToR/exclude-observations-with-missing-data.html
# https://search.r-project.org/CRAN/refmans/bimets/html/00Index.html
# https://epirhandbook.com/new_pages/dates.html
# https://bookdown.org/rwnahhas/IntroToR/exclude-observations-with-missing-data.html
# https://rstudio-education.github.io/hopr/modify.html
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "OBAFCO2h.csv")
View(data)
summary(data)
df <-data.frame(data)
# load library
# replace PM10 <2.48, PM2.5 <2.04, PM1< 1.83 #
# replace with PM10 = 1.24, PM2.5 = 1.02, PM1= 0.92 #
library(dplyr)
data_new <- data %>%
  mutate(CO2Mean = replace(CO2Mean, CO2Mean < 350, 0))
data_new
write.csv(data_new,"~/Desktop/OBAFCO2h_r.csv", 
          row.names = TRUE)


# combining time series data with Pathway
# https://pathway.com/developers/templates/combining_time_series


# Multiple Linear Regression Tutorial: 
# https://www.ldeo.columbia.edu/~danielmw/code/MLR-Tutorial.html


# ==============================================================
# loading required libraries
library(tidyverse)
library(lubridate)
library(SimDesign)
library(caTools)
library(reprex)
library(readr)
# load in data
# set the working directory
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/purpleair_office indoor outdoor/PA 74E9_outdoor 1")
# unpack all csv files into a list
purplefile <- list.files(pattern = ".csv")
# list columns of interest
impcolum <- c("UTCDateTime","current_temp_f", "current_humidity","current_dewpoint_f", "pressure",
              "pm1_0_cf_1","pm2_5_cf_1", "pm10_0_cf_1")
# Set the Working Directory
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/purpleair_office indoor outdoor/PA 74E9_outdoor 1")
# make an empty data frame to store the targeted value
purple <- data.frame(matrix(ncol = 8))
colnames(purple) <- impcolum
# loop through list of csv files
for (i in purplefile) {
  file <- read_csv(i, col_names = TRUE, quote = "") # read in each file, including column names
  file2 <- subset(file, select = impcolum) # select column of interest
  purple<- rbind(purple, file2) #add subsetted data frame to dataframe 
}
# remove null values from dataframe
purple<- na.omit(purple)
view(purple)
write.csv(purple,"~/Desktop/PA 74E9_outdoor 1.csv", 
          row.names = TRUE)



# ========================================
# 20240930
# Creating a multiple area chart https://biostats.w.uib.no/creating-a-multiple-area-chart/
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "EPAsite.csv")
View(data)
summary(data)
df <- data.frame(data)
df
# creat datetime
library(dplyr)
df_r1 <- df %>% 
  mutate(date = as.Date(Date))
View(df_r1)

# PM2.5 
# http://www.sthda.com/english/wiki/colors-in-r
# https://r-charts.com/color-palettes/?utm_content=cmp-true
# https://www.spsanderson.com/steveondata/posts/2024-04-03/index.html
# https://www.r-bloggers.com/2016/12/animating-plots-of-beer-ingredients-and-sin-taxes-over-time/#google_vignette
# https://cran.rstudio.com/web/packages/AquaBEHER/vignettes/AquaBEHER.html
# http://www.sthda.com/english/wiki/ggplot2-add-straight-lines-to-a-plot-horizontal-vertical-and-regression-lines
# https://ggplot2.tidyverse.org/reference/geom_abline.html
# https://allancameron.github.io/geomtextpath/reference/geom_textabline.html
# https://cran.r-project.org/web/packages/geomtextpath/readme/README.html
library(geomtextpath)
jpeg("timeseries PM10.jpg", width = 9, height = 5.5, units = 'in', res = 600)
pm10 <- ggplot(df_r1, aes(x = date, y = PM10, fill = Type, color = Type)) +
  geom_line(alpha = 1, show.legend = TRUE) + 
  geom_texthline(yintercept = 15, color = "red", linetype = "dashed", 
                 label = "WHO AQG Annual Limit value", hjust = 0.1, vjust = -0.2) + 
  geom_texthline(yintercept = 45, color = "darkgray", linetype = "dotted", 
                 label = "WHO AQG 24h Limit value", hjust = 0.1) +  
  scale_color_manual(values = c("Traffic" = "#FFC000", "Suburban" = "#5B9BD5", "Urban background" = "#70AD47")) + 
  scale_fill_manual(values = c("#FFC000", "#5B9BD5","#70AD47")) + 
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b-%Y", position = "bottom") + 
  theme(legend.position = "bottom") + 
  labs(x = "Date", y = expression(Concentration~(ug/m^3)), 
       title = expression(Time~series~of~reference~PM[10])) 
pm10
dev.off() 

jpeg("timeseries PM2.5.jpg", width = 9, height = 5.5, units = 'in', res = 600)
pm2.5 <- ggplot(df_r1, aes(x = date, y = PM2.5, fill = Type, color = Type)) +
  geom_line(alpha = 1, show.legend = TRUE) + 
  geom_texthline(yintercept = 5, color = "red", linetype = "dashed", 
                 label = "WHO AQG Annual Limit value", hjust = 0.1, vjust = -0.2) + 
  geom_texthline(yintercept = 15, color = "darkgray", linetype = "dotted", 
                 label = "WHO AQG 24h Target value", hjust = 0.1) +  
  scale_color_manual(values = c("Traffic" = "#E6B729", "Suburban" = "#2683C6", "Urban background" = "#3E8533")) + 
  scale_fill_manual(values = c("#E6B729", "#2683C6","#3E8533")) + 
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b-%Y", position = "bottom") + 
  theme(legend.position = "bottom") + 
  labs(x = "Date", y = expression(Concentration~(ug/m^3)), 
       title = expression(Time~series~of~reference~PM[2.5])) 
pm2.5
dev.off() 
# plot combined figure
library(ggpubr)
jpeg("timeseries pm2.5 pm10_r.jpg", width = 9, height = 10, units = 'in', res = 600)
ggarrange(pm2.5, pm10,  
          labels = c("a", "b"),
          ncol = 1,
          nrow = 2)
dev.off() 


# 20241001
# creat a date sequence
library(lubridate)
library(tidyverse)
# creat new column named datetime
# https://ehsanx.github.io/intro2R/date-time-data-with-lubridate.html
Cdatetime <- seq(ymd_h("2023-06-01-0"), ymd_h("2024-06-20-23"), by = "hours")
Cdatetime
write.csv(Cdatetime,"~/Desktop/ProjectDatetime.csv", 
          row.names = TRUE)
#===============
#https://www.statology.org/combine-date-and-time-in-r/#:~:text=POSIXct()%20functions%20from%20base,a%20new%20column%20named%20datetime.
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "C379IndoorAh_r.csv")
View(data)
summary(data)
df <-data.frame(data)
library(lubridate)
Pdatetime <- as.POSIXlt(paste(df$dateonly, df$datetime), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
Pdatetime

df <- mutate(df, Pdatetime)
View(df)
write.csv(df,"~/Desktop/C379IndoorAh_r.csv", 
          row.names = TRUE)

#===============
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "FADDIndoorAh_r.csv")
View(data)
summary(data)
df <-data.frame(data)

cbdatatime <- as.POSIXlt(paste(df$dateonly, df$timevalue), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
cbdatatime 

df <- mutate(df, cbdatatime)
View(df)
write.csv(df,"~/Desktop/FADDIndoorAh_r.csv", 
          row.names = TRUE)
#===============

setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "2861IndoorBh_r.csv")
View(data)
summary(data)
df <-data.frame(data)

cbdatatime <- as.POSIXlt(paste(df$Date, df$timevalue), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
cbdatatime 

df <- mutate(df, cbdatatime)
View(df)
write.csv(df,"~/Desktop/2861IndoorBh_r.csv", 
          row.names = TRUE)
#===============
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "036AIndoorBh_r.csv")
View(data)
summary(data)
df <-data.frame(data)

cbdatatime <- as.POSIXlt(paste(df$Date, df$timevalue), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
cbdatatime 

df <- mutate(df, cbdatatime)
View(df)
write.csv(df,"~/Desktop/2861IndoorBh_r.csv", 
          row.names = TRUE)
#===============
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "790DOutdoorh_r.csv")
View(data)
summary(data)
df <-data.frame(data)

Pdatatime <- as.POSIXlt(paste(df$dateonly, df$timevalue), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
Pdatatime 

df <- mutate(df, Pdatatime)
View(df)
write.csv(df,"~/Desktop/790DOutdoorh_r.csv", 
          row.names = TRUE)
#===============
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "8278PAOutdoorh_r.csv")
View(data)
summary(data)
df <-data.frame(data)

Pdatetime <- as.POSIXlt(paste(df$date, df$time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
Pdatetime 

df <- mutate(df, Pdatetime)
View(df)
write.csv(df,"~/Desktop/8278PAOutdoorh_r.csv", 
          row.names = TRUE)

#===============
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "OBAFCO2h_r.csv")
View(data)
summary(data)
df <-data.frame(data)

Pdatetime <- as.POSIXlt(paste(df$dateonly, df$datetime), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
Pdatetime 

df <- mutate(df, Pdatetime)
View(df)
write.csv(df,"~/Desktop/OBAFCO2h_r.csv", 
          row.names = TRUE)
#===============


## join the data 
## https://www.statology.org/cbind-in-r/
## https://datatofish.com/merge-two-dataframes-r/
# hourly data from indoor A
library(dplyr)
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data1 = read.csv(file = "ProjectDatetime.csv")
View(data1)
# Indoor A
data2a = read.csv(file = "C379IndoorAh_r.csv")
View(data2a)
data3a = read.csv(file = "FADDIndoorAh_r.csv")
data4a = read.csv(file = "F72BPAIndoorAh_r.csv")

# Indoor B
data2b = read.csv(file = "2861IndoorBh_r.csv")
data3b = read.csv(file = "036AIndoorBh_r.csv")

# Outdoor
data2 = read.csv(file = "8278PAOutdoorh_r.csv")
data3 = read.csv(file = "790DOutdoorh_rr.csv")
data4 = read.csv(file = "ambientclonskeagh.csv")

# Indoor B CO2
data5 = read.csv(file = "OBAFCO2h_r.csv")

df1 = data.frame(data1)
df2a = data.frame(data2a)
View(df2a)
df3a = data.frame(data3a)
df4a = data.frame(data4a)
df2b = data.frame(data2b)
df3b = data.frame(data3b)
df2 = data.frame(data2)
df3 = data.frame(data3)
View(df3)
df4 = data.frame(data4)
df5 = data.frame(data5)

# creat a dataframe for df2
# seperate the date hour min and second
library(lubridate)
library(tidyverse)
#check timezone 
Sys.timezone()

#Turorial: https://users.ssc.wisc.edu/~hemken/Rworkshops/dwr/merge-tidy.html
df2ar <- left_join(df1, df2a, by = "Pdatetime")
View(df2ar)
write.csv(df2ar,"~/Desktop/C379IndoorAh_r1.csv", 
          row.names = TRUE)
df3ar <- left_join(df1, df3a, by = "Pdatetime")
View(df3ar)
write.csv(df3ar,"~/Desktop/FADDIndoorAh_r1.csv", 
          row.names = TRUE)
df4ar <- left_join(df1, df4a, by = "Pdatetime")
View(df4ar)
write.csv(df4ar,"~/Desktop/F72BPAIndoorAh_r1.csv", 
          row.names = TRUE)
df2br <- left_join(df1, df2b, by = "Pdatetime")
View(df2br)
write.csv(df2br,"~/Desktop/2861IndoorBh_r1.csv", 
          row.names = TRUE)
df3br <- left_join(df1, df3b, by = "Pdatetime")
View(df3br)
write.csv(df3br,"~/Desktop/036AIndoorBh_r1.csv", 
          row.names = TRUE)

df2r <- left_join(df1, df2, by = "Pdatetime")
View(df2r)
write.csv(df2r,"~/Desktop/8278PAOutdoorh_r1.csv", 
          row.names = TRUE)
df3r <- left_join(df1, df3, by = "Pdatetime")
View(df3r)
write.csv(df3r,"~/Desktop/790DOutdoorh_r1.csv", 
          row.names = TRUE)
df4r <- left_join(df1, df4, by = "Pdatetime")
View(df4r)
write.csv(df4r,"~/Desktop/ambientclonskeagh_r1.csv", 
          row.names = TRUE)
df5r <- left_join(df1, df5, by = "Pdatetime")
View(df5r)
write.csv(df5r,"~/Desktop/OBAFCO2h_r1.csv", 
          row.names = TRUE)

# ================================================
library(ggplot2)
library(dplyr)
# plot with multiple lines: https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "ambient outdoor sck purpleair.csv")
View(data)
summary(data)
## solve the issue that "getting NA error when there is no NA in my dataset"
df01 <- data.frame(data)
df <- na.omit(df01)
df

## density plot
## q-q plot
# PM10
# load stuff
library(ggpubr)
library(fitdistrplus)
# plot the data with figure 
normal_dist <- fitdist(df$PM10_Clonskeagh, "norm")
plot(normal_dist)

ggdensity(df$PM10_Clonskeagh,
          main = "Density plot of ambient PM10 concentration",
          xlab = "PM10 concentration (ug/m3)")
ggqqplot(df$PM10_Clonskeagh)
library(nortest)
ad.test(data$PM10_Clonskeagh)
# A = 428.26, p-value < 2.2e-16
# A p-value > 0.05 means the null hypothesis (that the distribution is normal) is accepted. 
# A p-value < 0.05 means that the null hypothesis is rejected and the distribution is not normal.
#  https://r02pro.github.io/normal-distribution.html

# data with datas, the column 'date' is of class "Date"
## solve the issue that "getting NA error when there is no NA in my dataset"
df01 <- data.frame(data)
dfr <- na.omit(df01)
dfr
dfr <- data[data$Date >as.Date("2023-06-01"),]
# daily ambient PM10
dAmbientPM10 <- dfr %>% group_by(Date, Day) %>%
  summarise(Median = median(PM10_Clonskeagh, na.rm = TRUE))
dAmbientPM10
# daily outdoor PM10_SCK
dOutdoorPM10SCK <- dfr %>% group_by(Date, Day) %>%
  summarise(Median = median(OutdoorPM10_SCK, na.rm = TRUE))
dOutdoorPM10SCK
# daily outdoor PM10_PA
dOutdoorPM10PA <- dfr %>% group_by(Date, Day) %>%
  summarise(Median = median(OutdoorPM10_PA, na.rm = TRUE))
dOutdoorPM10PA

# daily ambient PM2.5
dAmbientPM2.5 <- dfr %>% group_by(Date, Day) %>%
  summarise(Median = median(PM2.5_Clonskeagh, na.rm = TRUE))
dAmbientPM2.5

# daily outdoor PM2.5_SCK
dOutdoorPM2.5SCK <- dfr %>% group_by(Date, Day) %>%
  summarise(Median = median(OutdoorPM2.5_SCK, na.rm = TRUE))
dOutdoorPM2.5SCK

# daily outdoor PM2.5_PA
dOutdoorPM2.5PA <- dfr %>% group_by(Date, Day) %>%
  summarise(Median = median(OutdoorPM2.5_PA, na.rm = TRUE))
dOutdoorPM2.5PA


# daily outdoor PM1_SCK
dOutdoorPM1SCK <- dfr %>% group_by(Date, Day) %>%
  summarise(Median = median(OutdoorPM1_SCK, na.rm = TRUE))
dOutdoorPM1SCK

# daily outdoor PM1_PA
dOutdoorPM1PA <- dfr %>% group_by(Date, Day) %>%
  summarise(Median = median(OutdoorPM1_PA, na.rm = TRUE))
dOutdoorPM1PA


# dataframe
library(lubridate)
library(zoo)
df1 <- data.frame(dAmbientPM10, dOutdoorPM10SCK,dOutdoorPM10PA)
View(df1)
summary(df1)
df2 <- df1[, c("Date", "Median", "Median.1", "Median.2")]
colnames(df2) <- c('Date', 'AmbientPM10', 'OutdoorPM10_SCK', 'OutdoorPM10_PA')
colnames(df2)
view(df2)

# dataframe
df1r <- data.frame(dAmbientPM2.5, dOutdoorPM2.5SCK,dOutdoorPM2.5PA)
View(df1r)
df2r <- df1r[, c("Date", "Median", "Median.1", "Median.2")]
colnames(df2r) <- c('Date', 'AmbientPM2.5', 'OutdoorPM2.5_SCK', 'OutdoorPM2.5_PA')
colnames(df2r)
view(df2r)



# https://r4ds.had.co.nz/
# https://r4ds.hadley.nz/
# https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
# https://r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
# Change manually the appearance of lines: http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software
# https://finchstudio.io/blog/ggplot-dual-y-axes/
# https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
# linetype: solid, longdash, dashed, dotted
# Date preparation
library(tidyverse)
df3 <- df2 %>% 
  gather(key = "variable", value = "value", -Date)
view(df3)
# datetime
df3r <- df2r %>% 
  gather(key = "variable", value = "value", -Date)
view(df3r)


# visualization
jpeg("PM10 daily variation.jpg", width = 8, height = 5, units = 'in', res = 600)
PM10 <- ggplot(df3, aes(x = as.Date(Date), y = value)) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, size = 0.7) + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2023-06-01", "2024-06-01")), date_breaks = "1 month", position = "bottom") +
  scale_y_continuous() + 
  scale_color_manual(values = c("darkred", "steelblue", "palegreen3")) + 
  scale_linetype_manual(values = c("longdash", "solid", "solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Date", y = expression(PM[10]~concentration~(ug/m^3)), title = expression(PM[10])) + 
  theme(legend.position = c(0.9, 0.9), legend.title = element_blank())
PM10
dev.off() 

# visualization
jpeg("PM2.5 daily variation.jpg", width = 8, height = 5, units = 'in', res = 600)
PM2.5 <- ggplot(df3r, aes(x = as.Date(Date), y = value)) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, size = 0.7) + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2023-06-01", "2024-06-01")), date_breaks = "1 month", position = "bottom") +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %Y", position = "bottom") +
  scale_y_continuous() + 
  scale_color_manual(values = c("darkred", "steelblue", "palegreen3")) + 
  scale_linetype_manual(values = c("longdash", "solid", "solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Date", y = expression(PM[2.5]~concentration~(ug/m^3)), title = expression(PM[2.5])) + 
  theme(legend.position = c(0.9, 0.9), legend.title = element_blank())
PM2.5
dev.off() 

# scatter plot visualization
# http://www.sthda.com/english/wiki/scatter-plots-r-base-graphs
# https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html
# https://stackoverflow.com/questions/59018156/position-of-two-regression-equations-ggplot-r
jpeg("PM10 scatter SCK.jpg", width = 4, height = 5, units = 'in', res = 600)
PM10SCK <- ggplot(df2, aes(x= AmbientPM10, y = OutdoorPM10_SCK)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5), 
             na.rm = TRUE, color = "steelblue") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="black", fill="lightblue") + 
  stat_regline_equation(label.y = 80, aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~"))) +
  labs(x = expression(Ambient~PM[10]~concentration~(ug/m^3)),
       y = expression(Outdoor~PM[10]~concentration~(ug/m^3)), 
       title = expression(Outdoor~PM[10]~SCK~vs~Ambient~PM[10])) 
PM10SCK
dev.off() 
#
jpeg("PM10 scatter PA.jpg", width = 4, height = 5, units = 'in', res = 600)
PM10PA<- ggplot(df2, aes(x= AmbientPM10, y = OutdoorPM10_PA)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5), 
             na.rm = TRUE, color = "steelblue") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="black", fill="lightblue") + 
  stat_regline_equation(label.y = 80, aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~"))) +
  labs(x = expression(Ambient~PM[10]~concentration~(ug/m^3)),
       y = expression(Outdoor~PM[10]~concentration~(ug/m^3)), 
       title = expression(Outdoor~PM[10]~PA~vs~Ambient~PM[10])) 
PM10PA
dev.off() 

######
jpeg("PM2.5 scatter SCK.jpg", width = 4, height = 5, units = 'in', res = 600)
PM2.5SCK <- ggplot(df2r, aes(x= AmbientPM2.5, y = OutdoorPM2.5_SCK)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5), 
             na.rm = TRUE, color = "palegreen3") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="black", fill="lightgreen") + 
  stat_regline_equation(label.y = 70, aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~"))) +
  labs(x = expression(Ambient~PM[2.5]~concentration~(ug/m^3)),
       y = expression(Outdoor~PM[2.5]~concentration~(ug/m^3)), 
       title = expression(Outdoor~PM[2.5]~SCK~vs~Ambient~PM[2.5])) 
PM2.5SCK
dev.off() 
#
jpeg("PM2.5 scatter PA.jpg", width = 4, height = 5, units = 'in', res = 600)
PM2.5PA<- ggplot(df2r, aes(x= AmbientPM2.5, y = OutdoorPM2.5_PA)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5), 
             na.rm = TRUE, color = "palegreen3") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="black", fill="lightgreen") + 
  stat_regline_equation(label.y = 70, aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~"))) +
  labs(x = expression(Ambient~PM[2.5]~concentration~(ug/m^3)),
       y = expression(Outdoor~PM[2.5]~concentration~(ug/m^3)), 
       title = expression(Outdoor~PM[2.5]~PA~vs~Ambient~PM[2.5])) 
PM2.5PA
dev.off() 

# different display of figures
jpeg("time series and scatter.jpg", width = 25, height = 14, units = 'in', res = 600)
ggarrange(PM2.5, PM2.5SCK, PM2.5PA, PM10, PM10SCK, PM10PA, 
          labels = c("a", "b", "c", "d", "e", "f"),
          ncol = 3,
          nrow = 2)
dev.off() 


# # # # # # # # # # # # # # 
# # # # # # # # # # # # # # 
# Mean
# data with datas, the column 'date' is of class "Date"
## solve the issue that "getting NA error when there is no NA in my dataset"
df01 <- data.frame(data)
dfr <- na.omit(df01)
dfr
dfr <- data[data$Date >as.Date("2023-06-01"),]
# daily ambient PM10
dAmbientPM10 <- dfr %>% group_by(Date, Day) %>%
  summarise(Mean = mean(PM10_Clonskeagh, na.rm = TRUE))
dAmbientPM10
# daily outdoor PM10_SCK
dOutdoorPM10SCK <- dfr %>% group_by(Date, Day) %>%
  summarise(Mean = mean(OutdoorPM10_SCK, na.rm = TRUE))
dOutdoorPM10SCK
# daily outdoor PM10_PA
dOutdoorPM10PA <- dfr %>% group_by(Date, Day) %>%
  summarise(Mean = mean(OutdoorPM10_PA, na.rm = TRUE))
dOutdoorPM10PA

# daily ambient PM2.5
dAmbientPM2.5 <- dfr %>% group_by(Date, Day) %>%
  summarise(Mean = mean(PM2.5_Clonskeagh, na.rm = TRUE))
dAmbientPM2.5

# daily outdoor PM2.5_SCK
dOutdoorPM2.5SCK <- dfr %>% group_by(Date, Day) %>%
  summarise(Mean = mean(OutdoorPM2.5_SCK, na.rm = TRUE))
dOutdoorPM2.5SCK

# daily outdoor PM2.5_PA
dOutdoorPM2.5PA <- dfr %>% group_by(Date, Day) %>%
  summarise(Mean = mean(OutdoorPM2.5_PA, na.rm = TRUE))
dOutdoorPM2.5PA

# daily outdoor PM1_SCK
dOutdoorPM1SCK <- dfr %>% group_by(Date, Day) %>%
  summarise(Mean = mean(OutdoorPM1_SCK, na.rm = TRUE))
dOutdoorPM1SCK

# daily outdoor PM1_PA
dOutdoorPM1PA <- dfr %>% group_by(Date, Day) %>%
  summarise(Mean = mean(OutdoorPM1_PA, na.rm = TRUE))
dOutdoorPM1PA

# daily outdoor PM2.5_PA ALT
dOutdoorPM2.5PAALT <- dfr %>% group_by(Date, Day) %>%
  summarise(Mean = mean(OutdoorPM2.5_PA.ALT, na.rm = TRUE))
dOutdoorPM2.5PAALT


# dataframe
library(lubridate)
library(zoo)
df1 <- data.frame(dAmbientPM10, dOutdoorPM10SCK,dOutdoorPM10PA)
View(df1)
summary(df1)
df2 <- df1[, c("Date", "Mean", "Mean.1", "Mean.2")]
colnames(df2) <- c('Date', 'AmbientPM10', 'OutdoorPM10_SCK', 'OutdoorPM10_PA')
colnames(df2)
view(df2)

# dataframe
df1r <- data.frame(dAmbientPM2.5, dOutdoorPM2.5SCK, dOutdoorPM2.5PA, dOutdoorPM2.5PAALT)
View(df1r)
df2r <- df1r[, c("Date", "Mean", "Mean.1", "Mean.2", "Mean.3")]
colnames(df2r) <- c('Date', 'AmbientPM2.5', 'OutdoorPM2.5_SCK', 'OutdoorPM2.5_PA', 'OutdoorPM2.5_PAALT')
colnames(df2r)
view(df2r)


# https://r4ds.had.co.nz/
# https://r4ds.hadley.nz/
# https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
# https://r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
# Change manually the appearance of lines: http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software
# https://finchstudio.io/blog/ggplot-dual-y-axes/
# https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
# linetype: solid, longdash, dashed, dotted
# Date preparation
library(tidyverse)
df3 <- df2 %>% 
  gather(key = "variable", value = "value", -Date)
view(df3)
# datetime
df3r <- df2r %>% 
  gather(key = "variable", value = "value", -Date)
view(df3r)


# visualization
jpeg("PM10 daily variation.jpg", width = 8, height = 5, units = 'in', res = 600)
PM10 <- ggplot(df3, aes(x = as.Date(Date), y = value)) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, size = 0.7) + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2023-06-01", "2024-06-01")), date_breaks = "1 month", position = "bottom") +
  scale_y_continuous() + 
  scale_color_manual(values = c("darkred", "steelblue", "palegreen3")) + 
  scale_linetype_manual(values = c("longdash", "solid", "solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Date", y = expression(PM[10]~concentration~(ug/m^3)), title = expression(PM[10])) + 
  theme(legend.position = c(0.9, 0.9), legend.title = element_blank()) 
PM10
dev.off() 

# visualization
jpeg("PM2.5 daily variation_r.jpg", width = 8, height = 5, units = 'in', res = 600)
PM2.5 <- ggplot(df3r, aes(x = as.Date(Date), y = value)) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, size = 0.7) + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2023-06-01", "2024-06-01")), date_breaks = "1 month", position = "bottom") +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %Y", position = "bottom") +
  scale_y_continuous() + 
  scale_color_manual(values = c("darkred", "steelblue", "#E69F00","palegreen3")) + 
  scale_linetype_manual(values = c("longdash", "solid", "solid","solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Date", y = expression(PM[2.5]~concentration~(ug/m^3)), title = expression(PM[2.5])) + 
  theme(legend.position = c(0.9, 0.9), legend.title = element_blank())
PM2.5
dev.off() 

# scatter plot visualization
# http://www.sthda.com/english/wiki/scatter-plots-r-base-graphs
# https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html
# https://stackoverflow.com/questions/59018156/position-of-two-regression-equations-ggplot-r
# https://cran.r-project.org/web/packages/ggpmisc/vignettes/model-based-annotations.html
# https://stackoverflow.com/questions/74753843/how-to-add-text-combined-with-indicators-r2-rsme-and-n-to-a-plot-or-to-ggplot
#Creat label
model <- lm(OutdoorPM10_SCK~AmbientPM10, data = df2)
ss <- summary(model)
mylabel <- c(paste("RSME ==", round(sqrt(mean(ss$residuals^2)), 2)), 
             paste("n == ", nrow(df)),
             paste("RSME ==", round(mean(ss$residuals^2), 2)))
# add rmse = https://stackoverflow.com/questions/63741925/use-npc-units-in-annotate
jpeg("PM10 scatter SCK.jpg", width = 4, height = 5, units = 'in', res = 600)
PM10SCK <- ggplot(df2, aes(x= AmbientPM10, y = OutdoorPM10_SCK)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5), 
             na.rm = TRUE, color = "palegreen3") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="black", fill="lightgreen") + 
  stat_regline_equation(label.y = 80, aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~"))) + 
  annotate("text", x = 7, y = 75, label = paste("RMSE =", round(mean(ss$residuals^2), 1))) + 
  labs(x = expression(Ambient~PM[10]~concentration~(ug/m^3)),
       y = expression(Outdoor~PM[10]~concentration~(ug/m^3)), 
       title = expression(Outdoor~PM[10]~SCK~vs~Ambient~PM[10])) 
  #  theme_bw()
PM10SCK
dev.off() 
#
#Creat label
model <- lm(OutdoorPM10_PA~AmbientPM10, data = df2)
ssPA <- summary(model)
mylabel <- c(paste("RSME ==", round(sqrt(mean(ssPA$residuals^2)), 2)), 
             paste("n == ", nrow(df)),
             paste("RSME ==", round(mean(ssPA$residuals^2), 2)))
jpeg("PM10 scatter PA.jpg", width = 4, height = 5, units = 'in', res = 600)
PM10PA<- ggplot(df2, aes(x= AmbientPM10, y = OutdoorPM10_PA)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5), 
             na.rm = TRUE, color = "steelblue") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="black", fill="lightblue") + 
  stat_regline_equation(label.y = 80, aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~"))) + 
  annotate("text", x = 7, y = 75, label = paste("RMSE =", round(mean(ssPA$residuals^2), 1))) + 
  labs(x = expression(Ambient~PM[10]~concentration~(ug/m^3)),
       y = expression(Outdoor~PM[10]~concentration~(ug/m^3)), 
       title = expression(Outdoor~PM[10]~PA~vs~Ambient~PM[10])) 
PM10PA
dev.off() 

######
model <- lm(OutdoorPM2.5_SCK~AmbientPM2.5, data = df2r)
ssPM2.5 <- summary(model)
jpeg("PM2.5 scatter SCK.jpg", width = 4, height = 5, units = 'in', res = 600)
PM2.5SCK <- ggplot(df2r, aes(x= AmbientPM2.5, y = OutdoorPM2.5_SCK)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5), 
             na.rm = TRUE, color = "palegreen3") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="black", fill="lightgreen") + 
  stat_regline_equation(label.y = 70, aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~"))) +   
  annotate("text", x = 5, y = 65, label = paste("RMSE =", round(mean(ssPA$residuals^2), 1))) + 
  labs(x = expression(Ambient~PM[2.5]~concentration~(ug/m^3)),
       y = expression(Outdoor~PM[2.5]~concentration~(ug/m^3)), 
       title = expression(Outdoor~PM[2.5]~SCK~vs~Ambient~PM[2.5])) 
PM2.5SCK
dev.off() 
#
model <- lm(OutdoorPM2.5_PA~AmbientPM2.5, data = df2r)
ssPAPM2.5 <- summary(model)
jpeg("PM2.5 scatter PA.jpg", width = 4, height = 5, units = 'in', res = 600)
PM2.5PA<- ggplot(df2r, aes(x= AmbientPM2.5, y = OutdoorPM2.5_PA)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5), 
             na.rm = TRUE, color = "steelblue") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="black", fill="lightblue") + 
  stat_regline_equation(label.y = 70, aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~"))) +   
  annotate("text", x = 5, y = 65, label = paste("RMSE =", round(mean(ssPAPM2.5$residuals^2), 1))) + 
  labs(x = expression(Ambient~PM[2.5]~concentration~(ug/m^3)),
       y = expression(Outdoor~PM[2.5]~concentration~(ug/m^3)), 
       title = expression(Outdoor~PM[2.5]~PA~vs~Ambient~PM[2.5])) 
PM2.5PA
dev.off() 
#
model <- lm(OutdoorPM2.5_PAALT~AmbientPM2.5, data = df2r)
ssPAALTPM2.5 <- summary(model)
ssPAALTPM2.5
jpeg("PM2.5 scatter PAALT.jpg", width = 4, height = 5, units = 'in', res = 600)
PM2.5PAALT<- ggplot(df2r, aes(x= AmbientPM2.5, y = OutdoorPM2.5_PAALT)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5), 
             na.rm = TRUE, color = "#E69F00") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="black", fill="gold1") + 
  stat_regline_equation(label.y = 40, aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~"))) + 
  labs(x = expression(Ambient~PM[2.5]~concentration~(ug/m^3)),
       y = expression(Outdoor~PM[2.5]~concentration~(ug/m^3)), 
       title = expression(Outdoor~PM[2.5]~PA~ALT~vs~Ambient~PM[2.5])) 
PM2.5PAALT
dev.off() 



# different display of figures
jpeg("time series and scatter1r.jpg", width = 25, height = 14, units = 'in', res = 600)
ggarrange(PM2.5, PM2.5SCK, PM2.5PA, PM2.5PAALT, PM10, PM10SCK, PM10PA,
          labels = c("a", "b", "c", "d", "e", "f", "g"),
          ncol = 4,
          nrow = 2,
          widths = c(2,1,1,1))
dev.off() 

###################################################
###################################################
#https://epirhandbook.com/new_pages/ggplot_tips.html
# dataframe
library(lubridate)
library(zoo)
dfPM10 <- data.frame(data$Date, data$PM10_Clonskeagh, 
                  data$OutdoorPM10_SCK, data$OutdoorPM10_PA)
View(dfPM10)

df1r <- dfPM10[, c("data.Date", "data.PM10_Clonskeagh", "data.OutdoorPM10_SCK", "data.OutdoorPM10_PA")]
colnames(df1r) <- c('Date', 'AmbientPM10', 'OutdoorPM10_SCK', 'OutdoorPM10_PA')
view(df1r)

# Date preparation
library(tidyverse)
dfr1 <- df1r %>% 
  gather(key = "variable", value = "value", -Date)
view(dfr1)
jpeg("PM10 hourly variation.jpg", width = 8, height = 5, units = 'in', res = 600)
PM10 <- ggplot(dfr1, aes(x = as.Date(Date), y = value)) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, size = 0.7) + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2023-06-01", "2024-06-01")), date_breaks = "1 month", position = "bottom") +
  scale_y_continuous() + 
  scale_color_manual(values = c("darkred", "steelblue", "palegreen3")) + 
  scale_linetype_manual(values = c("longdash", "solid", "solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Date", y = expression(PM[10]~concentration~(ug/m^3)), title = expression(PM[10])) + 
  theme(legend.position = c(0.9, 0.9), legend.title = element_blank()) 
PM10
dev.off() 

# visualization
# https://tilburgsciencehub.com/topics/visualization/data-visualization/graphs-charts/time-series-ggplot2/
# https://rkabacoff.github.io/datavis/Time.html
dfPM2.5 <- data.frame(data$Date, data$PM2.5_Clonskeagh, 
                  data$OutdoorPM2.5_SCK, data$OutdoorPM2.5_PA, data$OutdoorPM2.5_PA.ALT)
View(dfPM2.5)

dfr2 <- dfPM2.5[, c("data.Date", "data.PM2.5_Clonskeagh", "data.OutdoorPM2.5_SCK", "data.OutdoorPM2.5_PA", "data.OutdoorPM2.5_PA.ALT")]
colnames(dfr2) <- c('Date', 'AmbientPM2.5', 'OutdoorPM2.5_SCK', 'OutdoorPM2.5_PA','OutdoorM2.5_PA ALT')
view(dfr2)
df2r <- dfr2 %>% 
  gather(key = "variable", value = "value", -Date)
view(df2r)

jpeg("PM2.5 hourly variation.jpg", width = 8, height = 5, units = 'in', res = 600)
PM2.5 <- ggplot(df2r, aes(x = as.Date(Date), y = value)) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, size = 0.7) + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2023-06-01", "2024-06-01")), date_breaks = "1 month", position = "bottom") +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %Y", position = "bottom") +
  scale_y_continuous() + 
  scale_color_manual(values = c("darkred", "steelblue", "#E69F00","palegreen3")) + 
  scale_linetype_manual(values = c("longdash", "solid", "solid","solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Date", y = expression(PM[2.5]~concentration~(ug/m^3)), title = expression(PM[2.5])) + 
  theme(legend.position = c(0.9, 0.9), legend.title = element_blank())
PM2.5
dev.off() 

jpeg("time series hourly.jpg", width = 10, height = 8, units = 'in', res = 600)
ggarrange(PM2.5, PM10,
          labels = c("a", "b"),
          ncol = 1,
          nrow = 2)
dev.off() 


# plot hour scatter plot
library(dplyr)
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "ambient outdoor sck purpleair.csv")
view(data)
## solve the issue that "getting NA error when there is no NA in my dataset"
df <- data.frame(data)
dfr <- na.omit(df)
dfr
#
model <- lm(OutdoorPM10_SCK~PM10_Clonskeagh, data = dfr)
ssSCK <- summary(model)
jpeg("hPM10 scatter SCK.jpg", width = 4, height = 5, units = 'in', res = 600)
hPM10SCK <- ggplot(dfr, aes(x= PM10_Clonskeagh, y = OutdoorPM10_SCK)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5), 
             na.rm = TRUE, color = "palegreen3") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="black", fill="lightgreen") + 
  stat_regline_equation(label.y = 150, aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~"))) + 
  annotate("text", x = 25, y = 140, label = paste("RMSE =", round(mean(ssSCK$residuals^2), 1))) + 
  labs(x = expression(Ambient~PM[10]~concentration~(ug/m^3)),
       y = expression(Outdoor~PM[10]~concentration~(ug/m^3)), 
       title = expression(Outdoor~PM[10]~SCK~vs~Ambient~PM[10])) 
#  theme_bw()
hPM10SCK
dev.off() 
#
#Creat label
model <- lm(OutdoorPM10_PA~PM10_Clonskeagh, data = dfr)
ssPA <- summary(model)
mylabel <- c(paste("RSME ==", round(sqrt(mean(ssPA$residuals^2)), 2)), 
             paste("n == ", nrow(df)),
             paste("RSME ==", round(mean(ssPA$residuals^2), 2)))
jpeg("hPM10 scatter PA.jpg", width = 4, height = 5, units = 'in', res = 600)
hPM10PA<- ggplot(dfr, aes(x= PM10_Clonskeagh, y = OutdoorPM10_PA)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5), 
             na.rm = TRUE, color = "steelblue") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="black", fill="lightblue") + 
  stat_regline_equation(label.y = 150, aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~"))) + 
  annotate("text", x = 25, y = 140, label = paste("RMSE =", round(mean(ssPA$residuals^2), 1))) + 
  labs(x = expression(Ambient~PM[10]~concentration~(ug/m^3)),
       y = expression(Outdoor~PM[10]~concentration~(ug/m^3)), 
       title = expression(Outdoor~PM[10]~PA~vs~Ambient~PM[10])) 
hPM10PA
dev.off() 

######
model <- lm(OutdoorPM2.5_SCK~PM2.5_Clonskeagh, data = dfr)
ssPM2.5 <- summary(model)
jpeg("hPM2.5 scatter SCK.jpg", width = 4, height = 5, units = 'in', res = 600)
hPM2.5SCK <- ggplot(dfr, aes(x= PM2.5_Clonskeagh, y = OutdoorPM2.5_SCK)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5), 
             na.rm = TRUE, color = "palegreen3") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="black", fill="lightgreen") + 
  stat_regline_equation(label.y = 150, aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~"))) +   
  annotate("text", x = 25, y = 140, label = paste("RMSE =", round(mean(ssPA$residuals^2), 1))) + 
  labs(x = expression(Ambient~PM[2.5]~concentration~(ug/m^3)),
       y = expression(Outdoor~PM[2.5]~concentration~(ug/m^3)), 
       title = expression(Outdoor~PM[2.5]~SCK~vs~Ambient~PM[2.5])) 
hPM2.5SCK
dev.off() 
#
model <- lm(OutdoorPM2.5_PA~PM2.5_Clonskeagh, data = dfr)
ssPAPM2.5 <- summary(model)
jpeg("hPM2.5 scatter PA.jpg", width = 4, height = 5, units = 'in', res = 600)
hPM2.5PA<- ggplot(dfr, aes(x= PM2.5_Clonskeagh, y = OutdoorPM2.5_PA)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5), 
             na.rm = TRUE, color = "steelblue") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="black", fill="lightblue") + 
  stat_regline_equation(label.y = 150, aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~"))) +   
  annotate("text", x = 25, y = 140, label = paste("RMSE =", round(mean(ssPAPM2.5$residuals^2), 1))) + 
  labs(x = expression(Ambient~PM[2.5]~concentration~(ug/m^3)),
       y = expression(Outdoor~PM[2.5]~concentration~(ug/m^3)), 
       title = expression(Outdoor~PM[2.5]~PA~vs~Ambient~PM[2.5])) 
hPM2.5PA
dev.off() 
#
model <- lm(OutdoorPM2.5_PA.ALT~PM2.5_Clonskeagh, data = dfr)
ssPAALTPM2.5 <- summary(model)
ssPAALTPM2.5
jpeg("hPM2.5 scatter PAALT.jpg", width = 4, height = 5, units = 'in', res = 600)
hPM2.5PAALT<- ggplot(dfr, aes(x= PM2.5_Clonskeagh, y = OutdoorPM2.5_PA.ALT)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5), 
             na.rm = TRUE, color = "#E69F00") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="black", fill="gold1") + 
  stat_regline_equation(label.y = 100, aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~"))) +   
  annotate("text", x = 25, y = 90, label = paste("RMSE =", round(mean(ssPAPM2.5$residuals^2), 1))) + 
  labs(x = expression(Ambient~PM[2.5]~concentration~(µg/m^3)),
       y = expression(Outdoor~PM[2.5]~concentration~(µg/m^3)), 
       title = expression(Outdoor~PM[2.5]~PA~ALT~vs~Ambient~PM[2.5])) 
hPM2.5PAALT
dev.off() 

# different display of figures
jpeg("scatter hourly.jpg", width = 25, height = 14, units = 'in', res = 600)
ggarrange(hPM2.5SCK, hPM2.5PA, hPM2.5PAALT, hPM10SCK, hPM10PA,
          labels = c("a", "b", "c", "d", "e"),
          ncol = 3,
          nrow = 2,
          widths = c(1,1,1))
dev.off() 

############################################################
############################################################
# https://github.com/davidcarslaw/openairmaps/tree/master/inst/examples-shiny/01_polarmap
# https://www.ldeo.columbia.edu/~danielmw/code/MLR-Tutorial.html
# (An introduction to R for research) https://bookdown.org/rwnahhas/IntroToR/exclude-observations-with-missing-data.html
# Low-Cost PM Sensor Tutorial: https://nzimmerman-ubc.github.io/lcs-PM-demo/#Part_2:_US_EPA_Base_Testing_Metrics
#Create Awesome HTML Table with knitr::kable and kableExtra: https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Use_it_with_sparkline
# install packages #
if(!require(installr)) install.packages('installr')
if(!require(chron)) install.packages('chron')
library(chron)
if (!require(prodlim)) install.packages('prodlim')
library(prodlim)

if (!require(gtools)) install.packages('gtools')
library(gtools)

if (!require(caret)) install.packages('caret')
library(caret)


if (!require(openair)) install.packages('openair')
library(openair)

if (!require(kableExtra)) install.packages('kableExtra')
library(kableExtra)

if (!require(broom)) install.packages('broom')
library(broom)

if (!require(weathercan)) install.packages('weathercan')
library(weathercan)

if (!require(segmented)) install.packages('segmented')
library(segmented)

# load packages #
library(tidyverse)
library(installr)
library(dplyr)
library(ggplot2)
# seperate the date hour min and second
library(lubridate)
library(zoo)
# cut second to min
# prerequisites
library(data.table)
library(tibble)
library(xts)

### library statistical analysis
library(lme4)
library(lmerTest)
library(r2glmm)
library(Hmisc)


# 20241025
# view file #
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "ambient outdoor sck purpleair_hr.csv")
View(data)
summary(data)

# creat a dataframe
df <- data.frame(data)
summary(df)
view(df)
# check factor for PM2.5 or PM10
# select the columns in including in models PM2.5 SCK
library(dplyr)
df_cal_PM2.5 <- df[, c("PM2.5_Clonskeagh", "OutdoorPM2.5_SCK", "OutdoorPM2.5_PA", 
                         "OutdoorT_SCK", "OutdoorRH_SCK", "OutdoorT_PA", "OutdoorRH_PA")]
# keep complete cases for modeling
df_cal_PM2.5 <- df_cal_PM2.5[complete.cases(df_cal_PM2.5),]
# make index for splitting 80:20
# if (!require(caret)) install.packages('caret')
library(caret)
data_split_PM2.5 <- createDataPartition(df_cal_PM2.5$PM2.5_Clonskeagh, times =1, list = F, p = 0.8)
data_train_PM2.5 <- df_cal_PM2.5[data_split_PM2.5, ]
data_test_PM2.5 <- df_cal_PM2.5[-data_split_PM2.5, ]

### analysis-build models
model0 <- lm(PM2.5_Clonskeagh ~ OutdoorPM2.5_SCK + OutdoorT_SCK + OutdoorRH_SCK, data = data_train_PM2.5)
model1 <- lm(PM2.5_Clonskeagh ~ OutdoorPM2.5_SCK +  OutdoorRH_SCK, data = data_train_PM2.5)
model2 <- lm(PM2.5_Clonskeagh ~ OutdoorPM2.5_SCK +  OutdoorT_SCK, data = data_train_PM2.5)
model3 <- lm(PM2.5_Clonskeagh ~ OutdoorPM2.5_SCK, data = data_train_PM2.5)
model4 <- segmented(lm(PM2.5_Clonskeagh ~ OutdoorPM2.5_SCK, data = data_train_PM2.5), 
                    seg.Z = ~OutdoorPM2.5_SCK, psi = NA, control = seg.control(K =1))
model5 <- segmented(lm(PM2.5_Clonskeagh ~ OutdoorPM2.5_SCK + OutdoorRH_SCK, data = data_train_PM2.5), 
                    seg.Z = ~OutdoorPM2.5_SCK, psi = NA, control = seg.control(K =1))
model6 <- segmented(lm(PM2.5_Clonskeagh ~ OutdoorPM2.5_SCK + OutdoorT_SCK, data = data_train_PM2.5), 
                    seg.Z = ~OutdoorPM2.5_SCK, psi = NA, control = seg.control(K =1))
## predict the testing data 
predict_PM2.5model.0 <- predict(model0,data_test_PM2.5)
predict_PM2.5model.1 <- predict(model1,data_test_PM2.5)
predict_PM2.5model.2 <- predict(model2,data_test_PM2.5)
predict_PM2.5model.3 <- predict(model3,data_test_PM2.5)
predict_PM2.5model.4 <- predict(model4,data_test_PM2.5)
predict_PM2.5model.5 <- predict(model5,data_test_PM2.5)
predict_PM2.5model.6 <- predict(model6,data_test_PM2.5)

measured_test <- data_test_PM2.5 %>% pull(PM2.5_Clonskeagh)
##Store the data

df_cal_fit_PM2.5 <- data.frame(predict_PM2.5model.0, predict_PM2.5model.1, predict_PM2.5model.2, 
                               predict_PM2.5model.3, predict_PM2.5model.4, predict_PM2.5model.5, 
                               predict_PM2.5model.6, measured_test)
df_cal_fit_long <- df_cal_fit_PM2.5 %>% 
  pivot_longer(cols = starts_with("predict_PM2.5"), names_to = "model", 
               names_prefix = "predict_PM2.5 model", values_to = "predicted_PM2.5_test")
df_cal_fit_long <- setDT(df_cal_fit_long)

summary0 <- glance(model0)
summary1 <- glance(model1)
summary2 <- glance(model2)
summary3 <- glance(model3)
summary4 <- glance(model4)
summary5 <- glance(model5)
summary6 <- glance(model6)

summary_all_PM2.5models <- bind_rows(summary0, summary1, summary2, 
                                     summary3, summary4, summary5, summary6) %>%
  mutate(modelID = c("A", "B", "C", "D", "E", "F", "G"))
summary_all_PM2.5models

breakpt.4 <- summary.segmented(model4)$psi[2]
breakpt.5 <- summary.segmented(model5)$psi[2]
breakpt.6 <- summary.segmented(model6)$psi[2]
breakpts <- c(breakpt.4,breakpt.5,breakpt.6)
breakpts_ID <- c("Model E", "Model F", "Model G")
df_breakpoints <- data.frame(breakpts_ID,breakpts) 
df_breakpoints %>%
  kbl(caption="**Summary of breakpoint location (μg/m^3^)**",digits=2, col.names=c("Model ID","Breakpoint")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

######################################################################################################

# check fator
# select the columns in including in models
library(dplyr)
df_cal_PM10 <- df[, c("PM10_Clonskeagh", "OutdoorPM10_SCK", "OutdoorPM10_PA", 
                       "OutdoorT_SCK", "OutdoorRH_SCK", "OutdoorT_PA", "OutdoorRH_PA")]
# keep complete cases for modeling
df_cal_PM10 <- df_cal_PM10[complete.cases(df_cal_PM10),]
# make index for splitting 80:20
# if (!require(caret)) install.packages('caret')
library(caret)
data_split_PM10 <- createDataPartition(df_cal_PM10$PM10_Clonskeagh, times =1, list = F, p = 0.8)
data_train_PM10 <- df_cal_PM10[data_split_PM10, ]
data_test_PM10 <- df_cal_PM10[-data_split_PM10, ]

### analysis-build models
model0PM10 <- lm(PM10_Clonskeagh ~ OutdoorPM10_SCK + OutdoorT_SCK + OutdoorRH_SCK, data = data_train_PM10)
model1PM10 <- lm(PM10_Clonskeagh ~ OutdoorPM10_SCK +  OutdoorRH_SCK, data = data_train_PM10)
model2PM10 <- lm(PM10_Clonskeagh ~ OutdoorPM10_SCK +  OutdoorT_SCK, data = data_train_PM10)
model3PM10 <- lm(PM10_Clonskeagh ~ OutdoorPM10_SCK, data = data_train_PM10)
model4PM10 <- segmented(lm(PM10_Clonskeagh ~ OutdoorPM10_SCK, data = data_train_PM10), 
                    seg.Z = ~OutdoorPM10_SCK, psi = NA, control = seg.control(K =1))
model5PM10 <- segmented(lm(PM10_Clonskeagh ~ OutdoorPM10_SCK + OutdoorRH_SCK, data = data_train_PM10), 
                    seg.Z = ~OutdoorPM10_SCK, psi = NA, control = seg.control(K =1))
model6PM10 <- segmented(lm(PM10_Clonskeagh ~ OutdoorPM10_SCK + OutdoorT_SCK, data = data_train_PM10), 
                    seg.Z = ~OutdoorPM10_SCK, psi = NA, control = seg.control(K =1))
## predict the testing data 
predict_PM10model.0 <- predict(model0PM10,data_test_PM10)
predict_PM10model.1 <- predict(model1PM10,data_test_PM10)
predict_PM10model.2 <- predict(model2PM10,data_test_PM10)
predict_PM10model.3 <- predict(model3PM10,data_test_PM10)
predict_PM10model.4 <- predict(model4PM10,data_test_PM10)
predict_PM10model.5 <- predict(model5PM10,data_test_PM10)
predict_PM10model.6 <- predict(model6PM10,data_test_PM10)

measured_testPM10 <- data_test_PM10 %>% pull(PM10_Clonskeagh)
##Store the data

df_cal_fit_PM10 <- data.frame(predict_PM10model.0, predict_PM10model.1, predict_PM10model.2, 
                               predict_PM10model.3, predict_PM10model.4, predict_PM10model.5, 
                               predict_PM10model.6, measured_testPM10)
df_cal_fit_long_PM10 <- df_cal_fit_PM10 %>% 
  pivot_longer(cols = starts_with("predict_PM10"), names_to = "model", 
               names_prefix = "predict_PM10 model", values_to = "predicted_PM10_test")
df_cal_fit_long_PM10 <- setDT(df_cal_fit_long_PM10)

summary0PM10 <- glance(model0PM10)
summary1PM10 <- glance(model1PM10)
summary2PM10 <- glance(model2PM10)
summary3PM10 <- glance(model3PM10)
summary4PM10 <- glance(model4PM10)
summary5PM10 <- glance(model5PM10)
summary6Pm10 <- glance(model6PM10)

summary_all_PM10models <- bind_rows(summary0PM10, summary1PM10, summary2PM10, 
                                     summary3PM10, summary4PM10, summary5PM10, summary6Pm10) %>%
  mutate(modelID = c("A", "B", "C", "D", "E", "F", "G"))
summary_all_PM10models

breakpt.4PM10 <- summary.segmented(model4PM10)$psi[2]
breakpt.5PM10 <- summary.segmented(model5PM10)$psi[2]
breakpt.6PM10 <- summary.segmented(model6PM10)$psi[2]
breakptsPM10 <- c(breakpt.4PM10,breakpt.5PM10,breakpt.6PM10)
breakpts_ID <- c("Model E", "Model F", "Model G")
df_breakpointsPM10 <- data.frame(breakpts_ID,breakptsPM10) 
df_breakpointsPM10 %>%
  kbl(caption="**Summary of breakpoint location (μg/m^3^)**",digits=2, col.names=c("Model ID","Breakpoint")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

####################################################################################################

# check factor for PM2.5
# select the columns in including in models PM2.5 PurpleAir
library(dplyr)
df_cal_PM2.5PA <- df[, c("PM2.5_Clonskeagh", "OutdoorPM2.5_SCK", "OutdoorPM2.5_PA", 
                       "OutdoorT_SCK", "OutdoorRH_SCK", "OutdoorT_PA", "OutdoorRH_PA")]
# keep complete cases for modeling
df_cal_PM2.5PA <- df_cal_PM2.5PA[complete.cases(df_cal_PM2.5PA),]
# make index for splitting 80:20
# if (!require(caret)) install.packages('caret')
library(caret)
data_split_PM2.5PA <- createDataPartition(df_cal_PM2.5PA$PM2.5_Clonskeagh, times =1, list = F, p = 0.8)
data_train_PM2.5PA <- df_cal_PM2.5PA[data_split_PM2.5PA, ]
data_test_PM2.5PA <- df_cal_PM2.5PA[-data_split_PM2.5PA, ]

### analysis-build models
model0PA <- lm(PM2.5_Clonskeagh ~ OutdoorPM2.5_PA + OutdoorT_PA + OutdoorRH_PA, data = data_train_PM2.5PA)
model1PA <- lm(PM2.5_Clonskeagh ~ OutdoorPM2.5_PA +  OutdoorRH_PA, data = data_train_PM2.5PA)
model2PA <- lm(PM2.5_Clonskeagh ~ OutdoorPM2.5_PA +  OutdoorT_PA, data = data_train_PM2.5PA)
model3PA <- lm(PM2.5_Clonskeagh ~ OutdoorPM2.5_PA, data = data_train_PM2.5PA)
model4PA <- segmented(lm(PM2.5_Clonskeagh ~ OutdoorPM2.5_PA, data = data_train_PM2.5PA), 
                    seg.Z = ~OutdoorPM2.5_PA, psi = NA, control = seg.control(K =1))
model5PA <- segmented(lm(PM2.5_Clonskeagh ~ OutdoorPM2.5_PA + OutdoorRH_PA, data = data_train_PM2.5PA), 
                    seg.Z = ~OutdoorPM2.5_PA, psi = NA, control = seg.control(K =1))
model6PA <- segmented(lm(PM2.5_Clonskeagh ~ OutdoorPM2.5_PA + OutdoorT_PA, data = data_train_PM2.5PA), 
                    seg.Z = ~OutdoorPM2.5_PA, psi = NA, control = seg.control(K =1), display = T)
## predict the testing data 
predict_PM2.5PAmodel.0 <- predict(model0PA,data_test_PM2.5PA)
predict_PM2.5PAmodel.1 <- predict(model1PA,data_test_PM2.5PA)
predict_PM2.5PAmodel.2 <- predict(model2PA,data_test_PM2.5PA)
predict_PM2.5PAmodel.3 <- predict(model3PA,data_test_PM2.5PA)
predict_PM2.5PAmodel.4 <- predict(model4PA,data_test_PM2.5PA)
predict_PM2.5PAmodel.5 <- predict(model5PA,data_test_PM2.5PA)
predict_PM2.5PAmodel.6 <- predict(model6PA,data_test_PM2.5PA)

measured_testPA <- data_test_PM2.5PA %>% pull(PM2.5_Clonskeagh)
##Store the data

df_cal_fit_PM2.5PA <- data.frame(predict_PM2.5PAmodel.0, predict_PM2.5PAmodel.1, predict_PM2.5PAmodel.2, 
                               predict_PM2.5PAmodel.3, predict_PM2.5PAmodel.4, predict_PM2.5PAmodel.5, 
                               predict_PM2.5PAmodel.6, measured_testPA)
df_cal_fit_longPA <- df_cal_fit_PM2.5PA %>% 
  pivot_longer(cols = starts_with("predict_PM2.5"), names_to = "model", 
               names_prefix = "predict_PM2.5 model", values_to = "predicted_PM2.5_test")
df_cal_fit_longPA <- setDT(df_cal_fit_longPA)

summary0PA <- glance(model0PA)
summary1PA <- glance(model1PA)
summary2PA <- glance(model2PA)
summary3PA <- glance(model3PA)
summary4PA <- glance(model4PA)
summary5PA <- glance(model5PA)
summary6PA <- glance(model6PA)

summary_all_PM2.5PAmodels <- bind_rows(summary0PA, summary1PA, summary2PA, 
                                     summary3PA, summary4PA, summary5PA, summary6PA) %>%
  mutate(modelID = c("A", "B", "C", "D", "E", "F", "G"))
summary_all_PM2.5PAmodels

breakpt.4PA <- summary.segmented(model4PA)$psi[2]
breakpt.5PA <- summary.segmented(model5PA)$psi[2]
breakpt.6PA <- summary.segmented(model6PA)$psi[2]
breakptsPA <- c(breakpt.4PA,breakpt.5PA,breakpt.6PA)
breakpts_ID <- c("Model E", "Model F", "Model G")
df_breakpointsPA <- data.frame(breakpts_ID,breakptsPA) 
df_breakpointsPA %>%
  kbl(caption="**Summary of breakpoint location (μg/m^3^)**",digits=2, col.names=c("Model ID","Breakpoint")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

######################################################################################################

# check factor PM10 PA
# select the columns in including in models
library(dplyr)
df_cal_PM10PA <- df[, c("PM10_Clonskeagh", "OutdoorPM10_SCK", "OutdoorPM10_PA", 
                      "OutdoorT_SCK", "OutdoorRH_SCK", "OutdoorT_PA", "OutdoorRH_PA")]
# keep complete cases for modeling
df_cal_PM10PA <- df_cal_PM10PA[complete.cases(df_cal_PM10PA),]
# make index for splitting 80:20
# if (!require(caret)) install.packages('caret')
library(caret)
data_split_PM10PA <- createDataPartition(df_cal_PM10PA$PM10_Clonskeagh, times =1, list = F, p = 0.8)
data_train_PM10PA <- df_cal_PM10PA[data_split_PM10PA, ]
data_test_PM10PA <- df_cal_PM10PA[-data_split_PM10PA, ]

### analysis-build models
model0PM10PA <- lm(PM10_Clonskeagh ~ OutdoorPM10_PA + OutdoorT_PA + OutdoorRH_PA, data = data_train_PM10PA)
model1PM10PA <- lm(PM10_Clonskeagh ~ OutdoorPM10_PA +  OutdoorRH_PA, data = data_train_PM10PA)
model2PM10PA <- lm(PM10_Clonskeagh ~ OutdoorPM10_PA +  OutdoorT_PA, data = data_train_PM10PA)
model3PM10PA <- lm(PM10_Clonskeagh ~ OutdoorPM10_PA, data = data_train_PM10PA)
model4PM10PA <- segmented(lm(PM10_Clonskeagh ~ OutdoorPM10_PA, data = data_train_PM10PA), 
                        seg.Z = ~OutdoorPM10_PA, psi = NA, control = seg.control(K =1))
model5PM10PA <- segmented(lm(PM10_Clonskeagh ~ OutdoorPM10_PA + OutdoorRH_PA, data = data_train_PM10PA), 
                        seg.Z = ~OutdoorPM10_PA, psi = NA, control = seg.control(K =1))
model6PM10PA <- segmented(lm(PM10_Clonskeagh ~ OutdoorPM10_PA + OutdoorT_PA, data = data_train_PM10PA), 
                        seg.Z = ~OutdoorPM10_PA, psi = NA, control = seg.control(K =1))
## predict the testing data 
predict_PM10PAmodel.0 <- predict(model0PM10PA,data_test_PM10PA)
predict_PM10PAmodel.1 <- predict(model1PM10PA,data_test_PM10PA)
predict_PM10PAmodel.2 <- predict(model2PM10PA,data_test_PM10PA)
predict_PM10PAmodel.3 <- predict(model3PM10PA,data_test_PM10PA)
predict_PM10PAmodel.4 <- predict(model4PM10PA,data_test_PM10PA)
predict_PM10PAmodel.5 <- predict(model5PM10PA,data_test_PM10PA)
predict_PM10PAmodel.6 <- predict(model6PM10PA,data_test_PM10PA)

measured_testPM10PA <- data_test_PM10PA %>% pull(PM10_Clonskeagh)
##Store the data

df_cal_fit_PM10PA <- data.frame(predict_PM10PAmodel.0, predict_PM10PAmodel.1, predict_PM10PAmodel.2, 
                              predict_PM10PAmodel.3, predict_PM10PAmodel.4, predict_PM10PAmodel.5, 
                              predict_PM10PAmodel.6, measured_testPM10PA)
df_cal_fit_long_PM10PA <- df_cal_fit_PM10PA %>% 
  pivot_longer(cols = starts_with("predict_PM10"), names_to = "model", 
               names_prefix = "predict_PM10 model", values_to = "predicted_PM10_test")
df_cal_fit_long_PM10PA <- setDT(df_cal_fit_long_PM10PA)

summary0PM10PA <- glance(model0PM10PA)
summary1PM10PA <- glance(model1PM10PA)
summary2PM10PA <- glance(model2PM10PA)
summary3PM10PA <- glance(model3PM10PA)
summary4PM10PA <- glance(model4PM10PA)
summary5PM10PA <- glance(model5PM10PA)
summary6PM10PA <- glance(model6PM10PA)

summary_all_PM10PAmodels <- bind_rows(summary0PM10PA, summary1PM10PA, summary2PM10PA, 
                                    summary3PM10PA, summary4PM10PA, summary5PM10PA, summary6PM10PA) %>%
  mutate(modelID = c("A", "B", "C", "D", "E", "F", "G"))
summary_all_PM10PAmodels

breakpt.4PM10PA <- summary.segmented(model4PM10PA)$psi[2]
breakpt.5PM10PA <- summary.segmented(model5PM10PA)$psi[2]
breakpt.6PM10PA <- summary.segmented(model6PM10PA)$psi[2]
breakptsPM10PA <- c(breakpt.4PM10PA,breakpt.5PM10PA,breakpt.6PM10PA)
breakpts_ID <- c("Model E", "Model F", "Model G")
df_breakpointsPM10PA <- data.frame(breakpts_ID,breakptsPM10PA) 
df_breakpointsPM10PA %>%
  kbl(caption="**Summary of breakpoint location (μg/m^3^)**",digits=2, col.names=c("Model ID","Breakpoint")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


## # # # # # # # # # # # # # # # # #  # # # # # # # # # # # # # # # # # # # # 
model1 <- lm(OutdoorPM2.5_SCK ~ PM2.5_Clonskeagh +  OutdoorRH_SCK, data = data)
model2 <- lm(OutdoorPM10_SCK ~ PM10_Clonskeagh +  OutdoorRH_SCK, data = data)
model3 <- lm(OutdoorPM2.5_PA ~ PM2.5_Clonskeagh +  OutdoorRH_PA, data = data)
model4 <- lm(OutdoorPM10_PA ~ PM10_Clonskeagh +  OutdoorRH_PA, data = data)
model5 <- lm(OutdoorPM2.5_PA.ALT ~ PM2.5_Clonskeagh +  OutdoorRH_PA, data = data)
model5

s <- step(model5)
print(s)
summary(model5)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# corrected concentrations for PM2.5 and PM10 for SCK and PA
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Performance Metric of fitted model: https://cran.r-project.org/web/packages/PerMat/vignettes/PerMat.html
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "ambient outdoor sck purpleair_hr.csv")
View(data)
data_noNA <- na.omit(data)
summary(data)
library(PerMat)

# PM2.5 
PM2.5SCKRMSE <- RMSE(data_noNA$PM2.5_Clonskeagh, data_noNA$PM2.5SCK)
PM2.5SCKnRMSE <- NRMSE(data_noNA$PM2.5_Clonskeagh, data_noNA$PM2.5SCK)
PM2.5SCKR2 <- R2(data_noNA$PM2.5_Clonskeagh, data_noNA$PM2.5SCK)
PM2.5SCKMAE <- MAE(data_noNA$PM2.5_Clonskeagh, data_noNA$PM2.5SCK)
PM2.5SCKaccuracy <- accuracy(data_noNA$PM2.5_Clonskeagh, data_noNA$PM2.5SCK)

PM2.5PARMSE <- RMSE(data_noNA$PM2.5_Clonskeagh, data_noNA$PM2.5PA)
PM2.5PAnRMSE <- NRMSE(data_noNA$PM2.5_Clonskeagh, data_noNA$PM2.5PA)
PM2.5PAR2 <- R2(data_noNA$PM2.5_Clonskeagh, data_noNA$PM2.5PA)
PM2.5PAMAE <- MAE(data_noNA$PM2.5_Clonskeagh, data_noNA$PM2.5PA)
PM2.5PAaccuracy <- accuracy(data_noNA$PM2.5_Clonskeagh, data_noNA$PM2.5PA)

# apply daily correction factor for PM2.5 
PM2.5SCKRMSE <- RMSE(data_noNA$PM2.5_Clonskeagh, data_noNA$OutdoorPM2.5SCKDF)
PM2.5SCKnRMSE <- NRMSE(data_noNA$PM2.5_Clonskeagh, data_noNA$OutdoorPM2.5SCKDF)
PM2.5SCKR2 <- R2(data_noNA$PM2.5_Clonskeagh, data_noNA$OutdoorPM2.5SCKDF)
PM2.5SCKMAE <- MAE(data_noNA$PM2.5_Clonskeagh, data_noNA$OutdoorPM2.5SCKDF)
PM2.5SCKaccuracy <- accuracy(data_noNA$PM2.5_Clonskeagh, data_noNA$OutdoorPM2.5SCKDF)

PM2.5PARMSE <- RMSE(data_noNA$PM2.5_Clonskeagh, data_noNA$OutdoorPM2.5PADF)
PM2.5PAnRMSE <- NRMSE(data_noNA$PM2.5_Clonskeagh, data_noNA$OutdoorPM2.5PADF)
PM2.5PAR2 <- R2(data_noNA$PM2.5_Clonskeagh, data_noNA$OutdoorPM2.5PADF)
PM2.5PAMAE <- MAE(data_noNA$PM2.5_Clonskeagh, data_noNA$OutdoorPM2.5PADF)
PM2.5PAaccuracy <- accuracy(data_noNA$PM2.5_Clonskeagh, data_noNA$OutdoorPM2.5PADF)



# PM10 
PM10SCKRMSE <- RMSE(data_noNA$PM10_Clonskeagh, data_noNA$PM10SCK)
PM10SCKnRMSE <- NRMSE(data_noNA$PM10_Clonskeagh, data_noNA$PM10SCK)
PM10SCKR2 <- R2(data_noNA$PM10_Clonskeagh, data_noNA$PM10SCK)
PM10SCKMAE <- MAE(data_noNA$PM10_Clonskeagh, data_noNA$PM10SCK)
PM10SCKaccuracy <- accuracy(data_noNA$PM10_Clonskeagh, data_noNA$PM10SCK)
# PM10 model 0: y=b+b1*x
PM10SCKRMSE <- RMSE(data_noNA$PM10_Clonskeagh, data_noNA$PM10SCKb)
PM10SCKnRMSE <- NRMSE(data_noNA$PM10_Clonskeagh, data_noNA$PM10SCKb)
PM10SCKR2 <- R2(data_noNA$PM10_Clonskeagh, data_noNA$PM10SCKb)
PM10SCKMAE <- MAE(data_noNA$PM10_Clonskeagh, data_noNA$PM10SCKb)
PM10SCKaccuracy <- accuracy(data_noNA$PM10_Clonskeagh, data_noNA$PM10SCKb)

PM10PARMSE <- RMSE(data_noNA$PM10_Clonskeagh, data_noNA$PM10PA)
PM10PAnRMSE <- NRMSE(data_noNA$PM10_Clonskeagh, data_noNA$PM10PA)
PM10PAR2 <- R2(data_noNA$PM10_Clonskeagh, data_noNA$PM10PA)
PM10PAMAE <- MAE(data_noNA$PM10_Clonskeagh, data_noNA$PM10PA)
PM10PAaccuracy <- accuracy(data_noNA$PM10_Clonskeagh, data_noNA$PM10PA)



# explore relationships bettween variables
#https://www.r-bloggers.com/2021/08/exploring-relationship-between-variables-scatter-plot/
plot(~ PM2.5 + Relative.Humidity + Temperature, data = df, main = "Scatterplot Matrix")

# scatter plot using ggplot2
library(ggpmisc)
# PM2.5
jpeg("correction with RH.jpg", width = 8, height = 5, units = 'in', res = 500)
ggplot(df, aes(x = PM2.5, y = PM.sub.2.5..sub., color = Relative.Humidity)) + 
  geom_jitter() + 
  scale_fill_gradient(low = "green", high = "red", na.value = "gray50", aesthetics = "color") +
  theme_bw() + 
  scale_x_continuous(name = "Concentration_Low-cost sensor (ug/m3)") +
  scale_y_continuous(name = "Concentration_Ambient site (ug/m3)") +
  labs(title = "Low-cost sensor vs. Reference method PM2.5") + 
  stat_poly_line() +  #library(ggpmisc)
  stat_poly_eq(use_label(c("eq","adj.R2", "p", "n")))
dev.off() 

# PM10
jpeg("correction with RH-PM10.jpg", width = 8, height = 5, units = 'in', res = 500)
ggplot(df, aes(x = PM10, y = PM.sub.10..sub., color = Relative.Humidity)) + 
  geom_jitter() + 
  scale_fill_gradient(low = "lightblue", high = "red", na.value = "gray50", aesthetics = "color") +
  theme_bw() + 
  scale_x_continuous(name = "Concentration_Low-cost sensor (ug/m3)") +
  scale_y_continuous(name = "Concentration_Ambient site (ug/m3)") +
  labs(title = "Low-cost sensor vs. Reference method PM10") + 
  stat_poly_line() +  #library(ggpmisc)
  stat_poly_eq(use_label(c("eq","adj.R2", "p", "n")))
dev.off() 


################################################################################################################
# 20241103
# view file #
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data_day = read.csv(file = "ambient outdoor sck purpleair_daily.csv")
View(data_day)
summary(data_day)

# creat a dataframe
df_day <- data.frame(data_day)
summary(df_day)
view(df_day)
# check factor for PM2.5 or PM10
# select the columns in including in models PM2.5 SCK
library(dplyr)
df_cal_PM2.5_day <- df_day[, c("Average.of.PM2.5_Clonskeagh", "Average.of.OutdoorPM2.5_SCK", "Average.of.OutdoorPM2.5_PA", 
                       "Average.of.OutdoorT_SCK", "Average.of.OutdoorRH_SCK", "Average.of.OutdoorT_PA", "Average.of.OutdoorRH_PA")]
# keep complete cases for modeling
df_cal_PM2.5_day <- df_cal_PM2.5_day[complete.cases(df_cal_PM2.5_day),]
# make index for splitting 80:20
# if (!require(caret)) install.packages('caret')
library(caret)
data_split_PM2.5_day <- createDataPartition(df_cal_PM2.5_day$Average.of.PM2.5_Clonskeagh, times =1, list = F, p = 0.8)
data_train_PM2.5_day <- df_cal_PM2.5_day[data_split_PM2.5_day, ]
data_test_PM2.5_day <- df_cal_PM2.5_day[-data_split_PM2.5_day, ]

### analysis-build models
model0 <- lm(Average.of.PM2.5_Clonskeagh ~ Average.of.OutdoorPM2.5_SCK + Average.of.OutdoorT_SCK + Average.of.OutdoorRH_SCK, data = data_train_PM2.5_day)
model1 <- lm(Average.of.PM2.5_Clonskeagh ~ Average.of.OutdoorPM2.5_SCK + Average.of.OutdoorRH_SCK, data = data_train_PM2.5_day)
model2 <- lm(Average.of.PM2.5_Clonskeagh ~ Average.of.OutdoorPM2.5_SCK + Average.of.OutdoorT_SCK, data = data_train_PM2.5_day)
model3 <- lm(Average.of.PM2.5_Clonskeagh ~ Average.of.OutdoorPM2.5_SCK, data = data_train_PM2.5_day)
model4 <- segmented(lm(Average.of.PM2.5_Clonskeagh ~ Average.of.OutdoorPM2.5_SCK, data = data_train_PM2.5_day), 
                    seg.Z = ~Average.of.OutdoorPM2.5_SCK, psi = NA, control = seg.control(K =1))
model5 <- segmented(lm(Average.of.PM2.5_Clonskeagh ~ Average.of.OutdoorPM2.5_SCK + Average.of.OutdoorRH_SCK, data = data_train_PM2.5_day), 
                    seg.Z = ~Average.of.OutdoorPM2.5_SCK, psi = NA, control = seg.control(K =1))
model6 <- segmented(lm(Average.of.PM2.5_Clonskeagh ~ Average.of.OutdoorPM2.5_SCK + Average.of.OutdoorT_SCK, data = data_train_PM2.5_day), 
                    seg.Z = ~Average.of.OutdoorPM2.5_SCK, psi = NA, control = seg.control(K =1))
## predict the testing data 
predict_PM2.5model.0 <- predict(model0,data_test_PM2.5_day)
predict_PM2.5model.1 <- predict(model1,data_test_PM2.5_day)
predict_PM2.5model.2 <- predict(model2,data_test_PM2.5_day)
predict_PM2.5model.3 <- predict(model3,data_test_PM2.5_day)
predict_PM2.5model.4 <- predict(model4,data_test_PM2.5_day)
predict_PM2.5model.5 <- predict(model5,data_test_PM2.5_day)
predict_PM2.5model.6 <- predict(model6,data_test_PM2.5_day)

measured_test <- data_test_PM2.5_day %>% pull(Average.of.PM2.5_Clonskeagh)
##Store the data

df_cal_fit_PM2.5_day <- data.frame(predict_PM2.5model.0, predict_PM2.5model.1, predict_PM2.5model.2, 
                               predict_PM2.5model.3, predict_PM2.5model.4, predict_PM2.5model.5, 
                               predict_PM2.5model.6, measured_test)
df_cal_fit_long_day <- df_cal_fit_PM2.5_day %>% 
  pivot_longer(cols = starts_with("predict_PM2.5"), names_to = "model", 
               names_prefix = "predict_PM2.5 model", values_to = "predicted_PM2.5_test")
df_cal_fit_long_day <- setDT(df_cal_fit_long_day)

summary0 <- glance(model0)
summary1 <- glance(model1)
summary2 <- glance(model2)
summary3 <- glance(model3)
summary4 <- glance(model4)
summary5 <- glance(model5)
summary6 <- glance(model6)

summary_all_PM2.5models <- bind_rows(summary0, summary1, summary2, 
                                     summary3, summary4, summary5, summary6) %>%
  mutate(modelID = c("A", "B", "C", "D", "E", "F", "G"))
summary_all_PM2.5models

breakpt.4 <- summary.segmented(model4)$psi[2]
breakpt.5 <- summary.segmented(model5)$psi[2]
breakpt.6 <- summary.segmented(model6)$psi[2]
breakpts <- c(breakpt.4,breakpt.5,breakpt.6)
breakpts_ID <- c("Model E", "Model F", "Model G")
df_breakpoints <- data.frame(breakpts_ID,breakpts) 
df_breakpoints %>%
  kbl(caption="**Summary of breakpoint location (μg/m^3^)**",digits=2, col.names=c("Model ID","Breakpoint")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

######################################################################################################

# check fator
# select the columns in including in models
library(dplyr)
df_cal_PM10_day <- df_day[, c("Average.of.PM10_Clonskeagh", "Average.of.OutdoorPM10_SCK", "Average.of.OutdoorPM10_PA", 
                      "Average.of.OutdoorT_SCK", "Average.of.OutdoorRH_SCK", "Average.of.OutdoorT_PA", "Average.of.OutdoorRH_PA")]
# keep complete cases for modeling
df_cal_PM10_day <- df_cal_PM10_day[complete.cases(df_cal_PM10_day),]
# make index for splitting 80:20
# if (!require(caret)) install.packages('caret')
library(caret)
data_split_PM10_day <- createDataPartition(df_cal_PM10_day$Average.of.PM10_Clonskeagh, times =1, list = F, p = 0.8)
data_train_PM10_day <- df_cal_PM10_day[data_split_PM10_day, ]
data_test_PM10_day <- df_cal_PM10_day[-data_split_PM10_day, ]

### analysis-build models
model0PM10 <- lm(Average.of.PM10_Clonskeagh ~ Average.of.OutdoorPM10_SCK + Average.of.OutdoorT_SCK + Average.of.OutdoorRH_SCK, data = data_train_PM10_day)
model1PM10 <- lm(Average.of.PM10_Clonskeagh ~ Average.of.OutdoorPM10_SCK + Average.of.OutdoorRH_SCK, data = data_train_PM10_day)
model2PM10 <- lm(Average.of.PM10_Clonskeagh ~ Average.of.OutdoorPM10_SCK + Average.of.OutdoorT_SCK, data = data_train_PM10_day)
model3PM10 <- lm(Average.of.PM10_Clonskeagh ~ Average.of.OutdoorPM10_SCK, data = data_train_PM10_day)
model4PM10 <- segmented(lm(Average.of.PM10_Clonskeagh ~ Average.of.OutdoorPM10_SCK, data = data_train_PM10_day), 
                        seg.Z = ~Average.of.OutdoorPM10_SCK, psi = NA, control = seg.control(K =1))
model5PM10 <- segmented(lm(Average.of.PM10_Clonskeagh ~ Average.of.OutdoorPM10_SCK + Average.of.OutdoorRH_SCK, data = data_train_PM10_day), 
                        seg.Z = ~Average.of.OutdoorPM10_SCK, psi = NA, control = seg.control(K =1))
model6PM10 <- segmented(lm(Average.of.PM10_Clonskeagh ~ Average.of.OutdoorPM10_SCK + Average.of.OutdoorT_SCK, data = data_train_PM10_day), 
                        seg.Z = ~Average.of.OutdoorPM10_SCK, psi = NA, control = seg.control(K =1))
## predict the testing data 
predict_PM10model.0 <- predict(model0PM10,data_test_PM10_day)
predict_PM10model.1 <- predict(model1PM10,data_test_PM10_day)
predict_PM10model.2 <- predict(model2PM10,data_test_PM10_day)
predict_PM10model.3 <- predict(model3PM10,data_test_PM10_day)
predict_PM10model.4 <- predict(model4PM10,data_test_PM10_day)
predict_PM10model.5 <- predict(model5PM10,data_test_PM10_day)
predict_PM10model.6 <- predict(model6PM10,data_test_PM10_day)

measured_testPM10_day <- data_test_PM10_day %>% pull(Average.of.PM10_Clonskeagh)
##Store the data

df_cal_fit_PM10_day <- data.frame(predict_PM10model.0, predict_PM10model.1, predict_PM10model.2, 
                              predict_PM10model.3, predict_PM10model.4, predict_PM10model.5, 
                              predict_PM10model.6, measured_testPM10_day)
df_cal_fit_long_PM10_day <- df_cal_fit_PM10_day %>% 
  pivot_longer(cols = starts_with("predict_PM10"), names_to = "model", 
               names_prefix = "predict_PM10 model", values_to = "predicted_PM10_test")
df_cal_fit_long_PM10_day <- setDT(df_cal_fit_long_PM10_day)

summary0PM10 <- glance(model0PM10)
summary1PM10 <- glance(model1PM10)
summary2PM10 <- glance(model2PM10)
summary3PM10 <- glance(model3PM10)
summary4PM10 <- glance(model4PM10)
summary5PM10 <- glance(model5PM10)
summary6Pm10 <- glance(model6PM10)

summary_all_PM10models <- bind_rows(summary0PM10, summary1PM10, summary2PM10, 
                                    summary3PM10, summary4PM10, summary5PM10, summary6Pm10) %>%
  mutate(modelID = c("A", "B", "C", "D", "E", "F", "G"))
summary_all_PM10models

breakpt.4PM10 <- summary.segmented(model4PM10)$psi[2]
breakpt.5PM10 <- summary.segmented(model5PM10)$psi[2]
breakpt.6PM10 <- summary.segmented(model6PM10)$psi[2]
breakptsPM10 <- c(breakpt.4PM10,breakpt.5PM10,breakpt.6PM10)
breakpts_ID <- c("Model E", "Model F", "Model G")
df_breakpointsPM10 <- data.frame(breakpts_ID,breakptsPM10) 
df_breakpointsPM10 %>%
  kbl(caption="**Summary of breakpoint location (μg/m^3^)**",digits=2, col.names=c("Model ID","Breakpoint")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

####################################################################################################

# check factor for PM2.5
# select the columns in including in models PM2.5 PurpleAir
library(dplyr)
df_cal_PM2.5PA_day <- df_day[, c("Average.of.PM2.5_Clonskeagh", "Average.of.OutdoorPM2.5_SCK", "Average.of.OutdoorPM2.5_PA", 
                         "Average.of.OutdoorT_SCK", "Average.of.OutdoorRH_SCK", "Average.of.OutdoorT_PA", "Average.of.OutdoorRH_PA")]
# keep complete cases for modeling
df_cal_PM2.5PA_day <- df_cal_PM2.5PA_day[complete.cases(df_cal_PM2.5PA_day),]
# make index for splitting 80:20
# if (!require(caret)) install.packages('caret')
library(caret)
data_split_PM2.5PA_day <- createDataPartition(df_cal_PM2.5PA_day$Average.of.PM2.5_Clonskeagh, times =1, list = F, p = 0.8)
data_train_PM2.5PA_day <- df_cal_PM2.5PA_day[data_split_PM2.5PA_day, ]
data_test_PM2.5PA_day <- df_cal_PM2.5PA_day[-data_split_PM2.5PA_day, ]

### analysis-build models
model0PA <- lm(Average.of.PM2.5_Clonskeagh ~ Average.of.OutdoorPM2.5_PA + Average.of.OutdoorT_PA + Average.of.OutdoorRH_PA, data = data_train_PM2.5PA_day)
model1PA <- lm(Average.of.PM2.5_Clonskeagh ~ Average.of.OutdoorPM2.5_PA + Average.of.OutdoorRH_PA, data = data_train_PM2.5PA_day)
model2PA <- lm(Average.of.PM2.5_Clonskeagh ~ Average.of.OutdoorPM2.5_PA + Average.of.OutdoorT_PA, data = data_train_PM2.5PA_day)
model3PA <- lm(Average.of.PM2.5_Clonskeagh ~ Average.of.OutdoorPM2.5_PA, data = data_train_PM2.5PA_day)
model4PA <- segmented(lm(Average.of.PM2.5_Clonskeagh ~ Average.of.OutdoorPM2.5_PA, data = data_train_PM2.5PA_day), 
                      seg.Z = ~Average.of.OutdoorPM2.5_PA, psi = NA, control = seg.control(K =1))
model5PA <- segmented(lm(Average.of.PM2.5_Clonskeagh ~ Average.of.OutdoorPM2.5_PA + Average.of.OutdoorRH_PA, data = data_train_PM2.5PA_day), 
                      seg.Z = ~Average.of.OutdoorPM2.5_PA, psi = NA, control = seg.control(K =1))
model6PA <- segmented(lm(Average.of.PM2.5_Clonskeagh ~ Average.of.OutdoorPM2.5_PA + Average.of.OutdoorT_PA, data = data_train_PM2.5PA_day), 
                      seg.Z = ~Average.of.OutdoorPM2.5_PA, psi = NA, control = seg.control(K =1), display = T)
## predict the testing data 
predict_PM2.5PAmodel.0 <- predict(model0PA,data_test_PM2.5PA_day)
predict_PM2.5PAmodel.1 <- predict(model1PA,data_test_PM2.5PA_day)
predict_PM2.5PAmodel.2 <- predict(model2PA,data_test_PM2.5PA_day)
predict_PM2.5PAmodel.3 <- predict(model3PA,data_test_PM2.5PA_day)
predict_PM2.5PAmodel.4 <- predict(model4PA,data_test_PM2.5PA_day)
predict_PM2.5PAmodel.5 <- predict(model5PA,data_test_PM2.5PA_day)
predict_PM2.5PAmodel.6 <- predict(model6PA,data_test_PM2.5PA_day)

measured_testPA_day <- data_test_PM2.5PA_day %>% pull(Average.of.PM2.5_Clonskeagh)
##Store the data

df_cal_fit_PM2.5PA_day <- data.frame(predict_PM2.5PAmodel.0, predict_PM2.5PAmodel.1, predict_PM2.5PAmodel.2, 
                                 predict_PM2.5PAmodel.3, predict_PM2.5PAmodel.4, predict_PM2.5PAmodel.5, 
                                 predict_PM2.5PAmodel.6, measured_testPA_day)
df_cal_fit_longPA_day <- df_cal_fit_PM2.5PA_day %>% 
  pivot_longer(cols = starts_with("predict_PM2.5"), names_to = "model", 
               names_prefix = "predict_PM2.5 model", values_to = "predicted_PM2.5_test")
df_cal_fit_longPA_day <- setDT(df_cal_fit_longPA_day)

summary0PA <- glance(model0PA)
summary1PA <- glance(model1PA)
summary2PA <- glance(model2PA)
summary3PA <- glance(model3PA)
summary4PA <- glance(model4PA)
summary5PA <- glance(model5PA)
summary6PA <- glance(model6PA)

summary_all_PM2.5PAmodels_day <- bind_rows(summary0PA, summary1PA, summary2PA, 
                                       summary3PA, summary4PA, summary5PA, summary6PA) %>%
  mutate(modelID = c("A", "B", "C", "D", "E", "F", "G"))
summary_all_PM2.5PAmodels_day

breakpt.4PA <- summary.segmented(model4PA)$psi[2]
breakpt.5PA <- summary.segmented(model5PA)$psi[2]
breakpt.6PA <- summary.segmented(model6PA)$psi[2]
breakptsPA <- c(breakpt.4PA,breakpt.5PA,breakpt.6PA)
breakpts_ID <- c("Model E", "Model F", "Model G")
df_breakpointsPA <- data.frame(breakpts_ID,breakptsPA) 
df_breakpointsPA %>%
  kbl(caption="**Summary of breakpoint location (μg/m^3^)**",digits=2, col.names=c("Model ID","Breakpoint")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

######################################################################################################

# check factor PM10 PA
# select the columns in including in models
library(dplyr)
df_cal_PM10PA_day <- df_day[, c("Average.of.PM10_Clonskeagh", "Average.of.OutdoorPM10_SCK", "Average.of.OutdoorPM10_PA", 
                        "Average.of.OutdoorT_SCK", "Average.of.OutdoorRH_SCK", "Average.of.OutdoorT_PA", "Average.of.OutdoorRH_PA")]
# keep complete cases for modeling
df_cal_PM10PA_day <- df_cal_PM10PA_day[complete.cases(df_cal_PM10PA_day),]
# make index for splitting 80:20
# if (!require(caret)) install.packages('caret')
library(caret)
data_split_PM10PA_day <- createDataPartition(df_cal_PM10PA_day$Average.of.PM10_Clonskeagh, times =1, list = F, p = 0.8)
data_train_PM10PA_day <- df_cal_PM10PA_day[data_split_PM10PA_day, ]
data_test_PM10PA_day <- df_cal_PM10PA_day[-data_split_PM10PA_day, ]

### analysis-build models
model0PM10PA <- lm(Average.of.PM10_Clonskeagh ~ Average.of.OutdoorPM10_PA + Average.of.OutdoorT_PA + Average.of.OutdoorRH_PA, data = data_train_PM10PA_day)
model1PM10PA <- lm(Average.of.PM10_Clonskeagh ~ Average.of.OutdoorPM10_PA + Average.of.OutdoorRH_PA, data = data_train_PM10PA_day)
model2PM10PA <- lm(Average.of.PM10_Clonskeagh ~ Average.of.OutdoorPM10_PA + Average.of.OutdoorT_PA, data = data_train_PM10PA_day)
model3PM10PA <- lm(Average.of.PM10_Clonskeagh ~ Average.of.OutdoorPM10_PA, data = data_train_PM10PA_day)
model4PM10PA <- segmented(lm(Average.of.PM10_Clonskeagh ~ Average.of.OutdoorPM10_PA, data = data_train_PM10PA_day), 
                          seg.Z = ~Average.of.OutdoorPM10_PA, psi = NA, control = seg.control(K =1))
model5PM10PA <- segmented(lm(Average.of.PM10_Clonskeagh ~ Average.of.OutdoorPM10_PA + Average.of.OutdoorRH_PA, data = data_train_PM10PA_day), 
                          seg.Z = ~Average.of.OutdoorPM10_PA, psi = NA, control = seg.control(K =1))
model6PM10PA <- segmented(lm(Average.of.PM10_Clonskeagh ~ Average.of.OutdoorPM10_PA + Average.of.OutdoorT_PA, data = data_train_PM10PA_day), 
                          seg.Z = ~Average.of.OutdoorPM10_PA, psi = NA, control = seg.control(K =1))
## predict the testing data 
predict_PM10PAmodel.0 <- predict(model0PM10PA,data_test_PM10PA_day)
predict_PM10PAmodel.1 <- predict(model1PM10PA,data_test_PM10PA_day)
predict_PM10PAmodel.2 <- predict(model2PM10PA,data_test_PM10PA_day)
predict_PM10PAmodel.3 <- predict(model3PM10PA,data_test_PM10PA_day)
predict_PM10PAmodel.4 <- predict(model4PM10PA,data_test_PM10PA_day)
predict_PM10PAmodel.5 <- predict(model5PM10PA,data_test_PM10PA_day)
predict_PM10PAmodel.6 <- predict(model6PM10PA,data_test_PM10PA_day)

measured_testPM10PA_day <- data_test_PM10PA_day %>% pull(Average.of.PM10_Clonskeagh)
##Store the data

df_cal_fit_PM10PA_day <- data.frame(predict_PM10PAmodel.0, predict_PM10PAmodel.1, predict_PM10PAmodel.2, 
                                predict_PM10PAmodel.3, predict_PM10PAmodel.4, predict_PM10PAmodel.5, 
                                predict_PM10PAmodel.6, measured_testPM10PA_day)
df_cal_fit_long_PM10PA_day <- df_cal_fit_PM10PA_day %>% 
  pivot_longer(cols = starts_with("predict_PM10"), names_to = "model", 
               names_prefix = "predict_PM10 model", values_to = "predicted_PM10_test")
df_cal_fit_long_PM10PA_day <- setDT(df_cal_fit_long_PM10PA_day)

summary0PM10PA <- glance(model0PM10PA)
summary1PM10PA <- glance(model1PM10PA)
summary2PM10PA <- glance(model2PM10PA)
summary3PM10PA <- glance(model3PM10PA)
summary4PM10PA <- glance(model4PM10PA)
summary5PM10PA <- glance(model5PM10PA)
summary6PM10PA <- glance(model6PM10PA)

summary_all_PM10PAmodels_day <- bind_rows(summary0PM10PA, summary1PM10PA, summary2PM10PA, 
                                      summary3PM10PA, summary4PM10PA, summary5PM10PA, summary6PM10PA) %>%
  mutate(modelID = c("A", "B", "C", "D", "E", "F", "G"))
summary_all_PM10PAmodels_day

breakpt.4PM10PA <- summary.segmented(model4PM10PA)$psi[2]
breakpt.5PM10PA <- summary.segmented(model5PM10PA)$psi[2]
breakpt.6PM10PA <- summary.segmented(model6PM10PA)$psi[2]
breakptsPM10PA <- c(breakpt.4PM10PA,breakpt.5PM10PA,breakpt.6PM10PA)
breakpts_ID <- c("Model E", "Model F", "Model G")
df_breakpointsPM10PA_day <- data.frame(breakpts_ID,breakptsPM10PA) 
df_breakpointsPM10PA_day %>%
  kbl(caption="**Summary of breakpoint location (μg/m^3^)**",digits=2, col.names=c("Model ID","Breakpoint")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


## # # # # # # # # # # # # # # # # #  # # # # # # # # # # # # # # # # # # # # 
model1 <- lm(Average.of.OutdoorPM2.5_SCK ~ Average.of.PM2.5_Clonskeagh + Average.of.OutdoorRH_SCK, data = data_day)
model2 <- lm(Average.of.OutdoorPM10_SCK ~ Average.of.PM10_Clonskeagh +  Average.of.OutdoorRH_SCK, data = data_day)
model3 <- lm(Average.of.OutdoorPM2.5_PA ~ Average.of.PM2.5_Clonskeagh +  Average.of.OutdoorRH_PA, data = data_day)
model4 <- lm(Average.of.OutdoorPM10_PA ~ Average.of.PM10_Clonskeagh + Average.of.OutdoorRH_PA, data = data_day)
model7 <- lm(Average.of.OutdoorPM2.5_PA.ALT ~ Average.of.PM2.5_Clonskeagh +  Average.of.OutdoorRH_PA, data = data_day)
summary(model7)

# # # # # # # # # PM2.5
model5 <- lm(Average.of.OutdoorPM2.5_SCK ~ Average.of.PM2.5_Clonskeagh +  Average.of.OutdoorT_SCK, data = data_day)
model6 <- lm(Average.of.OutdoorPM2.5_PA ~ Average.of.PM2.5_Clonskeagh +  Average.of.OutdoorT_PA, data = data_day)



s <- step(model6)
print(s)
summary(model6)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# corrected concentrations for PM2.5 and PM10 for SCK and PA
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Performance Metric of fitted model: https://cran.r-project.org/web/packages/PerMat/vignettes/PerMat.html
# Create Awesome HTML Table with knitr::kable and kableExtra: https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Use_it_with_sparkline
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data_day = read.csv(file = "ambient outdoor sck purpleair_daily.csv")
View(data_day)
data_noNA <- na.omit(data_day)
summary(data_day)
library(PerMat)

# PM2.5 
PM2.5SCKRMSE <- RMSE(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5SCKD)
PM2.5SCKnRMSE <- NRMSE(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5SCKD)
PM2.5SCKR2 <- R2(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5SCKD)
PM2.5SCKMAE <- MAE(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5SCKD)
PM2.5SCKaccuracy <- accuracy(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5SCKD)

PM2.5PARMSE <- RMSE(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5PAD)
PM2.5PAnRMSE <- NRMSE(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5PAD)
PM2.5PAR2 <- R2(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5PAD)
PM2.5PAMAE <- MAE(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5PAD)
PM2.5PAaccuracy <- accuracy(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5PAD)

############### T, PM2.5
PM2.5SCKRMSE <- RMSE(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5SCKDT)
PM2.5SCKnRMSE <- NRMSE(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5SCKDT)
PM2.5SCKR2 <- R2(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5SCKDT)
PM2.5SCKMAE <- MAE(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5SCKDT)
PM2.5SCKaccuracy <- accuracy(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5SCKDT)

PM2.5PARMSE <- RMSE(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5PADT)
PM2.5PAnRMSE <- NRMSE(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5PADT)
PM2.5PAR2 <- R2(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5PADT)
PM2.5PAMAE <- MAE(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5PADT)
PM2.5PAaccuracy <- accuracy(data_noNA$Average.of.PM2.5_Clonskeagh, data_noNA$PM2.5PADT)



# PM10 
PM10SCKRMSE <- RMSE(data_noNA$Average.of.PM10_Clonskeagh, data_noNA$PM10SCKD)
PM10SCKnRMSE <- NRMSE(data_noNA$Average.of.PM10_Clonskeagh, data_noNA$PM10SCKD)
PM10SCKR2 <- R2(data_noNA$Average.of.PM10_Clonskeagh, data_noNA$PM10SCKD)
PM10SCKMAE <- MAE(data_noNA$Average.of.PM10_Clonskeagh, data_noNA$PM10SCKD)
PM10SCKaccuracy <- accuracy(data_noNA$Average.of.PM10_Clonskeagh, data_noNA$PM10SCKD)

PM10PARMSE <- RMSE(data_noNA$Average.of.PM10_Clonskeagh, data_noNA$PM10PAD)
PM10PAnRMSE <- NRMSE(data_noNA$Average.of.PM10_Clonskeagh, data_noNA$PM10PAD)
PM10PAR2 <- R2(data_noNA$Average.of.PM10_Clonskeagh, data_noNA$PM10PAD)
PM10PAMAE <- MAE(data_noNA$Average.of.PM10_Clonskeagh, data_noNA$PM10PAD)
PM10PAaccuracy <- accuracy(data_noNA$Average.of.PM10_Clonskeagh, data_noNA$PM10PAD)

# 1 hr vs 24 h for PM2.5 and PM10 
data_noNA <- na.omit(data_day)

PM10PARMSE <- RMSE(data_noNA$Average.of.PM10PA, data_noNA$PM10PAD)
PM10PAnRMSE <- NRMSE(data_noNA$Average.of.PM10PA, data_noNA$PM10PAD)
PM10PAR2 <- R2(data_noNA$Average.of.PM10PA, data_noNA$PM10PAD)
PM10PAMAE <- MAE(data_noNA$Average.of.PM10PA, data_noNA$PM10PAD)
PM10PAaccuracy <- accuracy(data_noNA$Average.of.PM10PA, data_noNA$PM10PAD)




##############################################################################
library(lubridate)
library(zoo)
# dataframe
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "ambient outdoor sck purpleair_daily.csv")
summary(data)
df <- data.frame(data)
View(df)

setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data1 = read.csv(file = "ambient outdoor sck purpleair_hr.csv")
summary(data1)
df <- data.frame(data1)
View(df)

# PM2.5
df1 <- df[, c("Date", "Average.of.PM2.5_Clonskeagh", "PM2.5SCKD", "PM2.5PAD","PM2.5PAALT",
              "IndoorAPM2.5SCK", "IndoorAPM2.5PA", "IndoorAPM2.5ALT", "IndoorBPM2.5SCK")]
colnames(df1) <- c('Date', 'AmbientPM2.5', 'OutdoorPM2.5CF1_SCK', 'OutdoorPM2.5CF1_PA', "OutdoorPM2.5ALT_PA",
                   "IndoorA_PM2.5CF1_SCK", "IndoorA_PM2.5CF1_PA","IndoorA_PM2.5ALT_PA", "IndoorB_PM2.5CF1_SCK")
colnames(df1)
view(df1)

dfin <- df[, c("Date", "Average.of.PM2.5_Clonskeagh", "IndoorAPM2.5SCK", "IndoorAPM2.5PA", 
               "IndoorAPM2.5ALT", "IndoorBPM2.5SCK")]
colnames(dfin) <- c('Date', 'AmbientPM2.5', "IndoorA_PM2.5CF1_SCK", "IndoorA_PM2.5CF1_PA",
                    "IndoorA_PM2.5ALT_PA", "IndoorB_PM2.5CF1_SCK")
view(dfin)

dfout <- df[, c("Date", "Average.of.PM2.5_Clonskeagh", "PM2.5SCKD", "PM2.5PAD","PM2.5PAALT")]
colnames(dfout) <- c('Date', 'AmbientPM2.5', 'OutdoorPM2.5CF1_SCK', 'OutdoorPM2.5CF1_PA', "OutdoorPM2.5ALT_PA")
view(dfout)

dfouthr <- df[, c("Date", "PM2.5_Clonskeagh", "PM2.5SCK", "PM2.5PA","PM2.5PA.ALT")]
colnames(dfouthr) <- c('Date', 'AmbientPM2.5', 'OutdoorPM2.5CF1_SCK', 'OutdoorPM2.5CF1_PA', "OutdoorPM2.5ALT_PA")
view(dfouthr)


# https://r4ds.had.co.nz/
# https://r4ds.hadley.nz/
# https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
# https://r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
# Change manually the appearance of lines: http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software
# https://finchstudio.io/blog/ggplot-dual-y-axes/
# https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
# linetype: solid, longdash, dashed, dotted
# Date preparation
library(tidyverse)
df2 <- df1 %>% 
  gather(key = "variable", value = "value", -Date)
view(df2)

df2in <- dfin %>% 
  gather(key = "variable", value = "value", -Date)
view(df2in)

df2out <- dfouthr %>% 
  gather(key = "variable", value = "value", -Date)
view(df2out)

# datetime
df3r <- df2r %>% 
  gather(key = "variable", value = "value", -Date)
view(df3r)


# visualization
jpeg("PM10 daily variation.jpg", width = 8, height = 5, units = 'in', res = 600)
PM10 <- ggplot(df3, aes(x = as.Date(Date), y = value)) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, size = 0.7) + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2023-06-01", "2024-06-01")), date_breaks = "1 month", position = "bottom") +
  scale_y_continuous() + 
  scale_color_manual(values = c("darkred", "palegreen3","steelblue",  "#8ecae6", 
                                "#219ebc", "#ffb703", "#fb8500","#023047")) + 
  scale_linetype_manual(values = c("longdash", "solid", "solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Date", y = expression(PM[10]~concentration~(ug/m^3)), title = expression(PM[10])) + 
  theme(legend.position = c(0.9, 0.9), legend.title = element_blank())
PM10
dev.off() 

# visualization
jpeg("PM2.5 daily variation_cal.jpg", width = 8, height = 5, units = 'in', res = 600)
PM2.5 <- ggplot(df2, aes(x = as.Date(Date), y = value)) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, size = 0.7) + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2023-06-01", "2024-06-01")), date_breaks = "1 month", position = "bottom") +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %Y", position = "bottom") +
  scale_y_continuous() + 
  scale_color_manual(values = c("darkred", "palegreen3","steelblue",  "#8ecae6", 
                                "#219ebc", "#ffb703", "#fb8500","#023047")) + 
  scale_linetype_manual(values = c("longdash", "solid", "solid","solid","solid","solid","solid","solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Date", y = expression(PM[2.5]~concentration~(ug/m^3)), title = expression(PM[2.5])) + 
  theme(legend.position = c(0.2, 0.75), legend.title = element_blank())
PM2.5
dev.off() 

# 
jpeg("PM2.5 daily variation_calin.jpg", width = 8, height = 5, units = 'in', res = 600)
PM2.5in <- ggplot(df2in, aes(x = as.Date(Date), y = value)) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, size = 0.7) + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2023-06-01", "2024-06-01")), date_breaks = "1 month", position = "bottom") +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %Y", position = "bottom") +
  scale_y_continuous() + 
  scale_color_manual(values = c("darkred", "palegreen3","steelblue",  "#88CCEE", 
                                "#44AA99")) + 
  scale_linetype_manual(values = c("longdash", "solid", "solid","solid","solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Date", y = expression(PM[2.5]~concentration~(ug/m^3)), title = expression(PM[2.5])) + 
  theme(legend.position = c(0.9, 0.75), legend.title = element_blank())
PM2.5in
dev.off() 

# 
jpeg("PM2.5 daily variation_calout_h.jpg", width = 8, height = 5, units = 'in', res = 600)
PM2.5outh <- ggplot(df2out, aes(x = as.Date(Date), y = value)) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, size = 0.7) + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2023-06-01", "2024-06-01")), date_breaks = "1 month", position = "bottom") +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %Y", position = "bottom") +
  scale_y_continuous() + 
  scale_color_manual(values = c("darkred", "palegreen3","#ffb703","#44AA99")) + 
  scale_linetype_manual(values = c("longdash", "solid", "solid","solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Date", y = expression(PM[2.5]~concentration~(μg/m^3)), title = expression(PM[2.5]~One-hour)) + 
  theme(legend.position = c(0.88, 0.85), legend.title = element_blank())
PM2.5outh
dev.off() 

# different display of figures
jpeg("time series and scatter.jpg", width = 25, height = 14, units = 'in', res = 600)
ggarrange(PM2.5, PM2.5SCK, PM2.5PA, PM10, PM10SCK, PM10PA, 
          labels = c("a", "b", "c", "d", "e", "f"),
          ncol = 3,
          nrow = 2)
dev.off() 


###########################################################################################
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "ambient outdoor sck purpleair_hr.csv")
summary(data)
df <- data.frame(data)
View(df)
##
df1 <- df[, c("Date", "PM2.5_Clonskeagh","PM2.5SCK", "PM2.5PA","PM2.5PA.ALT")]
colnames(df1) <- c('Date', 'AmbientPM2.5', 'OutdoorPM2.5CF1_SCK', 'OutdoorPM2.5CF1_PA', "OutdoorPM2.5ALT_PA")
colnames(df1)
View(df1)
dfout <- df1 %>% 
  gather(key = "variable", value = "value", -Date)
View(dfout)
##
df2 <- df[, c("Date", "IndoorAPM2.5PA", "IndoorAPM2.5PA.ALT","IndoorAPM2.5SCK")]
colnames(df2) <- c('Date', "IndoorA_PM2.5CF1_PA","IndoorA_PM2.5ALT_PA","IndoorA_PM2.5CF1_SCK")
colnames(df2)
View(df2)
dfinA<- df2%>% 
  gather(key = "variable", value = "value", -Date)
View(dfinA)

##
df3 <- df[, c("Date", "IndoorBPM2.5SCK")]
colnames(df3) <- c('Date', "IndoorB_PM2.5CF1_SCK")
colnames(df3)
view(df3)

##
df4 <- df[, c("Date", "IndoorAPM2.5PA", "IndoorAPM2.5PA.ALT","IndoorAPM2.5SCK","IndoorBPM2.5SCK")]
colnames(df4) <- c('Date', "IndoorA_PM2.5CF1_PA","IndoorA_PM2.5ALT_PA","IndoorA_PM2.5CF1_SCK","IndoorB_PM2.5CF1_SCK")
colnames(df4)
View(df4)
dfinAB<- df4%>% 
  gather(key = "variable", value = "value", -Date)
View(dfinAB)


# visualization
jpeg("PM2.5 houly variation calr.jpg", width = 8, height = 5, units = 'in', res = 600)
PM2.5cal <- ggplot(dfout, aes(x = as.Date(Date), y = value)) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, size = 0.7) + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2023-06-01", "2024-06-01")), date_breaks = "1 month", position = "bottom") +
  scale_y_continuous() + 
  scale_color_manual(values = c("darkred", "palegreen3","#ffb703","#44AA99")) + 
  scale_linetype_manual(values = c("longdash", "solid", "solid","solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Date", y = expression(PM[2.5]~concentration~(μg/m^3)), title = expression(Ambient/Outdoor~PM[2.5])) + 
  theme(legend.position = c(0.88, 0.85), legend.title = element_blank())
PM2.5cal
dev.off() 

# visualization indoor
jpeg("PM2.5 houly variation cal inA.jpg", width = 8, height = 5, units = 'in', res = 600)
PM2.5calinA <- ggplot(dfinA, aes(x = as.Date(Date), y = as.numeric(value))) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, linewidth = 0.7) + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2023-06-01", "2024-06-01")), date_breaks = "1 month", position = "bottom") +
  scale_y_continuous() + 
  scale_color_manual(values = c("palegreen3","#ffb703","#44AA99")) + 
  scale_linetype_manual(values = c("solid", "solid", "solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Date", y = expression(PM[2.5]~concentration~(μg/m^3)), title = expression(IndoorA~PM[2.5])) + 
  theme(legend.position = c(0.88, 0.85), legend.title = element_blank())
PM2.5calinA
dev.off() 

# visualization indoor
jpeg("PM2.5 houly variation cal inB.jpg", width = 8, height = 5, units = 'in', res = 600)
PM2.5calinB <- ggplot(df3, aes(x = as.Date(Date), y = IndoorB_PM2.5CF1_SCK)) + 
  geom_line(color = "#44AA99", linetype = "solid", na.rm = TRUE, linewidth = 0.7) + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2023-06-01", "2024-06-01")), date_breaks = "1 month", position = "bottom") +
  scale_y_continuous() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Date", y = expression(PM[2.5]~concentration~(μg/m^3)), title = expression(IndoorB~PM[2.5]~CF1_SCK)) + 
  theme(legend.position = c(0.9, 0.85), legend.title = element_blank())
PM2.5calinB
dev.off() 

# visualization indoor
jpeg("PM2.5 houly variation cal inAB.jpg", width = 8, height = 5, units = 'in', res = 600)
PM2.5calinAB <- ggplot(dfinAB, aes(x = as.Date(Date), y = as.numeric(value))) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, linewidth = 0.7) + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2023-06-01", "2024-06-01")), date_breaks = "1 month", position = "bottom") +
  scale_y_continuous() + 
  scale_color_manual(values = c("palegreen3","#ffb703","#44AA99","#023047")) + 
  scale_linetype_manual(values = c("solid", "solid", "solid","solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Date", y = expression(PM[2.5]~concentration~(μg/m^3)), title = expression(Indoors~PM[2.5])) + 
  theme(legend.position = c(0.88, 0.85), legend.title = element_blank())
PM2.5calinAB
dev.off() 

#diurnal: https://bookdown.org/david_carslaw/openair/sections/trend-analysis/time-variation.html#output
library(openair)
library(tidyverse)
library(lubridate)
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "ambient outdoor sck purpleair_hourly.csv")
summary(data)
df <- data.frame(data)
View(df)
##
df1 <- df[, c("Hour", "PM2.5.Clonskeagh","PM2.5SCK", "PM2.5PA","PM2.5PA.ALT")]
colnames(df1) <- c('Hour', 'AmbientPM2.5', 'OutdoorPM2.5CF1_SCK', 'OutdoorPM2.5CF1_PA', "OutdoorPM2.5ALT_PA")
colnames(df1)
View(df1)
dfout <- df1 %>% 
  gather(key = "variable", value = "value", -Hour)
View(dfout)
# hourly PM2.5
# visualization outdoor
jpeg("PM2.5 diurnal variation cal out.jpg", width = 8, height = 5, units = 'in', res = 600)
hPM2.5calout <- ggplot(dfout, aes(x = Hour, y = value)) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, linewidth = 0.7) + 
  scale_x_continuous(limits = c(0, 23), breaks = seq(0, 23, 1)) + 
  scale_y_continuous() + 
  scale_color_manual(values = c("darkred", "palegreen3","#ffb703","#44AA99")) + 
  scale_linetype_manual(values = c("longdash","solid", "solid", "solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Hour", y = expression(PM[2.5]~concentration~(μg/m^3))) + 
  theme(legend.position = c(0.2, 0.85), legend.title = element_blank())
hPM2.5calout
dev.off() 

# Diurnal indoor PM2.5
df2 <- df[, c("Hour", "IndoorAPM2.5PA", "IndoorAPM2.5PA.ALT","IndoorAPM2.5SCK","IndoorBPM2.5SCK")]
colnames(df2) <- c('Hour', "IndoorA_PM2.5CF1_PA","IndoorA_PM2.5ALT_PA","IndoorA_PM2.5CF1_SCK","IndoorB_PM2.5CF1_SCK")
colnames(df2)
View(df2)
dfinAB<- df2%>% 
  gather(key = "variable", value = "value", -Hour)
View(dfinAB)
# visualization indoor
jpeg("PM2.5 diurnal variation cal inAB.jpg", width = 8, height = 5, units = 'in', res = 600)
hPM2.5calinAB <- ggplot(dfinAB, aes(x = Hour, y = value)) + 
  geom_line(aes(color = variable, linetype = variable), na.rm = TRUE, linewidth = 0.7) + 
  scale_x_continuous(limits = c(0, 23), breaks = seq(0, 23, 1)) + 
  scale_y_continuous() + 
  scale_color_manual(values = c("palegreen3","#ffb703","#44AA99","#023047")) + 
  scale_linetype_manual(values = c("solid", "solid", "solid","solid")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Hour", y = expression(PM[2.5]~concentration~(μg/m^3))) + 
  theme(legend.position = c(0.6, 0.85), legend.title = element_blank())
hPM2.5calinAB
dev.off() 

# different display of figures
library(ggpubr)
jpeg("series and Dirunal.jpg", width = 20, height = 12, units = 'in', res = 600)
ggarrange(PM2.5cal, hPM2.5calout, PM2.5calinAB, hPM2.5calinAB,
          labels = c("a", "b", "c", "d"),
          ncol = 2,
          nrow = 2,
          widths = c(2,1))
dev.off() 

######
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "ambient outdoor sck purpleair_hr.csv")
summary(data)
df <- data.frame(data)
View(df)

library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)
# data frame
df1 <- df[, c("IndoorAPM2.5SCK", "IndoorAPM2.5PA", "IndoorA.PM10.SCK", "IndoorA.PM10.PA","IndoorAPM1_SCK","IndoorAPM1_PA", 
               "IndoorAT_SCK", "IndoorARH_SCK", "IndoorATVOCs_SCK","IndoorAT_PA", "IndoorARH_PA")]
colnames(df1) <- c("IndoorA_PM2.5CF1_PA","IndoorA_PM2.5ALT_PA","IndoorA_PM2.5CF1_SCK","IndoorB_PM2.5CF1_SCK")
colnames(df1)
View(df1)

df2 <- df[, c( "IndoorBPM2.5SCK", "IndoorB.PM10.SCK","IndoorBPM1_SCK",
               "IndoorBT_SCK","IndoorBRH_SCK", "IndoorBTVOCs_SCK","IndoorBCO2_SCK")]
colnames(df1) <- c("IndoorA_PM2.5CF1_PA","IndoorA_PM2.5ALT_PA","IndoorA_PM2.5CF1_SCK","IndoorB_PM2.5CF1_SCK")
colnames(df2)
View(df2)

# df2 <- df1[, c("Median", "Median.1", "Median.2", "Median.3", "Median.4")]
# colnames(df2) <- c(':PM[2.5]', ':NO[2]', ':O[3]', 'CO', ':CO[2]')
# colnames(df2)
# View(df2)
# asso <- df2[, c(":PM[2.5]", ":NO[2]", ":O[3]", "CO", ":CO[2]")]

#
library(corrplot)
library(ggcorrplot)
A = cor(na.omit(df1))
print(A)
corrplot(A, method = "number")
jpeg("RoomA corr_h.jpg", width = 12, height = 5.5, units = 'in', res = 600)
figa <- ggcorrplot(A,
           outline.color = "white",
           hc.order = TRUE,
           sig.level = 0.05,
           insig = "blank",
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE)
#title = "Correlation matrix of the monitored air quality features using median hourly concentrations"
figa
dev.off() 

#
B = cor(na.omit(df2))
print(B)
corrplot(B, method = "number")
jpeg("RoomB corr_h.jpg", width = 12, height = 5.5, units = 'in', res = 600)
figb <- ggcorrplot(B,
           outline.color = "white",
           hc.order = TRUE,
           sig.level = 0.05,
           insig = "blank",
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE)
#title = "Correlation matrix of the monitored air quality features using median hourly concentrations"
figb
dev.off() 

# different display of figures
library(ggpubr)
jpeg("corr of pollutants.jpg", width = 15, height = 8, units = 'in', res = 600)
ggarrange(figa, figb,
          labels = c("Room A", "Room B"),
          ncol = 2,
          nrow = 1)
dev.off() 


#######################################################################################################
#diurnal: https://bookdown.org/david_carslaw/openair/sections/trend-analysis/time-variation.html#output
library(openair)
library(calendR)
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "ambient outdoor sck purpleair_daily.csv")
summary(data)
df <- data.frame(data)
View(df)
##
df1 <- df[, c("Date", "Weekday", "Month","Week","Day", "IndoorBPM2.5SCK","IndoorBPM10SCK", "IndoorBTVOCs_SCK",
              "IndoorBCO2_SCK","IndoorBT_SCK","IndoorBRH_SCK")]
colnames(df1) <- c('Date', 'Weekday', 'Month', 'Week', 'Day', 'PM2.5', 'PM10',"tVOCs", 'CO2', "T","RH")
colnames(df1)
df1 <-na.omit(df1)
View(df1)

#dfout <- df1 %>% 
#  gather(key = "variable", value = "value", -Hour)
#View(dfout)

# https://r-coder.com/calendar-plot-r/
# https://r-charts.com/evolution/calendar-heatmap/
# https://medium.com/@rialma/calendar-heat-maps-in-r-with-tidyverse-68abb6919aa6
# https://www.columbia.edu/~sg3637/blog/Time_Series_Heatmaps.html
# calendar heatmap: https://www.columbia.edu/~sg3637/blog/Time_Series_Heatmaps.html
# https://www.r-bloggers.com/2021/08/calendar-heatmap-with-ggplot2-plotly/
# https://hslu-ige-laes.github.io/edar/calendar.html

####################################################################################
##### Month
# http://www.dartistics.com/adobeanalytics/int-heatmap.html
# A heatmap as calendar: https://www.r-bloggers.com/2020/12/a-heatmap-as-calendar/
# davg PM2.5
library(tidyverse)
library(lubridate)
library(ragg)
# import the data
df2 <- mutate(df1, 
              Week = case_when(Month == "Dec" & Week == 1~53,
                               Month == "Jan" & Week %in% 52:53 ~ 0,
                               TRUE ~ Week),
              PM2.5cat = cut(PM2.5, c(-1, 0, 15, 25, 40, 55)),
              text_col = ifelse(PM2.5cat %in% c("(0,15]", "(15,25]", "(24,40]", "(40,55]"),"white","black"))
df2
# color ramp
pubu <- RColorBrewer::brewer.pal(8, "Set2")
col_p <- colorRampPalette(pubu)
# theme calendar
theme_calendar <- function(){
  theme(aspect.ratio = 1/2,
        
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(),
        
        panel.grid = element_blank(),
        panel.background = element_blank(),
        
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        
        legend.position = "top",
        legend.text = element_text(hjust = .5),
        legend.title = element_text(size = 9, hjust = 1),
        
        plot.caption =  element_text(hjust = 1, size = 8),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(hjust = .5, size = 15, 
                                  face = "bold", 
                                  margin = margin(0,0,0.5,0, unit = "cm")),
        plot.subtitle = element_text(hjust = .5, size = 12)
  )
}
# plot calendar
# Reorder Facets in ggplot2 Plot in R: https://www.geeksforgeeks.org/reorder-facets-in-ggplot2-plot-in-r/
df2$Month <- factor(df2$Month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"))
cpm2.5 <- ggplot(df2, 
       aes(Weekday, -Week, fill = PM2.5cat)) +
  geom_tile(colour = "white", size = .4)  + 
  geom_text(aes(label = Day, colour = text_col), size = 2.5) +
  guides(fill = guide_colorsteps(barwidth = 25, 
                                 barheight = .4,
                                 title.position = "top")) +
  scale_fill_manual(values = c("white", col_p(13)),
                    na.value = "grey90", drop = FALSE) +
  scale_colour_manual(values = c("black", "white"), guide = FALSE) + 
  scale_x_discrete(expand = c(0,0), position = "bottom", 
                   limits = c("Mon", "Tue","Wed","Thu","Fri", "Sat", "Sun")) + 
  facet_wrap(.~ Month, nrow = 2, ncol = 4, scales = "free") +
  labs(title = expression(Indoor~PM[2.5]~from~Dec~2023~to~June~2024), 
       subtitle = expression(PM[2.5]),
       fill = "μg/m3") +
  theme_calendar()
cpm2.5
ggsave("pm2.5_calendar.png", height = 8, width = 16, device = agg_png())

####################################################################################
# CO2
# import the data
df2 <- mutate(df1, 
              Week = case_when(Month == "Dec" & Week == 1~53,
                               Month == "Jan" & Week %in% 52:53 ~ 0,
                               TRUE ~ Week),
              CO2cat = cut(CO2, c(-1, 0, 450, 800, 1000)),
              text_col = ifelse(CO2cat %in% c("(0,450]", "(450,800]", "(800,1000]"),"white","black"))
df2
# color ramp
pubu <- RColorBrewer::brewer.pal(8, "Accent")
col_p <- colorRampPalette(pubu)
# theme calendar
theme_calendar <- function(){
  theme(aspect.ratio = 1/2,
        
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(),
        
        panel.grid = element_blank(),
        panel.background = element_blank(),
        
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        
        legend.position = "top",
        legend.text = element_text(hjust = .5),
        legend.title = element_text(size = 9, hjust = 1),
        
        plot.caption =  element_text(hjust = 1, size = 8),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(hjust = .5, size = 15, 
                                  face = "bold", 
                                  margin = margin(0,0,0.5,0, unit = "cm")),
        plot.subtitle = element_text(hjust = .5, size = 12)
  )
}
# plot calendar
# Reorder Facets in ggplot2 Plot in R: https://www.geeksforgeeks.org/reorder-facets-in-ggplot2-plot-in-r/
df2$Month <- factor(df2$Month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"))
CO2 <- ggplot(df2, 
                 aes(Weekday, -Week, fill = CO2cat)) +
  geom_tile(colour = "white", size = .4)  + 
  geom_text(aes(label = Day, colour = text_col), size = 2.5) +
  guides(fill = guide_colorsteps(barwidth = 25, 
                                 barheight = .4,
                                 title.position = "top")) +
  scale_fill_manual(values = c("white", col_p(13)),
                    na.value = "grey90", drop = FALSE) +
  scale_colour_manual(values = c("black", "white"), guide = FALSE) + 
  scale_x_discrete(expand = c(0,0), position = "bottom", 
                   limits = c("Mon", "Tue","Wed","Thu","Fri", "Sat","Sun")) + 
  facet_wrap(.~ Month, nrow = 2, ncol = 4, scales = "free") +
  labs(title = expression(Indoor~CO[2]~from~Dec~2023~to~June~2024), 
       subtitle = expression(CO[2]),
       fill = "ppm") +
  theme_calendar()
CO2
ggsave("CO2_calendar.png", height = 8, width = 16, device = agg_png())


# plot combined figure
library(ggpubr)
jpeg("cal pm2.5 co2.jpg", width = 16, height = 14, units = 'in', res = 600)
cal <- ggarrange(cpm2.5, CO2, 
          ncol = 1,
          nrow = 2)
cal
dev.off() 

####################################################################################

##
# weekday
# tutorial: http://www.dartistics.com/adobeanalytics/int-heatmap.html
# daytime PM2.5
setwd("~/Desktop/TwinAIR/Indoor outdoor air quality in office in Dublin/SCK indoor outdoor/indoor outdoor data")
data = read.csv(file = "ambient outdoor sck purpleair_hr.csv")
summary(data)
dfr <- data.frame(data)
View(dfr)
##
df1r <- dfr[, c("Date", "weekday", "Month","Day", "Time", "IndoorBPM2.5SCK","IndoorB.PM10.SCK", "IndoorBTVOCs_SCK",
              "IndoorBCO2_SCK","IndoorBT_SCK","IndoorBRH_SCK")]
colnames(df1r) <- c('Date', 'Weekday', 'Month', 'Day', 'Hour', 'PM2.5', 'PM10',"tVOCs", 'CO2', "T","RH")
colnames(df1r)
df1r <-na.omit(df1r)
View(df1r)
# diruanl PM2.5
wdMeanPM2.5 <- df1r %>% group_by(Weekday, Hour) %>%
  summarise(Mean = mean(PM2.5))
wdMeanPM2.5

# diruanl PM10
wdMeanPM10 <- df1r %>% group_by(Weekday, Hour) %>%
  summarise(Mean = mean(PM10))
wdMeanPM10

# diruanl CO2
wdMeanCO2 <- df1r %>% group_by(Weekday, Hour) %>%
  summarise(Mean = mean(CO2))
wdMeanCO2

# diruanl tVOCs
wdMeantVOCs <- df1r %>% group_by(Weekday, Hour) %>%
  summarise(Mean = mean(tVOCs))
wdMeantVOCs

# diruanl T
wdMeanT <- df1r %>% group_by(Weekday, Hour) %>%
  summarise(Mean = mean(T))
wdMeanT

# diruanl RH
wdMeanRH <- df1r %>% group_by(Weekday, Hour) %>%
  summarise(Mean = mean(RH))
wdMeanRH

# plot
wPM2.5 <- ggplot(wdMeanPM2.5, aes(x = Weekday, y = Hour)) + 
  geom_tile(aes(fill = Mean)) + 
  scale_fill_gradient(name = 'PM2.5', low = "#f4fff6", high = "#0bb730") + 
  scale_y_reverse(name = "Hour", breaks = seq(0, 23, by = 2)) + 
  scale_x_discrete(expand = c(0,0), position = "bottom", 
                   labels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday", "Sunday")) + 
  labs(x = "Days of the Week")
wPM2.5

#CO2
wCO2 <- ggplot(wdMeanCO2, aes(x = Weekday, y = Hour)) + 
  geom_tile(aes(fill = Mean)) + 
  scale_fill_gradient(name = 'CO2', low = "#f4fff6", high = "#0bb730") + 
  scale_y_reverse(name = "Hour", breaks = seq(0, 23, by = 2)) + 
  scale_x_discrete(expand = c(0,0), position = "bottom", 
                   labels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday", "Sunday")) + 
  labs(x = "Days of the Week")
wCO2

#tVOCs
wtVOCs <- ggplot(wdMeantVOCs, aes(x = Weekday, y = Hour)) + 
  geom_tile(aes(fill = Mean)) + 
  scale_fill_gradient(name = 'tVOCs', low = "#f4fff6", high = "#0bb730") + 
  scale_y_reverse(name = "Hour", breaks = seq(0, 23, by = 2)) + 
  scale_x_discrete(expand = c(0,0), position = "bottom", 
                   labels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday", "Sunday")) + 
  labs(x = "Days of the Week")
wtVOCs

#T
wT <- ggplot(wdMeanT, aes(x = Weekday, y = Hour)) + 
  geom_tile(aes(fill = Mean)) + 
  scale_fill_gradient(name = 'Temp', low = "#f4fff6", high = "#0bb730") + 
  scale_y_reverse(name = "Hour", breaks = seq(0, 23, by = 2)) + 
  scale_x_discrete(expand = c(0,0), position = "bottom", 
                   labels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday", "Sunday")) + 
  labs(x = "Days of the Week")
wT

#RH
wRH <- ggplot(wdMeanRH, aes(x = Weekday, y = Hour)) + 
  geom_tile(aes(fill = Mean)) + 
  scale_fill_gradient(name = 'RH', low = "#f4fff6", high = "#0bb730") + 
  scale_y_reverse(name = "Hour", breaks = seq(0, 23, by = 2)) + 
  scale_x_discrete(expand = c(0,0), position = "bottom", 
                   labels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday", "Sunday")) + 
  labs(x = "Days of the Week")
wRH

# plot combined figure
library(ggpubr)
jpeg("weekday dirunal variation.jpg", width = 8, height = 15, units = 'in', res = 600)
diurnal <- ggarrange(wPM2.5, wCO2, wtVOCs, wT, wRH, 
          labels = c("a", "b", "c", "d", "e"),
          ncol = 1,
          nrow = 5)
diurnal
dev.off() 

# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
# The hourly heatmap: https://r-graph-gallery.com/283-the-hourly-heatmap.html
jpeg("cal and diruanl.jpg", width = 20, height = 12, units = 'in', res = 600)
ggarrange(cal, diurnal,
          ncol = 2,
          nrow = 1)
dev.off() 
