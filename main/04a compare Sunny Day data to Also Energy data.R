
library(tidyverse); library(ggplot2); library(lubridate)
Sys.setenv(tz = 'HST')




#### Sunny Portal data #####

# load all data
dat_sunnyPortal <- list.files('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/UH/Solar production/Sunny Portal sample data/', pattern = 'Energy_and_Power')
dat_sunnyPortal <- lapply(dat_sunnyPortal, function(csv) read.csv(file = paste0('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/UH/Solar production/Sunny Portal sample data/', csv)))

# format Sunny Portal data
dat_sunnyPortal <- lapply(dat_sunnyPortal, function(df) separate(df, col = colnames(df)[[1]], into = c("time","kW_sunnyPortal"), sep = ";"))
dat_sunnyPortal <- do.call(rbind, dat_sunnyPortal)
dat_sunnyPortal$kW_sunnyPortal <- as.numeric(dat_sunnyPortal$kW_sunnyPortal)
dat_sunnyPortal$kW_sunnyPortal[is.na(dat_sunnyPortal$kW_sunnyPortal)] <- 0
dat_sunnyPortal <- dat_sunnyPortal %>%
  mutate(datetime = seq.POSIXt(from = as.POSIXct('2020-01-01 00:15'),
                               to   = as.POSIXct('2020-01-09 00:00'),
                               by   = '15 min'),
         .before = time)
tz(dat_sunnyPortal) <- 'HST'
dat_sunnyPortal$time <- NULL
dat_sunnyPortal$kWh_sunnyPortal <- dat_sunnyPortal$kW_sunnyPortal * 0.25  # convert original kW data to kWh at a 15-min interval, to match Also Solar data

# plot first day
plotdat <- dat_sunnyPortal[day(dat_sunnyPortal$datetime) == 1,]
ggplot(plotdat) +
  geom_line(aes(x = datetime, y = kWh_sunnyPortal), size = 1.3)
rm(plotdat)



##### Also Solar data #####

# load and format
dat_alsoEnergy <- read.csv("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/UH/Solar production/historicalProduction15min.csv")[1:2]
colnames(dat_alsoEnergy) <- c('datetime', 'kWh_alsoEnergy')
dat_alsoEnergy$datetime <- as.POSIXct(dat_alsoEnergy$datetime, format = "%m-%d-%Y %H:%M:%S", tz = 'HST')

# merge with Sunny Portal data
dat_combined <- left_join(dat_sunnyPortal, dat_alsoEnergy, by = 'datetime')

# plot first day
plotdat <- dat_alsoEnergy[day(dat_alsoEnergy$datetime) == 1 & month(dat_alsoEnergy$datetime) == 1 & year(dat_alsoEnergy$datetime) == 2020,]
ggplot(plotdat) +
  geom_line(aes(x = datetime, y = kWh_alsoEnergy), size = 1.3)
rm(plotdat)



##### plots #####

# plot first day, both datasets combined on one graph
dat_jan1 <- dat_combined[day(dat_combined$datetime) == 1,]
dat_jan1 <- data.frame(value = c(dat_jan1$kWh_sunnyPortal, dat_jan1$kWh_alsoEnergy),
                       datetime = rep(dat_jan1$datetime, times = 2),
                       source = rep(c('Sunny Portal', 'Also Energy'), each = nrow(dat_jan1)))

ggplot(data = dat_jan1) +
  geom_line(aes(x = datetime, y = value, color = source), size = 1.3) +
  theme(text = element_text(size = 15)) +
  labs(x = NULL, y = '15-minute kWh', color = 'Data\nsource')

rm(dat_jan1)

# plot average by hour
dat_combined$hour <- hour(dat_combined$datetime)
dat_hrly <- aggregate(list(dat_combined$kWh_sunnyPortal, dat_combined$kWh_alsoEnergy), list(dat_combined$hour), mean)
colnames(dat_hrly) <- c('hour', 'kWh_sunnyPortal', 'kWh_alsoEnergy')
dat_hrly <- reshape2::melt(dat_hrly, id.vars = 'hour')
dat_hrly$variable <- ifelse(dat_hrly$variable == 'kWh_sunnyPortal', 'Sunny Portal', 'Also Energy')

ggplot(dat_hrly) +
  geom_line(aes(x = hour, y = value, color = variable), size = 1.3) +
  theme(text = element_text(size = 15)) +
  labs(x = 'Hour of day', y = 'Mean 15-min kWh', color = 'Data\nsource')
