
# this script adds 2021 MC data to the historical MC data

Sys.setenv(TZ='HST')

library(readxl); library(stringr)

# load data
load("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/00_smartMeterData.R")

# read new MC data
dat_mcNew <- read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/ferc714/heco mc 2021.xlsx")

# form into single vector
dat_mcNew2 <- data.frame(date_time = as.character(rep(dat_mcNew$Date, each = 24)),
                         hour = str_pad(0:23, 2, pad = "0"))
dat_mcNew2$date_time <- as.POSIXct(paste0(dat_mcNew2$date_time, ' ',
                                          dat_mcNew2$hour, ':00'),
                                   format = "%Y-%m-%d %H:%M")
dat_mcNew2$hour <- NULL
dat_mcNew2$mc <- as.numeric(as.vector(t(dat_mcNew[2:ncol(dat_mcNew)])))

# add new rows of MC to full dataset, remove duplicates (if any)
mcHeco <- rbind(mcHeco, dat_mcNew2)
mcHeco <- mcHeco[!duplicated(mcHeco$date_time),]
mcHeco <- mcHeco[order(mcHeco$date_time),]
rownames(mcHeco) <- 1:nrow(mcHeco)

# save data
rm(dat_mcNew, dat_mcNew2); gc()
save.image("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/00_smartMeterData.R")
