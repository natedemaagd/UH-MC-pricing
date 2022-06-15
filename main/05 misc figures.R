
# script for any miscellaneous figures

library(ggplot2); library(lubridate); library(zoo); library(patchwork)
Sys.setenv(tz = 'HST')

# load data
dat_MC <- readRDS('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Intermediate/03 marginal cost data.rds')


##### plot MC distribution by hour of day #####
dat_MC$hour <- hour(dat_MC$datetime)
ggplot(dat_MC,
       aes(x = factor(as.character(hour),
                      levels = 0:23),
           y = mc_dollarsPerMWh)) +
  geom_boxplot(outlier.alpha = 0.5) +
  labs(x = 'Hour of day', y = 'MC ($/MWh)') +
  theme(text = element_text(size = 15),
        panel.grid.major.x = element_blank())
ggsave(filename = "D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/05 MC boxplot by hour of day.png",
       height = 4, width = 7, dpi = 300)




##### plot MC distribution by year-month #####
dat_MC$yearmonth <- substr(dat_MC$date, 1, 7)
dat_MC$yearmonth <- factor(dat_MC$yearmonth,
                           levels = unique(dat_MC$yearmonth))
plotdat <- dat_MC[dat_MC$date >= as.POSIXct('2018-01-01') & dat_MC$date <= as.POSIXct('2020-11-30') & dat_MC$mc_dollarsPerMWh >= 35,]  # limit data plotted for cleaner figure

# plot monthly MC price history
gplot_monthlyMC <- ggplot(data = plotdat,
                          aes(x = date, y = mc_dollarsPerMWh, group = yearmonth)) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(x = 'Year-month', y = 'MC ($/MWh)') +
  theme(text = element_text(size = 15),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1))
ggsave(gplot_monthlyMC, 
       filename = "D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/05 MC boxplot by year-month.png",
       height = 4, width = 9, dpi = 300)




##### plot MC distribution by year-month with oil prices #####

# oil price history data
dat_oilPrices <- read.csv("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/oilPriceHistory_daily.csv")
dat_oilPrices$Date <- as.Date(dat_oilPrices$Date, format = '%m/%d/%Y')
dat_oilPrices <- dat_oilPrices[dat_oilPrices$Date >= as.Date('2018-01-01') & dat_oilPrices$Date <= as.Date('2020-11-30') & dat_oilPrices$Close.Last >= 0,]

# melt MC and oil data
dat_MCtemp <- dat_MC[dat_MC$date >= as.POSIXct('2018-01-01') & dat_MC$date <= as.POSIXct('2020-11-30') & dat_MC$mc_dollarsPerMWh >= 35,]
plotdat <- data.frame(value    = c(dat_MCtemp$mc_dollarsPerMWh, dat_oilPrices$Close.Last),
                      variable = c(rep('MC ($/MWh)', times = nrow(dat_MCtemp)),
                                   rep('Oil price ($/barrel)', times = nrow(dat_oilPrices))),
                      date     = c(dat_MCtemp$date, dat_oilPrices$Date))
plotdat$yearmonth <- factor(substr(plotdat$date, 1, 7))

gplot <- ggplot(data = plotdat,
                aes(x = date, y = value, group = yearmonth)) +
  geom_boxplot(outlier.alpha = 0.1) +
  facet_grid(rows = vars(variable),scales = 'free_y') +
  theme(text = element_text(size = 15)) +
  labs(x = NULL, y = 'Price')

ggrob <- ggplotGrob(gplot)
ggrob$heights[[7]] <- unit(2, 'null')
plot(ggrob)

ggsave(ggrob, filename = "D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/05 monthly MC and monthly oil price.png",
       height = 7, width = 9, dpi = 300)
