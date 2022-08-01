
# script for any miscellaneous figures

library(ggplot2); library(lubridate); library(zoo); library(patchwork)
library(dplyr)
Sys.setenv(tz = 'HST')

# load MC data
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




##### plot MC history against average cost per kWh history #####

# load recent bill data
dat_UHbill <- readRDS('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Intermediate/04b monthly bills under various PV scenarios - no battery.rds')

# load and format older bill data
dat_UHbill2 <- read.csv("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/UH/June '12 - Jan '17 Monthly Billing dataframe friendly.csv")
colnames(dat_UHbill2)[colnames(dat_UHbill2) == 'Cal..year...month'] <- 'date'
colnames(dat_UHbill2)[colnames(dat_UHbill2) == 'Billed.Usage..KWH.'] <- 'consumption_kWh_pv1MW'
colnames(dat_UHbill2)[colnames(dat_UHbill2) == 'Line.Item.Amount'] <- 'totalBillDollars_DSpricing_pv1MW'
dat_UHbill2 <- dat_UHbill2[c('date', 'consumption_kWh_pv1MW', 'totalBillDollars_DSpricing_pv1MW')]
dat_UHbill2$date <- gsub('/', '-', dat_UHbill2$date)
dat_UHbill2$date <- as.Date(paste0('01-', dat_UHbill2$date), format = '%d-%m-%Y')
dat_UHbill2$consumption_kWh_pv1MW <-
  as.numeric(gsub(',', '', dat_UHbill2$consumption_kWh_pv1MW))

# merge billing data
dat_UHbill <- dat_UHbill[colnames(dat_UHbill) %in% colnames(dat_UHbill2)]
dat_UHbill <- rbind(dat_UHbill, dat_UHbill2)
rm(dat_UHbill2)

# aggregate MC by year-month
dat_MC$year_month <- substr(dat_MC$date, 1, 7)
dat_MCmonthlyMean <- aggregate(dat_MC$mc_dollarsPerMWh, list(dat_MC$year_month),
                               mean, na.rm = TRUE)
colnames(dat_MCmonthlyMean) <- c('date', 'meanMC')
dat_MCmonthlyMean$date <- as.Date(paste0(dat_MCmonthlyMean$date, '-01'))

# aggregate $/kWh for UH bill
dat_UHbill$dollarsPerkWh <-
  dat_UHbill$totalBillDollars_DSpricing_pv1MW / dat_UHbill$consumption_kWh_pv1MW
dat_UHmonthlyMeanPrice <- aggregate(dat_UHbill$dollarsPerkWh,
                                    list(dat_UHbill$date),
                                    mean, na.rm = TRUE)
colnames(dat_UHmonthlyMeanPrice) <- c('date', 'dollarsPerkWh')

# merge mean data
dat <- left_join(dat_MCmonthlyMean, dat_UHmonthlyMeanPrice, 'date')

# format variables
dat$meanMC <- dat$meanMC / 1000

# plot
plotdat <- data.frame(date = rep(seq.Date(min(dat$date), max(dat$date), 'month'),
                                 each = 3))
plotdat2 <- data.frame(date = dat$date,
                       value = c(dat$dollarsPerkWh, dat$meanMC,
                                 dat$dollarsPerkWh - dat$meanMC),
                       facet = c(rep('Average Prices', times = nrow(dat)*2),
                                 rep('Difference', times =nrow(dat))),
                       label = rep(c('Average price', 'Average MC', 'Avg. price - Avg. MC'),
                                   each = nrow(dat)))
plotdat <- left_join(plotdat, plotdat2, 'date')
plotdat$facet <- rep(c('Average prices', 'Average prices', 'Difference'),
                     length.out = nrow(plotdat))
plotdat$label <- rep(c('Avg. price', 'Avg. MC', 'Avg. price - Avg. MC'),
                     length.out = nrow(plotdat))
rm(plotdat2)

plotdat <- plotdat[plotdat$date >= '2012-01-01' & plotdat$date <= '2020-10-31',]
ggplot(data = plotdat, aes(x = date, y = value,color = label)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = '$/kWh', color = NULL) +
  facet_grid(rows = vars(facet), scales = 'free_y') +
  geom_smooth(data = subset(plotdat, facet == 'Difference'),
              method = 'lm', color = 'gray35') +
  theme(text = element_text(size = 15), legend.position = 'bottom')
ggsave(filename = "D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/05 difference in avg MC and avg total price.png",
       height = 5, width = 8, dpi = 300)
