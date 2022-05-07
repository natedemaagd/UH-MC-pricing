library(stringr)

date_format <- function(df){
  df$month <- str_pad(df$month, width = 2, side = 'left', pad = 0)
  df$year_month <- paste(df$year, df$month, sep = '-')
  df <- df[c(4,5)]
  return(df)
}