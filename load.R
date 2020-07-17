require(data.table)
require(curl)
require(lubridate)
require(tidyr)
require(xts)

dGlobalUrl <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
dBrazilUrl <- "https://data.brasil.io/dataset/covid19/caso_full.csv.gz"

covidStats <- function( total )
{
  if( is.xts( total ) )
    total <- coredata( total )
  if( NCOL( total ) != 1 )
    simpleError( "covidStats: Invalid data")
  
  total_l <- log1p( total )
  day <- c( ifelse( total[ 1 ] == 0, 0, NA ), diff( total ) )
  day_m7 <- frollmean( day, 7 )
  day_m15 <- frollmean( day, 15 )
  day_s7 <- frollsum( day, 7 )
  day_sl7 <- log1p( day_s7 )
  growth_l7 <- growthRate( day, total, 7 )
  growth_l15 <- growthRate( day, total, 15 )

  result <- cbind( total, total_l, day, day_m7, day_m15, day_s7, day_sl7, growth_l7, growth_l15 )
  colnames( result ) <- c( "total", "total_l", "day", "day_m7", "day_m15", "day_s7", "day_sl7", "growth_l7", "growth_l15" )
  result
}

growthRate <- function( daily, total, period = 7L )
{
  sl <- log1p( frollsum( daily, period ) )
  tl <- log1p( total )
  ( sl - shift( sl, period ) ) / ( tl - shift( tl, period ) )
}

dGlobal <- fread( dGlobalUrl )
dBrazil <- fread( dBrazilUrl, encoding = "UTF-8" )

dBRStates <- dBrazil[ dBrazil$place_type == "state", -c(1,2,6,7,8,9,10,14,16) ]
dBRStates$date <- ymd( dBRStates$date )

BRStD <- pivot_wider( dBRStates[ , c(1,5,7) ], names_from = state, values_from = last_available_deaths, values_fill = 0 )
BRStD <- BRStD[ rowSums( BRStD[ , -1 ] ) != 0, ]
tsBRStD <- xts( BRStD[ , -1 ], BRStD[ , 1 ][[ 1 ]] )

dBRStD <- pivot_wider( dBRStates[ , c(1,7,8) ], names_from = state, values_from = new_deaths, values_fill = 0 )
dBRStD <- dBRStD[ rowSums( dBRStD[ , -1 ] ) != 0, ]
tsdBRStD <- xts( dBRStD[ , -1 ], dBRStD[ , 1 ][[ 1 ]] )

tsPRD <- xts( covidStats( tsBRStD$PR ), index( tsBRStD ) )
tsSPD <- xts( covidStats( tsBRStD$SP ), index( tsBRStD ) )
tsRJD <- xts( covidStats( tsBRStD$RJ ), index( tsBRStD ) )
tsBRD <- xts( covidStats( rowSums( BRStD[ , -1 ] ) ), BRStD[ ,1 ][[ 1 ]] )