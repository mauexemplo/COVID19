require(data.table)
require(curl)
require(lubridate)
require(tidyr)
require(xts)
require(dplyr)

dGlobalUrl <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
dBrazilUrl <- "https://data.brasil.io/dataset/covid19/caso_full.csv.gz"
cities <- c( "São Paulo", "Rio de Janeiro", "Curitiba", "Brasília" )
states <- c( "SP", "RJ", "PR", "DF", "BR" )

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

load_BrasilIo <- function( url = dBrazilUrl )
{ fread( url, encoding = "UTF-8" ) }

load_JHUGSSEGlobal <- function( url = dGlobalUrl )
{ fread( url ) }

dBrazil <- load_BrasilIo()

BRStD <- dBrazil %>%
  filter( last_available_deaths > 0, place_type == "state" ) %>%
  select( date, state, last_available_deaths ) %>%
  pivot_wider( names_from = state,
               values_from = last_available_deaths,
               values_fill = 0 )

BRStateStats <- dBrazil %>% 
  rename( total = last_available_deaths ) %>% 
  filter( total > 0, place_type == "state" ) %>%
  select( date, state, total ) %>%
  bind_rows( group_by( ., date ) %>% 
               summarise( state = "BR", total = sum( total ) ) ) %>% 
  arrange( state, date ) %>% 
  group_by( state ) %>% 
  mutate( day = total - lag( total, default = 0 ),
          week = frollsum( day, 7 ),
          week_m = frollmean( day, 7 ),
          week_l = log1p( week ),
          total_l = log1p( total ) ) %>% 
  ungroup()

tsBRStD <-
  xts( select( BRStD, sort( colnames( BRStD ) ), -date ),
       ymd( BRStD$date ) )

# dBRStates <- dBrazil[ dBrazil$place_type == "state", -c(1,2,6,7,8,9,10,14,16) ]
# dBRStates$date <- ymd( dBRStates$date )

# BRStD <- pivot_wider( dBRStates[ , c(1,5,7) ], names_from = state, values_from = last_available_deaths, values_fill = 0 )
# BRStD <- BRStD[ rowSums( BRStD[ , -1 ] ) != 0, ]
# tsBRStD <- xts( BRStD[ , -1 ], BRStD[ , 1 ][[ 1 ]] )

# dBRStD <- pivot_wider( dBRStates[ , c(1,7,8) ], names_from = state, values_from = new_deaths, values_fill = 0 )
# dBRStD <- dBRStD[ rowSums( dBRStD[ , -1 ] ) != 0, ]
# tsdBRStD <- xts( dBRStD[ , -1 ], dBRStD[ , 1 ][[ 1 ]] )

csPRD <- xts( covidStats( tsBRStD$PR ), index( tsBRStD ) )
csSPD <- xts( covidStats( tsBRStD$SP ), index( tsBRStD ) )
csRJD <- xts( covidStats( tsBRStD$RJ ), index( tsBRStD ) )
csBRD <- xts( covidStats( rowSums( BRStD[ , -1 ] ) ), ymd( BRStD$date ) )