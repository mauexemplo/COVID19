require(data.table)
require(curl)
require(lubridate)
require(tidyr)
require(xts)
require(dplyr)

dGlobalUrl <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
dBrazilUrl <- "https://data.brasil.io/dataset/covid19/caso_full.csv.gz"
cities <- list( city = c( "Belo Horizonte", "Brasília", "Curitiba",
                          "Fortaleza", "Goiânia", "Manaus", "Recife",
                          "Rio de Janeiro", "Salvador", "São Paulo" ),
                city_ibge_code = c( 3106200L, 5300108L, 4106902L, 2304400L,
                                    5208707L, 1302603L, 2611606L, 3304557L,
                                    2927408L, 3550308L )
)
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
          day_m7 = frollmean( day, 7 ),
          week_l = log1p( week ),
          total_l = log1p( total ) ) %>% 
  ungroup()

BRCityStats <- dBrazil %>% 
  rename( total = last_available_deaths ) %>% 
  filter( total > 0, place_type == "city" ) %>%
  select( date, city, city_ibge_code, state, total ) %>%
  arrange( city_ibge_code, date ) %>% 
  group_by( city_ibge_code, state ) %>% 
  mutate( day = total - lag( total, default = 0 ),
          week = frollsum( day, 7 ),
          day_m7 = frollmean( day, 7 ),
          week_l = log1p( week ),
          total_l = log1p( total ) ) %>% 
  ungroup()

todayStates <- BRStateStats %>%
  mutate( growth_l7 = ( week_l - shift( week_l, 7 ) ) /
            ( total_l - shift( total_l, 7 ) ) ) %>%
  filter( date == today() ) %>% arrange( desc( growth_l7 ) )

todayCities <- BRCityStats %>%
  mutate( growth_l7 = ( week_l - shift( week_l, 7 ) ) /
            ( total_l - shift( total_l, 7 ) ) ) %>%
  filter( date == today() ) %>% arrange( desc( growth_l7 ) )

tsBRStD <- xts( select( BRStD, sort( colnames( BRStD ) ), -date ),
                ymd( BRStD$date ) )

csPRD <- xts( covidStats( tsBRStD$PR ), index( tsBRStD ) )
csSPD <- xts( covidStats( tsBRStD$SP ), index( tsBRStD ) )
csRJD <- xts( covidStats( tsBRStD$RJ ), index( tsBRStD ) )
csBRD <- xts( covidStats( rowSums( BRStD[ , -1 ] ) ), ymd( BRStD$date ) )