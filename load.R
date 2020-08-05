require(data.table)
require(curl)
require(lubridate)
require(tidyr)
require(dplyr)

dGlobalUrl <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
dBrazilUrl <- "https://data.brasil.io/dataset/covid19/caso_full.csv.gz"
cities <- list( city = c( "Belo Horizonte", "Brasília", "Curitiba",
                          "Fortaleza", "Goiânia", "Manaus", "Recife",
                          "Rio de Janeiro", "Salvador", "São Paulo" ),
                city_ibge_code = c( "3106200", "5300108", "4106902", "2304400",
                                    "5208707", "1302603", "2611606", "3304557",
                                    "2927408", "3550308" )
)
states <- c( "MG", "DF", "PR", "CE", "GO", "AM", "PE", "RJ", "BA", "SP", "Brasil" )

# covidStats <- function( total )
# {
#   if( is.xts( total ) )
#     total <- coredata( total )
#   if( NCOL( total ) != 1 )
#     simpleError( "covidStats: Invalid data")
#   
#   total_l <- log1p( total )
#   day <- c( ifelse( total[ 1 ] == 0, 0, NA ), diff( total ) )
#   day_m7 <- frollmean( day, 7 )
#   day_m15 <- frollmean( day, 15 )
#   day_s7 <- frollsum( day, 7 )
#   day_sl7 <- log1p( day_s7 )
#   growth_l7 <- growthRate( day, total, 7 )
#   growth_l15 <- growthRate( day, total, 15 )
# 
#   result <- cbind( total, total_l, day, day_m7, day_m15, day_s7, day_sl7, growth_l7, growth_l15 )
#   colnames( result ) <- c( "total", "total_l", "day", "day_m7", "day_m15", "day_s7", "day_sl7", "growth_l7", "growth_l15" )
#   result
# }
# 
# growthRate <- function( daily, total, period = 7L )
# {
#   sl <- log1p( frollsum( daily, period ) )
#   tl <- log1p( total )
#   ( sl - shift( sl, period ) ) / ( tl - shift( tl, period ) )
# }

load_BrasilIo <- function( url = dBrazilUrl )
{ fread( url, encoding = "UTF-8" ) }

load_JHUGSSEGlobal <- function( url = dGlobalUrl )
{ fread( url ) }

dBrazil <- load_BrasilIo()

# BRStD <- dBrazil %>%
#   filter( last_available_deaths > 0, place_type == "state" ) %>%
#   select( date, state, last_available_deaths ) %>%
#   pivot_wider( names_from = state,
#                values_from = last_available_deaths,
#                values_fill = 0 )

BRStateStats <- dBrazil %>% 
  rename( total = last_available_deaths ) %>% 
  filter( total > 0, place_type == "state" ) %>%
  mutate( date = ymd( date ), location = state ) %>% 
  select( date, location, total ) %>%
  bind_rows( group_by( ., date ) %>% 
               summarise( location = "Brasil", total = sum( total ) ) ) %>% 
  arrange( location, date ) %>% 
  group_by( location ) %>% 
  mutate( day = total - lag( total, default = 0 ),
          week = frollsum( day, 7 ),
          day_m7 = frollmean( day, 7 ),
          week_l = log1p( week ),
          total_l = log1p( total ) ) %>% 
  ungroup()

BRCityStats <- dBrazil %>% 
  rename( total = last_available_deaths ) %>% 
  filter( total > 0, place_type == "city" ) %>%
  mutate( date = ymd( date ), location = as.character( city_ibge_code ) ) %>% 
  select( date, location, total ) %>%
  arrange( location, date ) %>% 
  group_by( location ) %>% 
  mutate( day = total - lag( total, default = 0 ),
          week = frollsum( day, 7 ),
          day_m7 = frollmean( day, 7 ),
          week_l = log1p( week ),
          total_l = log1p( total ) ) %>% 
  ungroup()

BRStats <- bind_rows( BRStateStats, BRCityStats )

todayStates <- BRStateStats %>%
  mutate( growth_l7 = ( week_l - shift( week_l, 7 ) ) /
            ( total_l - shift( total_l, 7 ) ) ) %>%
  filter( date == today() ) %>% arrange( desc( growth_l7 ) )

todayCities <- BRCityStats %>%
  mutate( growth_l7 = ( week_l - shift( week_l, 7 ) ) /
            ( total_l - shift( total_l, 7 ) ) ) %>%
  filter( date == today() ) %>% arrange( desc( growth_l7 ) )

# tsBRStD <- xts( select( BRStD, sort( colnames( BRStD ) ), -date ),
#                 ymd( BRStD$date ) )
# 
# csPRD <- xts( covidStats( tsBRStD$PR ), index( tsBRStD ) )
# csSPD <- xts( covidStats( tsBRStD$SP ), index( tsBRStD ) )
# csRJD <- xts( covidStats( tsBRStD$RJ ), index( tsBRStD ) )
# csBRD <- xts( covidStats( rowSums( BRStD[ , -1 ] ) ), ymd( BRStD$date ) )