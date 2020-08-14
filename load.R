require( data.table )
require( curl )
require( lubridate )
require( tidyr )
require( dplyr )

dGlobalUrl <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
dBrazilUrl <- "https://data.brasil.io/dataset/covid19/caso_full.csv.gz"
cities <- c( `Belo Horizonte - MG` = "3106200", `Brasília - DF` = "5300108",
             `Curitiba - PR` = "4106902", `Fortaleza - CE` = "2304400",
             `Goiânia - GO` = "5208707", `Manaus - AM` = "1302603",
             `Recife - PE` = "2611606", `Rio de Janeiro - RJ` = "3304557",
             `Salvador - BA` = "2927408", `São Paulo - SP` = "3550308" )
states <- c( "MG", "DF", "PR", "CE", "GO", "AM", "PE", "RJ", "BA", "SP" )
PRSaudeUrl <- "http://www.saude.pr.gov.br/Pagina/Coronavirus-COVID-19"

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

load_homePRSaude <- function( url = PRSaudeUrl )
{
  require( rvest )
  read_html( PRSaudeUrl, encoding = "UTF-8" )
}

load_PRGeral <- function( home = load_homePRSaude(), date = today() )
{
  # TODO: Add date filter
  
  res <- home %>%
    html_nodes( xpath = "//a[text()='Geral.csv']" ) %>%
    html_attr( "href" )
  fread( res[ 1 ], encoding = "UTF-8" )
}

load_PRCasos <- function( home = load_homePRSaude(), date = today() )
{
  # TODO: Add date filter
  
  res <- home %>%
    html_nodes( xpath = "//a[contains(.,'Casos e ')]" ) %>%
    html_attr( "href" )
  fread( res[ 1 ], encoding = "UTF-8" )
}

parse_PRCasos <- function( data, date = today() )
{
  data %>% mutate( date = date ) %>%
    select( date, location = Municipio, deaths = Obito )
}

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
  mutate( day = as.integer( total - lag( total, default = 0 ) ),
          week = as.integer( frollsum( day, 7 ) ),
          day_m7 = frollmean( day, 7 ),
          week_l = log1p( week ),
          total_l = log1p( total ) ) %>% 
  ungroup()

BRCityStats <- dBrazil %>% 
  rename( total = last_available_deaths ) %>% 
  filter( total > 0, place_type == "city" ) %>%
  mutate( date = ymd( date ), location = paste( city, state, sep = " - " ) ) %>% 
  select( date, location, total ) %>%
  arrange( location, date ) %>% 
  group_by( location ) %>% 
  mutate( day = as.integer( total - lag( total, default = 0 ) ),
          week = as.integer( frollsum( day, 7 ) ),
          day_m7 = frollmean( day, 7 ),
          week_l = log1p( week ),
          total_l = log1p( total ) ) %>% 
  ungroup()

BRStats <- bind_rows( BRStateStats, BRCityStats )
rm( BRCityStats )
rm( BRStateStats )

lastStats <- BRStats %>%
  mutate( growth_l7 = ( week_l - shift( week_l, 7 ) ) / ( total_l - shift( total_l, 7 ) ) ) %>%
  group_by( location ) %>% filter( date == max( date ) ) %>% ungroup() %>%
  arrange( desc( growth_l7 ) )
