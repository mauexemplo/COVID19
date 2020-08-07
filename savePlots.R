require( ggplot2 )
require( svglite )
require( purrr )
require( lubridate )

if ( !exists( "states" ) ||
     !exists( "cities" ) ||
     !exists( "BRStats" ) ||
     today() - max( BRStats$date ) > days( 1 ) )
{
  source( "load.R" )
}

fileFormats <- c( "png", "svg" )
plotFolder <- "plots"
plotCaption <- "Fonte: Brasil.IO"

locs <- c( states, cities$city_ibge_code )
loc_names <- c( states, cities$city )

plotGL7 <- function( data, loc, loc_name = loc )
{
  filter( data, location == loc ) %>% 
    ggplot( mapping = aes( x = total, y = week ) ) +
    geom_line() +
    geom_smooth( span = 0.5 ) +
    scale_x_log10(
      "Total (log)",
      breaks = c( 10, 100, 1000, 10000, 100000 ),
      labels = c( "10", "100", "1k", "10k", "100k" )
    ) +
    scale_y_log10(
      "Acumulado 7 dias (log)",
      breaks = c( 10, 100, 1000, 10000, 100000 ),
      labels = c( "10", "100", "1k", "10k", "100k" )
    ) +
    labs( title = paste0( loc_name, " - Taxa de crescimento de óbitos" ),
          caption = plotCaption )
}

plotM7 <- function( data, loc, loc_name = loc )
{
  filter( data, location == loc ) %>% 
    ggplot( mapping = aes( x = date, y = day_m7 ) ) +
    geom_line() +
    geom_smooth( span = 0.5 ) +
    labs( title = paste0( loc_name, " - Média móvel (7 dias) de óbitos" ),
          caption = plotCaption,
          x = "Data", y = "Média 7 dias" )
}

state_gl7s <- map2( states, states, plotGL7, data = BRStats )
state_m7s <- map2( states, states, plotM7, data = BRStats )

city_gl7s <- map2( cities$city_ibge_code, cities$city, plotGL7, data = BRStats )
city_m7s <- map2( cities$city_ibge_code, cities$city, plotM7, data = BRStats )

sumStates_gl7 <- plotGL7( BRStats, "Brasil" )
sumStates_m7 <- plotM7( BRStats, "Brasil" )

savePlot <- function( plot, loc_name, suffix )
{
  plotNames <- paste( file.path( plotFolder, paste0( loc_name, suffix ) ),
                      fileFormats, sep = "." )
  map( plotNames, ggsave, plot = plot )
}

multiStates_gl7 <- BRStateStats %>%
  filter( total >= 10, location %in% states ) %>%
  ggplot( mapping = aes( x = total, y = week, color = location ) ) +
  geom_line() + geom_smooth( span = 0.5 ) +
  scale_x_log10( "Total (log)",
                 breaks = c( 10, 100, 1000, 10000, 100000 ),
                 labels = c( "10", "100", "1k", "10k", "100k" ) ) +
  scale_y_log10( "Acumulado 7 dias (log)",
                 breaks = c( 10, 100, 1000, 10000, 100000 ),
                 labels = c( "10", "100", "1k", "10k", "100k" ) ) +
  labs( title = "Nacional e Estados Selecionados - Taxa de crescimento de óbitos",
        caption = "Fonte: Brasil.IO", color = NULL )

multiCities_gl7 <- BRCityStats %>%
  filter( total >= 10, location %in% cities$city_ibge_code ) %>%
  ggplot( mapping = aes( x = total, y = week, color = location ) ) +
  geom_line() + geom_smooth( span = 0.5 ) +
  scale_x_log10( "Total (log)",
                 breaks = c( 10, 50, 200, 1000, 5000, 10000 ),
                 labels = c( "10", "50", "200", "1k", "5k", "10k" ) ) +
  scale_y_log10( "Acumulado 7 dias (log)",
                 breaks = c( 10, 50, 200, 500, 1000 ),
                 labels = c( "10", "50", "200", "500", "1k" ) ) +
  labs( title = "Capitais Selecionadas - Taxa de crescimento de óbitos",
        caption = "Fonte: Brasil.IO", color = NULL )

saveAll <- function()
{
  map2( state_gl7s, states, savePlot, suffix = "_gl7" )
  map2( state_m7s, states, savePlot, suffix = "_m7" )
  map2( city_gl7s, cities$city_ibge_code, savePlot, suffix = "_gl7" )
  map2( city_m7s, cities$city_ibge_code, savePlot, suffix = "_m7" )
  savePlot( sumStates_gl7, "Brasil", "_gl7" )
  savePlot( sumStates_m7, "Brasil", "_m7" )
  savePlot( multiStates_gl7, "states", "_gl7" )
  savePlot( multiCities_gl7, "cities", "_gl7" )
}
