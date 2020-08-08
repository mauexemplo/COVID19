require( ggplot2 )
require( svglite )
require( purrr )
require( lubridate )

if ( !exists( "BRStats" ) ||
     today() - max( BRStats$date ) > days( 1 ) )
{
  source( "load.R" )
}

plotFolder <- "plots"
plotCaption <- "Fonte: Brasil.IO"
defaultPlotFileFormat <- "png"

plotGL7 <- function( data, loc, loc_name = loc, ... )
{
  filter( data, location == loc ) %>% 
    ggplot( mapping = aes( x = total, y = week ) ) +
    geom_line() +
    geom_smooth( span = 0.5 ) +
    scale_x_log10( "Total (log)",
      breaks = c( 10, 100, 1000, 10000, 100000 ),
      labels = c( "10", "100", "1k", "10k", "100k" )
    ) +
    scale_y_log10( "Acumulado 7 dias (log)",
      breaks = c( 10, 100, 1000, 10000, 100000 ),
      labels = c( "10", "100", "1k", "10k", "100k" )
    ) +
    labs( title = paste0( loc_name, " - Taxa de crescimento" ), ... )
}

plotM7 <- function( data, loc, loc_name = loc, ... )
{
  filter( data, location == loc ) %>% 
    ggplot( mapping = aes( x = date, y = day_m7 ) ) +
    geom_line() +
    geom_smooth( span = 0.5 ) +
    labs( title = paste0( loc_name, " - Média móvel (7 dias)" ),
          x = "Data", y = "Média 7 dias", ... )
}

state_gl7s <- map2( states, states, plotGL7, data = BRStats,
                    caption = plotCaption )
state_m7s <- map2( states, states, plotM7, data = BRStats,
                   caption = plotCaption )

city_gl7s <- map2( names( cities ), names( cities ), plotGL7,
                   data = BRStats, caption = plotCaption )
city_m7s <- map2( names( cities ), names( cities ), plotM7,
                  data = BRStats, caption = plotCaption )

sumStates_gl7 <- plotGL7( BRStats, "Brasil", caption = plotCaption )
sumStates_m7 <- plotM7( BRStats, "Brasil", caption = plotCaption )

savePlot <- function( plot, file_name, formats = defaultPlotFileFormat )
{
  plotNames <- paste( file.path( plotFolder, file_name ), formats, sep = "." )
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
  labs( title = "Estados Selecionados - Taxa de crescimento de óbitos",
        caption = "Fonte: Brasil.IO", color = NULL )

multiCities_gl7 <- BRCityStats %>%
  filter( total >= 10, location %in% names( cities ) ) %>%
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

saveAll <- function( fileFormats = c( "png", "svg" ) )
{
  map2( state_gl7s, paste0( states, "_gl7" ), savePlot, formats = fileFormats )
  map2( state_m7s, paste0( states, "_m7" ), savePlot, formats = fileFormats )

  map2( city_gl7s, paste0( cities, "_gl7" ), savePlot, formats = fileFormats )
  map2( city_m7s, paste0( cities, "_m7" ), savePlot, formats = fileFormats )
  
  savePlot( sumStates_gl7, "Brasil_gl7", fileFormats )
  savePlot( sumStates_m7, "Brasil_m7", fileFormats )
  
  savePlot( multiStates_gl7, "states_gl7", fileFormats )
  
  savePlot( multiCities_gl7, "cities_gl7", fileFormats )
}
