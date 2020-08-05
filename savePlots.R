require( ggplot2 )
require( svglite )
require( purrr )

# if ( !exists("csPRD") || !exists("csSPD") || !exists("csRJD") || !exists("csBRD") )
# {
#   source( "load.R" )
# } else {
#   require( xts )
#   if ( end( csBRD ) != Sys.Date() )
#   {
#     rm( list = ls() )
#     source( "load.R" )
#   }
# }

fileFormats <- c( "png", "svg" )
plotFolder <- "plots"
plotCaption <- "Fonte: Brasil.IO"

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

savePlot <- function( plot, loc_name, suffix )
{
  plotNames <- paste( file.path( plotFolder, paste0( loc_name, suffix ) ),
                      fileFormats, sep = "." )
  map( plotNames, ggsave, plot = plot )
}

locs <- c( states, cities$city_ibge_code )
loc_names <- c( states, cities$city )

gl7s <- map2( locs, loc_names, plotGL7, data = BRStats )
m7s <- map2( locs, loc_names, plotM7, data = BRStats )

map2( gl7s, loc_names, savePlot, suffix = "_gl7" )
map2( m7s, loc_names, savePlot, suffix = "_m7" )

savePlot( BRStateStats %>% filter( total >= 10, location %in% states ) %>%
            ggplot( mapping = aes( x = total, y = week, color = location ) ) +
            geom_line() + geom_smooth( span = 0.5 ) +
            scale_x_log10( "Total (log)", breaks = c( 10, 100, 1000, 10000, 100000 ),
                           labels = c( "10", "100", "1k", "10k", "100k" ) ) +
            scale_y_log10( "Acumulado 7 dias (log)", breaks = c( 10, 100, 1000, 10000, 100000 ),
                           labels = c( "10", "100", "1k", "10k", "100k" ) ) +
            labs( title = "Nacional e Estados Selecionados - Taxa de crescimento de óbitos",
                  caption = "Fonte: Brasil.IO",
                  color = NULL ),
          "states", "_gl7"
)
# ggsave( file.path( "plots", "states_gl7.png" ) )
# ggsave( file.path( "plots", "states_gl7.svg" ) )

savePlot( BRCityStats %>%
            filter( total >= 10, location %in% cities$city_ibge_code ) %>%
            ggplot( mapping = aes( x = total, y = week, color = location ) ) +
            geom_line() + geom_smooth( span = 0.5 ) +
            scale_x_log10( "Total (log)", breaks = c( 10, 50, 200, 1000, 5000, 10000 ),
                           labels = c( "10", "50", "200", "1k", "5k", "10k" ) ) +
            scale_y_log10( "Acumulado 7 dias (log)", breaks = c( 10, 50, 200, 500, 1000 ),
                           labels = c( "10", "50", "200", "500", "1k" ) ) +
            labs( title = "Capitais Selecionadas - Taxa de crescimento de óbitos",
                  caption = "Fonte: Brasil.IO",
                  color = NULL ),
          "cities", "_gl7"
)
# ggsave( file.path( "plots", "cities_gl7.png" ) )
# ggsave( file.path( "plots", "cities_gl7.svg" ) )
