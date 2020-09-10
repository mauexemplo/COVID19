require( ggplot2 )
require( svglite )
require( purrr )
require( lubridate )
require( stringr )

if ( !exists( "BRStats" ) ||
     today() - max( BRStats$date ) > days( 1 ) )
{
  source( "load.R", encoding = "UTF-8" )
}

plotFolder <- "plots"
plotCaption <- "Fonte: Brasil.IO"
JHUCaption <- "Fonte: JHU GSSE no GitHub"
defaultPlotFileFormat <- "png"

plot_GL7 <- function( data, loc_name, ... )
{
  ggplot( data, mapping = aes( x = total, y = week ) ) +
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

plot_LocGL7 <- function( data, loc, loc_name = loc, ... )
{
  plot_GL7( filter( data, location == loc ), loc_name, ... )
}

plot_M7 <- function( data, loc_name, ... )
{
  ggplot( data, mapping = aes( x = date, y = day_m7 ) ) +
    geom_line() +
    geom_smooth( span = 0.5 ) +
    labs( title = paste0( loc_name, " - Média móvel (7 dias)" ),
          x = "Data", y = "Média 7 dias", ... )
}

plot_LocM7 <- function( data, loc, loc_name = loc, ... )
{
  plot_M7( filter( data, location == loc ), loc_name, ... )
}

state_gl7s <- map2( states, states, plot_LocGL7, data = BRStats,
                    caption = plotCaption )
state_m7s <- map2( states, states, plot_LocM7, data = BRStats,
                   caption = plotCaption )

city_gl7s <- map2( names( cities ), names( cities ), plot_LocGL7,
                   data = BRStats, caption = plotCaption )
city_m7s <- map2( names( cities ), names( cities ), plot_LocM7,
                  data = BRStats, caption = plotCaption )

country_gl7s <- map2( countries, names( countries ), plot_LocGL7,
                      data = JHUStats, caption = JHUCaption )
country_m7s <- map2( countries, names( countries ), plot_LocM7,
                      data = JHUStats, caption = JHUCaption )

sumStates_gl7 <- plot_LocGL7( BRStats, "Brasil", caption = plotCaption )
sumStates_m7 <- plot_LocM7( BRStats, "Brasil", caption = plotCaption )

savePlot <- function( plot, file_name, formats = defaultPlotFileFormat )
{
  plotNames <- paste( file.path( plotFolder, file_name ), formats, sep = "." )
  map( plotNames, ggsave, plot = plot )
}

multiStates_gl7 <- BRStats %>%
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
        caption = plotCaption, color = NULL )

multiCities_gl7 <- BRStats %>%
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
        caption = plotCaption, color = NULL )

multiCountries_gl7 <- JHUStats %>%
  filter( location %in% countries ) %>%
  ggplot( mapping = aes( x = total, y = week, color = location ) ) +
  geom_line() + geom_smooth( span = 0.5, se = FALSE ) +
  scale_x_log10( "Total (log)",
                 breaks = c( 50, 200, 1000, 5000, 10000, 50000, 100000, 250000 ),
                 labels = c( "50", "200", "1k", "5k", "10k", "50k", "100k", "250k" ) ) +
  scale_y_log10( "Acumulado 7 dias (log)",
                 breaks = c( 50, 200, 500, 1000, 5000, 10000, 25000 ),
                 labels = c( "50", "200", "500", "1k", "5k", "10k", "25k" ) ) +
  scale_color_discrete( breaks = countries, labels = names( countries ) ) +
  labs( title = "Países Selecionados - Taxa de crescimento de óbitos",
        caption = JHUCaption, color = NULL )

multiMetros_gl7 <- BRStats %>% 
  filter( total >= 10, str_detect( location, prefix_metros ) ) %>%
  ggplot( mapping = aes( x = total, y = week, color = location ) ) +
  geom_line() + geom_smooth( span = 0.5 ) +
  scale_x_log10( "Total (log)",
                 breaks = c( 10, 50, 200, 1000, 5000, 10000 ),
                 labels = c( "10", "50", "200", "1k", "5k", "10k" ) ) +
  scale_y_log10( "Acumulado 7 dias (log)",
                 breaks = c( 10, 50, 200, 500, 1000 ),
                 labels = c( "10", "50", "200", "500", "1k" ) ) +
  labs( title = "Regiões Metropolitanas Selecionadas - Taxa de crescimento de óbitos",
        caption = plotCaption, color = NULL )


saveAll <- function( fileFormats = "svg" )
{
  map2( state_gl7s, paste0( states, "_gl7" ), savePlot, formats = fileFormats )
  map2( state_m7s, paste0( states, "_m7" ), savePlot, formats = fileFormats )

  map2( city_gl7s, paste0( cities, "_gl7" ), savePlot, formats = fileFormats )
  map2( city_m7s, paste0( cities, "_m7" ), savePlot, formats = fileFormats )
  
  savePlot( sumStates_gl7, "Brasil_gl7", fileFormats )
  savePlot( sumStates_m7, "Brasil_m7", fileFormats )
  
  savePlot( multiStates_gl7, "states_gl7", fileFormats )
  
  savePlot( multiCities_gl7, "cities_gl7", fileFormats )
  
  savePlot( multiCountries_gl7, "countries_gl7", fileFormats )
  
  savePlot( multiMetros_gl7, "metros_gl7", fileFormats )
}
