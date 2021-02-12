if ( !exists( "load_R" ) ) source( "load.R", encoding = "UTF-8" )

require( ggplot2 )
require( svglite )
require( purrr )
require( lubridate )
require( stringr )

plotFolder <- "plots"
lastUpdate <- format( Sys.time(), "%d/%m/%Y %H:%M" )
plotCaption <- paste0( "Fontes: Brasil.IO e Secretaria da Saúde PR (em ", lastUpdate, ")" )
JHUCaption <- paste( "Fonte: JHU GSSE no GitHub -", lastUpdate )
defaultPlotFileFormat <- "png"

plot_GL7 <- function( data, loc_name, ... )
{
  ggplot2::ggplot( data, mapping = ggplot2::aes( x = total, y = week ) ) +
    ggplot2::geom_line() +
    ggplot2::geom_smooth( span = 0.5 ) +
    ggplot2::scale_x_log10( "Total (log)",
                   breaks = c( 10, 100, 1000, 10000, 100000 ),
                   labels = c( "10", "100", "1k", "10k", "100k" )
    ) +
    ggplot2::scale_y_log10( "Acumulado 7 dias (log)",
                   breaks = c( 10, 100, 1000, 10000, 100000 ),
                   labels = c( "10", "100", "1k", "10k", "100k" )
    ) +
    ggplot2::labs( title = paste( loc_name, "- Taxa de crescimento" ), ... )
}

plot_LocGL7 <- function( data, loc, loc_name = loc, ... )
{
  plot_GL7( dplyr::filter( data, location == loc ), loc_name, ... )
}

plot_M7 <- function( data, loc_name, ... )
{
  ggplot2::ggplot( data, mapping = ggplot2::aes( x = date, y = day_m7 ) ) +
    ggplot2::geom_line() +
    ggplot2::geom_smooth( span = 0.5 ) +
    ggplot2::labs( title = paste( loc_name, "- Média móvel (7 dias)" ), x = "Data", y = "Média 7 dias", ... )
}

plot_LocM7 <- function( data, loc, loc_name = loc, ... )
{
  plot_M7( dplyr::filter( data, location == loc ), loc_name, ... )
}

multiPlotNamedLocGL7 <- function( data, loc, title = "Taxa de Crescimento" )
{
  dplyr::filter( data, location %in% loc ) %>%
    ggplot2::ggplot( mapping = ggplot2::aes( x = total, y = week, colour = name ) ) +
    ggplot2::geom_smooth( span = 0.4 ) +
    ggplot2::scale_x_log10( "Total (log)",
                            breaks = c( 10, 100, 1000, 10000, 100000 ),
                            labels = c( "10", "100", "1k", "10k", "100k" )
    ) +
    ggplot2::scale_y_log10( "Acumulado 7 dias (log)",
                            breaks = c( 10, 100, 1000, 10000, 100000 ),
                            labels = c( "10", "100", "1k", "10k", "100k" )
    ) +
    ggplot2::labs( title = title, colour = "Localidade", caption = plotCaption )
  
}

state_gl7s <- purrr::map2( states, states, plot_LocGL7, data = BRStats,
                    caption = plotCaption )
state_m7s <- purrr::map2( states, states, plot_LocM7, data = BRStats,
                   caption = plotCaption )

city_gl7s <- purrr::map2( names( cities ), names( cities ), plot_LocGL7,
                   data = BRStats, caption = plotCaption )
city_m7s <- purrr::map2( names( cities ), names( cities ), plot_LocM7,
                  data = BRStats, caption = plotCaption )

country_gl7s <- purrr::map2( countries, names( countries ), plot_LocGL7,
                      data = JHUStats, caption = JHUCaption )
country_m7s <- purrr::map2( countries, names( countries ), plot_LocM7,
                      data = JHUStats, caption = JHUCaption )

sumStates_gl7 <- plot_LocGL7( BRStats, "Brasil", caption = plotCaption )
sumStates_m7 <- plot_LocM7( BRStats, "Brasil", caption = plotCaption )

savePlot <- function( plot, file_name, formats = defaultPlotFileFormat )
{
  plotNames <- paste( file.path( plotFolder, file_name ), formats, sep = "." )
  purrr::map( plotNames, ggplot2::ggsave, plot = plot, scale = 1.5 )
}

multiStates_gl7 <- BRStats %>%
  filter( total >= 10, location %in% hls_State ) %>%
  ggplot( mapping = aes( x = total, y = week, color = location ) ) +
  geom_smooth( span = 0.4 ) +
  scale_x_log10( "Total (log)",
                 breaks = c( 10, 100, 1000, 10000, 100000 ),
                 labels = c( "10", "100", "1k", "10k", "100k" ) ) +
  scale_y_log10( "Acumulado 7 dias (log)",
                 breaks = c( 10, 100, 1000, 10000, 100000 ),
                 labels = c( "10", "100", "1k", "10k", "100k" ) ) +
  theme( legend.position = "bottom" ) +
  labs( title = "Estados Selecionados - Taxa de crescimento de óbitos",
        caption = plotCaption, color = NULL )

multiCities_gl7 <- BRStats %>%
  filter( total >= 10, location %in% names( cities ) ) %>%
  ggplot( mapping = aes( x = total, y = week, color = location ) ) +
  geom_smooth( span = 0.4 ) +
  scale_x_log10( "Total (log)",
                 breaks = c( 10, 50, 200, 1000, 5000, 10000 ),
                 labels = c( "10", "50", "200", "1k", "5k", "10k" ) ) +
  scale_y_log10( "Acumulado 7 dias (log)",
                 breaks = c( 10, 50, 200, 500, 1000 ),
                 labels = c( "10", "50", "200", "500", "1k" ) ) +
  theme( legend.position = "bottom" ) +
  labs( title = "Capitais Selecionadas - Taxa de crescimento de óbitos",
        caption = plotCaption, color = NULL )

multiCountries_gl7 <- JHUStats %>%
  filter( location %in% hls_Global ) %>%
  ggplot( mapping = aes( x = total, y = week, color = location ) ) +
  geom_smooth( span = 0.4 ) +
  scale_x_log10( "Total (log)",
                 breaks = c( 50, 200, 1000, 5000, 10000, 50000, 100000, 250000 ),
                 labels = c( "50", "200", "1k", "5k", "10k", "50k", "100k", "250k" ) ) +
  scale_y_log10( "Acumulado 7 dias (log)",
                 breaks = c( 50, 200, 500, 1000, 5000, 10000, 25000 ),
                 labels = c( "50", "200", "500", "1k", "5k", "10k", "25k" ) ) +
  theme( legend.position = "bottom" ) +
  labs( title = "Países Selecionados - Taxa de crescimento de óbitos",
        caption = JHUCaption, color = NULL )

multiMetros_gl7 <- BRStats %>% 
  filter( location %in% hls_RM ) %>%
  ggplot( mapping = aes( x = total, y = week, color = location ) ) +
  geom_smooth( span = 0.4 ) +
  scale_x_log10( "Total (log)",
                 breaks = c( 10, 50, 200, 1000, 5000, 10000 ),
                 labels = c( "10", "50", "200", "1k", "5k", "10k" ) ) +
  scale_y_log10( "Acumulado 7 dias (log)",
                 breaks = c( 10, 50, 200, 500, 1000 ),
                 labels = c( "10", "50", "200", "500", "1k" ) ) +
  theme( legend.position = "bottom" ) +
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
