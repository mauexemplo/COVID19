require( ggplot2 )
require( svglite )

if ( !exists("csPRD") || !exists("csSPD") || !exists("csRJD") || !exists("csBRD") )
{
  source( "load.R" )
} else {
  require( xts )
  if ( end( csBRD ) != Sys.Date() )
  {
    rm( list = ls() )
    source( "load.R" )
  }
}

ggplot( csPRD, mapping = aes( total_l, day_sl7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "PR - Óbitos, Taxa de crescimento 7d")
ggsave( file.path( "plots", "pr_gl7.png" ) )

ggplot( csPRD, mapping = aes( Index, day_m7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "PR - Óbitos, Média móvel 7d")
ggsave( file.path( "plots", "pr_m7.png" ) )

ggplot( csSPD, mapping = aes( total_l, day_sl7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "SP - Óbitos, Taxa de crescimento 7d")
ggsave( file.path( "plots", "sp_gl7.png" ) )

ggplot( csSPD, mapping = aes( Index, day_m7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "SP - Óbitos, Média móvel 7d")
ggsave( file.path( "plots", "sp_m7.png" ) )

ggplot( csRJD, mapping = aes( total_l, day_sl7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "RJ - Óbitos, Taxa de crescimento 7d")
ggsave( file.path( "plots", "rj_gl7.png" ) )

ggplot( csRJD, mapping = aes( Index, day_m7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "RJ - Óbitos, Média móvel 7d")
ggsave( file.path( "plots", "rj_m7.png" ) )

ggplot( csBRD, mapping = aes( total_l, day_sl7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "Nacional - Óbitos, Taxa de crescimento 7d")
ggsave( file.path( "plots", "br_gl7.png" ) )

ggplot( csBRD, mapping = aes( Index, day_m7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "Nacional - Óbitos, Média móvel 7d")
ggsave( file.path( "plots", "br_m7.png" ) )

BRStateStats %>%
  filter( total >= 10, state %in% states ) %>%
  ggplot( mapping = aes( x = total, y = week, color = state ) ) +
  geom_line() +
  geom_smooth( span = 0.5 ) +
  scale_x_log10( "Total (log)", breaks = c( 10, 100, 1000, 10000, 100000 ),
                 labels = c( "10", "100", "1k", "10k", "100k" ) ) +
  scale_y_log10( "Acumulado 7 dias (log)", breaks = c( 10, 100, 1000, 10000, 100000 ),
                 labels = c( "10", "100", "1k", "10k", "100k" ) ) +
  labs( title = "Estados Selecionados - Taxa de crescimento de óbitos",
        caption = "Fonte: Brasil.IO" )
ggsave( file.path( "plots", "states_gl7.png" ) )

BRCityStats %>%
  filter( total >= 10, city_ibge_code %in% cities$city_ibge_code ) %>%
  ggplot( mapping = aes( x = total, y = week, color = city ) ) +
  geom_line() +
  geom_smooth( span = 0.5 ) +
  scale_x_log10( "Total (log)", breaks = c( 10, 50, 200, 1000, 5000, 10000 ),
                 labels = c( "10", "50", "200", "1k", "5k", "10k" ) ) +
  scale_y_log10( "Acumulado 7 dias (log)", breaks = c( 10, 50, 200, 500, 1000 ),
                 labels = c( "10", "50", "200", "500", "1k" ) ) +
  labs( title = "Capitais Selecionadas - Taxa de crescimento de óbitos",
        caption = "Fonte: Brasil.IO" )
ggsave( file.path( "plots", "cities_gl7.png" ) )
