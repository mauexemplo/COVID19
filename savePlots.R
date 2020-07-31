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
  geom_smooth() +
  scale_x_log10() +
  scale_y_log10() +
  labs( x = "Total (log)", y = "Acumulado 7 dias (log)",
        title = "Estados Selecionados - Taxa de crescimento de óbitos",
        caption = "Fonte: Brasil.IO" )
ggsave( file.path( "plots", "states_gl7.png" ) )
