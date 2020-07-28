require( ggplot2 )

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
ggsave( "pr_gl7.png" )

ggplot( csPRD, mapping = aes( Index, day_m7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "PR - Óbitos, Média móvel 7d")
ggsave( "pr_m7.png" )

ggplot( csSPD, mapping = aes( total_l, day_sl7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "SP - Óbitos, Taxa de crescimento 7d")
ggsave( "sp_gl7.png" )

ggplot( csSPD, mapping = aes( Index, day_m7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "SP - Óbitos, Média móvel 7d")
ggsave( "sp_m7.png" )

ggplot( csRJD, mapping = aes( total_l, day_sl7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "RJ - Óbitos, Taxa de crescimento 7d")
ggsave( "rj_gl7.png" )

ggplot( csRJD, mapping = aes( Index, day_m7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "RJ - Óbitos, Média móvel 7d")
ggsave( "rj_m7.png" )

ggplot( csBRD, mapping = aes( total_l, day_sl7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "Nacional - Óbitos, Taxa de crescimento 7d")
ggsave( "br_gl7.png" )

ggplot( csBRD, mapping = aes( Index, day_m7 ) ) +
  geom_point() +
  geom_smooth( span = 0.5 ) +
  labs( title = "Nacional - Óbitos, Média móvel 7d")
ggsave( "br_m7.png" )
