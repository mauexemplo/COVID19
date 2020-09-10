require( data.table )
require( curl )
require( lubridate )
require( tidyr )
require( dplyr )
require( purrr )

dGlobalUrl <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
dBrazilUrl <- "https://data.brasil.io/dataset/covid19/caso_full.csv.gz"
cities <- c( `Belo Horizonte - MG` = "3106200", `Brasília - DF` = "5300108",
             `Curitiba - PR` = "4106902", `Fortaleza - CE` = "2304400",
             `Goiânia - GO` = "5208707", `Manaus - AM` = "1302603",
             `Recife - PE` = "2611606", `Rio de Janeiro - RJ` = "3304557",
             `Salvador - BA` = "2927408", `São Paulo - SP` = "3550308" )
states <- c( "MG", "DF", "PR", "CE", "GO", "AM", "PE", "RJ", "BA", "SP" )
countries <- c( `Brasil` = "Brazil", `Argentina` = "Argentina", `Canadá` = "Canada",
                `EUA` = "US", `Reino Unido` = "United Kingdom",
                `França` = "France", `Alemanha` = "Germany", `Itália` = "Italy" )
metroareas <- list(
  Curitiba = c( "Curitiba - PR", "Campo Largo - PR", "Pinhais - PR",
                "Colombo - PR", "Rio Negro - PR", "Contenda - PR",
                "São José dos Pinhais - PR", "Almirante Tamandaré - PR",
                "Rio Branco do Sul - PR", "Lapa - PR", "Quatro Barras - PR",
                "Araucária - PR", "Fazenda Rio Grande - PR",
                "Mandirituba - PR", "Campina Grande do Sul - PR",
                "Campo Magro - PR", "Piraquara - PR", "Balsa Nova - PR",
                "Quitandinha - PR", "Itaperuçu - PR", "Campo do Tenente - PR",
                "Agudos do Sul - PR", "Tijucas do Sul - PR", "Piên - PR",
                "Bocaiúva do Sul - PR", "Adrianópolis - PR", "Cerro Azul - PR",
                "Doutor Ulysses - PR", "Tunas do Paraná - PR" ),
  `São Paulo` = c( "São Paulo - SP", "Santana de Parnaíba - SP",
                   "Ferraz de Vasconcelos - SP", "Carapicuíba - SP",
                   "Mauá - SP", "Santo André - SP",
                   "São Bernardo do Campo - SP", "São Caetano do Sul - SP",
                   "Guarulhos - SP", "Barueri - SP", "Cotia - SP",
                   "Osasco - SP", "Suzano - SP", "Vargem Grande Paulista - SP",
                   "Mogi das Cruzes - SP", "Caieiras - SP",
                   "Embu das Artes - SP", "Poá - SP", "Ribeirão Pires - SP",
                   "Taboão da Serra - SP", "Arujá - SP", "Mairiporã - SP",
                   "Diadema - SP", "Cajamar - SP", "Itapevi - SP",
                   "Jandira - SP", "Franco da Rocha - SP",
                   "Itapecerica da Serra - SP", "Itaquaquecetuba - SP",
                   "Santa Isabel - SP", "Francisco Morato - SP",
                   "Embu-Guaçu - SP", "Rio Grande da Serra - SP",
                   "Guararema - SP", "Juquitiba - SP",
                   "São Lourenço da Serra - SP", "Biritiba-Mirim - SP",
                   "Salesópolis - SP", "Pirapora do Bom Jesus - SP" ),
  `Rio de Janeiro` = c( "Rio de Janeiro - RJ", "Niterói - RJ",
                        "Guapimirim - RJ", "Petrópolis - RJ",
                        "São Gonçalo - RJ", "Duque de Caxias - RJ",
                        "Belford Roxo - RJ", "Itaboraí - RJ",
                        "Nova Iguaçu - RJ", "Maricá - RJ",
                        "Seropédica - RJ", "Queimados - RJ", "Rio Bonito - RJ",
                        "São João de Meriti - RJ", "Itaguaí - RJ", "Magé - RJ",
                        "Mesquita - RJ", "Nilópolis - RJ", "Tanguá - RJ",
                        "Japeri - RJ", "Cachoeiras de Macacu - RJ",
                        "Paracambi - RJ" ),
  `Belo Horizonte` = c( "Belo Horizonte - MG", "Nova Lima - MG", 
                        "Sete Lagoas - MG", "Betim - MG", "Contagem - MG", "Sabará - MG",
                        "Santa Luzia - MG", "Ribeirão das Neves - MG", "Esmeraldas - MG", 
                        "Sarzedo - MG", "Matozinhos - MG", "Mário Campos - MG", "Pará de Minas - MG", 
                        "Ibirité - MG", "Jaboticatubas - MG", "Bonfim - MG", "Itaúna - MG", 
                        "Caeté - MG", "Itaguara - MG", "Lagoa Santa - MG", "Vespasiano - MG", 
                        "São José da Lapa - MG", "Belo Vale - MG", "Pedro Leopoldo - MG", 
                        "Itatiaiuçu - MG", "Juatuba - MG", "Igarapé - MG", "Raposos - MG", 
                        "Brumadinho - MG", "Santa Bárbara - MG", "Itabirito - MG", 
                        "São Joaquim de Bicas - MG", "Mateus Leme - MG", "São Gonçalo do Rio Abaixo - MG", 
                        "Rio Acima - MG", "Rio Manso - MG", "Florestal - MG", "Capim Branco - MG", 
                        "Barão de Cocais - MG", "Moeda - MG", "Nova União - MG", 
                        "Bom Jesus do Amparo - MG", "Confins - MG", "Inhaúma - MG", 
                        "Taquaraçu de Minas - MG", "Baldim - MG", "Fortuna de Minas - MG", 
                        "Funilândia - MG", "Prudente de Morais - MG", "São José da Varginha - MG" ),
  Brasília = c( "Brasília - DF", "Valparaíso de Goiás - GO", 
                "Luziânia - GO", "Goianésia - GO", "Águas Lindas de Goiás - GO", 
                "Cidade Ocidental - GO", "Pirenópolis - GO", "Formosa - GO", 
                "Santo Antônio do Descoberto - GO", "Planaltina - GO", "Barro Alto - GO", 
                "Novo Gama - GO", "Niquelândia - GO", "Alexânia - GO", "Corumbá de Goiás - GO", 
                "Cristalina - GO", "Alto Paraíso de Goiás - GO", "Padre Bernardo - GO", 
                "São João d'Aliança - GO", "Cabeceiras - GO", "Cocalzinho de Goiás - GO", 
                "Abadiânia - GO", "Cavalcante - GO", "Flores de Goiás - GO", 
                "Água Fria de Goiás - GO", "Mimoso de Goiás - GO", "Alvorada do Norte - GO", 
                "Vila Boa - GO", "Vila Propício - GO", "Simolândia - GO" ), 
  Goiânia = c( "Goiânia - GO", "Aparecida de Goiânia - GO", 
               "Hidrolândia - GO", "Trindade - GO", "Nova Veneza - GO", 
               "Senador Canedo - GO", "Bela Vista de Goiás - GO", "Guapó - GO", 
               "Nerópolis - GO", "Goianira - GO", "Aragoiânia - GO", "Inhumas - GO", 
               "Caturaí - GO", "Santo Antônio de Goiás - GO", "Caldazinha - GO", 
               "Abadia de Goiás - GO", "Bonfinópolis - GO", "Goianápolis - GO", 
               "Terezópolis de Goiás - GO", "Brazabrantes - GO", "Santa Bárbara de Goiás - GO" ),
  Salvador = c( "Salvador - BA", "Lauro de Freitas - BA", "Camaçari - BA",
                "Pojuca - BA", "Dias d'Ávila - BA", "Candeias - BA", 
                "Simões Filho - BA", "São Francisco do Conde - BA", "Vera Cruz - BA", 
                "Itaparica - BA", "Mata de São João - BA", "Madre de Deus - BA", 
                "São Sebastião do Passé - BA" ),
  Fortaleza = c( "Aquiraz - CE", "Fortaleza - CE", "Maranguape - CE", "Caucaia - CE", "Itaitinga - CE", 
                 "Maracanaú - CE", "Eusébio - CE", "Horizonte - CE", "Pacatuba - CE", 
                 "Pindoretama - CE", "Cascavel - CE", "São Luís do Curu - CE", 
                 "Pacajus - CE", "Trairi - CE", "Paracuru - CE", "São Gonçalo do Amarante - CE", 
                 "Chorozinho - CE", "Guaiúba - CE", "Paraipaba - CE" ),
  Recife = c( "Recife - PE", "Jaboatão dos Guararapes - PE", "Olinda - PE", "Camaragibe - PE", 
              "Goiana - PE", "São Lourenço da Mata - PE", "Cabo de Santo Agostinho - PE", 
              "Paulista - PE", "Igarassu - PE", "Moreno - PE", "Abreu e Lima - PE", 
              "Ipojuca - PE", "Itapissuma - PE", "Araçoiaba - PE", "Ilha de Itamaracá - PE" ),
  Manaus = c( "Manaus - AM", "Manacapuru - AM", "Itacoatiara - AM",
              "Novo Airão - AM", "Careiro da Várzea - AM", "Iranduba - AM", 
              "Presidente Figueiredo - AM", "Careiro - AM", "Autazes - AM", 
              "Rio Preto da Eva - AM", "Manaquiri - AM", "Silves - AM", 
              "Itapiranga - AM" ) )

prefix_metros <- "RM de "

PRSaudeUrl <- "http://www.saude.pr.gov.br/Pagina/Coronavirus-COVID-19"
countries_initial_deaths_cutoff <- 50L

load_BrasilIo <- function( url = dBrazilUrl )
{ fread( url, encoding = "UTF-8" ) }

load_JHUGSSEGlobal <- function( url = dGlobalUrl )
{
  temp <- fread( url )
  temp %>%
    pivot_longer( -(1:4), names_to = "date", values_to = "total",
                  names_transform = list( date = mdy ) ) %>%
    group_by( date, `Country/Region` ) %>% summarise( total = sum( total ) ) %>% ungroup() %>% 
    rename( location = `Country/Region` ) %>% 
    group_by( location ) %>% 
    mutate( day = as.integer( total - lag( total, default = 0 ) ),
            week = as.integer( total - lag( total, n = 7, default = 0 ) ),
            day_m7 = frollmean( day, 7 ) ) %>% 
    filter( total > countries_initial_deaths_cutoff )
}

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

calc_SubArea <- function( data, name, locs )
{
  data %>% filter( location %in% locs ) %>%
    group_by( date ) %>% 
    summarise( location = paste0( prefix_metros, name ), total = sum( total ) )
}

calcAll_SubAreas <- function( data, areas )
{
  map2_dfr( names( areas ), areas, calc_SubArea, data = data )
}

dBrazil <- load_BrasilIo()

BRStats <- dBrazil %>% 
  rename( total = last_available_deaths ) %>% 
  filter( total > 0 ) %>% 
  mutate( location = if_else( place_type == "state", state,
                              paste( city, state, sep = " - " ) ) ) %>%
  select( date, location, total )

BRSummary <- dBrazil %>% 
  rename( total = last_available_deaths ) %>% 
  filter( total > 0, place_type == "state" ) %>%
  group_by( date ) %>% 
  summarise( location = "Brasil", total = sum( total ) ) %>% 
  ungroup()

BRMetros <- calcAll_SubAreas( BRStats, metroareas )
  
BRStats <- bind_rows( BRStats, BRSummary, BRMetros ) %>%
  mutate( date = ymd( date ) ) %>% 
  arrange( location, date ) %>% 
  group_by( location ) %>% 
  mutate( day = as.integer( total - lag( total, default = 0 ) ),
          week = as.integer( total - lag( total, n = 7, default = 0 ) ),
          day_m7 = frollmean( day, 7 ) )

rm( BRSummary )
rm( BRMetros )
rm( dBrazil )
# rm( BRCityStats )
# rm( BRStateStats )

JHUStats <- load_JHUGSSEGlobal()

lastStats <- JHUStats %>% 
  filter( location != "Brazil" ) %>% 
  bind_rows( BRStats ) %>%
  group_by( location ) %>%
  mutate( growth_7 = 1 - ( lag( week, 7 ) / week ) ) %>%
  filter( date == max( date ) ) %>% ungroup() %>%
  arrange( desc( growth_7 ) )
