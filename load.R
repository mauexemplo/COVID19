# TODO: require() ibge once packaged
if( !exists( "ibge_R" ) ) source( "ibge.R", encoding = "UTF-8" )
load_R <- TRUE

require( data.table )
require( curl )
require( lubridate )
require( tidyr )
require( dplyr )
require( purrr )

httr::set_config( httr::user_agent( "https://github.com/mauexemplo/COVID19" ) )

dGlobalUrl <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
dBrazilUrl <- "https://data.brasil.io/dataset/covid19/caso_full.csv.gz"
dHomePRUrl <- "https://www.saude.pr.gov.br/Pagina/Coronavirus-COVID-19"

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
                "Unaí - MG", "Cidade Ocidental - GO", "Pirenópolis - GO", "Formosa - GO", 
                "Santo Antônio do Descoberto - GO", "Planaltina - GO", "Barro Alto - GO", 
                "Novo Gama - GO", "Niquelândia - GO", "Alexânia - GO", "Corumbá de Goiás - GO", 
                "Cristalina - GO", "Alto Paraíso de Goiás - GO", "Padre Bernardo - GO", 
                "São João d'Aliança - GO", "Cabeceiras - GO", "Cocalzinho de Goiás - GO", 
                "Abadiânia - GO", "Cavalcante - GO", "Flores de Goiás - GO", 
                "Água Fria de Goiás - GO", "Buritis - MG", "Mimoso de Goiás - GO", 
                "Arinos - MG", "Alvorada do Norte - GO", "Vila Boa - GO", "Cabeceira Grande - MG", 
                "Vila Propício - GO", "Simolândia - GO" ), 
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
              "Itapiranga - AM" ),
  `Porto Alegre`= c( "Campo Bom - RS", "Porto Alegre - RS", 
                     "Sapiranga - RS", "Alvorada - RS", "Taquara - RS", "Ivoti - RS", 
                     "Canoas - RS", "Charqueadas - RS", "Dois Irmãos - RS", "Eldorado do Sul - RS", 
                     "São Leopoldo - RS", "Estância Velha - RS", "Santo Antônio da Patrulha - RS", 
                     "Viamão - RS", "Gravataí - RS", "Rolante - RS", "Novo Hamburgo - RS", 
                     "Cachoeirinha - RS", "São Sebastião do Caí - RS", "Guaíba - RS", 
                     "Sapucaia do Sul - RS", "Esteio - RS", "Arroio dos Ratos - RS", 
                     "Glorinha - RS", "Montenegro - RS", "Triunfo - RS", "Portão - RS", 
                     "São Jerônimo - RS", "Nova Santa Rita - RS", "Parobé - RS", "Nova Hartz - RS", 
                     "Igrejinha - RS", "Capela de Santana - RS", "Araricá - RS" ),
  `Florianópolis` = c( "Florianópolis - SC", "Rancho Queimado - SC", 
                       "São José - SC", "Tijucas - SC", "Canelinha - SC", "São Pedro de Alcântara - SC", 
                       "Antônio Carlos - SC", "Biguaçu - SC", "Paulo Lopes - SC", "Governador Celso Ramos - SC", 
                       "Palhoça - SC", "Santo Amaro da Imperatriz - SC", "Águas Mornas - SC", 
                       "Garopaba - SC", "Nova Trento - SC", "São João Batista - SC", 
                       "Alfredo Wagner - SC", "Major Gercino - SC", "Anitápolis - SC", 
                       "São Bonifácio - SC", "Angelina - SC", "Leoberto Leal - SC" ),
  `Vitória` = c( "Vila Velha - ES", "Vitória - ES", 
                 "Cariacica - ES", "Serra - ES", "Guarapari - ES", "Viana - ES", 
                 "Fundão - ES" ),
  `São Luís` = c( "São Luís - MA", "Paço do Lumiar - MA", 
                  "São José de Ribamar - MA", "Icatu - MA", "Raposa - MA", "Cachoeira Grande - MA", 
                  "Santa Rita - MA", "Bacabeira - MA", "Morros - MA", "Alcântara - MA", 
                  "Rosário - MA", "Presidente Juscelino - MA", "Axixá - MA" ),
  Teresina = c( "Teresina - PI", "Timon - MA", "Altos - PI", 
                "Demerval Lobão - PI", "União - PI", "Nazária - PI", "José de Freitas - PI", 
                "Monsenhor Gil - PI", "Beneditinos - PI", "Coivaras - PI", "Lagoa do Piauí - PI", 
                "Curralinhos - PI", "Lagoa Alegre - PI", "Miguel Leão - PI" ),
  Natal = c( "Natal - RN", "Parnamirim - RN", 
             "Macaíba - RN", "Monte Alegre - RN", "São Gonçalo do Amarante - RN", 
             "São José de Mipibu - RN", "Ceará-Mirim - RN", "Extremoz - RN", 
             "Nísia Floresta - RN", "Ielmo Marinho - RN", "Arês - RN", "Bom Jesus - RN", 
             "Maxaranguape - RN", "Goianinha - RN", "Vera Cruz - RN" ),
  `João Pessoa` = c( "João Pessoa - PB", "Cabedelo - PB", 
                     "Santa Rita - PB", "Bayeux - PB", "Conde - PB", "Cruz do Espírito Santo - PB", 
                     "Caaporã - PB", "Pedras de Fogo - PB", "Lucena - PB", "Alhandra - PB", 
                     "Rio Tinto - PB", "Pitimbu - PB" ),
  `Maceió` = c( "Maceió - AL", "Marechal Deodoro - AL", 
                "Satuba - AL", "Rio Largo - AL", "Murici - AL", "Pilar - AL", 
                "Barra de Santo Antônio - AL", "Paripueira - AL", "Santa Luzia do Norte - AL", 
                "Barra de São Miguel - AL", "Coqueiro Seco - AL", "Messias - AL" ),
  Aracaju = c( "Aracaju - SE", "Nossa Senhora do Socorro - SE", 
               "São Cristóvão - SE", "Barra dos Coqueiros - SE" ),
  `Cuiabá` = c( "Cuiabá - MT", "Várzea Grande - MT", 
                "Chapada dos Guimarães - MT", "Poconé - MT", "Jangada - MT", 
                "Rosário Oeste - MT", "Santo Antônio do Leverger - MT", "Acorizal - MT", 
                "Nossa Senhora do Livramento - MT", "Nobres - MT", "Nova Brasilândia - MT", 
                "Barão de Melgaço - MT", "Planalto da Serra - MT" ),
  `Campinas` = c( "Campinas - SP", "Jaguariúna - SP", 
                  "Hortolândia - SP", "Valinhos - SP", "Paulínia - SP", "Americana - SP", 
                  "Nova Odessa - SP", "Vinhedo - SP", "Indaiatuba - SP", "Itatiba - SP", 
                  "Sumaré - SP", "Artur Nogueira - SP", "Santa Bárbara d'Oeste - SP", 
                  "Monte Mor - SP", "Morungaba - SP", "Engenheiro Coelho - SP", 
                  "Holambra - SP", "Cosmópolis - SP", "Santo Antônio de Posse - SP", 
                  "Pedreira - SP" ),
  `Vale do Paraíba (SP)` = c( "São José dos Campos - SP", "Taubaté - SP", 
                              "São Sebastião - SP", "Cachoeira Paulista - SP", "Santa Branca - SP", 
                              "Caraguatatuba - SP", "Pindamonhangaba - SP", "Caçapava - SP", 
                              "Jacareí - SP", "Guaratinguetá - SP", "Ilhabela - SP", "Campos do Jordão - SP", 
                              "Igaratá - SP", "Ubatuba - SP", "Cruzeiro - SP", "Lorena - SP", 
                              "Aparecida - SP", "Lavrinhas - SP", "Santo Antônio do Pinhal - SP", 
                              "Paraibuna - SP", "Cunha - SP", "Jambeiro - SP", "Bananal - SP", 
                              "Canas - SP", "São Luiz do Paraitinga - SP", "Silveiras - SP", 
                              "Tremembé - SP", "Potim - SP", "Roseira - SP", "São Bento do Sapucaí - SP", 
                              "Monteiro Lobato - SP", "Piquete - SP", "Natividade da Serra - SP", 
                              "Redenção da Serra - SP", "Arapeí - SP", "Queluz - SP", "Areias - SP", 
                              "Lagoinha - SP", "São José do Barreiro - SP" ),
  `Belém` = c( "Belém - PA", "Ananindeua - PA", 
               "Castanhal - PA", "Marituba - PA", "Benevides - PA", "Santa Bárbara do Pará - PA", 
               "Santa Izabel do Pará - PA" ),
  Sorocaba = c( "Tatuí - SP", "Sorocaba - SP", "Porto Feliz - SP", 
                "Salto de Pirapora - SP", "Votorantim - SP", "Itu - SP", "Itapetininga - SP", 
                "Boituva - SP", "Ibiúna - SP", "Salto - SP", "São Roque - SP", 
                "Araçoiaba da Serra - SP", "São Miguel Arcanjo - SP", "Araçariguama - SP", 
                "Mairinque - SP", "Pilar do Sul - SP", "Alambari - SP", "Piedade - SP", 
                "Capela do Alto - SP", "Sarapuí - SP", "Tietê - SP", "Iperó - SP", 
                "Tapiraí - SP", "Alumínio - SP", "Cerquilho - SP", "Cesário Lange - SP", 
                "Jumirim - SP" ),
  `Baixada Santista` = c( "Santos - SP", "Guarujá - SP", "São Vicente - SP", 
                          "Itanhaém - SP", "Praia Grande - SP", "Peruíbe - SP", "Cubatão - SP", 
                          "Mongaguá - SP", "Bertioga - SP" ),
  `Ribeirão Preto` = c( "Ribeirão Preto - SP", "Brodowski - SP", 
                        "Cravinhos - SP", "Jaboticabal - SP", "Orlândia - SP", "Monte Alto - SP", 
                        "Batatais - SP", "Mococa - SP", "Sertãozinho - SP", "Jardinópolis - SP", 
                        "Cajuru - SP", "Pontal - SP", "Morro Agudo - SP", "Santo Antônio da Alegria - SP", 
                        "Serrana - SP", "Barrinha - SP", "Pitangueiras - SP", "Santa Rita do Passa Quatro - SP", 
                        "Pradópolis - SP", "Guariba - SP", "Dumont - SP", "Santa Cruz da Esperança - SP", 
                        "Serra Azul - SP", "Luís Antônio - SP", "Altinópolis - SP", "Tambaú - SP", 
                        "Nuporanga - SP", "Sales Oliveira - SP", "Taiúva - SP", "São Simão - SP", 
                        "Santa Rosa de Viterbo - SP", "Cássia dos Coqueiros - SP", "Taquaral - SP", 
                        "Guatapará - SP" ),
  Piracicaba = c( "Iracemápolis - SP", "Rio Claro - SP", 
                  "Piracicaba - SP", "São Pedro - SP", "Limeira - SP", "Araras - SP", 
                  "Laranjal Paulista - SP", "Águas de São Pedro - SP", "Leme - SP", 
                  "Santa Gertrudes - SP", "Elias Fausto - SP", "Cordeirópolis - SP", 
                  "Rio das Pedras - SP", "Saltinho - SP", "Charqueada - SP", "Santa Maria da Serra - SP", 
                  "Conchal - SP", "Capivari - SP", "Ipeúna - SP", "Rafard - SP", 
                  "Corumbataí - SP", "Analândia - SP", "Mombuca - SP" ),
  `N e NE Catarinense` = c( "Joinville - SC", "Jaraguá do Sul - SC", 
                            "Papanduva - SC", "São Francisco do Sul - SC", "Mafra - SC", 
                            "São Bento do Sul - SC", "Barra Velha - SC", "Guaramirim - SC", 
                            "Massaranduba - SC", "Rio Negrinho - SC", "Schroeder - SC", "Itaiópolis - SC", 
                            "Balneário Barra do Sul - SC", "Araquari - SC", "Três Barras - SC", 
                            "Garuva - SC", "São João do Itaperiú - SC", "Porto União - SC", 
                            "Campo Alegre - SC", "Canoinhas - SC", "Monte Castelo - SC", 
                            "Itapoá - SC", "Corupá - SC", "Bela Vista do Toldo - SC", "Major Vieira - SC", 
                            "Irineópolis - SC" ),
  Londrina = c( "Londrina - PR", "Arapongas - PR", 
                "Cambé - PR", "Rolândia - PR", "Sertaneja - PR", "Primeiro de Maio - PR", 
                "Jataizinho - PR", "Assaí - PR", "Guaraci - PR", "Jaguapitã - PR", 
                "Ibiporã - PR", "Prado Ferreira - PR", "Lupionópolis - PR", "Alvorada do Sul - PR", 
                "Uraí - PR", "Centenário do Sul - PR", "Sertanópolis - PR", "Tamarana - PR", 
                "Bela Vista do Paraíso - PR", "Florestópolis - PR", "Rancho Alegre - PR", 
                "Porecatu - PR", "Sabáudia - PR", "Miraselva - PR", "Pitangueiras - PR" ) )

prefix_metros <- "RM "
highlights_week_cutoff <- 5L
highlights_count <- 10L
countries_initial_deaths_cutoff <- 50L
default_aggregations <- c( "regioes", "estados", "regioes-imediatas", "regioes-intermediarias" )

loadBrasilIO <- function( url = dBrazilUrl )
{ data.table::fread( url, encoding = "UTF-8" ) }

load_JHUGSSEGlobal <- function( url = dGlobalUrl )
{
  temp <- data.table::fread( url )
  temp %>%
    pivot_longer( -(1:4), names_to = "date", values_to = "total",
                  names_transform = list( date = mdy ) ) %>%
    group_by( date, `Country/Region` ) %>% summarise( total = sum( total ) ) %>%
    ungroup() %>% 
    rename( location = `Country/Region` ) %>% 
    # group_by( location ) %>% 
    # mutate( day = as.integer( total - lag( total, default = 0 ) ),
    #         week = as.integer( total - lag( total, n = 7, default = 0 ) ),
    #         day_m7 = frollmean( day, 7 ) ) %>% 
    filter( total > countries_initial_deaths_cutoff )
}

loadPR <- function( url = findPRUrl() )
{ return( data.table::fread( url, encoding = "UTF-8" ) ) }

parsePR <- function( data = loadPR() )
{
  parsed <- data %>% dplyr::transmute( date = lubridate::dmy( DATA_OBITO ), location = IBGE_RES_PR ) %>% 
    dplyr::filter( !is.na( date ), location != 9999999 ) %>%
    dplyr::group_by( date, location ) %>% dplyr::summarise( total = dplyr::n() ) %>%
    dplyr::group_by( location ) %>% dplyr::mutate( total = cumsum( total ) ) %>% 
    dplyr::ungroup() %>% dplyr::arrange( location, date ) %>% 
    dplyr::group_by( location ) %>%
    tidyr::complete( date = seq.Date( min( date ), today(), by = "day" ) ) %>% tidyr::fill( total ) %>% 
    dplyr::ungroup()
  return( parsed )
}

mergePRtoBrasilIO <- function( bio = parseCityDeathsBrasilIO(), pr = parsePR() )
{
  return( bio %>% dplyr::filter( !grepl( "^41", location ) ) %>% dplyr::bind_rows( pr ) )
}

findPRUrl <- function( homePRUrl = dHomePRUrl )
{
  return( xml2::read_html( homePRUrl ) %>% rvest::html_node( "[title=Geral] a" ) %>% rvest::html_attr( "href" ) )
}

calc_SubArea <- function( data, name, locs, prefix )
{
  data %>% filter( location %in% locs ) %>%
    group_by( date ) %>% 
    summarise( location = paste0( prefix, name ), total = sum( total ) )
}

calcAll_SubAreas <- function( data, areas, prefix )
{
  map2_dfr( names( areas ), areas, calc_SubArea, data = data, prefix = prefix )
}

isRM <- function( loc )
{
  substring( loc, 1, nchar( prefix_metros ) ) == prefix_metros
}

parseCityDeathsBrasilIO <- function( data = loadBrasilIO() )
{
  nacodes <- getEstado() %>% dplyr::mutate( nacode = as.integer( paste0( UF, "99999" ) ) ) %>%
    { setNames( .$nacode, .$Sigla_UF ) }
  data %<>% dplyr::filter( last_available_deaths > 0, place_type == "city" ) %>% 
    dplyr::mutate( location = unname( dplyr::if_else( is.na( city_ibge_code ), nacodes[ state ], city_ibge_code ) ),
                   date = lubridate::ymd( date ) ) %>%
    dplyr::select( date, location, total = last_available_deaths )
  
  return( data )
}

calcStats <- function( data = parseCityDeathsBrasilIO() )
{
  data %<>% dplyr::arrange( location, date ) %>% dplyr::group_by( location ) %>%
    dplyr::mutate( day = as.integer( total - lag( total, default = 0 ) ),
                   week = as.integer( total - lag( total, n = 7, default = 0 ) ),
                   days15 = as.integer( total - lag( total, n = 15, default = 0 ) ),
                   day_m7 = data.table::frollmean( day, 7 ),
                   day_m15 = data.table::frollmean( day, 15 ),
                   growth_7 = 1 - ( lag( week, n = 7 ) / week ),
                   growth_15 = 1 - ( lag( days15, n = 15 ) / days15 ) ) %>%
    dplyr::ungroup()
  return( data )
}

addIBGELocalidade <- function( data = parseCityDeathsBrasilIO() )
{
  ibgedata <- getLocalidade( "municipio", recurse = 9 )
  ibgedata %<>% dplyr::distinct( UF, Sigla_UF, Nome_UF, Região, Sigla_Região, Nome_Região ) %>%
    dplyr::mutate( Município = as.integer( paste0( UF, "99999" ) ), Nome_Município = "Importados / Indefinidos") %>%
    dplyr::bind_rows( ibgedata ) %>% dplyr::arrange( Município )

  result <- dplyr::left_join( data, ibgedata, by = c( "location" = "Município" ) ) %>%
    dplyr::select( date, total, Município = location, Nome_Município:`Nome_Região Intermediária`, UF:Nome_Região )
  return( result )
}

calcAggregate <- function( data = addIBGELocalidade(), group_by = default_aggregations )
{
  agg_forms <- paste0( "total ~ date + `", ibge_api_endpoints[ group_by, "name" ], "`" )
  col_names <- c( "date", "location", "total" )
  agg_l <- lapply( agg_forms, function( x ){ setNames( aggregate( as.formula( x ), data, sum ), col_names ) } )
  agg <- data.table::rbindlist( agg_l )
  return( agg )
}

loadParsedDeaths <- function()
{
  bio <- parseCityDeathsBrasilIO()
  pr <- parsePR()
  biopr <- mergePRtoBrasilIO( bio, pr )
  bpr_agg <- dplyr::bind_rows( biopr, calcAggregate( addIBGELocalidade( biopr ) ) )
  deaths <- dplyr::left_join( bpr_agg, getSimpleNames(), by = c( "location" = "id" ) ) %>% calcStats()
  return( deaths )
}
  
BRSummary <- dBrazil %>% 
  rename( total = last_available_deaths ) %>% 
  filter( total > 0, place_type == "state" ) %>%
  group_by( date ) %>% 
  summarise( location = "Brasil", total = sum( total ) ) %>% 
  ungroup()

BRMetros <- calcAll_SubAreas( BRStats, metroareas, prefix_metros )
  
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

JHUStats <- load_JHUGSSEGlobal()

lastStats <- JHUStats %>% 
  filter( location != "Brazil" ) %>% 
  bind_rows( BRStats ) %>%
  group_by( location ) %>%
  mutate( growth_7 = 1 - ( lag( week, 7 ) / week ) ) %>%
  filter( date == max( date ) ) %>% ungroup() %>%
  arrange( desc( growth_7 ) )

hls_Global <- lastStats %>%
  filter( growth_7 != Inf,
          location %in% JHUStats$location,
          week > highlights_week_cutoff ) %>% 
  head( highlights_count - 1L ) %>% 
  select( location ) %>% 
  bind_rows( data.frame( location = c( "Brazil" ) ) ) %>% 
  unlist( use.names = FALSE )

hls_RM <- lastStats %>%
  filter( growth_7 != Inf, isRM( location ),
          week > highlights_week_cutoff ) %>% 
  head( highlights_count ) %>% 
  select( location ) %>% 
  unlist( use.names = FALSE )

hls_State <- lastStats %>%
  filter( growth_7 != Inf,
          nchar( location ) == 2L,
          week > highlights_week_cutoff ) %>% 
  head( highlights_count ) %>% 
  select( location ) %>% 
  unlist( use.names = FALSE )
