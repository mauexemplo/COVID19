# TODO: Turn into package, remove this sourcing flag, update user agent
ibge_R <- TRUE
agent <- httr::user_agent( "https://github.com/mauexemplo/COVID19" )

ibge_rms_url <-
  "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/municipios_por_regioes_metropolitanas/Situacao_2020a2029/Composicao_RMs_RIDEs_AglomUrbanas_2020_06_30.xlsx"
ibge_rms_ext <- ".xlsx"

ibge_drb_url <-
  "https://geoftp.ibge.gov.br/organizacao_do_territorio/divisao_regional/divisao_regional_do_brasil/divisao_regional_do_brasil_em_regioes_geograficas_2017/tabelas/regioes_geograficas_composicao_por_municipios_2017_20180911.xlsx"
ibge_drb_ext <- ".xlsx"

ibge_dtb_url <-
  "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/divisao_territorial/2019/DTB_2019_v2.zip"
ibge_dtb_file <- "RELATORIO_DTB_BRASIL_MUNICIPIO.xls"
ibge_dtb_ext <- ".xls"

ibge_api_host <- "https://servicodados.ibge.gov.br"
ibge_loc_api_path <- "api/v1/localidades"
ibge_api_endpoints <- data.frame(
  name = c( "Região", "UF", "Município", "Distrito", "Subdistrito", "Região Intermediária", "Região Imediata",
            "Mesorregião", "Microrregião" ),
  group = factor( c( "Divisão Político-Administrativa", "Divisão Político-Administrativa",
                     "Divisão Político-Administrativa", "Divisão Político-Administrativa",
                     "Divisão Político-Administrativa", "Regiões Geográficas", "Regiões Geográficas",
                     "Mesorregiões e Microrregiões", "Mesorregiões e Microrregiões" ) ),
  order_in_group = c( 1, 2, 3, 4, 5, 1, 2, 1, 2 ),
  endpoint = c( "regioes", "estados", "municipios", "distritos", "subdistritos", "regioes-intermediarias",
                "regioes-imediatas", "mesorregioes", "microrregioes" ),
  filters = I( list( `regioes` = c(),
                     `estados` = c( "regiao" ),
                     `municipios` = c( "UF", "mesorregiao", "microrregiao", "regiao-imediata", "regiao-intermediaria",
                                       "regiao" ),
                     `distritos` = c( "UF", "mesorregiao", "microrregiao", "municipio", "regiao-imediata",
                                      "regiao-intermediaria", "regiao" ),
                     `subdistritos` = c( "distrito", "UF", "mesorregiao", "microrregiao", "municipio", "regiao" ),
                     `regioes-intermediarias` = c( "UF", "regiao" ),
                     `regioes-imediatas` = c( "UF", "regiao-intermediaria", "regiao" ),
                     `mesorregioes` = c( "UF", "regiao" ),
                     `microrregioes` = c( "UF", "mesorregiao", "regiao" )
                     ) ),
  parents = I( list( NULL, c( "regiao" ), c( "microrregiao", "regiao-imediata" ), c( "municipio" ), c( "distrito" ),
                     c( "UF" ), c( "regiao-intermediaria" ), c( "UF" ), c( "mesorregiao" ) ) )
)

ibge_loc_types <- c( "regiao", "UF", "municipio", "distrito", "subdistrito", "regiao-intermediaria", "regiao-imediata",
                     "mesorregiao", "microrregiao" )


# Download, unzip and import the DTB file
loadIBGEDTB <- function( path = ibge_dtb_url )
{
  tmpzip <- tempfile( fileext = ".zip" )
  # cols <- c( "numeric", "text", "numeric", "text", "numeric", "text",
  #            "numeric", "text", "numeric", "text", "numeric", "numeric",
  #            "text" )

  httr::GET( path, httr::write_disk( tmpzip ) )
  unzip( tmpzip, ibge_dtb_file, exdir = tempdir() )
  
  return ( readxl::read_excel( file.path( tempdir(), ibge_dtb_file ), col_names = TRUE ) )
}

# Download and import the RMs file
loadIBGERMs <- function( path = ibge_rms_url )
{
  tmp <- tempfile( fileext = ibge_rms_ext )
  # cols <- c( "text", "numeric", "text", "numeric", "text", "text", "numeric",
  #            "text", "numeric", "text", "text" )
  
  httr::GET( path, httr::write_disk( tmp ) )
  
  return ( readxl::read_excel( tmp, col_names = TRUE ) )
}

# Download and import the DRB file
loadIBGEDRB <- function( path = ibge_drb_url )
{
  tmp <- tempfile( fileext = ibge_drb_ext )
  # cols <- c( "text", "numeric", "numeric", "text", "numeric", "text" )

  httr::GET( path, httr::write_disk( tmp ) )
  
  return ( readxl::read_excel( tmp, col_names = TRUE ) )
}

# RMs in this file sometimes contain different COD (and therefore
# COD_CAT_ASSOC) for the same RM (as per NOME)
# This function assigns a new ID (RMID) for each distinct NOME alphabetically
addRMID <- function( rm_data = NULL )
{
  if ( is.null( rm_data ) )
    rm_data <- loadIBGERMs()
  
  if ( !( "RMID" %in% names( rm_data ) ) )
  {
    rm_data <- rm_data %>%
      dplyr::arrange( NOME ) %>%
      dplyr::group_by( NOME ) %>%
      dplyr::mutate( RMID = dplyr::cur_group_id() ) %>%
      dplyr::ungroup() %>% 
      # cur_group_id() assigns a group ID to cases where NOME = NA
      # Keeping unassigned cities to RMID = NA is more consistent
      dplyr::mutate( RMID = ifelse( is.na( NOME ), NA, RMID ) )
  }
  
  return ( rm_data )
}

# Assigns regions (GRANDE_REG) IDs (RID) alphabetically
addRegionID <- function( rm_data = NULL )
{
  if ( is.null( rm_data ) )
    rm_data <- loadIBGERMs()

  if ( !( "RID" %in% names( rm_data ) ) )
  {
    rm_data <- rm_data %>%
      dplyr::arrange( GRANDE_REG ) %>%
      dplyr::group_by( GRANDE_REG ) %>%
      dplyr::mutate( RID = dplyr::cur_group_id() ) %>%
      dplyr::ungroup()
  }
  
  return ( rm_data )
}

# Builds and returns a named vector of all states (COD_UF) in the RM file with
# their parent regions (GRANDE_REG)
getRegionMembership <- function( rm_data = NULL )
{
  if ( is.null( rm_data ) )
    rm_data <- loadIBGERMs()

  regions <- rm_data %>% dplyr::distinct( COD_UF, GRANDE_REG ) %>% 
    # AC (12) and MS (50) do not appear in any RMs, need to add manually
    dplyr::bind_rows(
      data.frame( COD_UF = c( 12, 50 ), GRANDE_REG = c( "Norte", "Centro-Oeste" ) ) ) %>% 
    dplyr::arrange( COD_UF )

  return ( with( regions, setNames( GRANDE_REG, COD_UF ) ) )
}

# Builds and returns a named vector of all states (COD_UF) in the RM file with
# their corresponding abbreviation (SIGLA_UF)
# NOTE: Manually adds entries for states with no RM
getStateAbbr <- function( rm_data = NULL )
{
  if ( is.null( rm_data ) )
    rm_data <- loadIBGERMs()

  abbr <- rm_data %>% dplyr::distinct( COD_UF, SIGLA_UF ) %>% 
    # AC (12) and MS (50) do not appear in any RMs, need to add manually
    dplyr::bind_rows( data.frame( COD_UF = c( 12, 50 ), SIGLA_UF = c( "AC", "MS" ) ) ) %>% 
    dplyr::filter( !is.na( SIGLA_UF ) ) %>% dplyr::arrange( COD_UF )

  return ( with( abbr, setNames( SIGLA_UF, COD_UF ) ) )
}

# Add the RM ID and NOME to the DTB table (requires casting the join key as
# int) and fills missing GRANDE_REG based on UF membership
# NOTE: After this, each municipality might appear more than once
addRMtoDTB <- function( rm_data = NULL, dtb_data = NULL )
{
  if ( is.null( rm_data ) )
    rm_data <- loadIBGERMs()
  if ( is.null( dtb_data ) )
    dtb_data <- loadIBGEDTB()

  # Cast city code vars to integer to allow join key matching
  rm_data$COD_MUN <- as.integer( rm_data$COD_MUN )
  dtb_data$`Código Município Completo` <-
    as.integer( dtb_data$`Código Município Completo` )
  
  full_data <- dtb_data %>%
    dplyr::left_join( rm_data, by = c( `Código Município Completo` = "COD_MUN" ) )

  # After the join, cities not belonging to any RM are also not assigned
  # to a region (GRANDE_REG), this uses UF from DTB to assign
  regions <- getRegionMembership( rm_data )
  full_data$GRANDE_REG <- unname( regions[ full_data$UF ] )
  
  return ( full_data )
}

# Provides a flat table with renamed DTB fields, plus RM ID + name + type,
# and region ID + name
# NOTE: DRB data is not used since presently all info is already in DTB
getCities <- function( dtb_data = NULL, rm_data = NULL, drb_data = NULL )
{
  flatCities <- addRMID( addRegionID( addRMtoDTB( rm_data, dtb_data ) ) )
  abbrs <- getStateAbbr( flatCities )
  flatCities <- flatCities %>%
    # Rename vars to keep with convention, and cast numbers to integer
    dplyr::mutate( `Região` = RID, `Nome_Região` = GRANDE_REG,
                   `Região Metropolitana` = RMID, `Nome_Região Metropolitana` = NOME,
                   `Tipo_Região Metropolitana` = TIPO,
                   # Meso and macro regions numbers restart for each UF
                   # Prefix those with the UF code to differentiate
                   `Microrregião Geográfica` = as.integer( paste0( UF, `Microrregião Geográfica` ) ),
                   `Mesorregião Geográfica` = as.integer( paste0( UF, `Mesorregião Geográfica` ) ),
                   UF = as.integer( UF ), Sigla_UF = unname( abbrs[ as.character( UF ) ] ),
                   `Região Geográfica Intermediária` = as.integer( `Região Geográfica Intermediária` ),
                   `Região Geográfica Imediata` = as.integer( `Região Geográfica Imediata` ),
                   `Município` = `Código Município Completo`,
                   `Nome_Região Geográfica Intermediária` = `Nome Região Geográfica Intermediária`,
                   `Nome_Região Geográfica Imediata` = `Nome Região Geográfica Imediata`,
                   `Nome_Mesorregião Geográfica` = `Nome_Mesorregião`,
                   `Nome_Microrregião Geográfica` = `Nome_Microrregião` ) %>%
    # Drop unused columns
    dplyr::select( `Município`, `Nome_Município`, `Microrregião Geográfica`, `Nome_Microrregião Geográfica`,
                   `Mesorregião Geográfica`, `Nome_Mesorregião Geográfica`, `Região Geográfica Imediata`,
                   `Nome_Região Geográfica Imediata`, `Região Geográfica Intermediária`,
                   `Nome_Região Geográfica Intermediária`, `Região Metropolitana`, `Nome_Região Metropolitana`,
                   `Tipo_Região Metropolitana`, `UF`, `Nome_UF`, Sigla_UF, `Região`, `Nome_Região` )
  
  return ( flatCities )
}

# Maps an API result type name to its index on ibge_api_endpoints
ibgeLocTypeID <- function( name )
{ return( match( name, ibge_loc_types ) ) }

# Shortcuts for getLocalidade
# Retrieve selected or all locations of a given type
getMunicipio <- function( id = NULL, filter = NULL )
{ return( getLocalidade( "municipio", id, filter ) ) }

getMicrorregiao <- function( id = NULL, filter = NULL )
{ return( getLocalidade( "microrregiao", id, filter ) ) }

getMesorregiao <- function( id = NULL, filter = NULL )
{ return( getLocalidade( "mesorregiao", id, filter ) ) }

getRegiaoImediata <- function( id = NULL, filter = NULL )
{ return( getLocalidade( "regiao-imediata", id, filter ) ) }

getRegiaoIntermediaria <- function( id = NULL, filter = NULL )
{ return( getLocalidade( "regiao-intermediaria", id, filter ) ) }

getEstado <- function( id = NULL, filter = NULL )
{ return( getLocalidade( "UF", id, filter ) ) }

getRegiao <- function( id = NULL, filter = NULL )
{ return( getLocalidade( "regiao", id, filter ) ) }

getDistrito <- function( id = NULL, filter = NULL )
{ return( getLocalidade( "distrito", id, filter ) ) }

getSubdistrito <- function( id = NULL, filter = NULL )
{ return( getLocalidade( "subdistrito", id, filter ) ) }

getLocalidade <- function( type = ibge_loc_types, id = NULL, filter = NULL, recurse = 0 )
{ return( parseAPIResult( queryLocalidade( type, id, filter ), recurse ) ) }

# Generic retrieval using the Localidade API
queryLocalidade <- function( type = ibge_loc_types, id = NULL, filter = NULL )
{
  type <- match.arg( type )
  endpoint <- ibge_api_endpoints[ ibgeLocTypeID( type ), "endpoint" ]
  url <- paste( ibge_api_host, ibge_loc_api_path, sep = "/" )
  
  if( length( id ) > 1 )
    id <- paste( id, collapse = "|" )
  
  if( !is.null( filter ) )
  {
    filter <- match.arg( filter, ibge_api_endpoints$filters[[ endpoint ]] )
    end_filter <- ibge_api_endpoints[ ibgeLocTypeID( filter ), "endpoint" ]
    url <- paste( url, end_filter, id, endpoint, sep = "/" )
  }
  else if( !is.null( id ) )
  { url <- paste( url, endpoint, id, sep = "/" ) }
  else
  { url <- paste( url, endpoint, sep = "/" ) }

  resp <- httr::GET( url, agent )

  if( httr::status_code( resp ) != 200 )
  { stop( paste0( "getLocalidade: IBGE API request failed (", httr::status_code( resp ), "), URL: ", url ) ) }
  else if( httr::http_type( resp ) != "application/json" )
  { stop( paste0( "getLocalidade: IBGE API response not JSON, URL: ", url ) ) }
  
  parsed <- jsonlite::fromJSON( httr::content( resp, "text" ) )

  result <- structure(
    list( content = parsed, type = type, id = id, filter = filter,
          response = resp ),
    class = "ibge_localidade_api"
  )
  
  return( result )
}

# Shortcut for column renaming during parse
parseColName <- function( name, type = ibge_loc_types )
{
  type <- match.arg( type )
  type_name <- ibge_api_endpoints[ ibgeLocTypeID( type ), "name" ]
  parsed <- switch( name,
    "id" = as.character( type_name ),
    "nome" = paste0( "Nome_", type_name ),
    "sigla" = paste0( "Sigla_", type_name ),
    as.character( ibge_api_endpoints[ ibgeLocTypeID( name ), "name" ] )
  )
  return( parsed )
}

parseAPIResult <- function( api_result, recurse = 0 )
{
  stopifnot( class( api_result ) == "ibge_localidade_api" )

  return( parseLocalidade( api_result$content, api_result$type, recurse ) )
}

# Transform api results list into a data.frame with renamed columns
# If recursive, parse parent localities
# Heavily based on jsonlite::flatten by Jeroen Ooms
parseLocalidade <- function( content, type = ibge_loc_types, recurse = 0 )
{
  type <- match.arg( type )

  parents <- vapply( content, is.list, logical( 1 ) )
  nr <- nrow( content )
  parsed <- content[ !parents ]
  names( parsed ) = vapply( names( parsed ), parseColName, character( 1 ), type )
  
  if( any( parents ) )
  {
    if( recurse > 0 )
    {
      parsed_parents <-
        do.call( c, mapply( parseLocalidade, content[ parents ], names( content[ parents ] ),
                            MoreArgs = list( recurse = recurse - 1 ), SIMPLIFY = FALSE, USE.NAMES = FALSE ) )
    }
    else
    {
      parsed_parents <- lapply( content[ parents ], '[[', 'id' )
      names( parsed_parents ) = vapply( names( parsed_parents ), parseColName, character( 1 ), type )
    }

    parsed <- c( parsed, parsed_parents )
  }

  class( parsed ) <- "data.frame"
  row.names( parsed ) <- if( is.null( nr ) ) 1 else 1:nr
  parsed <- parsed[ , !duplicated( colnames( parsed ), fromLast = TRUE ) ]
  return( parsed )
}

getSimpleNames <- function()
{
  cids <- setNames( getMunicipio()[ , c( "Município", "Nome_Município" ) ], c( "id", "name" ) )
  imeds <- getLocalidade( "regiao-imediata", recurse = 9 ) %>%
    transmute( id = `Região Imediata`, name = paste( "RGI", `Nome_Região Imediata`, "-", Sigla_UF ) )
  ints <- getLocalidade( "regiao-intermediaria", recurse = 9 ) %>%
    transmute( id = `Região Intermediária`, name = paste( "RGInt", `Nome_Região Intermediária`, "-", Sigla_UF ) )
  ufs <- setNames( getEstado()[ , c( "UF", "Nome_UF" ) ], c( "id", "name" ) )
  regs <- getRegiao() %>% transmute( id = `Região`, name = paste( "Região", `Sigla_Região` ) )
  return( rbind( cids, imeds, ints, ufs, regs ) )
}

getParents <- function( id, type = c( "rg", "mm" ) )
{
  
}
