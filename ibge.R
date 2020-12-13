require( dplyr )
require( httr )
require( readxl )

ibge_rms_url <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/municipios_por_regioes_metropolitanas/Situacao_2020a2029/Composicao_RMs_RIDEs_AglomUrbanas_2020_06_30.xlsx"
ibge_rms_ext <- ".xlsx"

ibge_drb_url <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/divisao_regional/divisao_regional_do_brasil/divisao_regional_do_brasil_em_regioes_geograficas_2017/tabelas/regioes_geograficas_composicao_por_municipios_2017_20180911.xlsx"
ibge_drb_ext <- ".xlsx"

ibge_dtb_url <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/divisao_territorial/2019/DTB_2019_v2.zip"
ibge_dtb_file <- "RELATORIO_DTB_BRASIL_MUNICIPIO.xls"
ibge_dtb_ext <- ".xls"

# Download, unzip and import the DTB file
loadIBGEDTB <- function( path = ibge_dtb_url )
{
  tmpzip <- tempfile( fileext = ".zip" )
  # cols <- c( "numeric", "text", "numeric", "text", "numeric", "text",
  #            "numeric", "text", "numeric", "text", "numeric", "numeric",
  #            "text" )

  httr::GET( path, httr::write_disk( tmpzip ) )
  unzip( tmpzip, ibge_dtb_file, exdir = tempdir() )
  
  return ( readxl::read_excel( file.path( tempdir(), ibge_dtb_file ),
                              col_names = TRUE ) )
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

# Builds and returns a named vector of all states (COD_UF) in the RM file
# with their parent regions (GRANDE_REG)
getRegionTable <- function( rm_data = NULL )
{
  if ( is.null( rm_data ) )
    rm_data <- loadIBGERMs()

  regions <- rm_data %>% dplyr::distinct( COD_UF, GRANDE_REG ) %>% 
    dplyr::select( COD_UF, GRANDE_REG ) %>%
    # AC (12) and MS (50) do not appear in any RMs, need to add manually
    dplyr::bind_rows(
      data.frame( COD_UF = c( 12, 50 ),
                  GRANDE_REG = c( "Norte", "Centro-Oeste" ) ) ) %>% 
    dplyr::arrange( COD_UF )

  return ( with( regions, setNames( GRANDE_REG, COD_UF ) ) )
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
    dplyr::left_join( rm_data,
                      by = c( `Código Município Completo` = "COD_MUN" ) )

  # After the join, cities not belonging to any RM are also not assigned
  # to a region (GRANDE_REG), this uses UF from DTB to assign
  regions <- getRegionTable( rm_data )
  full_data$GRANDE_REG <- unname( regions[ full_data$UF ] )
  
  return ( full_data )
}

# Provides a flat table with renamed DTB fields, plus RM ID + name + type,
# and region ID + name
# NOTE: DRB data is not used since presently all info is already in DTB
getCities <- function( dtb_data = NULL, rm_data = NULL, drb_data = NULL )
{
  flatCities <- addRMID( addRegionID( addRMtoDTB( rm_data, dtb_data ) ) )
  flatCities <- flatCities %>%
    # Rename vars to keep with convention, and cast numbers to integer
    dplyr::mutate( `Região` = RID, `Nome_Região` = GRANDE_REG,
                   `Região Metropolitana` = RMID,
                   `Nome_Região Metropolitana` = NOME,
                   `Tipo_Região Metropolitana` = TIPO,
                   # Meso and macro regions numbers restart for each UF
                   # Prefix those with the UF code to differentiate
                   `Microrregião Geográfica` =
                     as.integer( paste0( UF, `Microrregião Geográfica` ) ),
                   `Mesorregião Geográfica` =
                     as.integer( paste0( UF, `Mesorregião Geográfica` ) ),
                   UF = as.integer( UF ),
                   `Região Geográfica Intermediária` =
                     as.integer( `Região Geográfica Intermediária` ),
                   `Região Geográfica Imediata` =
                     as.integer( `Região Geográfica Imediata` ),
                   `Município` = `Código Município Completo`,
                   `Nome_Região Geográfica Intermediária` =
                     `Nome Região Geográfica Intermediária`,
                   `Nome_Região Geográfica Imediata` =
                     `Nome Região Geográfica Imediata`,
                   `Nome_Mesorregião Geográfica` = `Nome_Mesorregião`,
                   `Nome_Microrregião Geográfica` = `Nome_Microrregião` ) %>%
    # Drop unused columns
    dplyr::select( `Município`, `Nome_Município`, `Microrregião Geográfica`,
                   `Nome_Microrregião Geográfica`, `Mesorregião Geográfica`,
                   `Nome_Mesorregião Geográfica`, `Região Geográfica Imediata`,
                   `Nome_Região Geográfica Imediata`,
                   `Região Geográfica Intermediária`,
                   `Nome_Região Geográfica Intermediária`,
                   `Região Metropolitana`, `Nome_Região Metropolitana`,
                   `Tipo_Região Metropolitana`, `UF`, `Nome_UF`, `Região`,
                   `Nome_Região` )
  
  return ( flatCities )
}
