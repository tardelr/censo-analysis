library(lodown)

datasets.path <- "~/Desktop/datasets"

# examine all available CENSO microdata files
get_censo_cat <-
  get_catalog( "censo" ,
               output_dir = file.path( path.expand( datasets.path ) , "CENSO" ) )

# 2000 only
censo_cat <- subset( get_censo_cat , state == 'sp00')
# 'sp2_rm10'
# 'sp00'

# # download the microdata to your local computer
censo_cat <- lodown( "censo" , censo_cat )

# choose columns to import from both household and person files
cols00 <- c( 'v0103', 'areap' , 'v0214', 'v0215', 'v0216', 'v0217', 'v0218', 'v0219', 'v0220', 'v0221', 'v0222', 'v0223')
cols10 <- c( 'v0002', 'v0011', 'v0213','v0214', 'v0215', 'v0216', 'v0217', 'v0218', 'v0219', 'v0220', 'v0221', 'v0222')

columns_to_import <- cols00

# initiate a data.frame to stack all downloaded censo states
censo_df <- data.frame( NULL )

# only construct one censo design at a time (2000 and 2010 should not be stacked)
stopifnot( length( unique( censo_cat[ , 'year' ] ) ) == 1 )

# loop through all downloaded censo states
for( this_state in seq( nrow( censo_cat ) ) ){
  
  # add the design information to the columns to import
  these_columns_to_import <-
    unique( 
      c( 
        columns_to_import , 
        as.character( 
          censo_cat[ this_state , c( 'weight' , paste0( 'fpc' , 1:5 ) ) ] 
        ) 
      ) 
    )
  
  # remove NAs
  these_columns_to_import <- these_columns_to_import[ !is.na( these_columns_to_import ) ]
  
  # load structure files, lowercase variable names, set unwanted columns to missing
  dom_stru <- SAScii::parse.SAScii( censo_cat[ this_state , 'dom_sas' ] )
  dom_stru$varname <- tolower( dom_stru$varname )
  
  pes_stru <- SAScii::parse.SAScii( censo_cat[ this_state , 'pes_sas' ] )
  pes_stru$varname <- tolower( pes_stru$varname )
  
  # import fixed-width files
  this_censo_dom_df <- 
    data.frame( readr::read_fwf(
      censo_cat[ this_state , 'dom_file' ] ,
      readr::fwf_widths( 
        abs( dom_stru$width ) , col_names = dom_stru[ , 'varname' ] 
      ) ,
      col_types = 
        paste0( 
          ifelse( !( dom_stru$varname %in% these_columns_to_import ) , 
                  "_" , 
                  ifelse( dom_stru$char , "c" , "d" ) 
          ) , 
          collapse = "" 
        )
    ) )
  
  this_censo_pes_df <- 
    data.frame( readr::read_fwf(
      censo_cat[ this_state , 'pes_file' ] ,
      readr::fwf_widths( 
        abs( pes_stru$width ) , col_names = pes_stru[ , 'varname' ] 
      ) ,
      col_types = 
        paste0( 
          ifelse( !( pes_stru$varname %in% these_columns_to_import ) , 
                  "_" , 
                  ifelse( pes_stru$char , "c" , "d" ) 
          ) , 
          collapse = "" 
        )
    ) )
  
  # add decimals
  for( this_variable in these_columns_to_import ) {
    
    if( 
      ( this_variable %in% names( this_censo_dom_df ) ) & 
      !isTRUE( all.equal( 1 , dom_stru[ dom_stru$varname == this_variable , 'divisor' ] ) ) 
    ){
      this_censo_dom_df[ , this_variable ] <- 
        dom_stru[ dom_stru$varname == this_variable , 'divisor' ] * 
        this_censo_dom_df[ , this_variable ]
    }
    
    if( 
      ( this_variable %in% names( this_censo_pes_df ) ) & 
      !isTRUE( all.equal( 1 , pes_stru[ pes_stru$varname == this_variable , 'divisor' ] ) ) 
    ){
      this_censo_pes_df[ , this_variable ] <- 
        pes_stru[ pes_stru$varname == this_variable , 'divisor' ] * 
        this_censo_pes_df[ , this_variable ]
    }
    
  }
  
  # merge household and person tables
  this_censo_df <- merge( this_censo_dom_df , this_censo_pes_df )
  
  # confirm one record per person, with household information merged on
  stopifnot( nrow( this_censo_df ) == nrow( this_censo_pes_df ) )
  
  rm( this_censo_dom_df , this_censo_pes_df ) ; gc()
  
  # stack the merged tables
  censo_df <- rbind( censo_df , this_censo_df )
  
  rm( this_censo_df ) ; gc()
  
}
write.csv( censo_df, file = file.path( path.expand( datasets.path ), 'censo_assets' ,'sp00' ) )
