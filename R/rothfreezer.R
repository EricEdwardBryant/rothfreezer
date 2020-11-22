#---- src_rothfreezer ---------------------------------------------------------
#' Connect to the rothfreezer database
#' 
#' Connect to the rothfreezer SQLite database. 
#' 
#' @importFrom RSQLite SQLite
#' @importFrom easydb db_config db_build
#' @export
#' 
#' @examples 
#' # For a description of each table
#' vignette('database-overview', package = 'rothfreezer')
#' 
#' # Connect to the database
#' db <- src_rothfreezer()
#' db
#' 

src_rothfreezer <- function() {
  # Read configuration file
  cnf <- system.file('db/_rothfreezer.yaml', package = 'rothfreezer') %>% db_config()
  
  # Build the db if it does not exist
  if (!file.exists(cnf$db)) suppressWarnings(db_build(cnf))
  
  # Connect to the db. Mimics functionality of deprecated dbplyr::src_sqlite
  con <- DBI::dbConnect(RSQLite::SQLite(), cnf$db)
  RSQLite::initExtension(con)
  src <- list(con = con, disco = auto_disconnector(con), cnf = cnf)
  class(src) <-
    c("src_rothfreezer", "src_SQLiteConnection", "src_dbi", "src_sql", "src")

  return(src)
}

# Inspired by dbplyr:::db_disconnector
auto_disconnector <- function(con) {
  reg.finalizer(environment(), function(...) DBI::dbDisconnect(con))
  environment()
}

#---- src_rothscreen methods --------------------------------------------------
#' @export
print.src_rothfreezer <- function(x) {
  cat(paste0(
    'sqlite ', RSQLite::rsqliteVersion()[[2]], 
    ' [rothfreezer - ', packageVersion('rothfreezer'), ']\n',
    dplyr:::wrap('tbls: ', paste0(RSQLite::dbListTables(x$con), collapse = ', '))
  ))
}

#' @importFrom dbplyr tbl_sql
#' @export
tbl.src_rothfreezer <- function(src, from, ...) {
  dbplyr::tbl_sql('sqlite', src = src, from = from, ...) %>% add_class('tbl_rothfreezer')
}

#' @export
collect.tbl_rothfreezer <- function(x, ...) {
  
  result <- drop_class(x) %>% collect(...)
  coltypes <- x$src$cnf$column_types
  present <- intersect(names(coltypes), names(result))

  if (length(present)) {
    coerce <- coltypes[present]
    for (i in 1:length(coerce)) {
      type   <- coerce[[i]]
      column <- names(coerce[i])
      result[[column]] <- 
        switch(
          type,
          logical   = as.logical(result[[column]]),
          integer   = as.integer(result[[column]]),
          character = as.character(result[[column]]),
          numeric   = as.numeric(result[[column]]),
          double    = as.numeric(result[[column]]),
          factor    = as.factor(result[[column]])
        )
    }
  }
  # Coerce to desired column type
  return(result)
}
  

#---- OO Utilities ------------------------------------------------------------
# Add an S3 subclass in pipeline
# @param x an S3 object
# @param ... Character; S3 subclasses to add to object
add_class <- function(x, ...) { class(x) <- c(..., class(x)); return(x) }

# Drop an S3 subclass in pipeline
# @param x an S3 object
drop_class <- function(x) { class(x) <- class(x)[-1]; return(x)}
