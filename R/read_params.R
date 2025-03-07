##' Reads parameter estimates from Julia model
##' @param jp character, path to julia output
##' @param dp character, path to data folder, e.g.
##'   GermanMigration/data
##' @return data.table
##' @import data.table
##' @export read_params
##' @author Konstantin Hoffie
read_params <- function(jp) {
  flows <- read_output(jp, "germflows")
  params <- read_output(jp, "germparams")
  params_add_year(params, flows[, unique(year)])
  return(params)
}

params_add_year <- function(dt_params, years) {
  grp <- agegroup <- parname <- year <- i.year <- . <- NULL
  dt_params[, grp := 1 : .N, keyby = .(agegroup, parname)]
  n_years <- length(years)
  rec_grp <- data.table::data.table(grp = 1:n_years, year = years)
  dt_params[rec_grp, year := i.year, on = .(grp)]
}
