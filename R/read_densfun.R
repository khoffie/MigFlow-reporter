##' Reads density transition estimates from julia model
##'
##' @param jp character, path to julia output
##' @param dp character, path to data folder, e.g.
##'   GermanMigration/data
##' @return data.table
##' @import data.table
##' @export read_densfun
##' @author Konstantin Hoffie
read_densfun <- function(jp, dp) {
  flows <- read_output(jp, "germflows")
  dens <- read_output(jp, "germdensfun")
  districts <- data.table::fread(file.path(dp, "districts.csv"))

  dens_add_saxdens(dens, districts)
  dens_add_year(dens, flows)
  return(dens)
}

##' creates 'sax', small helper data.table with Saxonian districts.
##'
##' @param jp character, path to julia output
##' @param dp character, path to data folder, e.g.
##'   GermanMigration/data
##' @return data.table
##' @import data.table
##' @export read_sax
##' @author Konstantin Hoffie
read_sax <- function(jp, dp) {
  dens <- read_output(jp, "germdensfun")
  districts <- data.table::fread(file.path(dp, "districts.csv"))

  sax <- dens_add_saxdens(dens, districts)
  return(sax)
}

dens_add_saxdens <- function(dt_dens, districts) {
  logreldens <- year <- bl_name <- distcode <- name <- NULL
  minlogreldens <- maxlogreldens <- fromsax <- i.distcode <- NULL
  density <- . <- fromdens <- tosax <- todens <- NULL
  districts[, logreldens := log(density / stats::median(density))]
  sax <- districts[year == 2017][
    bl_name == "Sachsen"][, .(distcode, name, logreldens)]
  ##    sax <- sax[distcode %notin% c(14524, 14511, 14612, 14713)]
  tol <- 0.1 # Define a small tolerance
  sax[, minlogreldens := logreldens - tol]
  sax[, maxlogreldens := logreldens + tol]

  dt_dens[sax, fromsax := i.distcode,
          on = .(fromdens > minlogreldens, fromdens < maxlogreldens)]
  dt_dens[sax, tosax := i.distcode,
          on = .(todens > minlogreldens, todens < maxlogreldens)]
  message("Added approximate density of Saxonian districts")
  return(sax)
}

dens_add_year <- function(dt_dens, dt_flows) {
  grp <- agegroup <- year <- i.year <- . <- NULL
  ## there are 10000 rows per year and agegroup
  dt_dens[, grp := 1 : .N, keyby = agegroup]
  dt_dens[, grp := ceiling(grp / 10e3)]
  n_years <- dt_flows[, data.table::uniqueN(year)]
  years <- dt_flows[, unique(year)]
  rec_grp <- data.table::data.table(grp = 1:n_years, year = years)
  dt_dens[rec_grp, year := i.year, on = .(grp)]
}
