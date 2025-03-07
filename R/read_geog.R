##' Reads geog, with estimated locational desirabilies from Julia
##' model; augmented with various other data
##' @param jp character, path to julia output
##' @param dp character, path to data folder, e.g.
##'   GermanMigration/data
##' @return data.table
##' @import data.table
##' @import sf
##' @export read_geog
##' @author Konstantin Hoffie
read_geog <- function(jp, dp) {
  AGS <- NULL

  dt_geog <- read_output(jp, "germgeog")
  dt_flows <- read_output(jp, "germflows")
  districts <- data.table::fread(file.path(dp, "districts.csv"))
  f <- file.path(dp, "clean/shapes/districts.shp")
  shp <- data.table::setDT(sf::read_sf(f))[, AGS := as.integer(AGS)]
  pops <- data.table::fread(file.path(dp, "clean/germanpop.csv"))

  dt_flows <- augment_flows(dt_flows, districts)
  dt_geog <- augment_geog_germ(dt_geog, dt_flows, districts, shp, pops)

  return(dt_geog)
}

augment_geog_germ <- function(geog, flows, districts, shape, pops) {
  AGS <- year <- net <- i.net <- region <- agegroup <- total <- NULL
  geometry <- i.geometry <- pop_all <- i.pop <- distcode <- NULL
  total_pred <- i.total <- . <- pop_age_g <- i.german <- nmr <- NULL

  ## There is something wrong with 2017. Apparently it holds all years
  ## 2011 to 2017 but they are not distinguished.
  geog[, year := rep(c(2000:2002, 2004:2017), each = 401 * 6)]
  net <- helpeR::calculate_net(flows, "flows",
                               by = c("year", "agegroup"))
  net_pred <- helpeR::calculate_net(flows, "preds",
                                    by = c("year", "agegroup"))
  geog[net, net := i.net, on = .(distcode = region, year, agegroup)]
  geog[net, total := i.total, on = .(distcode = region, year, agegroup)]
  geog[shape, geometry := i.geometry, on = .(distcode = AGS)]
  geog[districts, pop_all := i.pop, on = .(distcode, year)]
  geog[net_pred, net_pred := i.net,
       on = .(distcode = region, year, agegroup)]
  geog[net_pred, total_pred := i.total,
       on = .(distcode = region, year, agegroup)]
  geog[pops, pop_age_g := i.german,
       on = .(distcode = region, year, agegroup)]
  geog[, nmr := net / pop_age_g * 100]
  return(geog)
}
