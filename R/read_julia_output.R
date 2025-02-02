##' Reads and augments data and output from julia fit
##' @title Read Julia input data and output data
##' @param juliaout_path character, path to julia output of fitted
##'   chains, extracted density evaluations, estimated locational
##'   desirability and plots
##' @param data_path character, path to data directory for project
##' @return list, list of data.tables
##' @author Konstantin Hoffie
##' @import data.table
##' @export read_julia_output
read_julia_output <- function(juliaout_path, data_path) {
  districts <- data.table::fread(file.path(data_path, "districts.csv"))
  ##    path <- file.path(path, "manuscript_input")
  path <- juliaout_path
  dt_geog <- read_output(path, "germgeog")
  dt_flows <- read_output(path, "germflows")
  dt_params <- read_output(path, "germparams")
  params_add_year(dt_params)
  dt_flows <- augment_flows(dt_flows, districts)
  dt_geog <- augment_geog_germ(data_path, dt_geog, dt_flows, districts)
  dt_dens <- read_output(path, "germdensfun")
  sax <- dens_add_saxdens(dt_dens, districts)
  dens_add_year(dt_dens, dt_flows)
  shp_st <- sf::read_sf(file.path(data_path, "raw/shapes/states.shp"))
  shp_st <- data.table::setDT(shp_st)
  cor_ext <- data.table::fread(file.path(data_path, "clean/correct.csv"))[year < 2021]
  out <- list(dt_flows = dt_flows, dt_geog = dt_geog, dt_dens = dt_dens,
              dt_params = dt_params, districts = districts,
              shp_st = shp_st, cor_ext = cor_ext, sax = sax)
  return(out)
}

read_output <- function(path, type, model = "US-model") {
  agegroup <-NULL
  age_lvls <- c("below18", "18-25", "25-30", "30-50", "50-65", "above65")
  read_agegroup <- function(path, file) {
    agegroup <- NULL
    dt <- data.table::fread(file.path(path, file))
    dt[, agegroup := get_age(file)]
    return(dt)
  }
  get_age <- function(file) {
    age <- data.table::tstrsplit(file, "_")[[3]]
    age <- data.table::tstrsplit(age, "\\.")[[1]]
    return(age)
  }
  dt <- data.table::rbindlist(lapply(list.files(path, pattern = type),
                                     function(x) read_agegroup(path, x)))
  dt[, agegroup := factor(agegroup, levels = age_lvls)]
  dt[, model := model]
  return(dt)
}
augment_flows <- function(flows, districts) {
  resid1 <- resid2 <- fromdens <- i.density <- distcode <- NULL
  year <- todens <- year <- fromdist <- todist <- . <- NULL
  preds <-
    flows[, resid1 := (flows - preds) / preds]
  flows[, resid2 := sign(resid1) * log(1 + abs(resid1))]
  flows[districts, fromdens := i.density, on = .(fromdist = distcode, year)]
  flows[districts, todens := i.density, on = .(todist = distcode, year)]
  impute_actual_zeros(dt_flows, -10)
  data.table::setnames(flows, "dist", "distance")
  dt_flows <- dt_flows[fromdist != 3159] ## these should not be in here!
  dt_flows <- dt_flows[todist != 3159] ## these should not be in here!
  return(flows)
}
impute_actual_zeros <- function(dt, mean = -15) {
  actual_imp <- flows <- NULL
  dt[, actual_imp := as.double(flows)]
  rnds <- stats::rlnorm(dt[flows == 0, .N], mean, log(2.0))
  dt[flows == 0, actual_imp := rnds]
  return(NULL)
}
augment_geog_germ <- function(data_path, geog, flows, districts) {
  AGS <- year <- net <- i.net <- region <- agegroup <- total <- NULL
  geometry <- i.geometry <- pop_all <- i.pop <- distcode <- NULL
  total_pred <- i.total <- . <- NULL
  shp <- data.table::setDT(sf::read_sf(file.path(data_path,
                                     "clean/shapes/districts.shp")))
  shp[, AGS := as.integer(AGS)]
  ## There is something wrong with 2017. Apparently it holds all years
  ## 2011 to 2017 but they are not distinguished.
  geog[, year := rep(c(2000:2002, 2004:2017), each = 401 * 6)]
  net <- helpeR::calculate_net(flows, "flows",
                               by = c("year", "agegroup"))
  net_pred <- helpeR::calculate_net(flows, "preds",
                                    by = c("year", "agegroup"))
  geog[net, net := i.net, on = .(distcode = region, year, agegroup)]
  geog[net, total := i.total, on = .(distcode = region, year, agegroup)]
  geog[shp, geometry := i.geometry, on = .(distcode = AGS)]
  geog[districts, pop_all := i.pop, on = .(distcode, year)]
  geog[net_pred, net_pred := i.net,
       on = .(distcode = region, year, agegroup)]
  geog[net_pred, total_pred := i.total,
       on = .(distcode = region, year, agegroup)]
  return(geog)
}
params_add_year <- function(dt_params) {
  grp <- agegroup <- parname <- year <- i.year <- NULL
  dt_params[, grp := 1 : .N, keyby = .(agegroup, parname)]
  years <- c(2000:2002, 2004:2017)
  n_years <- length(years)
  rec_grp <- data.table::data.table(grp = 1:n_years, year = years)
  dt_params[rec_grp, year := i.year, on = .(grp)]
}
dens_add_saxdens <- function(dt_dens, districts) {
  logreldens <- year <- bl_name <- distcode <- name <- NULL
  minlogreldens <- maxlogreldens <- fromsax <- i.distcode <- NULL
  density <- . <- NULL
  fromdens <- tosax <- todens <- NULL
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
