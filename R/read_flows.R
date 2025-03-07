##' Reads flows, with predictions from Julia model
##' @param jp character, path to julia output
##' @param dp character, path to data folder, e.g.
##'   GermanMigration/data
##' @return data.table
##' @import data.table
##' @export read_flows
##' @author Konstantin Hoffie
read_flows <- function(jp, dp) {
  flows <- read_output(jp, "germflows")
  districts <- data.table::fread(file.path(dp, "districts.csv"))
  flows <- augment_flows(flows, districts)
  return(flows)
}

augment_flows <- function(flows, districts) {
  resid1 <- resid2 <- fromdens <- i.density <- distcode <- NULL
  year <- todens <- year <- fromdist <- todist <- . <- NULL

  preds <-flows[, resid1 := (flows - preds) / preds]
  flows[, resid2 := sign(resid1) * log(1 + abs(resid1))]
  flows[districts, fromdens := i.density, on = .(fromdist = distcode, year)]
  flows[districts, todens := i.density, on = .(todist = distcode, year)]
  impute_actual_zeros(flows, -10)
  data.table::setnames(flows, "dist", "distance")
  flows <- flows[fromdist != 3159] ## these should not be in here!
  flows <- flows[todist != 3159] ## these should not be in here!
  return(flows)
}

impute_actual_zeros <- function(dt, mean = -15) {
  actual_imp <- flows <- NULL
  dt[, actual_imp := as.double(flows)]
  rnds <- stats::rlnorm(dt[flows == 0, .N], mean, log(2.0))
  dt[flows == 0, actual_imp := rnds]
  return(NULL)
}
