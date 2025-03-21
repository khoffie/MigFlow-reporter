##' Makes desirability maps in report
##' @param dt data.table
##' @param type character, what should be shown? Estimated
##'   desirability or net migration
##' @return ggplots
##' @author Konstantin Hoffie
##' @export make_desirmaps
##' @import ggplot2
##' @import helpeR
make_desirmaps <- function(dt, type = c("desir", "net")) {
  des <- desir_map(dt, type[1])
  net <- desir_map(dt, type[2])
  return(des + net)
}

desir_map <- function(dt, type = c("desir", "net", "pred")) {
  agegroup <- desirability <- net <- pop_all <- net_pred <- NULL
  type <- match.arg(type)
  age <- dt[, unique(agegroup)]
  if(type == "desir") {
    map <- ggplot2::ggplot(helpeR::set_geom(dt, FALSE)) +
      geom_sf(aes(fill = desirability)) +
      ggtitle(sprintf("%s, estimated desirability", age))
  }
  if(type == "net") {
    map <- ggplot2::ggplot(helpeR::set_geom(dt, FALSE)) +
      geom_sf(aes(fill = net / pop_all * 100)) +
      ggtitle(sprintf("%s, Net migration", age))
  }
  if(type == "pred") {
    map <- ggplot2::ggplot(helpeR::set_geom(dt, FALSE)) +
      geom_sf(aes(fill = net_pred / pop_all * 100)) +
      ggtitle(sprintf("%s, Predicted net migration", age))
  }
  map <- map +
    theme_bw() +
    theme(legend.position = "top")
  return(map)
}
