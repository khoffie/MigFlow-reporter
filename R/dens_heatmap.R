##' @title Make density transition heatmaps
##' @param dt data.table, dt_dens
##' @param sax data.table, only Saxonian districts
##' @param min_y integer, earliest year
##' @return ggplot object
##' @author Konstantin Hoffie
##' @import ggplot2
##' @import data.table
##' @export densheatmap
densheatmap <- function(dt, sax, min_y = 2011) {
  fromdens <- todens <- year <- agegroup <- funval <- NULL
  distcode <- fromsax <- tosax <- NULL
  dt2 <- data.table::copy(dt)
  replace_densities <- function(dt_dens, base_year) {
    year <- fromdens <- todens <- NULL
    ## for some reason fromdens and todens differ slightly between years
    ## 2000 to 2010, thus heatmaps can not be plotted together for years
    find_closest <- function(values, base_values) {
      sapply(values, function(x) base_values[which.min(abs(base_values - x))])
    }
    ## find_closest(dt_dens[year == 2001, unique(fromdens)], dt_dens[year == 2017, unique(fromdens)])
    base <- dt_dens[year == base_year, unique(fromdens)]
    dt_dens[, fromdens := find_closest(fromdens, base), keyby = year]
    dt_dens[, todens := find_closest(todens, base), keyby = year]
    message(sprintf("replaced fromdens and todens with closest density from %s", base_year))
  }
  replace_densities(dt2, 2017)
  reverse <- function(x) {
    meddens <- 199.9473 ## districts[, median(density)]
    y <- round(exp(x) * meddens, 0)
    return(y)
  }
  brks <- -1:3
  dt2 <- dt2[fromdens < 2.7 & todens < 2.7 & year >= min_y]
  age <- dt2[, unique(agegroup)]
    if(age == "below18") {
        age <- "unter 18"
    }
    main <- sprintf("Dichteübergangsfunktion %s", age)
    plt <- ggplot(dt2, aes(fromdens, todens, fill = funval)) +
        geom_tile() +
        facet_wrap(~year, scales = "fixed") +
        scale_fill_viridis_c() +
        theme_bw() +
        theme(legend.position = "none") +
        labs(title = main,
             x = "Bevölkerungsdichte Ursprung",
             y = "Bevölkerungsdichte Ziel") +
        geom_rug(data = dt2[fromsax %in% sax[, distcode]], aes(x = fromdens), sides = "b") +
        geom_rug(data = dt2[tosax %in% sax[, distcode]], aes(x = todens), sides = "l") +
        scale_x_continuous(breaks = brks, labels = reverse(brks)) +
        scale_y_continuous(breaks = brks, labels = reverse(brks)) 
    return(plt)
}
