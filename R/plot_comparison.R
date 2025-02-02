##' @title Plots comparison of baseline-flow model and gravity model
##' @param dt_flows data.table
##' @param age character
##' @param main character
##' @param n integer, number of data points
##' @param color logical, if TRUE, gravity and baseline flow have
##'   different colors
##' @return list, list of plots
##' @author Konstantin Hoffie
##' @import data.table
##' @import ggplot2
##' @export plot_comparison
plot_comparison <- function(dt_flows, age, main, n = 50e3, color = TRUE) {
  compare_gravity <- function(dt_flows) {
    fromdist <- todist <- year <- agegroup <- flows <- distance <- NULL
    . <- frompop <- topop <- preds <- actual_imp <- .SD <- NULL
    gravity <- NULL
    dt_plt <- dt_flows[, .(fromdist, todist, year, agegroup,
                           flows, distance, frompop, topop, preds, actual_imp)]
    dt_plt[, gravity := predict(helpeR::fit_gravity(.SD, offset = FALSE), type = "response"),
           keyby = .(agegroup)]
    data.table::setnames(dt_plt, "preds", "baseline-flow")
    dt_plt <- dt_plt[sample(1:.N, min(n, .N), replace = FALSE)]
    dtm <- data.table::melt(dt_plt,
                            measure.vars = c("gravity", "baseline-flow"),
                            variable.name = "model",
                            value.name = "preds")
    return(dtm)
    }
  make_plot <- function(dt, age, main, color) {
    agegroup <- distance <- flows <- preds <- model <- NULL
        if(color == TRUE) {
        fit <-  ggplot2::ggplot(dt[agegroup == age],
                       aes(distance, log(flows / preds, base = 2),
                           color = model))
    }
    if(color == FALSE) {
        fit <-  ggplot2::ggplot(dt[agegroup == age],
                       aes(distance, log(flows / preds, base = 2)))
    }
        cap <- sprintf("internal migration flows of %s year olds \nGermany, 2017, n = %s",
                       age, n)
    fit <- fit +
        geom_hline(yintercept = 0) +
        geom_point(alpha = .1, size =.5) +
        geom_smooth(se = FALSE, linewidth = 2) +
        theme_minimal() +
        ggtitle(main) +
        labs(caption = cap) +
        xlab("distance in km") +
        theme(legend.position = "top") 
    }
    dt_plt <- compare_gravity(dt_flows)
    plt1 <- make_plot(dt_plt[model == "gravity"], age, main, color) +
        scale_color_manual(values = c("red"))
    plt2 <- make_plot(dt_plt[model == "baseline-flow"], age, main, color) +
        scale_color_manual(values = c("blue"))
    plt3 <- make_plot(dt_plt, age, main, color) +
        scale_color_manual(values = c("red", "blue"))
    return(list(plt1, plt2, plt3))
}
