filter_rows <- function(dt_flows, dt_filter, n = 10, type = c("net", "total"), largest = TRUE) {
    get_districts_with_large_net_error <- function(dt, n, type, largest) {
        c <- type
        cp <- paste0(c, "_pred")
        dt[, error := (get(c) - get(cp)) / pop * 100]
        dt2 <- dt[order( - abs(error)), .SD, 
                  .SDcols = c("distcode", "agegroup", c, cp, "error")]
        dt2 <- dt2[, if (largest) head(.SD, n) else tail(.SD, n), keyby = agegroup]
        return(dt2)
    }
    filter_rows_ <- function(dt, dt_filter, type) {
        c <- type
        cp <- paste0(c, "_pred")
        age <- dt_filter[, agegroup]
        ags <- dt_filter[, distcode]
        err <- dt_filter[, round(get(c) - get(cp), 0)]
        dt2 <- dt[agegroup == age][fromdist == ags | todist == ags]
        dt2[, group := sprintf("error: %s", err)]
        dt2[, district := ags]
        return(dt2)
    }
    errs <- get_districts_with_large_net_error(dt_filter, n = n, type = type, largest = largest)
    fl <- rbindlist(lapply(1:nrow(errs), function(i) filter_rows_(dt_flows, errs[i, ], type = type)))
    fl[, type := fifelse(district == fromdist, "inflow", "outflow")]
    return(list(fl, errs))
}

plot_flows <- function(dt) {
    plt <- ggplot(dt, aes(log(flows), flows - preds, color = type)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
        geom_point(alpha = .2) +
        geom_smooth(se = FALSE) +
        facet_wrap(~group, scales = "free") +
        ggtitle(dt[, unique(agegroup)]) +
        theme_bw() +
        theme(legend.position = "top")
    return(plt)
}

plot_and_save <- function(dt, age, path) {
    plot_flows(dt[agegroup == age])
    fn <- paste0(age, ".pdf")
    helpeR::ggsavew(, filename = file.path(path, fn))
    return(NULL)
}
