plot_frac0 <- function(dt) {
    ### uhhh this code is ugly
    plot_frac0_dist <- function(dt) {
    ggplot(dt[, .(frac0 = mean(flows == 0)), keyby = distance],
           aes(distance, frac0)) +
        geom_smooth(se = FALSE, linewidth = 2) +
        theme_bw() +
        ylab("Fraction of zero moves") 
##        ggtitle("Fraction of zero flows v distance, smoothed")
}
    plot_frac0_dens <- function(dt) {
        ggplot(dt[, .(frac0 = mean(flows == 0)), keyby = log(fromdens * todens)],
               aes(log, frac0)) +
            geom_smooth(se = FALSE, linewidth = 2) +
            theme_bw() +
            ylab("") +
            xlab("Log(density origin * density destination)") 
##            ggtitle("Fraction of zero flows v log(density origin * density destination), smoothed")
}
    plt1 <- plot_frac0_dist(dt)
    plt2 <- plot_frac0_dens(dt)
    return(plt1 + plt2)
}
