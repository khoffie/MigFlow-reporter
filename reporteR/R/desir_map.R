desir_map <- function(dt, type = c("desir", "net", "pred")) {
    type <- match.arg(type)
    age <- dt[, unique(agegroup)]
    if(type == "desir") {
        map <- ggplot(set_geom(dt, F)) +
            geom_sf(aes(fill = desirability)) +
            ggtitle(sprintf("%s, estimated desirability", age))
    }
    if(type == "net") {
        map <- ggplot(set_geom(dt, F)) +
            geom_sf(aes(fill = net / pop_all * 100)) +
            ggtitle(sprintf("%s, Net migration", age))
    }
    if(type == "pred") {
        map <- ggplot(set_geom(dt, F)) +
            geom_sf(aes(fill = net_pred / pop_all * 100)) +
            ggtitle(sprintf("%s, Predicted net migration", age))
    }
    map <- map +
        theme_bw() +
        theme(legend.position = "top") 
    return(map)
}

make_desirmaps <- function(dt, type = c("desir", "net")) {
    des <- desir_map(dt, type[1])
    net <- desir_map(dt, type[2])
    return(des + net)
}
