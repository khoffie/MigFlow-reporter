read_output <- function(path, type, model = "US-model") {
  agegroup <- NULL
  age_lvls <- c("below18", "18-25", "25-30", "30-50", "50-65", "above65")

  dt <- data.table::rbindlist(lapply(list.files(path, pattern = type),
                                     function(x) read_agegroup(path, x)))
  dt[, agegroup := factor(agegroup, levels = age_lvls)]
  dt[, model := model]
  return(dt)
}

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
