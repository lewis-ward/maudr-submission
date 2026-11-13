# Internal helpers -----------------------------------------------------
make_timestamp <- function(base_dir) {
  stamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  run_id <- stamp
  n <- 1
  
  while (dir.exists(file.path(base_dir, run_id))) {
    n <- n + 1
    run_id <- paste0(stamp, "_", n)
  }
  
  run_id
}

fit_mm <- function(df) {
  df <- df |>
    dplyr::filter(substrate_conc > 0, is.finite(rate), rate >= 0)
  
  if (nrow(df) < 3) {
    return(tibble::tibble(Km = NA_real_, Vmax = NA_real_))
  }
  
  tryCatch(
    {
      fit <- stats::nls(
        rate ~ Vmax * substrate_conc / (Km + substrate_conc),
        data = df,
        start = list(
          Vmax = max(df$rate, na.rm = TRUE),
          Km = stats::median(df$substrate_conc, na.rm = TRUE)
        ),
        algorithm = "port",
        lower = c(Vmax = 0, Km = 0)
      )
      pars <- stats::coef(fit)
      tibble::tibble(
        Km = round(pars[["Km"]], 3),
        Vmax = round(pars[["Vmax"]], 3)
      )
    },
    error = function(e) {
      tibble::tibble(Km = NA_real_, Vmax = NA_real_)
    }
  )
}

.check_project <- function(path) {
  if (
    !dir.exists(file.path(path, "output")) ||
      !dir.exists(file.path(path, "data"))
  ) {
    stop("Project folders not found. Run initialiseProject() first.")
  }
}
