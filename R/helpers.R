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

.check_project <- function(path) {
  if (
    !dir.exists(file.path(path, "output")) ||
      !dir.exists(file.path(path, "data"))
  ) {
    stop("Project folders not found. Run initialiseProject() first.")
  }
}
