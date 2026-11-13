# Student files are absorbance readings from a spectrophotometer: they should
# never go below zero, even with heavy noise.

test_that("student files never contain negative absorbances", {
	proj <- initialiseProject(
		path = tempdir(),
		name = basename(tempfile("proj")),
		verbose = FALSE
	)
	setup <- assignReactions(project_path = proj, verbose = FALSE)
	generateAssignments(
		run_timestamp = setup$timestamp,
		project_path = proj,
		jitter_sd = 0.05,
		verbose = FALSE
	)
	
	files <- list.files(
		file.path(proj, "output", "assignments_output", setup$timestamp),
		pattern = "_data\\.xlsx$",
		full.names = TRUE
	)
	absorbances <- purrr::map(files, readxl::read_xlsx) |>
		purrr::map(\(d) unlist(d[grepl("_mM$", names(d))]))
	
	expect_true(all(unlist(absorbances) >= 0))
})