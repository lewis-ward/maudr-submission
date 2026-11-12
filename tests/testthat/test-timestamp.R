# Each assignReactions() run must get its own folder, so a rerun can never
# overwrite the metadata that earlier student files were generated from.

test_that("two runs in quick succession get separate folders", {
	proj <- initialiseProject(
		path = tempdir(),
		name = basename(tempfile("proj")),
		verbose = FALSE
	)
	
	run1 <- assignReactions(project_path = proj, seed = 1, verbose = FALSE)
	run2 <- assignReactions(project_path = proj, seed = 2, verbose = FALSE)
	
	expect_false(run1$timestamp == run2$timestamp)
})

test_that("a second run does not overwrite the first run's metadata", {
	proj <- initialiseProject(
		path = tempdir(),
		name = basename(tempfile("proj")),
		verbose = FALSE
	)
	
	run1 <- assignReactions(project_path = proj, seed = 1, verbose = FALSE)
	run2 <- assignReactions(project_path = proj, seed = 2, verbose = FALSE)
	
	saved <- readxl::read_xlsx(
		file.path(
			proj,
			"output",
			"assignments_output",
			run1$timestamp,
			"setup_metadata.xlsx"
		)
	)
	
	expect_equal(unique(saved$seed), 1)
})
