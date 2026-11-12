# Runs without a seed should differ (so a class list doesn't get the same data
# every year), but any run must be repeatable from the seed it saved.

test_that("runs without a seed get different seeds", {
	proj <- initialiseProject(
		path = tempdir(),
		name = basename(tempfile("proj")),
		verbose = FALSE
	)
	
	run1 <- assignReactions(project_path = proj, verbose = FALSE)
	run2 <- assignReactions(project_path = proj, verbose = FALSE)
	
	expect_false(unique(run1$metadata$seed) == unique(run2$metadata$seed))
})

test_that("a run can be repeated from its saved seed", {
	proj <- initialiseProject(
		path = tempdir(),
		name = basename(tempfile("proj")),
		verbose = FALSE
	)
	
	run1 <- assignReactions(project_path = proj, verbose = FALSE)
	run2 <- assignReactions(
		project_path = proj,
		seed = unique(run1$metadata$seed),
		verbose = FALSE
	)
	
	expect_equal(run1$metadata$rxn_substrate, run2$metadata$rxn_substrate)
	expect_equal(run1$metadata$inhibition_actual, run2$metadata$inhibition_actual)
})