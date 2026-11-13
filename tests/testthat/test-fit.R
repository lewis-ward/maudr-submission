# fit_mm() produces the Km and Vmax shown on every answer sheet.

test_that("fit_mm recovers Km and Vmax from noise-free data", {
	s <- c(0, 10, 20, 40, 80, 160)
	df <- data.frame(
		substrate_conc = s,
		rate = 1.41 * s / (17 + s)
	)
	
	est <- fit_mm(df)
	
	expect_equal(est$Km, 17, tolerance = 1e-3)
	expect_equal(est$Vmax, 1.41, tolerance = 1e-3)
})

test_that("fit_mm never returns a negative Km or Vmax", {
	s <- c(0, 10, 20, 40, 80, 160)
	set.seed(1)
	
	est <- purrr::map_df(1:500, \(i) {
		df <- data.frame(
			substrate_conc = s,
			rate = 0.2 * s / (9.5 + s) + rnorm(6, 0, 0.05)
		)
		fit_mm(df)
	})
	
	expect_true(all(est$Km >= 0, na.rm = TRUE))
	expect_true(all(est$Vmax >= 0, na.rm = TRUE))
})