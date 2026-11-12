# Bundled parameter table: inhibited rows should apply the intended factors
# to the uninhibited Km and Vmax for the same substrate

params <- readxl::read_xlsx(
	system.file(
		"extdata",
		"reaction_parameters.xlsx",
		package = "maudr"
	)
)

reference <- params |> 
	dplyr::filter(inhibition_actual == "no_inhibition") |> 
	dplyr::select(rxn_substrate, Km_ref = Km, Vmax_ref = Vmax)

comparison <- params |>
	dplyr::filter(inhibition_actual != "no_inhibition") |>
	dplyr::left_join(
		reference,
		by = "rxn_substrate"
	) |>
	dplyr::mutate(
		km_ratio = Km / Km_ref,
		vmax_ratio = Vmax / Vmax_ref
	)

# ---- Competitive: Km x 1.49, Vmax unchanged --------------------------------
test_that("competitive inhibition raises Km and leaves Vmax unchanged", {
	competitive <- comparison |>
		dplyr::filter(inhibition_actual == "competitive")
	
	km_ratio <- setNames(
		competitive$km_ratio,
		competitive$rxn_substrate
	)
	vmax_ratio <- setNames(
		competitive$vmax_ratio,
		competitive$rxn_substrate
	)
	
	expect_equal(
		km_ratio,
		setNames(rep(1.49, nrow(competitive)), competitive$rxn_substrate)
	)
	expect_equal(
		vmax_ratio,
		setNames(rep(1, nrow(competitive)), competitive$rxn_substrate)
	)
})

# ---- Uncompetitive: Km and Vmax both x 0.6 ---------------------------------
test_that("uncompetitive inhibition lowers Km and Vmax by the same factor", {
	uncompetitive <- comparison |>
		dplyr::filter(inhibition_actual == "uncompetitive")
	
	km_ratio <- setNames(
		uncompetitive$km_ratio,
		uncompetitive$rxn_substrate
	)
	vmax_ratio <- setNames(
		uncompetitive$vmax_ratio,
		uncompetitive$rxn_substrate
	)
	
	expect_equal(
		km_ratio,
		setNames(rep(0.6, nrow(uncompetitive)), uncompetitive$rxn_substrate)
	)
	expect_equal(
		vmax_ratio,
		setNames(rep(0.6, nrow(uncompetitive)), uncompetitive$rxn_substrate)
	)
})

# ---- Noncompetitive: Km unchanged, Vmax x 0.6 ------------------------------
test_that("noncompetitive inhibition lowers Vmax and leaves Km unchanged", {
	noncompetitive <- comparison |>
		dplyr::filter(inhibition_actual == "noncompetitive")
	
	km_ratio <- setNames(
		noncompetitive$km_ratio,
		noncompetitive$rxn_substrate
	)
	vmax_ratio <- setNames(
		noncompetitive$vmax_ratio,
		noncompetitive$rxn_substrate
	)
	
	expect_equal(
		km_ratio,
		setNames(rep(1, nrow(noncompetitive)), noncompetitive$rxn_substrate)
	)
	expect_equal(
		vmax_ratio,
		setNames(rep(0.6, nrow(noncompetitive)), noncompetitive$rxn_substrate)
	)
})