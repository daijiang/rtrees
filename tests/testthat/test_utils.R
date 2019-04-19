test_that("Convert a vector of sp to data frame", {
  expect_equal(ncol(sp_list_df(test_plant_list$species, taxon = "plant")), 3)
  expect_equal(ncol(sp_list_df(test_fish_list$species, taxon = "fish")), 3)
  
  expect_warning(sp_list_df(test_plant_list$species, taxon = "fish"))
  expect_warning(sp_list_df(test_fish_list$species, taxon = "plant"))
})

test_that("Convert the first letter to upper case", {
  expect_equal(cap_first_letter("aha"), "Aha")
  expect_equal(cap_first_letter("Aha"), "Aha")
})