test_that("Test get tree for plants", {
  tt = get_tree(test_plant_list, taxon = "plant", scenario = "S1")
  expect_s3_class(tt, "phylo")
  expect_equal(ape::Ntip(tt), nrow(test_plant_list))
  
  # tt = get_tree(test_plant_list, taxon = "plant", scenario = "S2")
  # expect_s3_class(tt, "phylo")
  # expect_equal(ape::Ntip(tt), nrow(test_plant_list))
  # 
  # tt = get_tree(test_plant_list, taxon = "plant", scenario = "S3")
  # expect_s3_class(tt, "phylo")
  # expect_equal(ape::Ntip(tt), nrow(test_plant_list))
  
  # test bind_tip with output file above
  tt2 = bind_tip(tt, where = "Acer_cordatum", tip_label = "Test_sp")
  expect_equal(ape::Ntip(tt2), nrow(test_plant_list) + 1)
  tt3 = bind_tip(tt, where = "N37580", tip_label = "Test_sp")
  expect_equal(ape::Ntip(tt3), nrow(test_plant_list) + 1)
  tt4 = bind_tip(tt, where = "N37580", tip_label = "Test_sp", new_node_above = T)
  expect_equal(ape::Nnode(tt3), ape::Nnode(tt4) - 1)
  # message if add above root
  expect_message(bind_tip(tree = tt, where = "mrcaott2ott969", tip_label = "Test_sp", new_node_above = T))
})

test_that("Test get tree for fish", {
  expect_message(tt <- get_tree(test_fish_list, taxon = "fish", scenario = "S1"))
  expect_s3_class(tt, "phylo")
  expect_equal(ape::Ntip(tt), nrow(test_plant_list) - 1)
})