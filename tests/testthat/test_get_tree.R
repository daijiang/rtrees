context("Test different trees")

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
  expect_message(tt <- get_tree(sp_list = test_fish_list, taxon = "fish", scenario = "S1"))
  expect_s3_class(tt, "phylo")
  expect_equal(ape::Ntip(tt), nrow(test_plant_list) - 1)
})

test_that("Test get tree for bird", {
  tt = get_tree(test_bird_list, taxon = "bird", show_grafted = T)
  expect_equal(ape::Ntip(tt), nrow(test_bird_list))
})

test_that("Test get tree for mammal", {
  tt = get_tree(test_mammal_list, taxon = "mammal", show_grafted = T)
  expect_equal(ape::Ntip(tt), nrow(test_mammal_list))
  # plot(ape::ladderize(tt))
})

test_that("Test user provided tree", {
  test_tree_sp2 = test_tree_sp[test_tree_sp != "Sorbus_sp"]
  expect_error(get_tree(sp_list = test_tree_sp, tree = test_tree, tree_by_user = T))
  tt = get_tree(sp_list = test_tree_sp, tree = test_tree, taxon = "plant", tree_by_user = T) # this should work
  tt = get_tree(sp_list = test_tree_sp2, tree = test_tree, tree_by_user = T) # this should work
  expect_error(get_tree(sp_list = test_tree_sp, tree = test_tree, tree_by_user = T))
  
  tt = get_tree(sp_list = test_tree_sp_df, tree = test_tree, taxon = "plant", show_grafted = T, tree_by_user = T)
})