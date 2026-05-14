# Extracted from test_get_tree.R:24

# prequel ----------------------------------------------------------------------
context("Test different trees")

# test -------------------------------------------------------------------------
tt = get_tree(sp_list = test_plant_list, taxon = "plant", scenario = "at_basal_node")
expect_s3_class(tt, "phylo")
expect_equal(ape::Ntip(tt), nrow(test_plant_list))
tt2 = bind_tip(tree = tt, where = "Acer_cordatum", tip_label = "Test_sp")
expect_equal(ape::Ntip(tt2), nrow(test_plant_list) + 1)
tt3 = bind_tip(tt, where = "N37580", tip_label = "Test_sp")
expect_equal(ape::Ntip(tt3), nrow(test_plant_list) + 1)
tt4 = bind_tip(tt, where = "N37580", tip_label = "Test_sp", new_node_above = T)
expect_equal(ape::Nnode(tt3), ape::Nnode(tt4) - 1)
expect_message(bind_tip(tree = tt, where = "mrcaott2ott969", tip_label = "Test_sp", new_node_above = T))
