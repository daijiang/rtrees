context("Core functionality")

# -----------------------------------------------------------------------
# 1. Output species set: derived tree must contain exactly the requested
#    species -- no more, no fewer.
# -----------------------------------------------------------------------

test_that("output tip labels match requested species exactly", {
  tt = suppressMessages(get_tree(test_plant_list, taxon = "plant"))
  expect_setequal(tt$tip.label, test_plant_list$species)
})

test_that("output tree is rooted", {
  tt = suppressMessages(get_tree(test_plant_list, taxon = "plant"))
  expect_true(ape::is.rooted(tt))
})

test_that("output tip labels match for user-provided tree", {
  tt = suppressMessages(
    get_tree(test_tree_sp_df, tree = test_tree, taxon = "plant", tree_by_user = TRUE)
  )
  expected = gsub("\\*", "", tt$tip.label)      # rm_stars equivalent
  expect_setequal(expected, test_tree_sp_df$species)
})

# -----------------------------------------------------------------------
# 2. graft_status content: correct levels for known in/out species.
#    From helper_splists.R:
#      in-tree:  Carya_alba, Carya_cordiformis, Stewartia_sinensis,
#                Pterocarya_hupehensis, Ulmus_davidiana, Diplospora_dubia
#      out-tree: Meliosma_laui, Acer_cordatum, Fraxinus_mandshurica,
#                Ormosia_pinnata, Aglaia_dasyclada, Claoxylon_indicum
# -----------------------------------------------------------------------

test_that("graft_status reports 'exisiting species' for species present in megatree", {
  in_tree = c("Carya_alba", "Carya_cordiformis", "Stewartia_sinensis",
              "Pterocarya_hupehensis", "Ulmus_davidiana", "Diplospora_dubia")
  tt = suppressMessages(get_tree(test_plant_list, taxon = "plant"))
  gs = get_graft_status(tt)
  in_status = gs$status[gs$species %in% in_tree]
  expect_true(all(in_status == "exisiting species in the megatree"))
})

test_that("graft_status reports grafted status for species absent from megatree", {
  out_tree = c("Meliosma_laui", "Acer_cordatum", "Fraxinus_mandshurica",
               "Ormosia_pinnata", "Aglaia_dasyclada", "Claoxylon_indicum")
  tt = suppressMessages(get_tree(test_plant_list, taxon = "plant"))
  gs = get_graft_status(tt)
  out_status = gs$status[gs$species %in% out_tree]
  expect_true(all(out_status %in% c("grafted at genus level", "grafted at family level")))
})

test_that("graft_status covers every requested species", {
  tt = suppressMessages(get_tree(test_plant_list, taxon = "plant"))
  gs = get_graft_status(tt)
  expect_true(all(test_plant_list$species %in% gs$species))
})

# -----------------------------------------------------------------------
# 3. show_grafted and rm_stars
# -----------------------------------------------------------------------

test_that("show_grafted = TRUE adds asterisks to grafted tips only", {
  in_tree = c("Carya_alba", "Carya_cordiformis", "Stewartia_sinensis",
              "Pterocarya_hupehensis", "Ulmus_davidiana", "Diplospora_dubia")
  tt = suppressMessages(get_tree(test_plant_list, taxon = "plant", show_grafted = TRUE))

  # species in the megatree must have no asterisk suffix
  in_tips = tt$tip.label[gsub("\\*+$", "", tt$tip.label) %in% in_tree]
  expect_false(any(grepl("\\*$", in_tips)))

  # grafted species must carry one or two asterisks
  expect_true(any(grepl("\\*$", tt$tip.label)))
})

test_that("show_grafted = FALSE (default) produces no asterisks in tip labels", {
  tt = suppressMessages(get_tree(test_plant_list, taxon = "plant", show_grafted = FALSE))
  expect_false(any(grepl("\\*", tt$tip.label)))
})

test_that("rm_stars removes all trailing asterisks and preserves tip count", {
  tt       = suppressMessages(get_tree(test_plant_list, taxon = "plant", show_grafted = TRUE))
  tt_clean = rm_stars(tt)
  expect_false(any(grepl("\\*", tt_clean$tip.label)))
  expect_equal(ape::Ntip(tt_clean), ape::Ntip(tt))
  expect_setequal(tt_clean$tip.label, test_plant_list$species)
})

# -----------------------------------------------------------------------
# 4. add_root_info: structure and internal consistency
# -----------------------------------------------------------------------

test_that("add_root_info attaches genus_family_root with required columns", {
  cls = unique(rtrees::classifications[rtrees::classifications$taxon == "plant",
                                       c("genus", "family")])
  tt  = add_root_info(test_tree, classification = cls)
  gfr = tt$genus_family_root

  expect_s3_class(gfr, "data.frame")
  expect_true(all(c("family", "genus", "basal_node", "basal_time",
                    "root_node", "root_time", "n_genus", "n_spp", "only_sp") %in% names(gfr)))
})

test_that("add_root_info: all node labels in genus_family_root exist in the tree", {
  cls = unique(rtrees::classifications[rtrees::classifications$taxon == "plant",
                                       c("genus", "family")])
  tt  = add_root_info(test_tree, classification = cls)
  gfr = tt$genus_family_root

  tree_labels = c(tt$tip.label, tt$node.label)
  expect_true(all(gfr$basal_node %in% tree_labels))
  expect_true(all(gfr$root_node  %in% tree_labels))
})

test_that("add_root_info: root_time >= basal_time for every row", {
  cls = unique(rtrees::classifications[rtrees::classifications$taxon == "plant",
                                       c("genus", "family")])
  tt  = add_root_info(test_tree, classification = cls)
  gfr = tt$genus_family_root
  # invariant required by random_below_basal branch-length weighting
  expect_true(all(gfr$root_time >= gfr$basal_time - 1e-8))
})

test_that("add_root_info: n_spp matches observed tip counts per genus", {
  cls = unique(rtrees::classifications[rtrees::classifications$taxon == "plant",
                                       c("genus", "family")])
  tt  = add_root_info(test_tree, classification = cls)
  gfr = tt$genus_family_root

  genus_rows = gfr[!is.na(gfr$genus), ]
  for (g in genus_rows$genus) {
    actual   = sum(grepl(paste0("^", g, "_"), tt$tip.label))
    recorded = genus_rows$n_spp[genus_rows$genus == g]
    expect_equal(actual, recorded, info = paste("n_spp mismatch for genus", g))
  }
})

# -----------------------------------------------------------------------
# 5. bind_tip: branch length correctness for different frac values.
#    Uses a small tree with known branch lengths so expectations are exact.
#    Tree:  ((A:10,B:10):5,C:15);  → root age 15, internal node age 10
# -----------------------------------------------------------------------

known_tree = ape::read.tree(text = "((A:10,B:10):5,C:15);")
known_tree$node.label = paste0("N", seq_len(ape::Nnode(known_tree)))

test_that("bind_tip to a tip with frac = 0.5 splits branch correctly", {
  # A has branch.length = 10; new node inserted at height 5 (= 10 * 0.5)
  # both children of new node should have branch.length = 5
  tt2     = bind_tip(known_tree, where = "A", tip_label = "New_sp", frac = 0.5)
  tt2_tbl = tidytree::as_tibble(tt2)

  target_bl  = tt2_tbl$branch.length[tt2_tbl$label == "A"]
  new_tip_bl = tt2_tbl$branch.length[tt2_tbl$label == "New_sp"]

  expect_equal(target_bl,  5.0, tolerance = 1e-8)
  expect_equal(new_tip_bl, 5.0, tolerance = 1e-8)
})

test_that("bind_tip to a tip with frac = 0.25 puts new node closer to parent", {
  # A has branch.length = 10; frac = 0.25 means new node 2.5 below parent
  # both children get branch.length = 10 * 0.75 = 7.5
  tt2     = bind_tip(known_tree, where = "A", tip_label = "New_sp", frac = 0.25)
  tt2_tbl = tidytree::as_tibble(tt2)

  target_bl  = tt2_tbl$branch.length[tt2_tbl$label == "A"]
  new_tip_bl = tt2_tbl$branch.length[tt2_tbl$label == "New_sp"]

  expect_equal(target_bl,  7.5, tolerance = 1e-8)
  expect_equal(new_tip_bl, 7.5, tolerance = 1e-8)
})

test_that("bind_tip to an internal node attaches tip with branch length = node age", {
  # internal node N1 is at height 10; new tip attached to it should have bl = 10
  node_lbl = "N1"
  node_hts = ape::branching.times(known_tree)

  tt2     = bind_tip(known_tree, where = node_lbl, tip_label = "New_sp")
  tt2_tbl = tidytree::as_tibble(tt2)

  new_tip_bl = tt2_tbl$branch.length[tt2_tbl$label == "New_sp"]
  expect_equal(new_tip_bl, unname(node_hts[node_lbl]), tolerance = 1e-8)
})

# -----------------------------------------------------------------------
# 6. sp_list_df: content correctness
# -----------------------------------------------------------------------

test_that("sp_list_df extracts genus correctly from species name", {
  df = sp_list_df(c("Quercus_robur", "Pinus_sylvestris", "Betula_pendula"))
  expect_equal(df$genus,   c("Quercus", "Pinus", "Betula"))
  expect_equal(df$species, c("Quercus_robur", "Pinus_sylvestris", "Betula_pendula"))
})

test_that("sp_list_df normalises spaces to underscores and capitalises first letter", {
  df = sp_list_df(c("quercus robur", "pinus sylvestris"))
  expect_equal(df$species, c("Quercus_robur", "Pinus_sylvestris"))
  expect_equal(df$genus,   c("Quercus", "Pinus"))
})

test_that("sp_list_df handles phylomatic format (family/genus/species)", {
  phy = c("juglandaceae/Carya/Carya_alba",
          "sapindaceae/Acer/Acer_cordatum",
          "oleaceae/Fraxinus/Fraxinus_mandshurica")
  df = sp_list_df(phy)
  expect_equal(nrow(df), 3)
  expect_true("family" %in% names(df))
  expect_equal(df$species, c("Carya_alba", "Acer_cordatum", "Fraxinus_mandshurica"))
  expect_equal(df$genus,   c("Carya", "Acer", "Fraxinus"))
  expect_equal(tolower(df$family), c("juglandaceae", "sapindaceae", "oleaceae"))
})

test_that("sp_list_df de-duplicates repeated species names", {
  df = sp_list_df(c("Quercus_robur", "Quercus_robur", "Pinus_sylvestris"))
  expect_equal(nrow(df), 2)
})

test_that("sp_list_df maps genus to family correctly for plants", {
  df = sp_list_df(c("Carya_alba", "Quercus_robur"), taxon = "plant")
  expect_true("family" %in% names(df))
  expect_equal(df$family[df$genus == "Carya"],  "Juglandaceae")
  expect_equal(df$family[df$genus == "Quercus"], "Fagaceae")
})

# -----------------------------------------------------------------------
# 7. multiPhylo output: all trees valid with identical tip sets
# -----------------------------------------------------------------------

test_that("bird multiPhylo output has correct length and all trees are phylo", {
  tt = suppressMessages(get_tree(test_bird_list, taxon = "bird"))
  expect_true(inherits(tt, c("multiPhylo", "list")))
  expect_true(all(sapply(tt, inherits, "phylo")))
})

test_that("all trees in bird multiPhylo have correct tip count", {
  tt = suppressMessages(get_tree(test_bird_list, taxon = "bird"))
  expect_true(all(sapply(tt, ape::Ntip) == nrow(test_bird_list)))
})

test_that("all trees in bird multiPhylo share the same tip label set", {
  tt    = suppressMessages(get_tree(test_bird_list, taxon = "bird"))
  tips1 = sort(tt[[1]]$tip.label)
  same  = sapply(tt, function(x) identical(sort(x$tip.label), tips1))
  expect_true(all(same))
})

test_that("get_graft_status works on individual trees from multiPhylo output", {
  tt = suppressMessages(get_tree(test_bird_list, taxon = "bird", show_grafted = TRUE))
  gs = get_graft_status(tt[[1]])
  expect_s3_class(gs, "data.frame")
  expect_true(all(c("tip_label", "species", "status") %in% names(gs)))
  expect_true(all(test_bird_list$species %in% gs$species))
})
