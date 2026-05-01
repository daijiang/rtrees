context("Bug fix regression tests")

# Small user-provided trees used across multiple tests below.
# rosaceae_tree: ultrametric, 5 tips, 2 genera (Rosa, Rubus)
rosaceae_tree = ape::read.tree(text = paste0(
  "(((Rosa_setigera:3.4,Rosa_arkansana:3.4):2.0,",
  "Rosa_acicularis:5.4):5.0,",
  "(Rubus_odoratus:4.0,Rubus_parviflorus:4.0):6.4);"
))
rosaceae_tree$node.label = paste0("N", seq_len(ape::Nnode(rosaceae_tree)))

# tiny_tree: 3 tips across 2 families; Sabiaceae has only 1 member
tiny_tree = ape::read.tree(text = paste0(
  "((Rosa_setigera:3.4,Rosa_arkansana:3.4):5.0,",
  "Meliosma_oldenburgii:8.4);"
))
tiny_tree$node.label = paste0("N", seq_len(ape::Nnode(tiny_tree)))

# -----------------------------------------------------------------------
# Bug 2 (lines 186, 206, 208): close_sp for a genus not yet in the tree
#   Before: isTRUE() missing at line 186 caused logical(0) crash; root_time
#   stored branch.length instead of node age; idx_row not updated after
#   tibble::add_row() so n_spp stayed at 1 for subsequent congenerics.
# -----------------------------------------------------------------------

test_that("close_sp for new genus (at_basal_node) does not crash and produces valid tree", {
  sp = tibble::tibble(
    species     = c("Rosa_setigera", "Rosa_arkansana", "Rosa_acicularis",
                    "Rubus_odoratus", "Rubus_parviflorus", "Novus_sp1"),
    genus       = c("Rosa", "Rosa", "Rosa", "Rubus", "Rubus", "Novus"),
    family      = rep("Rosaceae", 6),
    close_sp    = c(NA, NA, NA, NA, NA, "Rosa_acicularis"),
    close_genus = rep(NA_character_, 6)
  )
  tt = suppressMessages(suppressWarnings(
    get_tree(sp, tree = rosaceae_tree, taxon = "plant",
             scenario = "at_basal_node", tree_by_user = TRUE)
  ))
  expect_s3_class(tt, "phylo")
  expect_equal(ape::Ntip(tt), nrow(sp))
  expect_false(any(tt$edge.length < -1e-8))
  expect_false(any(is.nan(tt$edge.length) | is.na(tt$edge.length)))
})

test_that("close_sp for new genus + random_below_basal (3 congenerics) produces valid tree", {
  # Novus_sp1 introduced via close_sp; Novus_sp2 and Novus_sp3 are then
  # grafted normally and exercise the root_time lookup on line 232.
  sp = tibble::tibble(
    species     = c("Rosa_setigera", "Rosa_arkansana", "Rosa_acicularis",
                    "Rubus_odoratus", "Rubus_parviflorus",
                    "Novus_sp1", "Novus_sp2", "Novus_sp3"),
    genus       = c("Rosa", "Rosa", "Rosa", "Rubus", "Rubus",
                    "Novus", "Novus", "Novus"),
    family      = rep("Rosaceae", 8),
    close_sp    = c(NA, NA, NA, NA, NA, "Rosa_acicularis", NA, NA),
    close_genus = rep(NA_character_, 8)
  )
  set.seed(1)
  tt = suppressMessages(suppressWarnings(
    get_tree(sp, tree = rosaceae_tree, taxon = "plant",
             scenario = "random_below_basal", tree_by_user = TRUE)
  ))
  expect_s3_class(tt, "phylo")
  expect_equal(ape::Ntip(tt), nrow(sp))
  expect_false(any(tt$edge.length < -1e-8))
  expect_false(any(is.nan(tt$edge.length) | is.na(tt$edge.length)))
  expect_true(ape::is.ultrametric(tt, tol = 1e-4))
})

# -----------------------------------------------------------------------
# Bug 3 (lines 260-261): when a family has exactly 1 tip and a new genus is
#   grafted there, root_node and root_time were set equal to basal_node and
#   basal_time. On random_below_basal for a 3rd congeneric, the difference
#   root_time - basal_time was 0, giving sum(bls) == 0 and division by zero.
# -----------------------------------------------------------------------

test_that("family with 1 member + new genus + random_below_basal does not divide by zero", {
  sp = tibble::tibble(
    species = c("Rosa_setigera", "Rosa_arkansana",
                "Meliosma_oldenburgii",
                "Sabia_sp1", "Sabia_sp2", "Sabia_sp3"),
    genus   = c("Rosa", "Rosa", "Meliosma", "Sabia", "Sabia", "Sabia"),
    family  = c("Rosaceae", "Rosaceae", "Sabiaceae",
                "Sabiaceae", "Sabiaceae", "Sabiaceae")
  )
  set.seed(2)
  tt = suppressMessages(suppressWarnings(
    get_tree(sp, tree = tiny_tree, taxon = "plant",
             scenario = "random_below_basal", tree_by_user = TRUE)
  ))
  expect_s3_class(tt, "phylo")
  expect_equal(ape::Ntip(tt), nrow(sp))
  expect_false(any(tt$edge.length < -1e-8))
  expect_false(any(is.nan(tt$edge.length) | is.na(tt$edge.length)))
  expect_true(ape::is.ultrametric(tt, tol = 1e-4))
})

test_that("family with 1 member + new genus + at_basal_node still works", {
  sp = tibble::tibble(
    species = c("Rosa_setigera", "Rosa_arkansana",
                "Meliosma_oldenburgii",
                "Sabia_sp1", "Sabia_sp2"),
    genus   = c("Rosa", "Rosa", "Meliosma", "Sabia", "Sabia"),
    family  = c("Rosaceae", "Rosaceae", "Sabiaceae", "Sabiaceae", "Sabiaceae")
  )
  tt = suppressMessages(suppressWarnings(
    get_tree(sp, tree = tiny_tree, taxon = "plant",
             scenario = "at_basal_node", tree_by_user = TRUE)
  ))
  expect_s3_class(tt, "phylo")
  expect_equal(ape::Ntip(tt), nrow(sp))
  expect_false(any(tt$edge.length < -1e-8))
})

# -----------------------------------------------------------------------
# Bug 5 (line 360): the ultrametric correction `branch.length - ages_diff`
#   could produce negative branch lengths when a tip is farther from the
#   root than the median. The fix wraps the correction in pmax(0, ...).
# -----------------------------------------------------------------------

test_that("ultrametric correction never produces negative branch lengths", {
  sp = tibble::tibble(
    species = c("Carya_alba", "Carya_cordiformis", "Meliosma_laui",
                "Acer_cordatum", "Fraxinus_mandshurica", "Ormosia_pinnata"),
    genus   = c("Carya", "Carya", "Meliosma", "Acer", "Fraxinus", "Ormosia"),
    family  = c("Juglandaceae", "Juglandaceae", "Sabiaceae",
                "Sapindaceae", "Oleaceae", "Fabaceae")
  )
  tt = suppressMessages(get_tree(sp, taxon = "plant", scenario = "at_basal_node"))
  expect_false(any(tt$edge.length < -1e-8))

  set.seed(42)
  tt_rbb = suppressMessages(get_tree(sp, taxon = "plant", scenario = "random_below_basal"))
  expect_false(any(tt_rbb$edge.length < -1e-8))
  expect_true(ape::is.ultrametric(tt_rbb, tol = 1e-4))
})

test_that("random_below_basal output is ultrametric when megatree is ultrametric", {
  set.seed(7)
  tt = suppressMessages(
    get_tree(test_plant_list, taxon = "plant", scenario = "random_below_basal")
  )
  expect_s3_class(tt, "phylo")
  expect_false(any(tt$edge.length < -1e-8))
  expect_true(ape::is.ultrametric(tt, tol = 1e-4))
})

# -----------------------------------------------------------------------
# graft_status: returned tibble must have correct columns and cover all
# species regardless of grafting outcome.
# -----------------------------------------------------------------------

test_that("get_graft_status returns correct structure", {
  tt = suppressMessages(
    get_tree(test_plant_list, taxon = "plant", scenario = "at_basal_node")
  )
  gs = get_graft_status(tt)
  expect_s3_class(gs, "data.frame")
  expect_true(all(c("tip_label", "species", "status") %in% names(gs)))
  # all requested species must appear in graft_status
  expect_true(all(test_plant_list$species %in% gs$species))
  # status values must be from the known set
  valid_status = c("grafted at genus level", "grafted at family level",
                   "exisiting species in the megatree",
                   "skipped as no co-family in the megatree")
  expect_true(all(gs$status %in% valid_status))
})
