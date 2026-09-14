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

# -----------------------------------------------------------------------
# Bug: a genus listed under two families in the classification database
#   `sp_list_df()` left-joins by genus, so such a genus returned two rows per
#   species. get_one_tree() then grafted the species once per row, which
#   produced duplicated tips when both families were in the mega-tree,
#   reported the species as having no co-family species while it had in fact
#   been grafted, and made the trailing-* join return more rows than tips,
#   silently shifting the labels of every grafted tip after the first
#   duplicate onto the wrong branch.
# -----------------------------------------------------------------------

test_that("classifications has exactly one family per genus in every taxon", {
  expect_false(any(duplicated(rtrees::classifications[, c("taxon", "genus")])))
})

test_that("sp_list_df returns one row per species", {
  sp = c("Muscari_armeniacum", "Phacelia_purshii", "Salsola_tragus",
         "Viburnum_rafinesqueanum", "Prosartes_hookeri", "Thesium_alpinum",
         "Senegalia_greggii", "Buchnera_americana")
  out = sp_list_df(sp, taxon = "plant")
  expect_equal(nrow(out), length(sp))
  expect_false(any(duplicated(out$species)))
})

# Novus has no congener in the tree, so it is grafted at family level, and both
# of its candidate families are represented. Before the fix it was grafted once
# per family and came out with two tips. Muscari/Asparagaceae+Hyacinthaceae was
# the real-world instance; this is the same shape on a tree we control.
dup_family_sp = tibble::tibble(
  species = c("Rosa_setigera", "Rosa_arkansana", "Rosa_acicularis",
              "Rubus_odoratus", "Rubus_parviflorus", "Novus_sp1", "Novus_sp1"),
  genus   = c("Rosa", "Rosa", "Rosa", "Rubus", "Rubus", "Novus", "Novus"),
  family  = c(rep("Rosaceae", 3), rep("Rubaceae", 2), "Rosaceae", "Rubaceae")
)

test_that("a species with two candidate families is grafted exactly once", {
  tt = suppressMessages(suppressWarnings(
    get_tree(dup_family_sp, tree = rosaceae_tree, taxon = "plant",
             show_grafted = TRUE, tree_by_user = TRUE)
  ))
  labs = rm_stars(tt)$tip.label
  expect_false(any(duplicated(labs)))
  expect_equal(sum(labs == "Novus_sp1"), 1)
  expect_setequal(labs, unique(dup_family_sp$species))
  expect_equal(ape::Ntip(tt), length(unique(dup_family_sp$species)))
})

test_that("graft_status agrees with the tip labels and covers every species", {
  tt = suppressMessages(suppressWarnings(
    get_tree(dup_family_sp, tree = rosaceae_tree, taxon = "plant",
             show_grafted = TRUE, tree_by_user = TRUE)
  ))
  gs = tt$graft_status
  expect_setequal(gs$species, unique(dup_family_sp$species))
  expect_false(any(duplicated(gs$species)))
  # every tip the tree marks as grafted is recorded as grafted, and vice versa
  expect_equal(sum(grepl("\\*$", tt$tip.label)),
               sum(gs$status != "exisiting species in the megatree"))
  expect_equal(gs$status[gs$species == "Novus_sp1"], "grafted at family level")
})

test_that("a grafted tip keeps its own label instead of a neighbour's", {
  # the trailing-* step used a join that could return more rows than tips,
  # which shifted every later grafted label onto the wrong branch
  sp = dplyr::bind_rows(
    dup_family_sp,
    tibble::tibble(species = "Rosa_novum", genus = "Rosa", family = "Rosaceae")
  )
  tt = suppressMessages(suppressWarnings(
    get_tree(sp, tree = rosaceae_tree, taxon = "plant",
             show_grafted = TRUE, tree_by_user = TRUE)
  ))
  labs = rm_stars(tt)$tip.label
  expect_setequal(labs, unique(sp$species))
  # the grafted species sits with its own congeners, not with the species that
  # happens to precede it in the grafting order
  i = match("Rosa_novum", labs)
  par = tt$edge[match(i, tt$edge[, 2]), 1]
  sis = setdiff(labs[intersect(tt$edge[tt$edge[, 1] == par, 2], seq_along(labs))],
                "Rosa_novum")
  expect_true(any(grepl("^Rosa_", sis)))
})

# -----------------------------------------------------------------------
# Bug: `%fin%` (fastmatch) compares the string encoding flag, base match()
#   does not. A species name holding a non-ASCII character therefore failed
#   to match a byte-identical tip label that was not flagged UTF-8, so the
#   trailing * was never appended and graft_status reported the species as
#   already present in the mega-tree.
# -----------------------------------------------------------------------

test_that("species names with non-ASCII characters are marked as grafted", {
  hybrid = "Rosa_×_hybrida" # UTF-8 flagged, as read from an .rds
  sp = tibble::tibble(
    species = c("Rosa_setigera", "Rosa_arkansana", "Rubus_odoratus", hybrid),
    genus   = c("Rosa", "Rosa", "Rubus", "Rosa"),
    family  = rep("Rosaceae", 4)
  )
  tt = suppressMessages(suppressWarnings(
    get_tree(sp, tree = rosaceae_tree, taxon = "plant",
             show_grafted = TRUE, tree_by_user = TRUE)
  ))
  expect_true(paste0(hybrid, "*") %in% tt$tip.label)
  gs = tt$graft_status
  expect_equal(gs$status[gs$species == hybrid], "grafted at genus level")
})
