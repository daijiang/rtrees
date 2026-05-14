# Extracted from test_invariants.R:118

# prequel ----------------------------------------------------------------------
context("Algorithmic invariants")
inv_base <- ape::read.tree(text = paste0(
  "(((Rosa_setigera:3.4,Rosa_arkansana:3.4):2.0,",
  "Rosa_acicularis:5.4):5.0,",
  "(Rubus_odoratus:4.0,Rubus_parviflorus:4.0):6.4);"
))
inv_base$node.label <- paste0("N", seq_len(ape::Nnode(inv_base)))
inv_base_coph <- ape::cophenetic.phylo(inv_base)
inv_tiny <- ape::read.tree(text = paste0(
  "((Rosa_setigera:3.4,Rosa_arkansana:3.4):5.0,",
  "Meliosma_oldenburgii:8.4);"
))
inv_tiny$node.label <- paste0("N", seq_len(ape::Nnode(inv_tiny)))
is_pure_clade <- function(tree, tips) {
  if (length(tips) < 2L) return(NA)
  mrca <- ape::getMRCA(tree, tips)
  if (is.null(mrca)) return(FALSE)
  if (mrca == ape::Ntip(tree) + 1L) return(setequal(tree$tip.label, tips))
  setequal(ape::extract.clade(tree, node = mrca)$tip.label, tips)
}

# test -------------------------------------------------------------------------
sp <- tibble::tibble(
    species = c("Rosa_setigera", "Rosa_arkansana", "Rubus_odoratus",
                "Novus_sp1", "Novus_sp2", "Novus_sp3"),
    genus   = c("Rosa", "Rosa", "Rubus", "Novus", "Novus", "Novus"),
    family  = rep("Rosaceae", 6)
  )
tt <- suppressMessages(suppressWarnings(
    get_tree(sp, tree = inv_base, taxon = "plant",
             scenario = "at_basal_node", tree_by_user = TRUE)
  ))
novus_tips <- grep("^Novus_", tt$tip.label, value = TRUE)
expect_length(novus_tips, 3)
expect_true(is_pure_clade(tt, novus_tips))
