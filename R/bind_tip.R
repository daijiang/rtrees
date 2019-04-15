#' Bind a tip to a phylogeny
#'
#' Insert a tip to a phylogeny at location specified.
#'
#' @param tree A phylogeny, with class of "phylo".
#' @param where Location where to insert the tip. It can be either tip label or node label, but must be characters. If the location does not have a name, assign it first.
#' @param tip_label Name of the new tip inserted.
#' @param frac The fraction of branch length, must be between 0 and 1. This only applies when location is a tip or \code{new_node_above = TRUE}. The distance from the new inserted node to the location (a node or a tip) is the branch length of the location * (1 - frac).
#' @param new_node_above Whether to insert the new node above when the location is a node? Default is \code{FALSE}, which will attach the new tip to the location node.
#' @param node_label Name of the new node created. This only applies when location is a tip or \code{new_node_above = TRUE}.
#' @param return_tree Whether to return a phylogeny with class "phylo?" Default is \code{TRUE}. Otherwise, it will return a data frame.
#' @return Either a phylogeny or a data frame, which can be then converted to a phylogeny later.

bind_tip = function(tree = test_tree, where = "N2", tip_label = "test_sp",
                    frac = 2/3, new_node_above = FALSE,
                    node_label = "", return_tree = TRUE){
  if(frac > 1 | frac < 0) stop("frac must be between 0 and 1.")
  tree = ape::makeLabel(tree, tips = FALSE)
  phy_df = tidytree::as_tibble(tree) %>%
     dplyr::mutate(isTip = (! node %in% parent))
  phy_df_node = phy_df[which(phy_df$label == where),] # original node
  if(!phy_df_node$isTip){
    where_offspring = tidytree::offspring(phy_df, .node = where)
    max_offspring = max(where_offspring$node[where_offspring$isTip])
    node_height = ape::branching.times(tree)[where]
  }
  node_orig = phy_df_node$node # original node number of target location
  phy_df_2 = phy_df # to hold results

  if(!phy_df_node$isTip) { # the target is a node
      if(new_node_above){ # insert above the target location node
        if(phy_df_node$parent == phy_df_node$node)
          stop("Cannot add a new node above the root.")
        # add node first, push the numbers of nodes if they are after the inserted one
        phy_df_2$parent[phy_df_2$parent >= node_orig] = phy_df_2$parent[phy_df_2$parent >= node_orig] + 1
        phy_df_2$node[phy_df_2$node >= node_orig] = phy_df_2$node[phy_df_2$node >= node_orig] + 1
        # insert the new node
        phy_df_2 = dplyr::bind_rows(phy_df_2,
                                    tibble::tibble(parent = node_orig - 1, node = node_orig,
                                                   branch.length = phy_df_node$branch.length * frac,
                                                   label = node_label))
        # add tip
        phy_df_2$parent = phy_df_2$parent + 1 # all nodes will be added 1
        phy_df_2$node = ifelse(phy_df_2$node > max_offspring, phy_df_2$node + 1, phy_df_2$node)
        phy_df_2 = dplyr::bind_rows(phy_df_2,
                                    tibble::tibble(parent = phy_df_2$node[phy_df_2$label == node_label],
                                                   node = max_offspring + 1,
                                                   branch.length = phy_df_node$branch.length * (1 - frac) + node_height,
                                                   label = tip_label))
        # update orginal node, which will have the inserted node as its parent
        phy_df_2$parent[phy_df_2$label == where] =
          phy_df_2$node[phy_df_2$label == node_label]
      } else { # attach to a target node
        if(phy_df_node$parent == phy_df_node$node){ # root
          phy_df_new = tibble::tibble(parent = node_orig + 1, node = 1,
                                      branch.length = node_height, label = tip_label)
          phy_df$parent = phy_df$parent + 1
          phy_df$node = phy_df$node + 1
          phy_df_2 = dplyr::bind_rows(phy_df_new, phy_df)
        } else { # an internal node
          phy_df_2 = phy_df
          phy_df_2$parent = phy_df_2$parent + 1 # all nodes will be added 1
          phy_df_2$node = ifelse(phy_df_2$node > max_offspring, phy_df_2$node + 1, phy_df_2$node)
          phy_df_2 = dplyr::bind_rows(phy_df_2,
                                      tibble::tibble(parent = phy_df_2$node[phy_df_2$label == where],
                                                     node = max_offspring + 1,
                                                     branch.length = node_height,
                                                     label = tip_label))
        }
      }
    } else { # the target is a tip
      parent_orig = phy_df_node$parent
      # add node first, push the numbers of nodes if they are after the inserted one
      phy_df_2$parent[phy_df_2$parent > parent_orig] = phy_df_2$parent[phy_df_2$parent > parent_orig] + 1
      phy_df_2$node[phy_df_2$node > parent_orig] = phy_df_2$node[phy_df_2$node > parent_orig] + 1
      # insert the new node
      phy_df_2 = dplyr::bind_rows(phy_df_2,
                                  tibble::tibble(parent = parent_orig, node = parent_orig + 1,
                                                 branch.length = phy_df_node$branch.length * frac,
                                                 label = node_label))
      # add tip
      phy_df_2$parent = phy_df_2$parent + 1 # all nodes will be added 1
      phy_df_2$node = ifelse(phy_df_2$node > node_orig, phy_df_2$node + 1, phy_df_2$node)
      phy_df_2 = dplyr::bind_rows(phy_df_2,
                                  tibble::tibble(parent = phy_df_2$node[phy_df_2$label == node_label],
                                                 node = node_orig + 1,
                                                 branch.length = phy_df_node$branch.length * (1 - frac),
                                                 label = tip_label))
      # update orginal node, which will have the inserted node as its parent
      phy_df_2$parent[phy_df_2$label == where] =
        phy_df_2$node[phy_df_2$label == node_label]
      phy_df_2$branch.length[phy_df_2$label == where] =
        phy_df_2$branch.length[phy_df_2$label == tip_label]

      # # another way
      # phy_df_above = phy_df[1:node_orig, ]
      # phy_df_below = phy_df[-(1:node_orig), ]
      # # add tip
      # phy_df_above$parent = phy_df_above$parent + 1
      # phy_df_below$parent = phy_df_below$parent + 1
      # phy_df_below$node = phy_df_below$node + 1
      # phy_df_new = tibble::tibble(parent = phy_df_above$parent[phy_df_above$label == where],
      #                             node = node_orig + 1,
      #                             branch.length = phy_df_above$branch.length[phy_df_above$label == where],
      #                             label = tip_label)
      # phy_df_2 = dplyr::bind_rows(phy_df_above, phy_df_new, phy_df_below)
      # # add node
      # loc_in_df2 = which(phy_df_2$node == phy_df_new$parent)
      # phy_df_2$parent = ifelse(phy_df_2$parent > phy_df_new$parent, phy_df_2$parent + 1, phy_df_2$parent)
      # phy_df_2$parent[phy_df_2$label %in% c(where, tip_label)] = phy_df_new$parent + 1
      # phy_df_2$node = ifelse(phy_df_2$node > phy_df_new$parent, phy_df_2$node + 1, phy_df_2$node)
      # phy_df_2 = dplyr::bind_rows(phy_df_2,
      #                             tibble::tibble(parent = phy_df_new$parent,
      #                                            node = phy_df_new$parent + 1,
      #                                            branch.length = phy_df_new$branch.length * frac,
      #                                            label = "")) %>%
      #   dplyr::arrange(node)
      # phy_df_2$branch.length[phy_df_2$label %in% c(where, tip_label)] = phy_df_new$branch.length * (1 - frac)
    }

  phy_df_2 = phy_df_2 %>%
    dplyr::arrange(node) %>%
    dplyr::select(-isTip)

  if(!inherits(phy_df_2, "tbl_tree")) class(phy_df_2) = c("tbl_tree", class(phy_df_2))

  if(return_tree) return(tidytree::as.phylo(phy_df_2))
  phy_df_2
}
