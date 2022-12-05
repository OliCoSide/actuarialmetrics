#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
get_lr_table <- function(pred_table,
                         ref_name,
                         comp_name,
                         expo_name = "Exposure",
                         loss_name = "ClaimNbResp",
                         n_cuts = 10,
                         balance = TRUE){
  
  tbl <- data.frame("P_ref" = pred_table[[ref_name]],
                    "P_comp" = pred_table[[comp_name]],
                    "expo" = pred_table[[expo_name]],
                    "loss" = pred_table[[loss_name]])
  
  if(balance){
    tbl$P_ref <- tbl$P_ref * (mean(tbl$loss)/mean(tbl$P_ref))
    tbl$P_comp <- tbl$P_comp * (mean(tbl$loss)/mean(tbl$P_comp))
  }
  
  ## 2. On calcule les relativites
  tbl$rr <- tbl$P_comp/tbl$P_ref
  
  ## Si on a pas assez de diversite
  is_not_enough_values_of_rel <- (tbl$rr %>% round(5) %>% table %>% length)/n_cuts <= 1
  if(is_not_enough_values_of_rel){
    tbl$rr2 <- tbl$rr %>% round(4)
    
    ## 3. Trier les individus
    sort_id <- sort.int(tbl$rr2, index.return = TRUE)$ix
    
    ## trier la base de donnee
    test_sort <- tbl[sort_id, ]
    
    ## values
    values <- test_sort$rr2 %>% table %>% names %>%  as.numeric()
    
    test_sort$r_group2 <- sapply(1:nrow(test_sort), function(obs){
      which(test_sort$rr2[obs] == values)
    })
    test_sort$r_group <- values[test_sort$r_group2]
    
    ## On renomme
    n_cuts <- length(values)
    
  } else {
    ## 3. Trier les individus
    sort_id <- sort.int(tbl$rr, index.return = TRUE)$ix
    sorted_rr <- tbl$rr[sort_id]
    
    ## 4. On cree 5 groupes qui contiennent la meme exposition
    expo_cumul <- cumsum(tbl$expo[sort_id])
    cut_points <- sum(tbl$expo)/n_cuts*(0:n_cuts) # La decision d'utiliser 5 groupes est arbitraire.
    
    groups <- cut(expo_cumul, cut_points, labels = FALSE)
    # Ici, on veut simplement avoir des valeurs de 1 a 5 dependamment du groupe (quintile) d'appartenance
    
    ## We assign the labels correctly
    new_groups <- put_labels(groups, sorted_rr)
    
    ## 5. On calcule le Loss Ratio par groupe 
    test_sort <- tbl[sort_id, ]
    test_sort$r_group <- new_groups
    test_sort$r_group2 <- groups 
  }
  
  LossRatio_table <- aggregate(cbind(P_ref, P_comp, loss, expo) ~ r_group + r_group2, data = test_sort, FUN = sum)
  LossRatio_table$LR_ref <- LossRatio_table$loss/LossRatio_table$P_ref
  LossRatio_table$LR_comp <- LossRatio_table$loss/LossRatio_table$P_comp
  
  l <- list("table" = LossRatio_table,
            "balance" = balance,
            "ref_name" = ref_name,
            "comp_name" = comp_name)
  return(l)
}