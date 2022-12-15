#' Prepare a dataset for actuarial tools
#'
#' @param pred_table A \code{dataframe} containing a column per ratemaking structure (at least 2, \code{ref_name} and \code{comp_name}), a column for exposure (\code{expo_name}, which is optionnal) and a column for the response variable \code{loss_name}.
#' @param ref_name The name of the reference ratemaking structure. It is the name of a column of \code{pred_table}.
#' @param comp_name The name of the ratemaking structure to compare. It is the name of a column of \code{pred_table}.
#' @param expo_name (\emph{optionnal}) The name of the loss exposure for each observation. It is the name of a column of \code{pred_table}. If nothing is specified, constant exposure of 1 is assumed accross all observations.
#' @param loss_name The name of the response variable for each observation. It is the name of a column of \code{pred_table}.
#' @param n_cuts The amount of groups for the data aggregation. The data will be sorted by relativity and grouped (equal exposure groups) by ascending relativity order. Default value is 10.
#' @param balance boolean indicating wether or not to balance total premiums with the total response variable. The default value is \code{TRUE} since the objective of the analysis is to assess segmentation.
#' @return A table ready to input to \emph{double lift chart} or \emph{Loss ratio lift} functions from \code{actuarialmetrics}.
#' @examples
#' library(actuarialmetrics)
#' data("pred_table")
#' table_to_g <- get_lr_table(pred_table,
#'                            ref_name = "mod2",
#'                              comp_name = "mod3",
#'                              loss_name = "Y",
#'                              n_cuts = 6)
#'
#' @export
get_lr_table <- function(pred_table,
                         ref_name,
                         comp_name,
                         expo_name = NULL,
                         loss_name = NULL,
                         n_cuts = 10,
                         balance = TRUE){

  if(is.null(loss_name)) stop("the user must specify the response variable using the 'loss_name' argument")
  if(is.null(expo_name)) warning("Since no exposure is specified, the exposure is assume to be constant accross observations")

  tbl <- data.frame("P_ref" = pred_table[[ref_name]],
                    "P_comp" = pred_table[[comp_name]],
                    "loss" = pred_table[[loss_name]])

  ## if there is a specified exposure
  if(!is.null(expo_name)){
    ## add it to the table
    tbl$expo <- pred_table[[expo_name]]
  } else {
    ## assume constant exposure
    tbl$expo <- rep(1, nrow(pred_table))
  }

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
