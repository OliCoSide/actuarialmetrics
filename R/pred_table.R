#' Table with premiums and losses to compare
#'
#' The models 1, 2 and 3 try to predict severity of losses.
#'
#' @format ## `who`
#' A data frame with 2500 rows and 4 columns:
#' \describe{
#'   \item{mod1, mod2, mod3}{Premium charged by specific models. The goals of there premium is to be similair to the response variable}
#'   \item{y}{Response variable. These are the losses}
#' }
#' @source Predictions derived from simulations.
"pred_table"
