## function to assign cooler labels to relativity groups
#' @export
put_labels <- function(groups, sorted_rr){
  ## get unique values for groups
  values <- unique(groups)
  
  ## create labels according to relativities
  new_labels <- sapply(values, function(t){
    val <- sorted_rr[groups == t]
    a <- paste0("[", round(min(val), 3),", ", round(max(val), 3), ")" )
    return(a)
  })
  ## Assign correct label to the correct level of the factor
  new_groups <- factor(groups)
  for (s in values) {
    levels(new_groups)[which(levels(new_groups) == s)] <- new_labels[s]
  }
  return(new_groups)
}