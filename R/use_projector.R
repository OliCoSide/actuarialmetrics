#' @export
use_projector <- function(new_split_df, projector){
  
  to_remove_from_proj <- c(new_split_df$response,
                           new_split_df$exposure)
  
  ## split df
  newX <- new_split_df$X %>% select(-all_of(to_remove_from_proj))
  newD <- new_split_df$D
  
  ## if D is only 1 var... 
  if(is_null(dim(newD))){
    newD <- as.data.frame(new_split_df$D)
    names(newD) <- (projector[[1]]$coefficients %>%  names)[2]
  }
  
  ## calculation of residuals
  X_star2 <- as.data.frame(sapply(1:length(projector), function(b){
    mod <- projector[[b]]
    var <- newX[[b]]
    var - predict(mod, newdata = newD)
  }))
  
  ## add back them names
  names(X_star2) <- names(newX)
  
  ## add back exposure and resp if necessary
  if(!is.null(new_split_df$response)){
    X_star2[[new_split_df$response]] <- new_split_df$X %>% pull(new_split_df$response)
    
  }
  if(!is.null(new_split_df$exposure)){
    X_star2[[new_split_df$exposure]] <- new_split_df$X %>% pull(new_split_df$exposure)
  }
  
  return(X_star2)
}