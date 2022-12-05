#' @export
create_projector <- function(split_df){

to_remove_from_proj <- c(split_df$response,
                         split_df$exposure)

## we have our X and D
X_df <- split_df$X %>% select(-all_of(to_remove_from_proj))
D_df <- split_df$D

# ## we name the D so it does bug
# names(D_df) <- sapply(1:ncol(D_df), function(s) paste0("var", s))
# 
## We model
models_for_proj <- lapply(X_df, function(a){
  df <- data.frame("resp" = a,
                   D_df)
  
  lm(resp~., data = df)
})
return(models_for_proj)
}