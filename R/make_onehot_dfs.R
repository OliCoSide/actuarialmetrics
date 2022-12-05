#' @export
make_onehot_dfs <- function(formula,
                            data, 
                            banned_variables,
                            exposure = NULL,
                            response = NULL){
  ## create a dummy model just to have a matrix
  dummy_mod <- glm(formula = formula,
                   data = data,
                   family = "poisson")
  xnd <- model.matrix(dummy_mod) # get the matrix
  
  ## identify the variables in D
  d_id <- sapply(banned_variables, function(s){
    which(grepl(s, colnames(xnd))) 
  })
  
  ## make D and X matrix
  X <- xnd[, -1 * c(1, d_id)] # On veut retirer la colonne de 1 avant la projection
  D <- xnd[, d_id] # On veut utiliser la colonne de 1 pour
  
  ## make X and D df and fix names
  to_output_x <- as.data.frame(X)
  names(to_output_x) <- colnames(xnd)[-1 * c(1, d_id)]
  to_output_d <- as.data.frame(D)
  names(to_output_d) <- colnames(xnd)[d_id]
  
  ## add exposure if there is exposure
  if(!is.null(exposure)){
    to_output_x[[exposure]] <- data[[exposure]]
  }
  
  ## add exposure if there is exposure
  if(!is.null(response)){
    to_output_x[[response]] <- data[[response]]
  }
  
  ## return a list
  list("X" = to_output_x,
       "D" = to_output_d,
       "response" = response,
       "exposure" = exposure)
}