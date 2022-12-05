#' @export
split_the_splitted_df <- function(split_df, split_id){
  list("X" = split_df$X[split_id],
       "D" = split_df$D[split_id],
       "response" = split_df$response,
       "exposure" = split_df$exposure)
}