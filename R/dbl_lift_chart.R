#' Display a \emph{double lift chart} with \code{ggplot2} synthax
#'
#' @param table_list A table produced by \code{get_lr_table}
#' @param Prem_names The names of the ratemaking structure to compare. The default is given in \code{table_list}
#' @param palette The palette to use for the visualisation (from \code{Rcolorbrewer}). The default is \code{"Dark2"}.
#' @param scale The scale to be used for the double lift chart. Default is \code{"obs"}. if anything other than \code{"obs"} is used, such as \code{"LR"}, the graph is scaled to the observed (The observed line is therefore removed).
#' @param style The number of the style preset for the graph. This customizes the width of the line, the size of the points and the transparency of the line and the points. Default value is \code{2}.
#' @param language The language to display text on axis. Supported values are \code{'french'} (default value) and \code{'english'}.
#' @param angle_x_label The angle of the \code{x} labels. For display and readability purpose only.
#' @return A \code{ggplot2} graph of a \emph{double lift chart}
#' @examples
#' library(actuarialmetrics)
#' data("pred_table")
#' table_to_g <- get_lr_table(pred_table,
#'                            ref_name = "mod2",
#'                              comp_name = "mod3",
#'                              loss_name = "Y",
#'                              n_cuts = 6)
#'
#' dbl_lift_chart(table_to_g)
#' @export
dbl_lift_chart <- function(table_list,
                           Prem_names = c(table_list$ref_name,
                                          table_list$comp_name),
                           palette = "Dark2",
                           scale = "dollar",
                           style = 2,
                           language = "french",
                           angle_x_label = NULL){

  ## we get the table from the list
  table <- table_list$table

  n_cuts <- nrow(table)
  # # couleurs
  # col <- hcl.colors(n_cuts, colors)
  # col_dark <- rgb(t(col2rgb(col)/2), maxColorValue = 255)
  # col_light <- rgb(t(255 - ((255 - col2rgb(col))/10)), maxColorValue = 255)

  ## X labels style
  if(is.null(angle_x_label)){
    angle_for_label <- ifelse(n_cuts >= 8, 20, 0)
  } else {
    angle_for_label <- angle_x_label
  }

  ## Type of graph
  if(scale == "dollar"){
    table_to_graph <- reshape2::melt(table,
                                     measure.vars = c("P_ref",
                                                      "P_comp",
                                                      "loss")) %>%
      mutate(r_group3 = factor(r_group, levels = unique(r_group)))
  } else if(scale == "percent") {
    table_to_graph <- reshape2::melt(table,
                                     measure.vars = c("LR_ref",
                                                      "LR_comp")) %>%
      mutate(r_group3 = factor(r_group, levels = unique(r_group)))
  } else {
    stop("Supported values for scale are 'dollar' and 'percent'.")
  }

  ## Names of various textual elements of the graph
  if(language == 'french'){
    if(scale == "dollar") {
      the_title <- "Pertes en fonction du groupe de relativite"
      the_y <- "Pertes"
    } else if(scale == "percent") {
      the_title <- "Ratio de perte en fonction du groupe de relativite"
      the_y <- "Ratio de perte"
    }
    the_subtitle <- paste0("Il y a ",
                           table_list$table$expo[1],
                           " unite d'exposition par groupe, et relativite=",
                           Prem_names[2],
                           "/",
                           Prem_names[1])

    the_x <- "Relativite"

    the_name_of_the_legend <- "Modele"
    the_loss_for_the_legend <- "Observee"

    balance_capt <- ifelse(table_list$balance,
                           "et les primes sont balancees.",
                           "")
    the_caption <- paste0("Les primes ", Prem_names[1],
                          " sont les primes de reference ", balance_capt)

  } else if(language == "english") {
    if(scale == "dollar") {
      the_title <- "Losses per group of relativity"
      the_y <- "Losses"
    } else if(scale == "percent") {
      the_title <- "Loss ratio per group of relativity"
      the_y <- "Loss ratio"
  }
    the_subtitle <- paste0("There are ",
                           table_list$table$expo[1],
                           " exposure unit per group, and relativity=",
                           Prem_names[2],
                           "/",
                           Prem_names[1])

    the_x <- "Relativity"

    the_name_of_the_legend <- "Model"
    the_loss_for_the_legend <- "Observed"

    balance_capt <- ifelse(table_list$balance,
                           "and premiums are balanced.",
                           "")
    the_caption <- paste0("Premium ", Prem_names[1],
                          " are the reference ", balance_capt)
  } else {
    stop("Supported values for language are 'french' and 'english'.")
  }

  ## Style of the graph
  if(style == 1){
    alpha_path <- 0.8
    lwd_path <- 1
    size_point <- 2
    alpha_point <- 1
  } else if(style == 2){
    alpha_path <- 0.4
    lwd_path <- 2
    size_point <- 4
    alpha_point <- 0.8
  } else if(style == 3){
    alpha_path <- 0.8
    lwd_path <- 1
    size_point <- 4
    alpha_point <- 0.4
  } else {
    stop("Supported numeric values for style are 1, 2, or 3")
  }

  g <- ggplot(table_to_graph, aes(x = r_group3,
                                  y = value,
                                  colour = variable,
                                  group = variable)) +
    geom_path(alpha = alpha_path, lwd = lwd_path) +
    geom_point(size = size_point, alpha = alpha_point) +
    theme_bw() +
    scale_y_continuous(limits = c(min(table_to_graph$value) - 0.1,
                                  max(table_to_graph$value) + 0.1),
                       labels = ifelse(scale == "dollar",
                                       scales::dollar,
                                       scales::percent)) +
    labs(title = the_title,
         subtitle = the_subtitle,
         caption = the_caption,
         x= the_x,
         y= the_y) +
    theme( axis.text.x = element_text(angle = angle_for_label,
                                      hjust = 1)) +
    scale_color_brewer(palette = palette,
                       name= the_name_of_the_legend,
                       breaks=unique(table_to_graph$variable),
                       labels=c(paste0(Prem_names[1],
                                       " (ref)"),
                                paste0(Prem_names[2],
                                       " (comp)"),
                                unlist(ifelse(scale == "dollar",
                                       the_loss_for_the_legend,
                                       list(NULL))))) +
    geom_hline(yintercept= unlist(ifelse(scale == "dollar",
                                  list(NULL),
                                  1)), alpha = 0.4, lty = 2, size =0.6 )
  ## "Viridis", "Plasma, "Purple-Orange"
  ## "Zissou1", "SunsetDark"
  return(g)
}
