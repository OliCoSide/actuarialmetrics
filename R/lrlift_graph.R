## Graphique de loss ratio lift
#' @export
lrlift_graph <- function(table_list,
                         colors = "SunsetDark",
                         Prem_names = c(table_list$ref_name, table_list$comp_name)){
  
  ## we get the table from the list 
  table <- table_list$table %>% 
    mutate(r_group3 = factor(r_group, levels = unique(r_group)))
  
  n_cuts <- nrow(table)
  
  # couleurs 
  col <- hcl.colors(n_cuts, colors)
  col_dark <- rgb(t(col2rgb(col)/2), maxColorValue = 255)
  col_light <- rgb(t(255 - ((255 - col2rgb(col))/10)), maxColorValue = 255)
  
  angl <- ifelse(n_cuts >= 8, 20, 0)
  
  balance_capt <- ifelse(table_list$balance, " et les primes ont ete balancees.",".")
  
  range <- c(0, max(table$LR_ref) + 0.1)
  
  g <- ggplot(table, aes(x = r_group3, y = LR_ref, fill = as.factor(LR_ref), col = as.factor(LR_ref))) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes( label = 1:n_cuts,
                   y= LR_ref), col = factor(table$LR_ref, labels = col_light), vjust = +2)+
    geom_text(aes(label = scales::percent(LR_ref),
                  y= LR_ref),
              col = factor(table$LR_ref, labels = col_dark),
              vjust = -.5)+
    theme_bw() +
    
    scale_y_continuous(limits = range ,labels = scales::percent) + 
    
    labs(title = "Ratio de perte en fonction du groupe de relativite",
         subtitle = paste0("Les relativites sont calculees en divisant les primes ",
                           Prem_names[2],
                           " par les primes ",
                           Prem_names[1]),
         x="Relativite",
         y="Ratio de perte",
         caption = paste0("Les ratios de pertes sont calcules selon la structure de tarification de reference (", Prem_names[1],
                          ")", balance_capt)) +
    
    theme(legend.position="none", axis.text.x = element_text(angle = angl)) +
    
    scale_color_manual(values = col) + 
    scale_fill_manual(values = col) + 
    geom_hline(yintercept=1, alpha = 0.4, lty = 2, size =0.6 )
  ## "Viridis", "Plasma, "Purple-Orange"
  ## "Zissou1", "SunsetDark"
  return(g)
}
