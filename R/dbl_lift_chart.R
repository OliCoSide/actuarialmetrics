
#' @export
dbl_lift_chart <- function(table_list,
                           Prem_names = c("P_ref", "P_comp"),
                           palette = "Dark2"){
  
  ## we get the table from the list 
  table <- table_list$table
  
  n_cuts <- nrow(table)
  # # couleurs 
  # col <- hcl.colors(n_cuts, colors)
  # col_dark <- rgb(t(col2rgb(col)/2), maxColorValue = 255)
  # col_light <- rgb(t(255 - ((255 - col2rgb(col))/10)), maxColorValue = 255)
  
  angl <- ifelse(n_cuts >= 8, 20, 0)
  
  balance_capt <- ifelse(table_list$balance,
                         "et les primes sont balancees.",
                         "")
  
  table_to_graph <- reshape2::melt(table,
                                   measure.vars = c("LR_ref",
                                                    "LR_comp")) %>% 
    mutate(r_group3 = factor(r_group, levels = unique(r_group)))
  
  g <- ggplot(table_to_graph, aes(x = r_group3,
                                  y = value,
                                  colour = variable,
                                  group = variable)) +
    geom_path(alpha = 0.8, lwd = 1) +
    geom_point(size = 2) + 
    theme_bw() +
    scale_y_continuous(limits = c(min(table_to_graph$value) - 0.1, max(table_to_graph$value) + 0.1) ,labels = scales::percent) + 
    
    labs(title = "Ratio de perte en fonction du groupe de relativite",
         subtitle = paste0("Les relativites sont calculees en divisant les primes ",
                           Prem_names[2],
                           " par les primes ",
                           Prem_names[1]),
         caption = paste0("Les primes ", Prem_names[1],
                          " sont les primes de reference ", balance_capt),
         x="Relativite",
         y="Ratio de perte") +
    
    theme( axis.text.x = element_text(angle = angl)) + 
    scale_color_brewer(palette = palette,
                       name="Modele",
                       breaks=c("LR_ref", "LR_comp"),
                       labels=c(paste0(table_list$ref_name,
                                       " (ref)"),
                                paste0(table_list$comp_name,
                                       " (comp)"))) + 
    geom_hline(yintercept=1, alpha = 0.4, lty = 2, size =0.6 )
  ## "Viridis", "Plasma, "Purple-Orange"
  ## "Zissou1", "SunsetDark"
  return(g)
}