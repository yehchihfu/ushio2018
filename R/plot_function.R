
plot = function(int_time_series){
  level <- unique(int_time_series$species_to_species)

  df <- int_time_series %>%
    mutate(species_to_species = factor(species_to_species,
                                       ordered = TRUE,
                                       levels = level),
           time = (int_time_series$time * 14)/365 + 2002 + 157/365)

  plot_ly(df, y =~time , z = ~strength, x =~species_to_species ) %>%
    # group_by(species_to_species) %>%
    add_lines(color = ~species_to_species) %>%
    layout(scene = list(yaxis = list(title = "Sample Time"),
                        zaxis = list(title = "Interaction Strength"),
                        xaxis = list(title = "Species Interactions",
                                     showticklabels = FALSE)))
}
