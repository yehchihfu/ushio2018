plot = function(){

  plot_ly(int_time_series, x =~time , y = ~strength, z =~species_to_species ) %>%
    group_by(species_to_species) %>%
    add_lines(color = ~species_to_species)
}
