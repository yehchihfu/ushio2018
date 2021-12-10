# plot_time_3d -----------------------------
#' Generate a plot of species interactions over time
#' @param int_time_series A data frame is the output of the function `int_extract` with columns `species_to_species` (characters naming species interaction) `time` (observations of time points), and `strength` (the strength of the species interaction).
#' @import plotly
#' @return A plot_ly 3D object
#' @export
plot_time_3d <- function(int_time_series){
  level <- unique(int_time_series$species_to_species)

  df <- int_time_series %>%
    mutate(species_to_species = factor(species_to_species,
                                       ordered = TRUE,
                                       levels = level),
           time = (int_time_series$time * 14)/365 + 2002 + 157/365)

  plot_ly(df, y =~time , z = ~strength, x =~species_to_species ) %>%
    add_lines(color = ~species_to_species) %>%
    layout(scene = list(yaxis = list(title = "Sample Time"),
                        zaxis = list(title = "Interaction Strength"),
                        xaxis = list(title = "Species Interactions",
                                     showticklabels = FALSE)))
}
