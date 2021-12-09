#' Rearreange the order of xmap_from and xmap_to
#'
#' @param num.for.smapc dataframe with intraspecific and interespecific interactions
#' @return a rearranged dataframe where the intraspecific interactions are in the first row for each species
#' @import dplyr
#' @export
arrange_interactions <- function(num.for.smapc) {
  num.for.smapc %>%
    group_by(xmap_from) %>%
    mutate(new_order = ifelse(xmap_from == xmap_to, 0, xmap_to) ) %>%
    arrange(xmap_from,new_order) %>%
    ungroup %>%
    select(-new_order)%>%
    as.data.frame()
}
