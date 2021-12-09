#' extract_causality
#'
#' @param d_bestE0 dataframe with...
#' @return a dataframe with significant intraspecific and interespecific interactions
#' @import dplyr
#' @export
extract_causality <- function(d_bestE0) {
  # set parameters
  kURhoThreshold <- 0 #Difference between upper limit of 95%CI and terminal rho
  kDRhoThreshold <- 0.1 #Threshold of delta rho (= terminal rho minus initial rho)

  # Detect significant causal interactions
  d_bestE<- d_bestE0 %>%
    subset(ccm_values.L == max(ccm_values.L)) %>%
    filter(ccm_values.u_rho > kURhoThreshold, d_rho > kDRhoThreshold)

  # Preparation for S-map coefficients analysis
  d_bestE %>%
    select(c(14,15,1))%>%
    rename('best_E'= 'ccm_values.E')
}
