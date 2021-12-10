.onLoad <- function(libname, pkgname) {
  if (packageVersion("rEDM") > "0.4.4") {
    msg <- paste("ushio2018 requires rEDM <= 0.4.4. You have ", packageVersion("rEDM"))
    packageStartupMessage(msg)
  }

  if (packageVersion("plotly") < "4.10.0") {
    msg <- paste("ushio2018 requires plotly >= 4.10.0. You have ", packageVersion("plotly"))
    packageStartupMessage(msg)
  }
  reticulate::configure_environment(pkgname)
}
