R_version_update <- function() {
  if (!require(installr)) {
    install.packages("installr")
    require(installr)
  } #load / install+load installr
  updateR()
}