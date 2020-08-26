#' Start Sampling Application
#'
#'
#' @description Running this function starts the
#' the COVID19 CATI Surveys
#' Sampling Application for household survey sub-sample
#' in your browser
#'
#' @param launch.browser if TRUE starts in your default browser, otherwise in your IDE viewer
#' @param quiet if TRUE start-up messages are not printed
#'
#' @import shiny
#' @import data.table
#' @import rhandsontable
#' @import R2BEAT
#' @import flexdashboard
#' @importFrom stats complete.cases
#'
#' @export
suso_samplingApp<-function(launch.browser = T, quiet = T) {
  require(ReGenesees)
  require(sampling)
  require(magrittr)
  require(data.table)
  require(dplyr)
  require(plyr)
  require(ggplot2)
  fp<-file.path('sampling', 'suso_sampling')
  appFlex <- system.file(fp, "twostage_main.Rmd", package = "R2BEATapp")
  if (appFlex == "") {
    stop("Could not find example directory. Try re-installing `R2BEATapp`.", call. = FALSE)
  }
  rmarkdown::run(appFlex, shiny_args = list(launch.browser, quiet))
}
