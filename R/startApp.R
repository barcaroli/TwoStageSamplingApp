
TwoStageSamplingApp<-function(launch.browser = T, quiet = T) {
  require(ReGenesees)
  require(sampling)
  require(magrittr)
  require(data.table)
  require(dplyr)
  require(plyr)
  require(ggplot2)
  fp<-file.path('twostage_sampling')
  appFlex <- system.file(fp, "twostage_main.Rmd", package = "TwoStageSamplingApp")
  if (appFlex == "") {
    stop("Could not find example directory. Try re-installing `TwoStageSamplingApp`.", call. = FALSE)
  }
  rmarkdown::run(appFlex, shiny_args = list(launch.browser, quiet))
}
