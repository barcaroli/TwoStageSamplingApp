
downloadBigUI<-function(id, label) {
  ## style
  invisibleButton<-c("color: #FFFFFF; background-color: #FFFFFF;
                  border-color: #FFFFFF; margin:0% 0% 0% 0%;height:2px;visibility:hidden; ")
  ns<-NS(id)
  options(java.parameters = "- Xmx5g")
  tagList(
    useShinyjs(rmd = T),
    actionButton(inputId = ns("dwl_sample"), label = label, width = "100%"),
    downloadButton(ns("dwl_sample_dwl"), "Not visible",
                   style=invisibleButton)
  )
}

downloadBig<-function(input, output, session,
                      strata,
                      rho,
                      effst,
                      iterations,
                      sensitivity,
                      expected,
                      design, 
                      sample,
                      seed,
                      modid) {

  ####################################
  ##  2. Download
  ##  2.1. Prepare Data
  strata_frame<-reactiveVal();
  rho_frame<-reactiveVal();
  effst_frame<-reactiveVal();
  iterations_frame<-reactiveVal();
  sensitivity_frame<-reactiveVal();
  expected_frame<-reactiveVal();
  design_frame<-reactiveVal()
  sample_frame<-reactiveVal()
  seed<-reactiveVal()
  observeEvent(input$dwl_sample, {
    strata_frame(strata);
    rho_frame(rho);
    effst_frame(effst);
    iterations_frame(iterations);
    sensitivity_frame(sensitivity);
    expected_frame(expected);
    design_frame(design); 
    sample_frame(sample);
    ## REQUIRES TO USE FULL NAME (WITH ns PART provided by modid)
    runjs(paste0("$('#", modid,"-dwl_sample_dwl')[0].click();"))
  })

  ## 2.2 Download Data
  output$dwl_sample_dwl <- downloadHandler(filename = function() {
    st<-paste0(str_remove_all(Sys.time(), "[:space:]|[:punct:]"), "_", seed())
    paste("Design_and_Sample-", st, ".zip", sep="")
  },
  content = function(file) {
    st<-paste0(str_remove_all(Sys.time(), "[:space:]|[:punct:]"), "_", seed())
    s1 <- paste("Strata-", st, ".csv", sep="")
    s2 <- paste("Rho-", st, ".csv", sep="")
    s3 <- paste("Effst-", st, ".csv", sep="")
    s4 <- paste("Iterations-", st, ".csv", sep="")
    s5 <- paste("Sensitivity-", st, ".csv", sep="")
    s6 <- paste("Expected-", st, ".csv", sep="")
    s7 <- paste("Design-", st, ".csv", sep="")
    s8 <- paste("Sample-", st, ".csv", sep="")
    temp.dir<-tempdir()
    wdOld<-getwd()
    setwd(temp.dir)
    on.exit(setwd(wdOld))
    withProgress(message = paste('Preparing data for download'),
                 value = 0, {
                   ##  CSV only (too big for excel)
                   write_csv(strata_frame(), s1)
                   write_csv(rho_frame(), s2)
                   write_csv(effst_frame(), s3)
                   write_csv(iterations_frame(), s4)
                   write_csv(sensitivity_frame(), s5)
                   write_csv(expected_frame(), s6)
                   write_csv(design_frame(), s7)
                   write_csv(sample_frame(), s8)
                   incProgress(1/2)
                   zip::zipr(zipfile=file, files= c(
                                                    s1,
                                                    s2,
                                                    s3,
                                                    s4,
                                                    s5,
                                                    s6,
                                                    s7,
                                                    s8
                                                    ), include_directories = F)
                   incProgress(1/2)
                 })
  }, contentType = "application/zip")
}





















