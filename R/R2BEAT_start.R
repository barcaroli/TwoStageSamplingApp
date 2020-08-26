R2BEATUI<-function(id) {
  ns<-NS(id)
  options(java.parameters = "- Xmx5g")
  tagList(
    fluidRow(
      column(4,
             column(4,
                    h4("Iterations", style = "text-align: center;"),
                    DT::dataTableOutput(ns("iterations"))
      ),
      column(4,
             h4("Sensitivity", style = "text-align: center;"),
             DT::dataTableOutput(ns("expected"))
      ),
      column(4,
             h4("Expected CVs", style = "text-align: center;"),
             DT::dataTableOutput(ns("expected"))
      )
    )
  )
  )
}


R2BEATSRV<-function(input, output, session, 
                    dataset1,
                    dataset2,
                    dataset3,
                    frame_CV_in,
                    start_design,
                    id_PSU,
                    id_SSU,
                    domain_var,
                    target_var,
                    strat_var,
                    deff_var,
                    PSU_id,
                    stratum_var,
                    mos_var,
                    delta,
                    minimum,
                    seed
                    ) {
  ## styles
  # smTab<-list(dom="tp")
  ## reactives --- PUT HERE THE OUTPUTS
  solutionsList<-reactiveValues();
  # frame_XY_final<-reactiveVal(NULL); 
  # FF_final<-reactiveVal(NULL)
  observeEvent(start_design() ,{
    ###################################################
    ## Collecting inputs
    DES<-dataset1
    CAL<-dataset2
    PSU<-dataset3
    frame_CV<-frame_CV_in()
    id_PSU<-id_PSU()
    id_SSU<-id_SSU()
    domain_var<-domain_var()
    target_var<-target_var()
    strat_var<-strat_var()
    deff_var<-deff_var()
    PSU_id<-PSU_id()
    stratum_var<-stratum_var()
    mos_var<-mos_var()
    delta<-delta()
    minimum<-minimum()
    seed<-seed()

    ###################################################
    shiny::validate(need(DES, message = "DES"))
    shiny::validate(need(CAL, message = "CAL"))
    shiny::validate(need(PSU, message = "PSU"))
    shiny::validate(need(frame_CV, message = "frame_CV"))
    shiny::validate(need(id_PSU, message = "id_PSU"))
    shiny::validate(need(id_SSU, message = "id_SSU"))
    shiny::validate(need(domain_var, message = "domain_var"))
    shiny::validate(need(target_var, message = "target_var"))
    shiny::validate(need(strat_var, message = "strat_var"))
    shiny::validate(need(deff_var, message = "deff_var"))
    shiny::validate(need(PSU_id, message = "PSU_id"))
    shiny::validate(need(stratum_var, message = "stratum_var"))
    shiny::validate(need(mos_var, message = "mos_var"))
    shiny::validate(need(delta, message = "delta"))
    shiny::validate(need(minimum, message = "minimum"))
    shiny::validate(need(seed, message = "seed"))

    ###########################################################
    ###     EXECUTE SAMPLE DESIGN OPTIMIZATION
    ###########################################################       
    withProgress(message = paste('Preparing data ...'),
                 value = 0,{
                   ########################################################################################################
                   incProgress(message = "Running for Two-stage Sample Design", amount = 0.0)
                   #---------------------------------------
                   incProgress(message = "Preparing inputs (1) ...", amount = 0.25)
                   Sys.sleep(1)
                   inp1 <<- input_to_beat.2st_1(RGdes=force(DES),
                                               RGcal=force(CAL),
                                               id_PSU=force(id_PSU),
                                               id_SSU=force(id_SSU),
                                               strata_vars=force(strat_var),
                                               target_vars=force(target_var),
                                               deff_vars=force(deff_var),
                                               domain_vars=force(domain_var))
                   #---------------------------------------
                   incProgress(message = "Preparing inputs (2) ...", amount = 0.25)
                   Sys.sleep(5)
                   #
                   # psu=PSU
                   # psu_id="municipality"        # Identifier of the PSU
                   # stratum_var="stratum"        # Identifier of the stratum
                   # mos_var="ind"                # Variable to be used as 'measure of size'
                   # delta=1                      # Average number of SSUs for each selection unit
                   # minimum <- 50                # Minimum number of SSUs to be selected in each PSU
                   # inp2 <- input_to_beat.2st_2(psu,
                   #                             psu_id,
                   #                             stratum_var,
                   #                             mos_var,
                   #                             delta,
                   #                             minimum)
                   #
                   
                   inp2 <<- input_to_beat.2st_2(psu=force(PSU),
                                               psu_id=force(PSU_id),
                                               stratum_var=force(stratum_var),
                                               mos_var=force(mos_var),
                                               delta=force(delta),
                                               minimum=force(minimum))
                   #---------------------------------------
                   incProgress(message = "Optimal allocation ...", amount = 0.25)
                   Sys.sleep(1)
                   set.seed(seed())
                   
                   # frame_CV <- as.data.frame(list(DOM=c("DOM1"),
                   #                          CV1=c(0.01),
                   #                          CV2=c(0.01)
                   #                          # ,
                   #                          # CV3=c(0.05),
                   #                          # CV4=c(0.05)
                   #                          ))
                   # stratif = inp1$strata
                   # errors = frame_CV
                   # des_file = inp2$des_file
                   # psu_file = inp2$psu_file
                   # rho = inp1$rho
                   # effst = inp1$effst
                   # 
                   # allocation <- beat.2st(stratif,
                   #                   errors,
                   #                   des_file,
                   #                   psu_file,
                   #                   rho,
                   #                   deft_start = NULL,
                   #                   effst,
                   #                   epsilon1 = 5,
                   #                   mmdiff_deft = 1,
                   #                   maxi = 15,
                   #                   epsilon = 10^(-11),
                   #                   minnumstrat = 2,
                   #                   maxiter = 200,
                   #                   maxiter1 = 25)
                   
                   inp2$des_file$STRATUM <- as.factor(inp2$des_file$STRATUM)
                   inp2$psu_file$STRATUM <- as.factor(inp2$psu_file$STRATUM)
                   allocation <- beat.2st(stratif=inp1$strata,
                                          errors=force(frame_CV),
                                          des_file=inp2$des_file,
                                          psu_file=inp2$psu_file,
                                          rho=inp1$rho,
                                          deft_start = NULL,
                                          effst=inp1$effst,
                                          epsilon1 = 5,
                                          mmdiff_deft = 1,maxi = 15,
                                          epsilon = 10^(-11), minnumstrat = 2, maxiter = 200, maxiter1 = 25)
                   # #---------------------------------------
                   incProgress(message = "PSU selection ...", amount = 0.25)
                   Sys.sleep(1)
                   allocat <- allocation$alloc[-nrow(allocation$alloc),]
                   sample_2st <- StratSel(dataPop= inp2$psu_file,
                                          idpsu= ~ PSU_ID, 
                                          dom= ~ STRATUM, 
                                          final_pop= ~ PSU_MOS, 
                                          size= ~ PSU_MOS, 
                                          PSUsamplestratum= 1, 
                                          min_sample= force(minimum),
                                          min_sample_index= FALSE, 
                                          dataAll=allocat,
                                          domAll= ~ factor(STRATUM), 
                                          f_sample= ~ ALLOC, 
                                          planned_min_sample= NULL, 
                                          launch= F)
                   
                   incProgress(message = "Sample design terminated", amount = 0.0)
                   Sys.sleep(1)
                   ########  PLOTS
                   # des <- sample_2st[[2]]
                   # des$strat <- c(as.character(as.numeric(des$Domain[1:(nrow(des)-1)])),"Tot")
                   # des2<-NULL
                   # des2$stratum <- rep(NA,2*nrow(des))
                   # des2$type <- rep(NA,2*nrow(des))
                   # des2$nPSU <- rep(NA,2*nrow(des))
                   # des2 <- as.data.frame(des2)
                   # j<-0
                   # for(i in c(1:nrow(des)))  {
                   #   j<-j+1
                   #   des2$stratum[j] <- des$strat[i]
                   #   des2$type[j] <- "SR"
                   #   des2$nPSU[j] <- des$SRdom[i]
                   #   des2$nSSU[j] <- des$SR_PSU_final_sample_unit[i]
                   #   j=j+1
                   #   des2$stratum[j] <- des$strat[i]
                   #   des2$type[j] <- "NSR"
                   #   des2$nPSU[j] <- des$nSRdom[i]
                   #   des2$nSSU[j] <- des$NSR_PSU_final_sample_unit[i]
                   # }
                   # # des2$stratum <- as.factor(des2$stratum)
                   # library(dplyr)
                   # library(ggplot2)
                   # des3 <- ddply(des2, "stratum",
                   #               transform, number_PSU=cumsum(nPSU))
                   # p1 <- ggplot(data=des3, aes(x=stratum, number_PSU, fill=type)) +
                   #   geom_bar(stat="identity")
                   # p1 <- p1 + theme(axis.text.x = element_text(angle = 90)) +
                   #   labs(title = "Number of PSUs by strata")
                   # des3 <- ddply(des2, "stratum",
                   #  transform, number_SSU=cumsum(nSSU))
                   # p2 <- ggplot(data=des3, aes(x=stratum, number_SSU, fill=type)) +
                   #   geom_bar(stat="identity")
                   # p2 <- p2 + theme(axis.text.x = element_text(angle = 90)) +
                   #   labs(title = "Number of SSUs by strata")
                   # 
                   sample <- sample_2st[[4]]
                   sample <- sample[sample$PSU_final_sample_unit > 0,]
                   allo <- sample_2st[[2]]
                   solutionsList$strata <- inp1$strata
                   solutionsList$rho <- inp1$rho
                   solutionsList$effst <- inp1$effst
                   solutionsList$iterations <- allocation$iteractions
                   solutionsList$sensitivity <- allocation$sensitivity
                   solutionsList$expected <- allocation$expected
                   solutionsList$allocation <- allo
                   solutionsList$sample <- sample
                   
                 })  # end of withprogress
  })

  ######################################################
  ## return
  
  

  return(list(
    strata=reactive({solutionsList$strata}),
    rho=reactive({solutionsList$rho}),
    effst=reactive({solutionsList$effst}),
    iterations=reactive({solutionsList$iterations}),
    sensitivity=reactive({solutionsList$sensitivity}),
    expected=reactive({solutionsList$expected}),
    allocation=reactive({solutionsList$allocation}),
    sample=reactive({solutionsList$sample})
    # plot1=reactive({solutionsList$plot1}),
    # plot2=reactive({solutionsList$plot2})
  ))

}
