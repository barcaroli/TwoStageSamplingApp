R2BEATInput_para <- function(id) {
  ns <- NS(id)
  tagList (
    actionButton(ns("start_design"), "Start Sampling Design", width = "100%"),
    br(),
    rHandsontableOutput(ns("hotout"), height = 400, width = "100%")
  )
}


#' Shiny server module to create a rhandsontable for R2BEAT inputs
#'
#'
#'
#' @param input standard shiny input
#' @param output standard shiny output
#' @param session standard shiny session object
#' @param dataset frame dataset used for the stratification
#' @param domain_var the selected domain variables
#' @param target_var the selected target variables
#'
#'
#' @export
#'
#'
R2BEATOutput_para <- function(input, output, session,
                                      dataset,
                                      domain_var,
                                      target_var) {
  frame_CV<-eventReactive(target_var(),{
    domain_var<-domain_var()
    target_var<-target_var()
    shiny::validate(need(domain_var, message = F))
    shiny::validate(need(target_var, message = F))
    frame_CV <- NULL
    frame_CV$DOM <- paste0("DOM",c(1:(length(c(domain_var))+1)))
    frame_CV<-data.frame(frame_CV)
    frame_CV[,c(2:(length(target_var)+1))]<-matrix(0.05, nrow = length(c(domain_var))+1,
                     ncol = (length(target_var)))
    
    names(frame_CV)<-c("DOM",sprintf("CV%d", 1:length(target_var)))
    frame_CV
  })

  output$hotout<-renderRHandsontable({
    frame_CV<-frame_CV()
    rhandsontable((frame_CV))
  })

  frame_CV_in<-eventReactive(input$start_strat, {
    # shiny::validate(need(hot, message = F))
    frame_CV<-hot_to_r(input$hotout)
    frame_CV
  })
  frame_CV <<- frame_CV
  return(list(frame_CV_in = frame_CV))
}
