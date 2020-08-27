R2BEATInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    selectizeInput(ns("id_PSU"), "PSU identifier", choices = c(""),
                   options = list(
                     placeholder = 'Upload design object first',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    ),
    selectizeInput(ns("id_SSU"), "SSU identifier", choices = c(""),
                   options = list(
                     placeholder = 'Upload design object first',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    ),
    selectizeInput(ns("domain_var"), "Domain Variable", choices = c(""),
                   multiple=T,
                   options = list(
                     placeholder = 'Upload design object first',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    ),
    selectizeInput(ns("target_var"), "Target Variable(s)", choices = c(""),
                   multiple=T,
                   options = list(
                     placeholder = 'Upload design object first',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    ),
    selectizeInput(ns("strat_var"), "Stratification Variable(s)", choices = c(""),
                   multiple=T,
                   options = list(
                     placeholder = 'Upload design object first'
                   )
    ),
    selectizeInput(ns("deff_var"), "DEFF Variable(s)", choices = c(""),
                   multiple=T,
                   options = list(
                     placeholder = 'Upload design object first'
                   )
    ),
    selectizeInput(ns("PSU_id"), "PSU identifier", choices = character(0),
                   # multiple=T,
                   options = list(
                     placeholder = 'Upload design object first'
                   )
    ),
    selectizeInput(ns("mos_var"), "Measure of size", choices = character(0),
                   # multiple=T,
                   options = list(
                     placeholder = 'Upload design object first'
                   )
    ),
    selectizeInput(ns("stratum_var"), "Stratum variable", choices = character(0),
                   # multiple=T,
                   options = list(
                     placeholder = 'Upload design object first'
                   )
    )
  )
}


R2BEATOutput1 <- function(input, output, session, dataset) {
  ##  2. Update Input fields
  observe({
    FF<-dataset
    shiny::validate(need(FF, message = F))
    #################################
    ## PSU id
    updateSelectizeInput(session = session, inputId = "id_PSU",
                         label = "PSU identifier",
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable below',
                           onInitialize = I('function() { this.setValue("municipality"); }')
                         )
    )    
    #################################
    ## SSU id
    updateSelectizeInput(session = session, inputId = "id_SSU",
                         label = "SSU identifier",
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable below',
                           onInitialize = I('function() { this.setValue("id_hh"); }')
                         )
    )    
    #################################
    ## Domain
    updateSelectizeInput(session = session, inputId = "domain_var",
                         label = "Domain Variable",
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable below',
                           onInitialize = I('function() { this.setValue("region"); }')
                         )
    )
    #################################
    ## Target Var
    updateSelectizeInput(session = session, inputId = "target_var",
                         label = "Target Variable(s)",
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable(s) below',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
    )
    #################################
    ## Stratification Var
    updateSelectizeInput(session = session, inputId = "strat_var",
                         label = "Stratification Variable(s)",
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable(s) below',
                           onInitialize = I('function() { this.setValue("stratum"); }')
                         )
    )

    #################################
    ## DEFF var
    updateSelectizeInput(session = session, inputId = "deff_var",
                         label = "DEFF Variable(s)", 
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable(s) below',
                           onInitialize = I('function() { this.setValue("stratum"); }')
                         )
    )
  })

}

R2BEATOutput2 <- function(input, output, session, dataset) {
  ##  2. Update Input fields
  observe({
    FF<-dataset
    shiny::validate(need(FF, message = F))
    #################################
    ## PSU id
    updateSelectizeInput(session = session, inputId = "PSU_id",
                         label = "PSU identifier",
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable below',
                           onInitialize = I('function() { this.setValue("municipality"); }')
                         )
    )   
    #################################
    ## MOS Var
    updateSelectizeInput(session = session, inputId = "mos_var",
                         label = "Measure of Size (MOS) Variable",
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable below',
                           onInitialize = I('function() { this.setValue("ind"); }')
                         )
    )
    #################################
    ## Stratification
    updateSelectizeInput(session = session, inputId = "stratum_var",
                         label = "Stratification Variable",
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable below',
                           onInitialize = I('function() { this.setValue("stratum"); }')
                         )
    )

  })

}
