#' Shiny UI module for upload of text (csv, tab) file
#
loadFileInput <- function(id, label, accept) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fileInput(ns("frame_file"), label, multiple = F,
              accept = accept)
  )
}


loadFile <- function(input, output, session, sep = ",") {
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$frame_file, message = FALSE))
    input$frame_file
  })

  frameFile<-reactive({
    # data.table::fread(userFile()$datapath, sep = sep)
    e = new.env()
    name <- load(userFile()$datapath, envir = e)
    data <- e[[name]]
    # tab <<- data$variables
  })

  
  return(frameFile)

}
