
zipFileInput <- function(id, label, accept) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fileInput(ns("frame_file"), label, multiple = F,
              accept = accept)
  )
}


zipFile <- function(input, output, session, sep = ",") {
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$frame_file, message = FALSE))
    input$frame_file
  })

  frameFile<-reactive({
    data.table::fread(userFile()$datapath, sep = sep)
  })

  return(frameFile)

}
