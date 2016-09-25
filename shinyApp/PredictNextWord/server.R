library(shiny)

# Init : load functions and model
source("predictWordShiny.R")
dfmModel <- readRDS(MODEL_FILE)


# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {
  currentTokens <- ""
  
  ngramInput <- reactive({
    txt <- input$userTxt
    
    # Update currentTokens only if last character is a space
    if(nchar(txt) == 0 || substr(txt, nchar(txt), nchar(txt)) == " ") {
      # "<<-" to update object in current session and not just in the function context
      currentTokens <<- tokenizeInput(txt)
      #browser()
    }
    
    currentTokens
  })
  

  output$wordPredicted <- renderText({
    predictNextWordFast(ngramInput(), dfmModel)
  })
  
})