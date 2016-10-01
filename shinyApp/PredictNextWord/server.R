library(shiny)

# Init : load functions and model

loadingMsg <- "Initializing..."
source("predictWordShiny.R")
# Initialize model as a multi-instances object
dfmModel <- NULL


# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {
  
  # Load model only if it doesn't exist (multi-instances object)
  # Loading done inside the shinyServer function to show a progress bar
  if(is.null(dfmModel)) {
    withProgress( 
      {
        setProgress(value = 0.1, message = loadingMsg)
        # Don't forget the double "<<" outside scope affectation
        dfmModel <<- readRDS(MODEL_FILE)
        setProgress(value = 1, message = loadingMsg)
      }
    , message = loadingMsg
    )
  }
  
  # Default ngram input
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