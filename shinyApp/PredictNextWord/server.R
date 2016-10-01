library(shiny)

# Init : load functions and model
loadingMsg <- "Initializing..."
source("predictWordShiny.R")
# Initialize model as a multi-instances object
dfmModel <- NULL


# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {
  # Default ngram input
  currentTokens <- ""
  # Default predicted words
  wordPredicted1 <- ""
  wordPredicted2 <- ""
  wordPredicted3 <- ""
  wordPredicted4 <- ""
  wordPredicted5 <- ""
  
  
  # Load model only if it doesn't exist (multi-instances object)
  # Loading done inside the shinyServer() function to be able to show a progress bar
  if(is.null(dfmModel)) {
    withProgress( 
      {
        setProgress(message = loadingMsg)
        # Don't forget the double "<<" outside scope affectation
        dfmModel <<- readRDS(MODEL_FILE)
        setProgress(value = 1, message = loadingMsg)
      }
    , message = loadingMsg
    )
  }
  
  
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
  
  
  # Mapping outputs to reactive input
  wordPredictedVect <- reactive({
    predictNextWordFast(ngramInput(), dfmModel)
  })
  
  output$wordPredicted1 <- renderText({
    wordPredictedVect()[1]
  })
  
  output$wordPredicted2 <- renderText({
    wordPredictedVect()[2]
  })
  
  output$wordPredicted3 <- renderText({
    wordPredictedVect()[3]
  })
  
  output$wordPredicted4 <- renderText({
    wordPredictedVect()[4]
  })
  
  output$wordPredicted5 <- renderText({
    wordPredictedVect()[5]
  })
})