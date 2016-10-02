library(shiny)

shinyServer(function(input, output) {
  # Default ngram input
  currentTokens <- ""
  # Default predicted words
  wordPredicted1 <- ""
  wordPredicted2 <- ""
  wordPredicted3 <- ""
  wordPredicted4 <- ""
  wordPredicted5 <- ""
  
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