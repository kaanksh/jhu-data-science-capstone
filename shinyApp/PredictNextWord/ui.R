library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Word prediction"),
  

  sidebarLayout(
    sidebarPanel(
      textInput("userTxt", "Type here :", "")
      
      # TODO : space to explain app goal (type and X most likely next words shown, most likely first
      #         , update after space or end sentence, ...)
      # TODO : explain context app (DSS capstone w/ link) + link presentation
    ),
    
    
    # Show the caption, a summary of the dataset and an HTML 
    # table with the requested number of observations
    mainPanel(
      #textOutput("wordPredicted", container = span)
      
      tableOutput("wordPredicted")
    )
  )
))