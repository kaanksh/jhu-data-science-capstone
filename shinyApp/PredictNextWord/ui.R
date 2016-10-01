library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(theme = "bootstrap.min.css",
  
  # Application title
  titlePanel("Next word prediction with R"),

  sidebarLayout(position = "left",
    sidebarPanel(
      h3("What is this ?"),
      p("This application predicts the most likely next words based on what you typed."),
      p("It is developed in R and it is the final project for the Data Science Specialization on Coursera."),
      
      h3("How to use it ?"),
      p("Just start typing. The predictions are updated everytime you use a space."),
      
      h3("Additional information"),
      div(
        a("Data Science Specialization on Coursera by the Johns Hopkins University",
        href="https://www.coursera.org/specializations/jhu-data-science")
      ),
      div(
        a("**** TODO : link to RPUBS slide deck ????? *****",
          href="TODO")
      )
    ),
      
    mainPanel(
      textInput("userTxt", "Type here :", ""),
      h4("Next word predictions"),
      textOutput("wordPredicted1", container = div),
      textOutput("wordPredicted2", container = div),
      textOutput("wordPredicted3", container = div),
      textOutput("wordPredicted4", container = div),
      textOutput("wordPredicted5", container = div)
    )
  ),
  div(class = "footer",
      #style="text-align: center;",
      p("Samy Soualem - October 2016")
  )
))