library(shiny)
shinyUI(navbarPage("Word Prediction",
                   tabPanel("App",
                            fluidRow(
                              column(3,
                              wellPanel(p("Enter a few words in the text box 
                                             on the right side and click on predict next word, 
                                             The app will bring the suggestions in descending probability order")
                              )
    ),
    column(6,
          textInput("word_input",label="Enter some words"),
          actionButton("predict","Predict Next Word")
        ),
    
    column(3,
           tableOutput("wordlist") )
      )
    )
  )
)