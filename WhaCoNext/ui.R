# ui.R ####
# Coursera Data Science Capstone Project (https://www.coursera.org/course/dsscapstone)
# Shiny UI script
# Javier A. Rodón
# 10.02.2018

# Define the frontend ####
shinyUI(fluidPage(
      # Enable the reset function ####
      useShinyjs(),
      extendShinyjs(text = jsResetCode),
      # Set theme ####
      theme = shinytheme("slate"),
      # Application title ####
      titlePanel("What comes next?", windowTitle = "What comes next? - Simple Word Predictor"),
      hr(),
      # Layout ####
      # Upper Panel ####
      fluidRow(
            # Left Panel ####    
            column(5,
                   wellPanel(
                         # User text input
                         textInput("text", label = "Please enter some text", 
                                   value = "Good day, to you and me!"),
                         # Selection of the ngram level
                         radioButtons('ngrams', 'n-Gram level (higher is better, but slower)',
                                      choices = c(2:4), selected = 3, inline = T)
                   ),
                   # Action buttons
                   actionButton("submit", label = "Predict", icon = icon("refresh")),
                   actionButton("reset", label = "Reset", style = "float:right"),
                   br(),
                   # Button's descriptions
                   helpText(HTML("<text style='color:grey'>
                                 Click the 'Predict' button to obtain the predictions.</br>
                                 The 'Reset' button reloads the application.</text>")),
                   br(),
                   HTML("<text style='color:DarkGoldenRod'>
                        The first prediction takes a bit longer, please be patient!</text>")
            ),
            # Right Panel ####
            column(7,
                   # Text output
                   h4(HTML("<text style='color:SkyBlue'>
                           I believe what comes next is...</text>")),
                   wellPanel(htmlOutput("prediction")),
                   hr(),
                   # Table output
                   h4(HTML("<text style='color:SkyBlue'>
                           My other (less probable) predictions are</text>")),
                   wellPanel(htmlOutput("lessProb"))
            )
      ),
      hr(),
      # Lower Panel ####
      fluidRow(
            # Left Panel ####
           column(4, 
                   wellPanel(
                         # Link to report
                         helpText(a(HTML("<text style='color:OrangeRed'>
                                         More information on the project </text>"),
                                    href='http://dataexcursions.com/Word-Prediction-Shiny-App', 
                                    target = '_blank')
                         ),
                         # Link to repo
                         helpText(a(HTML("<text style='color:OrangeRed'>
                                         GitHub Repo</text>"),
                                    href='https://github.com/arttuK/word-prediction/tree/master/shiny',
                                    target = '_blank')
                         )
                   )
            ),
           # Right Panel ####
           column(8,
                  # Probabilites table
                  h4(HTML("<text style='color:SkyBlue'>
                          These are the probabilities and scores of the options</text>")),
                  dataTableOutput("table")
           )
      )
)
) 
