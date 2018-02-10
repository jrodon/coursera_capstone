# server.R ####
# Coursera Data Science Capstone Project (https://www.coursera.org/course/dsscapstone)
# Shiny server script
# Javier A. Rodón
# 10.02.2018

# Define backend logic ####
shinyServer(function(input, output) {
      # Capture the input string ####
      inpText <- eventReactive(input$submit, {
            paste(makeTokens(input$text, save = F), collapse = " ")
      })
      # Create the prediction table ####
      predTable <- eventReactive(inpText(), {
            funProb(input$text, as.numeric(input$ngrams))
      })
      # Extract the most likely prediction from the table ####
      observeEvent(input$reset, {js$reset()})
      predText <- eventReactive(input$submit, {
            sapply(1:5, function(x) tail(funInput(gsub("_", " ", predTable()[order(-sbo)][x]$ngram, fixed = T)), 1))
      })
      # Output data table ####
      output$table <- renderDataTable({
            datatable(
                  predTable(), 
                  options = list(pageLength = 5,
                                 dom = 't',
                                 columnDefs = list(list(title = "n-Gram", targets = 1),
                                                   list(title = "Counts", targets = 2),
                                                   list(title = "ML Estimate", targets = 3),
                                                   list(title = "SBO Score", targets = 4),
                                                   list(processing = TRUE)
                                 )
                  )
            ) %>%
                  formatRound(3:4, 3) %>%
                  formatStyle(1, color ='black', 
                              backgroundColor = 'lightgrey', 
                              fontWeight = 'bold') %>%
                  formatStyle(2:4, color ='black', 
                              backgroundColor = 'white')
      })
      # Output most likely prediction ####
      output$prediction <- renderUI({
            HTML(paste("<h4>", inpText(), "<text style='color:Gold'><b>", predText()[1], "</b></text></h4>"))
      })
      # Output the remaining predictions ####
      output$lessProb <- renderUI({
            HTML(paste("<h4>", inpText(), "<text style='color:SpringGreen'><b>", predText()[2:5], "</b></text></h4>"))
      })
})
