library(shiny)
library(quanteda)
library(qdap)
library(data.table)
library(stringr)

ngrams <- readRDS("./data/ngrams.rds")

ui <- fluidPage(
    navbarPage(
        
        title = "NextWord",
        
        tabPanel(title = "Predicting next wor(l)d!", 
                 sidebarLayout(
                      sidebarPanel(
                          h3("Input Box"),
                          br(),
                          textInput("text", "Type any phrase:", value = "Hi! Welcome to my"),
                          br(),
                          submitButton("Predict Now")
                        ),
                      mainPanel(
                          h3("Predicted next word:"),
                          textOutput("nextword_1"),
                          br(),
                          h3("Top 10 predicted next words:"),
                          tableOutput("nextword_2"),
                          br()
                      )
                 )
            ),
            
        tabPanel("About NextWord", 
                 
                 h4 ("Dear my friend,"),
                 
                 p("This application is the final capstone project required by the Data Science
                 Specialization designed by Johns Hopkins University Bloomberg School of Public
                 Health and its industry partner SwiftKey on Coursera."),
                 
                 p("The Capstone Project aims to develop an English-language application that is
                   able to predict the next word based on previous words a user has typed in."),
                 
                 p("I established a N-grams probablistic language model using three collections 
                   of English documents provided by the course: en_US.blogs, en_US.news and 
                   en_US.twitter. The application will return the most likely next word and a table
                   containing all top 10 ranked ones."),
                 
                 p("Original codes are available here in ", 
                   a("Github.", href="https://github.com/arthurwang95/capstone")),
                 
                 br(),
                 p("By Arthur", br(), "April 15, 2020")
                 )
        
    )
)

server <- function(input, output) {
    
    table <- reactive ({
        
        test <- input$text
        test <- gsub("'","'", test)
        test <- replace_contraction(test)
        test <- tolower(test)
        test.tokens <- tokens(test, what = "word", 
                              split_hyphens = TRUE, remove_numbers = TRUE, 
                              remove_punct = TRUE,  remove_symbols = TRUE, remove_url = TRUE)

        l <- length(test.tokens[[1]])
        if (l >= 3) {term <- test.tokens[[1]][c((l-2):l)]
        } else {term <- test.tokens[[1]]}
        
        for (i in length(term):1) {
            if (sum(ngrams[[i]]$term == paste(term[c((length(term)+1-i):length(term))], collapse = "_")))
            { ngramlevel <- i; break }}
        
        backoff <- function(newterm, n, rank, words_checked){
            
            # If we have backed to the unigram level (at the same time, newterm will be empty), we will pick top 5 most frequent words that have not been scored so far.
            if (n == 0) 
            {
                # Remove all checked/scored words from unigram table
                temp_uni <-  ngrams[[1]][!(ngrams[[1]]$term %in% words_checked), ]
                
                # Calculate the denominator: just the total words N
                denominator <- sum(ngrams[[1]]$count)
                
                rank <- rbind(rank, data.table(
                    TokenMatch = temp_uni$term[c(1:5)],
                    NextWord =  temp_uni$term[c(1:5)],
                    Score =  (0.4)^(ngramlevel)*temp_uni$count[c(1:5)] / denominator
                ))
            } 
            else {
                # If n >= 1
                # First, we find the n+1 grams whose preterm matches newterm (e.g. a_case)
                matchID <- as.integer()
                matchID <- which(ngrams[[n+1]]$preterm == paste(newterm, collapse = "_"))
                
                # We need to remove the matched n+1 grams whose last term has been scored at higher N-grams.
                matchID <- matchID [!(ngrams[[n+1]]$lastterm[matchID] %in% words_checked)]
                
                # We need to record which words have been scored.
                words_checked <- c(words_checked, ngrams[[n+1]]$lastterm[matchID])
                
                # Calculate the denominator
                denominator <- ngrams[[n]]$count[ngrams[[n]]$term == paste(newterm, collapse = "_")]
                
                rank <- rbind(rank, data.table(
                    TokenMatch = ngrams[[n+1]]$term[matchID], 
                    NextWord = ngrams[[n+1]]$lastterm[matchID],
                    Score = (0.4)^(ngramlevel-n)* ngrams[[n+1]]$count[matchID] /denominator  ))
                
                # Lastly, we backoff the lower N-gram  
                rank <- backoff(newterm[-1], n-1, rank, words_checked)
            }
            rank
        }
        
        rank <- data.frame()
        words_checked <- as.character() 
        rank <- backoff(newterm = term[c((4-ngramlevel):3)], n = ngramlevel, rank, words_checked)    

        head(rank,10)
    })
    
    output$nextword_1 <- renderText({as.character(table()[1,2])})
    output$nextword_2 <- renderTable (table())
}


shinyApp(ui = ui, server = server)
