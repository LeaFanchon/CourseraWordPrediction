
library(shiny)
library(quanteda)
library(dplyr)

options(datatable.fread.datatable=FALSE)
options(digits=16)

values <- reactiveValues()
values$ready <- FALSE
values$first <- TRUE


# Define server logic
shinyServer(function(input, output) {
  
  sentence          <- reactive({input$sentence})
  ALPHA             <- reactive({input$ALPHA})
  WEIGHT_NGRAMS     <- reactive({input$WEIGHT_NGRAMS})
  WEIGHT_CONTEXT    <- reactive({input$WEIGHT_CONTEXT})
  STOPWORDS         <- reactive({input$STOPWORDS})
  NUMBER_RESULTS    <- reactive({input$NUMBER_RESULTS})
  
  output$prediction <- renderTable(digits = -4, {
    
    if (values$first == TRUE) {
      
      values$first <- FALSE
      
      load("binaryFiles")
      
      # Load files
      values$unigrams  <- unigrams
      values$bigrams   <- bigrams
      values$trigrams  <- trigrams
      values$fourgrams <- fourgrams
      values$context   <- context
      
      rm(list=c("unigrams", "bigrams", "trigrams", "fourgrams", "context"))
      
      values$stopwords        <- data.frame(unique(readLines("stopwords.txt")), stringsAsFactors = FALSE)
      names(values$stopwords) <- c("OUTPUT")
      
      values$ready <- TRUE 
    }
    

    prediction <- data.frame(Output=character(),
                             Katz=numeric(), 
                             Context=numeric(), 
                             Final=numeric()) 

    if (values$ready==TRUE) {
      
        if (sentence()!="") {
    
          prediction <- predict(sentence(), ALPHA(), WEIGHT_NGRAMS(), WEIGHT_CONTEXT(), STOPWORDS())
    
          if (nrow(prediction)>NUMBER_RESULTS()) {
            prediction <- prediction[1:NUMBER_RESULTS(),]
          }
        }
    }
    names(prediction) <- c("Output", "Katz Back-Off", "Context", "Final weight")
    
    prediction
    
  })
  
})



predict <- function(sentence, ALPHA, WEIGHT_NGRAMS, WEIGHT_CONTEXT, STOPWORDS) {
  
  # Tokenize 
  tokens <- tokens(sentence, what = "word", 
                   remove_numbers = TRUE, remove_punct = TRUE,
                   remove_symbols = TRUE, remove_hyphens = TRUE)
  
  tokens <- tokens_tolower(tokens)
  
  # Get bigrams and trigrams
  tokens_bi <- tokens_ngrams(tokens, n = 2)
  tokens_tri <- tokens_ngrams(tokens, n = 3)
  
  one <- ""
  bi  <- ""
  tri <- ""
  
  if (length(tokens$text1)>0) {
    one <- tokens$text1[length(tokens$text1)]
  } 
  
  if (length(tokens_bi$text1)>0) {
    bi  <- tokens_bi$text1[length(tokens_bi$text1)]
  }
  
  if (length(tokens_tri$text1)>0) {
    tri <- tokens_tri$text1[length(tokens_tri$text1)]
  }
  
  # Ngram model
  
  outputs_fou <- values$fourgrams[values$fourgrams$NGRAM==tri,]
  outputs_tri <- values$trigrams[values$trigrams$NGRAM==bi,]
  outputs_bi  <- values$bigrams[values$bigrams$NGRAM==one,]
  outputs_uni <- values$unigrams
  
  outputs_fou <- outputs_fou[, names(outputs_fou) %in% c("OUTPUT", "PROB")]
  outputs_tri <- outputs_tri[, names(outputs_tri) %in% c("OUTPUT", "PROB")]
  outputs_bi  <- outputs_bi[, names(outputs_bi) %in% c("OUTPUT", "PROB")]
  outputs_uni <- outputs_uni[, names(outputs_uni) %in% c("NGRAM", "PROB")]
  
  
  names(outputs_fou) <- c("OUTPUT", "PROB4")
  names(outputs_tri) <- c("OUTPUT", "PROB3")
  names(outputs_bi)  <- c("OUTPUT", "PROB2")
  names(outputs_uni) <- c("OUTPUT", "PROB1")
  
  outputs     <- full_join(outputs_fou, outputs_tri, by = c("OUTPUT"))
  outputs     <- full_join(outputs, outputs_bi, by = c("OUTPUT"))
  outputs     <- left_join(outputs, outputs_uni, by = c("OUTPUT"))
  
  if (nrow(outputs) != 0 ) {
      
      outputs[is.na(outputs)] <- 0
      
      outputs$PROBNGRAMS <-  outputs$PROB4 + (outputs$PROB4==0)*outputs$PROB3*ALPHA 
      outputs$PROBNGRAMS <-  outputs$PROBNGRAMS + (outputs$PROBNGRAMS==0)*outputs$PROB2*(ALPHA^2)
      outputs$PROBNGRAMS <-  outputs$PROBNGRAMS + (outputs$PROBNGRAMS==0)*outputs$PROB1*(ALPHA^3)
      
      # Context model
    
      Token.x        <- tokens$text1
      Token.x        <- as.data.frame(Token.x, stringsAsFactors=FALSE)
      names(Token.x) <- c("Token.x")
    
      Token.y        <- as.data.frame(outputs$OUTPUT, stringsAsFactors=FALSE)
      names(Token.y) <- c("Token.y")
    
      context_tmp    <- inner_join(values$context, Token.x, by = c("Token.x"))
      context_tmp    <- inner_join(context_tmp, Token.y, by = c("Token.y"))
      context_tmp    <- context_tmp[, names(context_tmp) %in% c("Token.y", "PROBCONTEXT")]
      
      if (nrow(context_tmp)!=0) {
        context_tmp    <- aggregate(. ~ Token.y, context_tmp, sum)
      }
      
      names(context_tmp) <- c("OUTPUT", "PROBCONTEXT")
    
      outputs <- left_join(outputs, context_tmp, by=c("OUTPUT"))
      outputs[is.na(outputs)] <- 0
    
      mean_ngrams  <- 1/(mean(outputs$PROBNGRAMS)+1)
      mean_context <- 1/(mean(outputs$PROBCONTEXT)+1)
 
      outputs$FINALPROB <- WEIGHT_NGRAMS*mean_ngrams*outputs$PROBNGRAMS + WEIGHT_CONTEXT*mean_context*outputs$PROBCONTEXT
    
      outputs <- outputs[, names(outputs) %in% c("PROBNGRAMS", "PROBCONTEXT", "FINALPROB", "OUTPUT")]
      
      # Remove the "retweet" word rt
      outputs <- outputs[outputs$OUTPUT!="rt",]
      
      if (!STOPWORDS) {
        outputs <- anti_join(outputs, values$stopwords, by=c("OUTPUT"))
        
      }
      outputs <- outputs[order(-outputs$FINALPROB),]
      
  } else {
    outputs <- data.frame(Output=character(),
                          Katz=numeric(), 
                          Context=numeric(), 
                          Final=numeric()) 
  }
  
  outputs
}

