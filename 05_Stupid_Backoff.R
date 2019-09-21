
numerify <- function(n, df) {
  
  if(n>0){
    df$PROB          <- as.numeric(gsub(",", ".",df$PROB))
    
  } else if (n==0){
    df$PROBCONTEXT    <- as.numeric(df$PROBCONTEXT)
    df$Token.x        <- as.character(df$Token.x)
    df$Token.y        <- as.character(df$Token.y)
    
  } else if (n==-1) {
    df$probs          <-  as.numeric(gsub(",", ".", df$probs))
    
  }
  
  df
}

predict <- function() {
  

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
  
  outputs_fou <- fourgrams[fourgrams$NGRAM==tri,]
  outputs_tri <- trigrams[trigrams$NGRAM==bi,]
  outputs_bi  <- bigrams[bigrams$NGRAM==one,]
  outputs_uni <- unigrams
  
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
  
  context_tmp    <- inner_join(context, Token.x, by = c("Token.x"))
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
  outputs <- outputs[order(-outputs$FINALPROB),]
}


# Libraries

library(quanteda)
library(reshape2)
library(dplyr)
library(sqldf)

options(digits=16)

# Time the code execution
start.time <- Sys.time()

# Load files

unigrams  <- read.csv("counts_ngrams_1_uni.csv", sep=";", stringsAsFactors=FALSE, fileEncoding="latin1")
bigrams   <- read.csv("counts_ngrams_2_bi.csv" , sep=";", stringsAsFactors=FALSE, fileEncoding="latin1")
trigrams  <- read.csv("counts_ngrams_3_tri.csv", sep=";", stringsAsFactors=FALSE, fileEncoding="latin1")
fourgrams <- read.csv("counts_ngrams_4_fou.csv", sep=";", stringsAsFactors=FALSE, fileEncoding="latin1")


unigrams  <- unigrams[, names(unigrams) %in% c("NGRAM", "PROB")]
bigrams   <- bigrams[, names(bigrams) %in% c("NGRAM", "OUTPUT", "PROB")]
trigrams  <- trigrams[, names(trigrams) %in% c("NGRAM", "OUTPUT", "PROB")]
fourgrams <- fourgrams[, names(fourgrams) %in% c("NGRAM", "OUTPUT", "PROB")]

unigrams  <- numerify(1, unigrams)
bigrams   <- numerify(2, bigrams)
trigrams  <- numerify(3, trigrams)
fourgrams <- numerify(4, fourgrams)

context   <- read.csv("context.csv", sep=" ", stringsAsFactors=FALSE, fileEncoding="latin1")
context   <- numerify(0, context)

stopwords        <- data.frame(unique(readLines("stopwords.txt")), stringsAsFactors = FALSE)
names(stopwords) <- c("OUTPUT")

sentence <- "The connections in my brain are "

ALPHA                   <- 0.4
WEIGHT_NGRAMS           <- 1
WEIGHT_CONTEXT          <- 0.1

options <- predict()
View(options)

# Total time of execution. 
total.time <- Sys.time() - start.time
total.time