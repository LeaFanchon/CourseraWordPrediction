#-----------------------------------------------------------------------------------
# Create a matrix M(n,n) where each column and each line is a token.
# For each line, we call w_i the word of the line. 
# In each column, we store the number of times the word w_j of the column 
# appeared in the same document as the word w_i of the line.
#-----------------------------------------------------------------------------------
  
process <- function(chunk) {
  
  tokens     <- tokens(chunk, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)
  
  tokens     <- tokens_tolower(tokens)
  
  # Keep only pre-selected tokens
  tokens     <- tokens_keep(tokens, unigrams$NGRAM)
  
  # Remove stop words (https://www.ranks.nl/stopwords)
  stopwords  <- readLines("stopwords.txt")
  tokens     <- tokens_remove(tokens, stopwords)
  
  tokens_dfm <- dfm(tokens, tolower = FALSE)
  
  # Compute context
  
  tokens_df  <- t(as.data.frame(tokens_dfm))
  
  tokens_df        <- melt(tokens_df)
  names(tokens_df) <- c("Token", "Document", "Count")
  
  # tokens_df$Token  <- as.character(tokens_df$Token)
  tokens_df$Count  <- as.numeric(tokens_df$Count)
  tokens_df        <- tokens_df[tokens_df$Count > 0, ]

  
  if(file.exists(fileContext)){
    
      # Read existing file
      context <- read.csv(fileContext, sep=" ", fileEncoding="latin1")
      
      context$Count.x       <- as.numeric(context$Count.x)
      context$Count.y       <- as.numeric(context$Count.y)
      
      df         <- tokens_df
      df         <- inner_join(df, tokens_df, by=c("Document"))
      df         <- df[ , names(df) != "Document"]
      df         <- aggregate(. ~ Token.x + Token.y, data = df, FUN = "sum")
      df         <- df[df$Token.x != df$Token.y, ]
      df         <- df[df$Count.x>param & df$Count.y>param, ]

      # Join with Context. First align factor levels

      combined.x <- sort(union(levels(df$Token.x), levels(context$Token.x)))
      combined.y <- sort(union(levels(df$Token.y), levels(context$Token.y)))
      df         <- mutate(df, Token.x=factor(Token.x, levels=combined.x))
      df         <- mutate(df, Token.y=factor(Token.y, levels=combined.y))
      context    <- mutate(context, Token.x=factor(Token.x, levels=combined.x))
      context    <- mutate(context, Token.y=factor(Token.y, levels=combined.y))

      rm(list = c("combined.x", "combined.y"))

      df         <- left_join(df, context, by=c("Token.x", "Token.y"))

      df[is.na(df)] <- 0

      names(df ) <- c("Token.x", "Token.y", "Count.x.df",
                      "Count.y.df",  "Count.x.ctxt", "Count.y.ctxt")

      df$Count.x.df         <- as.numeric(df$Count.x.df)
      df$Count.y.df         <- as.numeric(df$Count.y.df)
      df$Count.x.ctxt       <- as.numeric(df$Count.x.ctxt)
      df$Count.y.ctxt       <- as.numeric(df$Count.y.ctxt)

      df$Count.x       <- df$Count.x.df + df$Count.x.ctxt
      df$Count.y       <- df$Count.y.df + df$Count.y.ctxt

      drops       <- c("Count.x.df",
                       "Count.y.df",
                       "Count.x.ctxt",
                       "Count.y.ctxt")

      df          <- df[, !names(df) %in% drops]
      
      complementary    <- anti_join(context, df, by=c("Token.x", "Token.y"))
      df               <- rbind(df, complementary)

      write.table(x=df, fileContext, append = FALSE, quote=FALSE, col.names = TRUE, row.names=FALSE)
      
      
  } else {
    
    # File doesn't exist yet.
    
    unique <- unique(tokens_df$Token)
    append <- FALSE
    
    df         <- tokens_df
    df         <- inner_join(df, tokens_df, by=c("Document"))
    df         <- df[ , names(df) != "Document"]
    df         <- aggregate(. ~ Token.x + Token.y, data = df, FUN = "sum")
    df         <- df[df$Token.x != df$Token.y, ]
    df         <- df[df$Count.x>param & df$Count.y>param,]
  
    write.table(x=df, fileContext, append = FALSE, quote=FALSE, col.names = TRUE, row.names=FALSE)
    
  }
}

countSort <- function(){
  
  # Read existing file
  context    <- read.csv(fileContext, sep=" ", stringsAsFactors=FALSE, fileEncoding="latin1")
  
  context$Count.x        <- as.numeric(context$Count.x)
  context$Count.y        <- as.numeric(context$Count.y)
  context$Token.x        <- as.character(context$Token.x)
  context$Token.y        <- as.character(context$Token.y)
  
  # names(unigrams) =  "NGRAM"     "PROB"   "NEWCOUNT   "REALCOUNT"   
  df_counts              <- unigrams[, names(unigrams) %in% c("NGRAM", "REALCOUNT")]
  
  names(df_counts)       <- c("Token.x", "GlobalCount.x")
  context                <- inner_join(context, df_counts, by=c("Token.x"))
  
  names(df_counts)        <- c("Token.y", "GlobalCount.y")
  context                 <- inner_join(context, df_counts, by=c("Token.y"))
  context_tmp$PROBCONTEXT <- (context_tmp$Count.y + context_tmp$Count.x)
  context_tmp$PROBCONTEXT <- context_tmp$PROBCONTEXT/(context_tmp$GlobalCount.y* coef + context_tmp$GlobalCount.x* coef)
  
  drops                   <- c("Prob.x", "Prob.y", "Count.x", "Count.y", "GlobalCount.x", "GlobalCount.y")
  context                 <- context[, !names(context) %in% drops]
  context                 <- context[order(context$Token.x, context$Token.y),]
  
  write.table(x=context, fileContext, append = FALSE, quote=FALSE, col.names = TRUE, row.names=FALSE)
  
}

library(quanteda)
library(reshape2)
library(dplyr)
library(sqldf)

options(digits=16)

# Time the code execution
start.time <- Sys.time()

unigrams  <- read.csv("counts_ngrams_1_uni.csv", sep=";", stringsAsFactors=FALSE, fileEncoding="latin1")

coef        <- 1
start       <- 1
block       <- 5000
fileContext <- "Context.csv"
param       <- 1

####################################
# TWITTER
####################################

print("TWITTER")

# Read the file
twitter <- readLines("train_twitter.txt")

# Get length
t_l <- length(twitter)

ind_twi   <- sample(1:t_l, trunc(t_l*coef))
twitter   <- twitter[ind_twi]

# Remove too long elements to have equivalent chunks 
twitter         <- twitter[nchar(twitter)<=300]

print(length(twitter))

while(start<=length(twitter)) {
  print(start)
  end <- min(start+block-1, length(twitter))
  chunk <- twitter[start:end]
  process(chunk)
  start <- end + 1
}

rm(list=c("twitter", "chunk"))


####################################
# BLOGS
####################################

print("BLOGS")

start       <- 1
block       <- 2000

# Read the file
blogs <- readLines("train_blogs.txt")

# Get length
t_l <- length(blogs)

ind_blogs   <- sample(1:t_l, trunc(t_l*coef))
blogs   <- blogs[ind_blogs]

# Remove too long elements to have equivalent chunks 
blogs         <- blogs[nchar(blogs)<=300]

print(length(blogs))

while(start<=length(blogs)) {
  print(start)
  end <- min(start+block-1, length(blogs))
  chunk <- blogs[start:end]
  process(chunk)
  start <- end + 1
}

rm(list=c("blogs", "chunk"))


####################################
# NEWS
####################################

print("NEWS")

start       <- 1

# Read the file
news <- readLines("train_news.txt")

# Get length
t_l <- length(news)

ind_news   <- sample(1:t_l, trunc(t_l*coef))
news   <- news[ind_news]

# Remove too long elements to have equivalent chunks 
news         <- news[nchar(news)<=300]

print(length(news))

while(start<=length(news)) {
  print(start)
  end <- min(start+block-1, length(news))
  chunk <- news[start:end]
  process(chunk)
  start <- end + 1
}

rm(list=c("news", "chunk"))

countSort()

# Total time of execution. 
total.time <- Sys.time() - start.time
total.time

