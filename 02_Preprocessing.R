#######################
# Get counts          #
#######################

mostfrequent <- function(dfm) {
  df <- as.data.frame(colSums(dfm))
  df <- melt(t(df))
  df <- df[, names(df) != "Var1"]
  names(df) <- c("NGRAM", "COUNT")
  df
}

#######################
# Update file         #
#######################

update <- function(freq, prefix) {
  
  file <- paste(prefix, "csv", sep=".")
  
  if(file.exists(file)){
    
      set       <- read.csv(file, sep=";", fileEncoding="latin1")
      
      set$COUNT <- as.numeric(set$COUNT)
      
      if ("OUTPUT" %in% names(set)){
        by <- c("NGRAM", "OUTPUT")
        
        combined.NGRAM  <- sort(union(levels(set$NGRAM), levels(freq$NGRAM)))
        combined.OUTPUT <- sort(union(levels(set$OUTPUT), levels(freq$OUTPUT)))
        set         <- mutate(set, NGRAM=factor(NGRAM, levels=combined.NGRAM))
        set         <- mutate(set, OUTPUT=factor(OUTPUT, levels=combined.OUTPUT))
        freq        <- mutate(freq, NGRAM=factor(NGRAM, levels=combined.NGRAM))
        freq        <- mutate(freq, OUTPUT=factor(OUTPUT, levels=combined.OUTPUT))
        
        
      } else {
        by <- c("NGRAM")
        
        combined.NGRAM  <- sort(union(levels(set$NGRAM), levels(freq$NGRAM)))
        set         <- mutate(set, NGRAM=factor(NGRAM, levels=combined.NGRAM))
        freq        <- mutate(freq, NGRAM=factor(NGRAM, levels=combined.NGRAM))
      }
      
      
      
      set             <- full_join(set, freq, by = by) 
      set[is.na(set)] <- 0
      set$COUNT.x[is.nan(set$COUNT$x)] <- 0
      set$COUNT.y[is.nan(set$COUNT$y)] <- 0
      set$COUNT       <- set$COUNT.x + set$COUNT.y
      drops           <- c("COUNT.x","COUNT.y")
      set             <- set[ , !(names(set) %in% drops)]
      
      set             <- set[set$COUNT>1, ]
      write.csv2(x=set, file = file, quote=FALSE, row.names=FALSE)
    
  } else {
      freq             <- freq[freq$COUNT>1, ]
      write.csv2(x=freq, file = file, quote=FALSE, row.names=FALSE)
    
  }
}


#######################
# Add chunk to model  #
#######################

addToModel <- function(chunk) {
  
  # Tokenize the training set
  tokens <- tokens(chunk, what = "word", 
            remove_numbers = TRUE, remove_punct = TRUE,
            remove_symbols = TRUE, remove_hyphens = TRUE)
  
  tokens <- tokens_tolower(tokens)
  
  # Remove bad words 
  bw <- "full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt"
  badwords   <- readLines(bw)
  tokens <- tokens_select(tokens, badwords, selection = "remove")
  
  # Remove unwanted tokens 
  tokens <- tokens_select(tokens, ".*[#@].*", selection = "remove", valuetype = "regex")
  tokens <- tokens_select(tokens, ".*<.*>.*", selection = "remove", valuetype = "regex")
  tokens <- tokens_select(tokens, "", selection = "remove", valuetype = "fixed")
  
  # Unigrams
  tokens_dfm <- dfm(tokens, tolower = FALSE)
  freq       <- mostfrequent(tokens_dfm)
  freq$NGRAM <- as.factor(freq$NGRAM)
  
  # Update file
  update(freq, "ngrams_1_uni")
  rm(list=c("freq", "tokens_dfm"))
  
  # Bigrams
  tokens_bi       <- tokens_ngrams(tokens, n = 2)
  tokens_dfm_bi   <- dfm(tokens_bi, tolower = FALSE)
  freq_bi         <- mostfrequent(tokens_dfm_bi)
  freq_bi$NGRAM   <- as.character(freq_bi$NGRAM)
  freq_bi$OUTPUT  <- sapply(freq_bi$NGRAM, function(x) strsplit(x,"_")[[1]][2])
  freq_bi$NGRAM   <- sapply(freq_bi$NGRAM, function(x) strsplit(x,"_")[[1]][1])
  freq_bi$OUTPUT  <- as.factor(freq_bi$OUTPUT)
  freq_bi$NGRAM   <- as.factor(freq_bi$NGRAM)
  
  #Update file
  update(freq_bi, "ngrams_2_bi")
  rm(list=c("freq_bi", "tokens_dfm_bi"))


  # Trigrams
  tokens_tri      <- tokens_ngrams(tokens, n = 3)
  tokens_dfm_tri  <- dfm(tokens_tri, tolower = FALSE)
  freq_tri        <- mostfrequent(tokens_dfm_tri)
  freq_tri$NGRAM  <- as.character(freq_tri$NGRAM)
  freq_tri$OUTPUT <- sapply(freq_tri$NGRAM, function(x) strsplit(x,"_")[[1]][3])
  freq_tri$NGRAM  <- sapply(freq_tri$NGRAM, function(x)
                     paste(strsplit(x,"_")[[1]][1], strsplit(x,"_")[[1]][2], sep="_"))
  freq_tri$OUTPUT <- as.factor(freq_tri$OUTPUT)
  freq_tri$NGRAM  <- as.factor(freq_tri$NGRAM)
  
  #Update file
  update(freq_tri, "ngrams_3_tri")
  rm(list=c("freq_tri", "tokens_dfm_tri"))
   
  
  # Quadrigrams
  tokens_fou      <- tokens_ngrams(tokens, n = 4)
  tokens_dfm_fou  <- dfm(tokens_fou, tolower = FALSE)
  freq_fou        <- mostfrequent(tokens_dfm_fou)
  freq_fou$NGRAM  <- as.character(freq_fou$NGRAM)
  freq_fou$OUTPUT <- sapply(freq_fou$NGRAM, function(x) strsplit(x,"_")[[1]][4])
  freq_fou$NGRAM  <- sapply(freq_fou$NGRAM, function(x)
                     paste(paste(strsplit(x,"_")[[1]][1], strsplit(x,"_")[[1]][2], sep="_"),
                           strsplit(x, "_")[[1]][3], sep="_"))
  freq_fou$OUTPUT <- as.factor(freq_fou$OUTPUT)
  freq_fou$NGRAM  <- as.factor(freq_fou$NGRAM)
  
  #Update file
  update(freq_fou, "ngrams_4_fou")
  rm(list=c("freq_fou", "tokens_dfm_fou"))
  
}


#######################################################
# Read file and split it between train and test sets  #
#######################################################

readfile <- function(filename, name) {
  
    # Read the file
    set <- readLines(filename, encoding="UTF-8", skipNul = TRUE)
    l <- length(set)

    # Split the set into a training set and a test set
    ind       <- sample(1:l, trunc(l*0.7))
    train_set <- set[ind]
    test_set  <- set[-ind]

    # Save this to a file
    write(x=train_set, file = paste("train", paste(name, "txt", sep="."), sep="_"))
    write(x=test_set, file = paste("test", paste(name, "txt", sep="."), sep="_"))

    rm(list=c("test_set", "set"))
  
    train_set
}    
    

getFileName <- function(ngram) {
  
  file <- ""
  
  if       (ngram == 1) { file <- "ngrams_1_uni.csv" }
  else if  (ngram == 2) { file <- "ngrams_2_bi.csv" }
  else if  (ngram == 3) { file <- "ngrams_3_tri.csv" }
  else if  (ngram == 4) { file <- "ngrams_4_fou.csv" }
  
  file
}


#######################################################
# Process the file chunk by chunk                     #
#######################################################

processfile <- function(filename, name) {

  train_set <- readfile(filename, name)
  
  train_set <- gsub(pattern="[^[:alpha:]]", train_set, replacement=" ")
  
  start <- 1
  block <- 50000
  
  while(start<=length(train_set)) {
  #while(start<=50002) {
    end <- min(start+block-1, nrow(train_set))
    chunk <- train_set[start:end]
    addToModel(chunk)
    start <- end + 1
  }
  
  rm(list=c("train_set", "chunk"))
  
}


library(quanteda)
library(reshape2)
library(dplyr)

set.seed(1234)

# Time the code execution
start.time <- Sys.time()

processfile("en_US.blogs.txt", "blogs")
processfile("en_US.news.txt", "news")
processfile("en_US.twitter.txt", "twitter")

  
# Sort files
for(i in 1:4) {
    set <- read.csv(getFileName(i), sep=";", stringsAsFactors=FALSE, fileEncoding="latin1")
    set$COUNT <- as.numeric(set$COUNT)
    set <- set[order(set$NGRAM),]
    write.csv2(x=set, file = getFileName(i), quote=FALSE, row.names=FALSE)
}

rm(list=c("set"))

# Total time of execution. 
total.time <- Sys.time() - start.time
total.time
