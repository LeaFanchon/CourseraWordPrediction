
getFileName <- function(ngram) {
  
  file <- ""
  
  if       (ngram == 1) { file <- "ngrams_1_uni.csv" }
  else if  (ngram == 2) { file <- "ngrams_2_bi.csv" }
  else if  (ngram == 3) { file <- "ngrams_3_tri.csv" }
  else if  (ngram == 4) { file <- "ngrams_4_fou.csv" }
  
  file
}


#############################################
# Get probabilities for each ngram          #
#############################################

computeCounts <- function(unseen, ngram) {

    # Load data set
    file <- getFileName(ngram)
    set_n <- read.csv(file, sep=";", stringsAsFactors=FALSE, fileEncoding="latin1")
    set_n$COUNT <- as.numeric(set_n$COUNT)
    
    
    if (ngram==1) {
       
        S               <- sum(set_n$COUNT)
        set_n$PROB      <- set_n$COUNT / S 
        set_n$NEWCOUNT  <- set_n$COUNT
        set_n$REALCOUNT <- set_n$COUNT
        set_n           <- set_n[ , !(names(set_n) %in% c("COUNT"))]
         
        write.csv2(x=set_n, file = paste("counts", file, sep="_"), quote=FALSE, row.names=FALSE)
      
        rm(list=c("set_n")) 
        
    } else {
      
        # Remove some of the count to take into account the missing ngrams
        set_n$REALCOUNT <- set_n$COUNT
        set_n$NEWCOUNT  <- set_n$COUNT - unseen
        set_n           <- set_n[ , !(names(set_n) %in% c("COUNT"))]
        
        # Load the (n-1)gram data set
        set_n_minus_1           <- read.csv(paste("counts", getFileName(ngram-1), sep="_"), sep=";", stringsAsFactors=FALSE, fileEncoding="latin1")
        set_n_minus_1$REALCOUNT <- as.numeric(gsub(",", ".", set_n_minus_1$REALCOUNT))
        
        if (ngram-1>1) {
          set_n_minus_1$NGRAM <- paste(set_n_minus_1$NGRAM, set_n_minus_1$OUTPUT, sep="_")
          drops <- c("OUTPUT", "PROB", "NEWCOUNT")
        } else {
          drops <- c("PROB", "NEWCOUNT")
        }
        
        set_n_minus_1       <- set_n_minus_1[ , !(names(set_n_minus_1) %in% drops)]
        names(set_n_minus_1)<-c("NGRAM", "REALCOUNT_MINUS_1")
        
        set       <- left_join(set_n, set_n_minus_1, by = "NGRAM") 
        set$PROB  <- set$NEWCOUNT / set$REALCOUNT_MINUS_1
        set       <- set[, !names(set) %in% c("REALCOUNT_MINUS_1")]
          
        write.csv2(x=set, file = paste("counts", file, sep="_"), quote=FALSE, row.names=FALSE)
        
        rm(list=c("set_n", "set_n_minus_1", "set"))
 
        
    }
      
}


library(quanteda)
library(reshape2)
library(dplyr)
library(sqldf)

options(digits=16)

# Time the code execution
start.time <- Sys.time()

computeCounts(0.5, 1)
computeCounts(0.5, 2)
computeCounts(0.5, 3)
computeCounts(0.5, 4)

# Total time of execution. 
total.time <- Sys.time() - start.time
total.time

