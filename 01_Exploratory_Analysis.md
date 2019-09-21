# Capstone - Week 2"

## I. Pre-processing of the data

Pre-processing steps:  

1. Load the data into R,   

2. Separate each data set into a training set (70%) and a test set (30%)

2. Tokenize the data sets,   

3. Remove bad words,  

4. Get n-grams

5. Create document-frequency matrixes


Note: to perform this analysis, we chose to pick the same number of observations from each dataset, for performance reasons, and because the size of the dataset has an influence on the answer to Coursera's question about coverage ("how many words are necessary to cover 50% of the dataset?"). Obviously, for a very small dataset, very few words are needed, whereas more are needed for a bigger one. To make meaningful comparisons, we chose to explore the same number of observations in each context (Twitter, News and Blogs).

<br>

```{r pretreatment1}

set.seed(1234)

# Load the needed libraries
library(quanteda)
library(reshape2)
library(ggplot2)
library(plyr)
library(gridExtra)
library(caret)

# Time the code execution
start.time <- Sys.time()

# Get in the right directory
setwd("en_US")

# Define the color code for plots
color_1grams <- "lightgreen"
color_2grams <- "pink"
color_3grams <- "cornflowerblue"

# Read the files
twitter <- readLines("en_US.twitter.txt", encoding="UTF-8")
news    <- readLines("en_US.news.txt", encoding="UTF-8")
blogs   <- readLines("en_US.blogs.txt", encoding="UTF-8")

# Get lengths of three sets
t_l <- length(twitter)
n_l <- length(news)
b_l <- length(blogs)

# Get sizes of three sets
t_s <- object.size(twitter)
n_s <- object.size(news)
b_s <- object.size(blogs)

tab <- as.data.frame(cbind(c("Twitter", "News", "Blogs"), c(t_l, n_l, b_l), c(t_s, n_s, b_s)))
names(tab) <- c("Context", "Length", "Size in Mb")
print(tab)

```


```{r pretreatment2}

# Split each set into a training set and a test set
ind_twi <- sample(1:t_l, trunc(t_l*0.7))
twitter <- twitter[ind_twi]
twitter_test <- twitter[-ind_twi]

ind_news <- sample(1:n_l, trunc(n_l*0.7))
news <- news[ind_news]
news_test <- news[-ind_news]

ind_blogs <- sample(1:b_l, trunc(b_l*0.7))
blogs <- blogs[ind_blogs]
blogs_test <- blogs[-ind_blogs]

# Get lengths of training set
twitter_l <- length(twitter)
news_l    <- length(news)
blogs_l   <- length(blogs)

l <- min(twitter_l, news_l, blogs_l)

# Select only the same number of observations out of each data frame
twitter <- twitter[sample(twitter_l, l)]
news    <- news[sample(news_l, l)]
blogs   <- blogs[sample(blogs_l, l)]

# Merge all vectors to get all the data in the same one
all <- append(twitter, append(news, blogs))

# Tokenize the training set
twitter_tokens <- tokens(twitter, what = "word", 
                     remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_hyphens = TRUE)

news_tokens    <- tokens(news, what = "word", 
                     remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_hyphens = TRUE)

blogs_tokens   <- tokens(blogs, what = "word", 
                     remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_hyphens = TRUE)

all_tokens     <- tokens(all, what = "word", 
                     remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_hyphens = TRUE)

twitter_tokens <- tokens_tolower(twitter_tokens)
news_tokens    <- tokens_tolower(news_tokens)
blogs_tokens   <- tokens_tolower(blogs_tokens)
all_tokens     <- tokens_tolower(all_tokens)

# Remove bad words found here:
# https://www.freewebheaders.com/full-list-of-bad-words-banned-by-google/
bw <- "full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt"
badwords   <- readLines(bw)
twitter_tokens <- tokens_select(twitter_tokens, badwords, selection = "remove")
news_tokens    <- tokens_select(news_tokens, badwords, selection = "remove")
blogs_tokens   <- tokens_select(blogs_tokens, badwords, selection = "remove")
all_tokens     <- tokens_select(all_tokens, badwords, selection = "remove")

# Get bigrams and trigrams
twitter_tokens_bi <- tokens_ngrams(twitter_tokens, n = 2)
news_tokens_bi    <- tokens_ngrams(news_tokens, n = 2)
blogs_tokens_bi   <- tokens_ngrams(blogs_tokens, n = 2)
all_tokens_bi     <- tokens_ngrams(all_tokens, n = 2)

twitter_tokens_tri <- tokens_ngrams(twitter_tokens, n = 3)
news_tokens_tri    <- tokens_ngrams(news_tokens, n = 3)
blogs_tokens_tri   <- tokens_ngrams(blogs_tokens, n = 3)
all_tokens_tri     <- tokens_ngrams(all_tokens, n = 3)

# Create document frequency matrixes.
twitter_tokens_dfm <- dfm(twitter_tokens, tolower = FALSE)
twitter_tokens_dfm_bi <- dfm(twitter_tokens_bi, tolower = FALSE)
twitter_tokens_dfm_tri <- dfm(twitter_tokens_tri, tolower = FALSE)

news_tokens_dfm <- dfm(news_tokens, tolower = FALSE)
news_tokens_dfm_bi <- dfm(news_tokens_bi, tolower = FALSE)
news_tokens_dfm_tri <- dfm(news_tokens_tri, tolower = FALSE)

blogs_tokens_dfm <- dfm(blogs_tokens, tolower = FALSE)
blogs_tokens_dfm_bi <- dfm(blogs_tokens_bi, tolower = FALSE)
blogs_tokens_dfm_tri <- dfm(blogs_tokens_tri, tolower = FALSE)

all_tokens_dfm <- dfm(all_tokens, tolower = FALSE)
all_tokens_dfm_bi <- dfm(all_tokens_bi, tolower = FALSE)
all_tokens_dfm_tri <- dfm(all_tokens_tri, tolower = FALSE)

```

Number of observations in each of the three data sets : **`r l`**.

<br>

## II. Exploratory analysis

<br>

### 1. Most frequent n-grams and frequency distributions

Let us first find the most frequent words, 2-grams and 3-grams, per data source and overall.

```{r ex11}

mostfrequent <- function(dfm, name) {
  df <- as.data.frame(colSums(dfm))
  df <- melt(t(df))
  df <- df[, names(df) != "Var1"]
  names(df) <- c(name, "COUNT")
  df <- df[with(df, order(-COUNT)), ]
  df
}

# Get the 10 most frequent 1-grams, 2-grams and 3-grams for each data source
freq_twi <- mostfrequent(twitter_tokens_dfm, "WORD")
freq_twi_bi <- mostfrequent(twitter_tokens_dfm_bi, "BIGRAM")
freq_twi_tri <- mostfrequent(twitter_tokens_dfm_tri, "TRIGRAM")

freq_news <- mostfrequent(news_tokens_dfm, "WORD")
freq_news_bi <- mostfrequent(news_tokens_dfm_bi, "BIGRAM")
freq_news_tri <- mostfrequent(news_tokens_dfm_tri, "TRIGRAM")

freq_blogs <- mostfrequent(blogs_tokens_dfm, "WORD")
freq_blogs_bi <- mostfrequent(blogs_tokens_dfm_bi, "BIGRAM")
freq_blogs_tri <- mostfrequent(blogs_tokens_dfm_tri, "TRIGRAM")

# Get the 10 most frequent 1-grams, 2-grams and 3-grams overall
frequencies <- mostfrequent(all_tokens_dfm, "WORD")
frequencies_bi <- mostfrequent(all_tokens_dfm_bi, "BIGRAM")
frequencies_tri <- mostfrequent(all_tokens_dfm_tri, "TRIGRAM")

```


<br>

##### The 10 most frequent n-grams are, with their counts: 

```{r ex12}


plotmostfrequent <- function(df, df_bi, df_tri, title) {
  
      hf <- head(df, 10)
      t1 <- ggplot(data = hf, aes(x=reorder(WORD, -COUNT), y=COUNT))  +
                        geom_bar(stat="identity", fill=color_1grams) +
                        labs(x = "Word", y="Count", 
                             title = paste("The 10 most frequent words", title, sep=" ")) +
                        scale_y_continuous(labels = scales::comma)
      hf_bi <- head(df_bi, 10)
      
      t2 <- ggplot(data = hf_bi, aes(x=reorder(BIGRAM, -COUNT), y=COUNT) ) +
                        geom_bar(stat="identity", fill=color_2grams) +
                        labs(x = "Bigrams", y="Count", 
                             title = paste("The 10 most frequent bi-grams", title, sep=" ")) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                        scale_y_continuous(labels = scales::comma)
      
      hf_tri <- head(df_tri, 10)
      t3 <- ggplot(data = hf_tri, aes(x=reorder(TRIGRAM, -COUNT), y=COUNT))  +
                        geom_bar(stat="identity", fill=color_3grams) +
                        labs(x = "Trigrams", y="Count", 
                             title = paste("The 10 most frequent tri-grams", title, sep=" ")) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                        scale_y_continuous(labels = scales::comma)
      
      grid.arrange(t1, t2, t3, ncol=3, nrow=1)
  
}

plotmostfrequent(freq_twi, freq_twi_bi, freq_twi_tri, "in Twitter")
plotmostfrequent(freq_news, freq_news_bi, freq_news_tri, "in News")
plotmostfrequent(freq_blogs, freq_blogs_bi, freq_blogs_tri, "in Blogs")
plotmostfrequent(frequencies, frequencies_bi, frequencies_tri, "overall")


```

<br>

##### Let us now plot the distribution of ngram frequencies overall the data sets. 

```{r ex13}

getfreqdis <- function(df, breaks, names, title, color) {
  
  Frequency  <- c(sum(df$COUNT==1))
  i <- 1
  
  while(i < length(breaks)) {
    Frequency <- c(Frequency, sum(df$COUNT>breaks[i] & df$COUNT<=breaks[i+1]))
    i <- i + 1
  }
  Frequency <- c(Frequency, sum(df$COUNT>breaks[i]))
  Interval <- names
  distr_fq <- as.data.frame(cbind(Interval, Frequency))
  names(distr_fq) <- c("Interval", "Frequency")
  
  distr_fq <- transform(distr_fq, Frequency = as.numeric(as.character(Frequency)))
  
  ggplot(data = distr_fq, aes(x=reorder(Interval, - Frequency), y=c(Frequency) )) +
                 geom_bar(stat="identity", fill=color) +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                 labs(x = "Interval", y="Frequency", title = title) +
                 scale_y_continuous(labels = scales::comma)
}

names <- c("Ones", "Between 2 And 10", "Between 11 And 50",
           "Between 51 And 100", "Between 101 And 1000","Above 1001")

breaks <- c(1, 10, 50, 100, 1000)

p_uni <- getfreqdis(frequencies, breaks, names, "Words", color_1grams)
p_bi <-  getfreqdis(frequencies_bi, breaks, names, "Bigrams", color_2grams)
p_tri <- getfreqdis(frequencies_tri, breaks, names, "Trigrams", color_3grams)

grid.arrange(p_uni, p_bi, p_tri, nrow=1, ncol=3)


```

<br>

##### From these frequency distribution plots, we can see that :   

1. The most frequent frequency is one for 1, 2 and 3-grams, meaning that most words, 2-grams and 3-grams appear only one time. The scarcity of n-grams increases with n : it is even likelier for a 3-gram to appear only one time in the data set than it is for words or 2-grams.  

2. The context does influence the list of the most frequent words. In Twitter, "I love you" is one of the most frequent trigrams, reflecting the fact that the context is rather personal. In the News data set, the impersonal trigrams ("according to the", "going to be", etc) show a content probably more influenced by current general / political / topical events. Similarly, the words "you" and "I" are absent from the most frequent words of the News data set, but present in Twitter; "I" is in Blogs' top ten, but not "you". We could use this to rank the data sources from the most to the least personal : Twitter - Blogs - News. 

<br>

### 2. Coverage

##### How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?

```{r ex31}

getCoverage <- function(df, percentage) {
  total <- sum(df$COUNT)
  i <- 1
  sum <- 0
  coverage <- 0
  while (i < nrow(df) && coverage < percentage) 
  {
     sum <- sum + df[i, 2]
     coverage <- (sum / total)*100
     i <- i + 1
  }
  i
}

per50_twi <- getCoverage(freq_twi, 50)
per90_twi <- getCoverage(freq_twi, 90)

per50_news <- getCoverage(freq_news, 50)
per90_news <- getCoverage(freq_news, 90)

per50_blogs <- getCoverage(freq_blogs, 50)
per90_blogs <- getCoverage(freq_blogs, 90)

per50_all <- getCoverage(frequencies, 50)
per90_all <- getCoverage(frequencies, 90)

``` 


In Twitter :  

- **`r per50_twi`** words are needed to cover 50% of all word instances.   

- **`r per90_twi`** words are needed to cover 90% of all word instances.   


In News :    

- **`r per50_news`** words are needed to cover 50% of all word instances.   

- **`r per90_news`** words are needed to cover 90% of all word instances.   


In Blogs :      

- **`r per50_blogs`** words are needed to cover 50% of all word instances.   

- **`r per90_blogs`** words are needed to cover 90% of all word instances.   

<br>
  
##### From this, we understand that the vocabulary used in the News is richer than in Twitter posts or blogs, since more words are needed to get a similar coverage. 
  
<br>
  
Overall : 

- **`r per50_all`** words are needed to cover 50% of all word instances.   

- **`r per90_all`** words are needed to cover 90% of all word instances.   

<br>

##### How do you evaluate how many of the words come from foreign languages?

To evaluate which words are foreign, we could :     

- The obvious: get dictionaries of foreign words.  

- Get way more data in english, to the point where the most frequent frequency is not one. The words that would then appear only one time or significantly less often than others could be marked as potentially foreign.  

- To get a better coverage, we could also stem the tokens of our dataset. For instance, "mother", "motherhood" and "motherly" could all be stemmed as "mother". If a new stemmed word does not fit with any of the stems in the data set, it is likely foreign.  

<br>

##### Can you think of a way to increase the coverage of the model?

Aim : Identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases.  

- As said above, stemming could increase the coverage. But it would not necessarily be a good idea for our goal of predicting the next word. When a word is stemmed, it looses its grammatical function, which is crucial to predict what's coming next. 

- When an unknown word is given to the model, we still must predict what comes next. A possibility for that is to take the very rare words in the corpora and turn them into one symbol. We include this symbol in the model, and replace any incoming word we don't know by it.  

Note: When trying to predict the next word, we must keep in mind that the correct prediction might not be in our corpora. We'll have to implement some algorithm (Kneser-Ney or Stupid Backoff) to acknowledge the probability that the upcoming word is not in our base.


<br>

## III. Basic n-gram model

<br>

### 1. Preliminary questions

<br>

##### How can you efficiently store an n-gram model (think Markov Chains)?

For each n>1, we store all the n-grams of the training set in the following way.   

All the n-grams starting with the same n-1 tokens are stored on the same line.

The dataframe / file / data structure contains two columns:

- The (n-1)gram formed by the n-1 first tokens of the n-gram.

- An array containing all the last tokens of the corresponding n-grams with their counts.

For instance, if we have the following 4-grams:  

- "i want to do"   : 1 time  

- "i want to have" : 2 times  

- "i want to make" : 1 time    

- "i think I will" : 1 time  

They will be stored that way, sorted in alphabetical order :  

- **(n-1)GRAM       -      Possible outcomes ** 
   
- "i think i"  - [("will", 1)]  

- "i want to"  - [("have", 2), ("do", 1), ("make", 1)]  



<br>

##### How can you use the knowledge about word frequencies to make your model smaller and more efficient?

We could :  

- Store only the n-grams appearing at least two times  

- For each line, sort the array (column **Possible outcomes**) mentioned in the question above by decreasing count.

<br>

##### How many parameters do you need (i.e. how big is n in your n-gram model)?

I chose to consider 1 to 4-grams.

<br>

##### Can you think of simple ways to "smooth" the probabilities (think about giving all n-grams a non-zero probability even if they aren't observed in the data) ?

We could implement a Katz back-off model. Not done yet but in future assignments. 

<br>

##### How do you evaluate whether your model is any good?

By trying it on the test set. 

<br>

##### How can you use backoff models to estimate the probability of unobserved n-grams?

As said above.

<br>

### 2. Create a basic model

For now, we will just create a very basic ngram model as follows:

```{r model1}

# Get 4-grams
all_tokens_fou     <- tokens_ngrams(all_tokens, n = 4)
all_tokens_dfm_fou <- dfm(all_tokens_fou, tolower = FALSE)
frequencies_fou    <- mostfrequent(all_tokens_dfm_fou, "QUADRIGRAM")

# Force strings
frequencies$WORD           <- as.character(frequencies$WORD)
frequencies_bi$BIGRAM      <- as.character(frequencies_bi$BIGRAM)
frequencies_tri$TRIGRAM    <- as.character(frequencies_tri$TRIGRAM)
frequencies_fou$QUADRIGRAM <- as.character(frequencies_fou$QUADRIGRAM)

# Sort the dataframes by alphabetical order
frequencies_bi  <- frequencies_bi[order(frequencies_bi$BIGRAM),] 
frequencies_tri <- frequencies_tri[order(frequencies_tri$TRIGRAM),] 
frequencies_fou <- frequencies_fou[order(frequencies_fou$QUADRIGRAM),]

n <- c("NGRAM", "COUNT")
names(frequencies_bi) <- n
names(frequencies_tri) <- n
names(frequencies_fou) <- n

# Split the n-grams into two columns: (n-1)grams and output 
frequencies_bi$OUTPUT  <- sapply(frequencies_bi$NGRAM, function(x) strsplit(x,"_")[[1]][2])
frequencies_tri$OUTPUT <- sapply(frequencies_tri$NGRAM, function(x) strsplit(x,"_")[[1]][3])
frequencies_fou$OUTPUT <- sapply(frequencies_fou$NGRAM, function(x) strsplit(x,"_")[[1]][4])

# Split the ngrams into the first (n-1)grams and the last token
frequencies_bi$NGRAM  <-  sapply(frequencies_bi$NGRAM, function(x) strsplit(x,"_")[[1]][1])

frequencies_tri$NGRAM <-  sapply(frequencies_tri$NGRAM, function(x) 
                              paste(strsplit(x,"_")[[1]][1], strsplit(x,"_")[[1]][2], sep="_"))

frequencies_fou$NGRAM <-  sapply(frequencies_fou$NGRAM, function(x) 
                               paste(paste(strsplit(x,"_")[[1]][1], strsplit(x,"_")[[1]][2], sep="_"),
                               strsplit(x, "_")[[1]][3], sep="_"))


basic <- function(sentence){
  
  # Tokenize in the same way
  tokens <- tokens(sentence, what = "word", 
                     remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_hyphens = TRUE)
  
  tokens <- tokens_tolower(tokens)

  # Get bigrams and trigrams
  tokens_bi <- tokens_ngrams(tokens, n = 2)
  tokens_tri <- tokens_ngrams(tokens, n = 3)
  
  one <- tokens$text1[length(tokens$text1)]
  bi  <- tokens_bi$text1[length(tokens_bi$text1)]
  tri <- tokens_tri$text1[length(tokens_tri$text1)]
  
  if ((length(tokens_tri$text1) !=0) && (tri %in% frequencies_fou$NGRAM)){
    outputs <- frequencies_fou[frequencies_fou$NGRAM==tri,]
    result  <- outputs[outputs$COUNT==max(outputs$COUNT), ]$OUTPUT[1]
     
  } else if ((length(tokens_bi$text1) !=0) && (bi %in% frequencies_tri$NGRAM)){
    outputs <- frequencies_tri[frequencies_tri$NGRAM==bi,]
    result  <- outputs[outputs$COUNT==max(outputs$COUNT), ]$OUTPUT[1]
    
  } else if ((length(tokens$text1) !=0) && (one %in% frequencies_bi$NGRAM)){
    outputs <- frequencies_bi[frequencies_bi$NGRAM==one,]
    result  <- outputs[outputs$COUNT==max(outputs$COUNT), ]$OUTPUT[1]
    
  } else {
    result <- frequencies[1, ]$NGRAM
  }
  result
}

```

Let's try this basic model.

- I love **`r basic(" I love")`**   

- What was the **`r basic("What was the")`**   

- Who thought that **`r basic("who thought that")`**  

- I don't really think  **`r basic("I don't really think")`**   

- Do you **`r basic("Do you")`**   

- Betty called her and **`r basic("Betty called her and")`**  

<br>
```{r time}

# Total time of execution. 
total.time <- Sys.time() - start.time

total.time

```
