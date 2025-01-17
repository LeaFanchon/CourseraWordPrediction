
# Word Prediction

## Goal

This application is the last assignment from Coursera's Data Science Specialization. More information about this course can be found here : https://www.coursera.org/specializations/jhu-data-science 

- The aim is to build a small Shiny app which takes as input the beginning of a sentence and returns a suggestion, or a list of suggestions, for the next word. 

- The data was provided by Coursera and contained some texts of variable lengths extracted from twitter, the news and blogs. 

- This repository contains all the scripts needed to create this application.


## Predictive model : Stupid Back-off


The Stupid Back-Off model has a parameter named **ALPHA**, that defaults at 0,4. The model:  

- Takes the last three tokens of the user's input, and creates a list of possible outcomes, by getting all the tokens in the data that have been already found following either the last 3-gram, 2-gram or 1-gram of the input.  

- Computes a weight for each of these possible outputs. If the output has been found in an existing 4-gram, the weight of this output will be the probability of the 4-gram. If found in a 3-gram, its weight will be the probability of the 3-gram multiplied by **ALPHA**, if found in a 2-gram, it will be by **ALPHA** squared, and so on.  

- This works well for small inputs (3 words or less), but doesn't take into account the words preceding the last three words. If the user inputs "I'm so in love I am going to", the only tokens used by the model will be "am", "going" and "to", which lack some crucial information and could lead to invalid prediction ("I'm so in love I am going to *the mall*").


## Context

In order to take into account the first words of long inputs as well, we added to our prediction a "context" weight.  

- We first created a file containing all the pairs of tokens that had a tendency to appear in the same texts (but not necessarily following each other). For instance, "politics" and "president". Or "cake" and "pie". We also calculated a weight for each of those pairs (details in the github files).  

- In this context file, we removed all the "stopwords" of the english language: "it", the", "a", etc, which are words with a grammatical function but that carry no context.


## Mixing the two models  

We then mixed the two models to optimize prediction:  

- The Stupid Backoff model provides a list of possible outcomes, and a weight for each one of those outcomes based on the frequency of the highest-level ngram where this outcome is found.   

- The contextual model then computes a contextual weight for each of the possible outcomes generated by the ngram model, based on all the words of the input that are not stopwords.   

- Those two weights are then combined with multiplicative coefficients (**WEIGHT_NGRAMS** and **WEIGHT_CONTEXT**) to produce the final weight of the outcome:  

- FINALWEIGHT =  **WEIGHT_NGRAMS**.NGRAM_MODEL + **WEIGHT_CONTEXT**.CONTEXT_MODEL


## The app

The Shiny app can be found here:  https://camillecuriace.shinyapps.io/Word_Prediction/
The user can choose:  

- The **ALPHA** parameter of the Katz Back-Off Model  

- The weights of the Katz model and of the context : **WEIGHT_NGRAMS** and **WEIGHT_CONTEXT**  

- If stopwords are or not included in the final prediction (uncheck this box if tired of seeing only very common english words like "the" or "a").

**Please note that the first prediction takes time as the server has to load the data. The following predictions are a lot faster.**


