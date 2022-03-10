################################################################################
## Word Embeddings: word2vec
################################################################################

# load packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(word2vec, text2vec, Rtsne, scales, ggrepel, tidyverse, tm)

# substract the text from the tweets
tweets_text <- tweets$text
# set to lower case
tweets_text <- str_to_lower(tweets_text)

# build the model
set.seed(1234)
model <- word2vec(x = tweets_text, 
                  type = "skip-gram", 
                  dim = 15, 
                  iter = 50, 
                  window = 5,
                  stopwords = stopwords())

# inspect embeddings
embedding <- as.matrix(model)
head(embedding)


################################################################################
## Word Embeddings: word2vec
################################################################################




















