################################################################################
## Word Embeddings: word2vec
################################################################################

# load packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(word2vec, text2vec, Rtsne, scales, ggrepel, tidyverse, tm)

load(file = "C:\\Users\\bertj\\OneDrive\\Documenten\\GitHub\\SMWA_Performance\\data\\all_tweets.Rdata")

# substract the text from the tweets
tweets_text <- all_tweets$text
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
## Word Embeddings: GloVe
################################################################################

# tokenize the text from the tweets
tokens <- space_tokenizer(tweets_text %>%
                            tolower() %>%
                            removePunctuation() %>%
                            removeWords(words = stopwords()) %>% 
                            stripWhitespace())

# create vocabulary
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)

# remove sparse words
vocab <- prune_vocabulary(vocab, term_count_min = 3L)

# use filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)

# create co-occurence matrix
tcm <- create_tcm(it, vectorizer, skip_grams_window = 10L)

# initialize GloVe model
glove <- GloVe$new(rank = 100, x_max = 10)

# fit model
word_vectors_main <- glove$fit_transform(tcm, n_iter = 20)

# use sum of context and main word vectors
word_vectors_components <- glove$components
word_vectors <- word_vectors_main + t(word_vectors_components)

# look at words related to Russia
russia <- word_vectors["russia", ,drop = FALSE] 
cos_sim <- sim2(x = word_vectors, y = russia, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)



################################################################################
## Word Embeddings: Plotting with t-SNE
################################################################################

# be sure stopwords are filtered out
keep_words <- setdiff(rownames(word_vectors), stopwords())
word_vec <- word_vectors[keep_words,]

# prepare data for training
train_df <- data.frame(word_vec) %>% rownames_to_column("word")

# train t-SNE
tsne <- Rtsne(train_df[,-1], dims = 2, perplexity = 2, verbose=TRUE, max_iter = 1200)

# create plot
colors = rainbow(length(unique(train_df$word)))
names(colors) = unique(train_df$word)

plot_df <- data.frame(tsne$Y) %>%
  mutate(word = train_df$word, col = colors[train_df$word]) %>%
    left_join(vocab, by = c("word" = "term")) 

ggplot(plot_df, aes(X1, X2, label = word, color = col)) + 
  geom_text(size = 4) +
  xlab("") + ylab("") +
  theme(legend.position = "none")   






