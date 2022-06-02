
#---load packages
# Load
library(RColorBrewer)
library(tidyverse)
library(SentimentAnalysis)
library(textstem)
library(caret)
require(tm)
require(SnowballC)
library(naivebayes)
require(syuzhet)
require(wordcloud)
require(tidytext)
require(reshape2)
library(ggplot2)
#library(ggplot)
library(stargazer)
library(gtsummary)
library(foreign)
library(ggraph)
library(igraph)
library(widyr)


# Load data and remove rows with no rating

d <- read.csv("data/airport_reviews3.csv") %>% 
  mutate(Unique.Identifier = 1:n()) %>% 
  # Convert text to ASCII
  mutate(comments = iconv(Text, from="UTF-8",to="ASCII//TRANSLIT")) %>% 
  mutate(comments = trimws(comments)) %>% 
  filter(!is.na(Rating)) %>% 
  filter(!is.na(Text))


# analyze sentiments using the syuzhet package based on the NRC sentiment dictionary
emotions <- get_nrc_sentiment(d$Text)
emo_bar <- colSums(emotions)
emo_sum <- data.frame(count=emo_bar, emotion=names(emo_bar))

#---plotting sentiments------
ggplot(emo_sum, aes(x = reorder(emotion, -count), y = count)) + 
  geom_bar(stat = 'identity', fill = "blue")+
  theme(axis.text.x = element_text(angle = 45, vjust = .5, hjust = 1))


# sentiment analysis with the tidytext package using the "bing" lexicon
bing_word_counts <- d %>% unnest_tokens(output = word, input = Text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

# select top 10 words by sentiment
bing_top_10_words_by_sentiment <- bing_word_counts %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 
bing_top_10_words_by_sentiment


# create a barplot showing contribution of words to sentiment
bing_top_10_words_by_sentiment %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL) + 
  coord_flip() 

# create a postive/negativ wordcloud for bing sentiment
bing_word_counts_wordcloud <- d %>% unnest_tokens(output = word, input = Text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 800)

# sentiment analysis with the tidytext package using the "loughran" lexicon
loughran_word_counts <- d %>% unnest_tokens(output = word, input = Text) %>%
  inner_join(get_sentiments("loughran")) %>%
  count(word, sentiment, sort = TRUE) 

# select top 10 words by sentiment
loughran_top_10_words_by_sentiment <- loughran_word_counts %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n))
loughran_top_10_words_by_sentiment


# create a barplot showing contribution of words to sentiment
loughran_top_10_words_by_sentiment %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL) + 
  coord_flip() 

# sentiment analysis with the tidytext package using the "nrc" lexicon
nrc_word_counts <- d %>% unnest_tokens(output = word, input = Text) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE)

# select top 10 words by sentiment
nrc_top_10_words_by_sentiment <- nrc_word_counts %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n))
nrc_top_10_words_by_sentiment


# create a barplot showing contribution of words to sentiment
nrc_top_10_words_by_sentiment %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL) + 
  coord_flip() 

#Wordcloud
wordcloud(nrc_word_counts$word, nrc_word_counts$n, random.order = FALSE, rot.per = 0.3, scale = c(4,.5), max.words = 1000,colors = brewer.pal(8,"Dark2"))
wordcloud(GI_word_counts$word, GI_word_counts$n, random.order = FALSE, rot.per = 0.3, scale = c(4,.5), max.words = 1000,colors = brewer.pal(8,"Dark2"))
wordcloud(HE_word_counts$word, HE_word_counts$n, random.order = FALSE, rot.per = 0.3, scale = c(4,.5), max.words = 1000,colors = brewer.pal(8,"Dark2"))
wordcloud(LM_word_counts$word, LM_word_counts$n, random.order = FALSE, rot.per = 0.3, scale = c(4,.5), max.words = 1000,colors = brewer.pal(8,"Dark2"))
wordcloud(QDAP_word_counts$word, QDAP_word_counts$n, random.order = FALSE, rot.per = 0.3, scale = c(4,.5), max.words = 1000,colors = brewer.pal(8,"Dark2"))
wordcloud(nrc_word_counts$word, nrc_word_counts$n, random.order = FALSE, rot.per = 0.3, scale = c(4,.5), max.words = 1000,colors = brewer.pal(8,"Dark2"))

# Get sentiment using Net Optimism score of each Text
sentiment <- analyzeSentiment(d$Text)
sentiment


sentiment_GI <- convertToBinaryResponse(sentiment$SentimentGI)
sentiment_QDAP <- convertToBinaryResponse(sentiment$SentimentQDAP)
sentiment_LM <- convertToBinaryResponse(sentiment$SentimentLM)
sentiment_HE <- convertToBinaryResponse(sentiment$SentimentHE)


d <- d %>% 
  cbind(sentiment_GI) %>% 
  cbind(sentiment_QDAP) %>% 
  cbind(sentiment_LM) %>% 
  cbind(sentiment_HE)
d


d2 <- d %>% unnest_tokens(output = word, input = Text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[:alpha:]")) %>%
  distinct()%>%
  count(word, name = "review_n", sentiment_GI, sentiment_HE, sentiment_LM, sentiment_QDAP, sort = TRUE)


#-----------------------GI------------------------------------------------------
GI_word_counts <- d %>% unnest_tokens(output = word, input = Text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sentiment_GI, sort = TRUE) 

# Top 10 sentiment GI
sentiment_GI_top_10_words <- GI_word_counts %>% 
  group_by(sentiment_GI) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ungroup() %>%
  distinct()%>%
  mutate(word = reorder(word, n))


# Bar plot showing top 10 GI sentiment
sentiment_GI_top_10_words %>% 
  ggplot(aes(word, n, fill = sentiment_GI)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment_GI, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL) + 
  coord_flip() 


GI_wordcloud <- d %>% unnest_tokens(output = word, input = Text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sentiment_GI, sort = TRUE) %>%
  acast(word ~ sentiment_GI, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 100)


#------------HE-------------------------------------------------

HE_word_counts <- d %>% unnest_tokens(output = word, input = Text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sentiment_HE, sort = TRUE) 

sentiment_HE_top_10_words <- HE_word_counts %>% 
  group_by(sentiment_HE) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ungroup() %>%
  distinct()%>%
  mutate(word = reorder(word, n))

sentiment_HE_top_10_words %>% 
  ggplot(aes(word, n, fill = sentiment_HE)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment_HE, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL) + 
  coord_flip() 

HE_wordcloud <- d %>% unnest_tokens(output = word, input = Text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sentiment_HE, sort = TRUE) %>%
  acast(word ~ sentiment_HE, value.var = "n", fill = 0) %>%
  tm_map(sentiment_HE, stemDocument, language="english") %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 800)

wordcloud(HE_word_counts$word, HE_word_counts$n, random.order = FALSE, rot.per = 0.3, scale = c(4,.5), max.words = 1000,colors = brewer.pal(8,"Dark2"))


#---------------LM-------------------------------------------------------------
LM_word_counts <- d %>% unnest_tokens(output = word, input = Text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sentiment_LM, sort = TRUE) 

sentiment_LM_top_10_words <- LM_word_counts %>% 
  group_by(sentiment_LM) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ungroup() %>%
  distinct()%>%
  mutate(word = reorder(word, n))

sentiment_LM_top_10_words %>% 
  ggplot(aes(word, n, fill = sentiment_LM)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment_LM, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL) + 
  coord_flip() 

LM_wordcloud <- d %>% unnest_tokens(output = word, input = Text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sentiment_LM, sort = TRUE) %>%
  acast(word ~ sentiment_LM, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 900)


#--------------QDAP----------------------------------------------------------------
QDAP_word_counts <- d %>% unnest_tokens(output = word, input = Text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sentiment_QDAP, sort = TRUE) 

sentiment_QDAP_top_10_words <- QDAP_word_counts %>% 
  group_by(sentiment_QDAP) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ungroup() %>%
  distinct()%>%
  mutate(word = reorder(word, n))

sentiment_QDAP_top_10_words %>% 
  ggplot(aes(word, n, fill = sentiment_QDAP)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment_QDAP, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL) + 
  coord_flip() 

QDAP_wordcloud <- d %>% unnest_tokens(output = word, input = Text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sentiment_QDAP, sort = TRUE) %>%
  acast(word ~ sentiment_QDAP, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 100)



d3 <- d %>% unnest_tokens(output = word, input = Text) %>%
  anti_join(stop_words, by = "word") %>%
  distinct()%>%
  count(word, name = "review_n") %>%
  filter(review_n >=100)

word_correlations <- d2 %>%
  anti_join(stop_words, by = "word") %>%
  distinct() %>%
  semi_join(d3, by = "word") %>%
  pairwise_cor(item = word, feature = review_n) %>%
  filter(correlation >= 0.6)


graph_from_data_frame(d = word_correlations,
                      vertices = d3 %>%
                        semi_join(word_correlations, by = c("word" = "item1"))) %>%
  
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation)) +
  geom_node_point() + 
  geom_node_text(aes(color = review_n, label = name), repel = TRUE)


## Custom dictionary with LASSO regularization
dict <- generateDictionary(d$Text, d$Rating, modelType = "lasso")
summary(dict)
dict
pred <- predict(dict, d$Text)
pred
plotSentimentResponse(pred, d$Rating)


## Custom dictionary with LASSO regularization
dictlasso <- generateDictionary(d$Text, d$Rating)

# Show generated dictionary
dictlasso
summary(dictlasso)

function(dict, col)
  
  
  
hist(dict$scores, main = "Histogram of Lasso Regularization", 
       ylab = "Frequency", 
       xlab = "Rating", 
       col = "steelblue")

plot(dict, col="Blue")+xlim(c(-0.2,0.2))+ ggtitle("KDE plot") 



#TEST


# Compute in-sample performance lasso
options(scipen = 999)

pred_dictlasso <- predict(dict, d$Text)
pred_dictlasso

compareToResponse(pred_dictlasso, d$Rating)
statsdictlasso <- compareToResponse(pred_dictlasso, d$Rating)
round(statsdictlasso,4)

plotSentimentResponse(pred_dictlasso,d$Rating, smoothing = "gam") + ggtitle("Scatterplot")


stargazer(statsdictlasso,title = "Lasso Regularization", style = "default",out = "regression.html")

#Use instead lambda.min from the LASSO estimation
dictlamdbamin <- generateDictionary(d$Text, d$Rating, control=list(s="lambda.min"))
summary(dictlamdbamin)
plot(dictlamdbamin)
plot(dictlamdbamin) + xlim(c(-0.1,0.1))
pred_dictlambdamin <- predict(dictlamdbamin, d$Text)
pred_dictlambdamin
statsdictlambdamin <- compareToResponse(pred_dictlambdamin, d$Rating)
statsdictlambdamin
plotSentimentResponse(pred_dictlambdamin,d$Rating,smoothing = "gam")

# Generate new dictionary with spike-and-slab regression instead of LASSO regularization
library(spikeslab)
dictspikeslab <- generateDictionary(d$Text, d$Rating, modelType="spikeslab")
dictspikeslab
summary(dictspikeslab)
plot(dictspikeslab)
plot(dictspikeslab) + xlim(c(-0.1,0.1))

# Compute in-sample performance with spike-and-slab
pred_dictspikeslab <- predict(dictspikeslab, d$Text)
pred_dictspikeslab
compareToResponse(pred_dictspikeslab, d$Rating)
statsdictspikeslab <- compareToResponse(pred_dictspikeslab, d$Rating)
statsdictspikeslab
plotSentimentResponse(pred_dictspikeslab,d$Rating,smoothing = "gam")

# Generate new dictionary with tf weighting instead of tf-idf
dicttf <- generateDictionary(d$Text, d$Rating, weighting=weightTf)
dicttf
summary(dicttf)
plot(dicttf)
plot(dicttf) + xlim(c(-0.1,0.1))

# Compute in-sample performance with tf
pred_dicttf <- predict(dicttf, d$Text)
pred_dicttf
compareToResponse(pred_dicttf, d$Rating)
statsdicttf <- compareToResponse(pred_dicttf, d$Rating)
statsdicttf
plotSentimentResponse(pred_dicttf,d$Rating,smoothing = "gam")

# Use instead OLS as estimation method
dictOls <- generateDictionary(d$Text, d$Rating, modelType="lm")
dictOls
summary(dictOls)
plot(dictOls)
pred_dictOls <- predict(dictOls, d$Text)
pred_dictOls
statsdictOls <- compareToResponse(pred_dictOls,d$Rating)
statsdictOls
plotSentimentResponse(pred_dictOls,d$Rating,smoothing = "gam")

stargazer(statsdictOls, title = "OLS Regression", style = "default",out = "regression.html")

# Performance Evaluation of generated Dictionary


# DIctionary comparisons
compareDictionaries(dictlasso, loadDictionaryQDAP())
compareDictionaries(dictlasso, loadDictionaryGI())
compareDictionaries(dictlasso, loadDictionaryLM())
compareDictionaries(dictlasso, loadDictionaryHE())

CompareQDAP <- compareDictionaries(dictlasso, loadDictionaryQDAP())
CompareGI <- compareDictionaries(dictlasso, loadDictionaryGI())
CompareLM <- compareDictionaries(dictlasso, loadDictionaryLM())
CompareHE <- compareDictionaries(dictlasso, loadDictionaryHE())

stargazer(CompareQDAP, style = "default",out = "regression.html")


# Implementation of Machine Learning Models
data_corpus = Corpus(VectorSource(d$Text))
data_corpus = tm_map(data_corpus, content_transformer(tolower))
data_corpus = tm_map(data_corpus, removePunctuation)
data_corpus = tm_map(data_corpus, removeWords, c("the", "and", stopwords("english")))
data_corpus <- tm_map(data_corpus, stemDocument, language="english")
data_corpus =  tm_map(data_corpus, stripWhitespace)

d_dtm <- DocumentTermMatrix(data_corpus, control = list(weighting = weightTfIdf))
d_dtm = removeSparseTerms(d_dtm, 0.90)
d_dtm

d_dtm2 <- as.data.frame(as.matrix(d_dtm))
colnames(d_dtm2) <- make.names(colnames(d_dtm2))
frequency <- make.names(colnames(d_dtm2))
frequency <- sort(frequency, decreasing=TRUE)
d_dtm$Rating <- d$Rating


train_index <- createDataPartition(d$Rating,
                                   # 85% for training
                                   p = 2/3,
                                   times = 1,
                                   list = FALSE)
train_index

d_train <- d_dtm[ train_index, ] # Training
d_train
d_test  <- d_dtm[-train_index, ] # Testing
d_test

d_test_labels <- d_test$Rating
d_test <- d_test %>% select(-Rating)
d_test

# K Nearest Neighbors Model
set.seed(7)
modelKNN <- train(as.factor(Rating) ~., data=d_train, method="knn" )
modelKNN

# Decision Trees
set.seed(7)
modelDT <- train(as.factor(Rating)~., data=d_train, method="rpart")
modelDT

# train the NB model
set.seed(7)
modelNB <- train(as.factor(Rating) ~ ., 
                 data = d_train, 
                 method = "naive_bayes", 
                 usepoisson = TRUE)
modelNB


# predictRF <- as.integer(predict(modelRF, newdata = d_test))
predictNB <- as.factor(predict(modelNB, newdata = d_test))
predictNB
predictDT <- as.factor(predict(modelDT, newdata = d_test))
predictDT
predictKNN <- as.factor(predict(modelKNN, newdata = d_test))
predictKNN


confusionMatrix(
  factor(d_test_labels, levels = 1:6),
  factor(predictNB, levels = 1:6)
)

confusionMatrix(
  factor(d_test_labels, levels = 1:6),
  factor(predictDT, levels = 1:6)
)

confusionMatrix(
  factor(d_test_labels, levels = 1:6),
  factor(predictKNN, levels = 1:6)
)

# Plot ML models 
ggplot(modelNB)
ggplot(modelKNN)
ggplot(modelDT)


# predictRF <- as.integer(predict(modelRF, newdata = d_test))
predictNB <- as.factor(predict(modelNB, newdata = d_test))
predictDT <- as.factor(predict(modelDT, newdata = d_test))
predictKNN <- as.factor(predict(modelKNN, newdata = d_test))

