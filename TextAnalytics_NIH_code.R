#Reading psychcentral data
library(data.table)
pcd <- fread("psychcentral_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
str(pcd)

#Loading libraries “dplyr” and “tidytext” to tokenize column q_content
library(dplyr)
library(tidytext)
tidy_text <- pcd %>% unnest_tokens(word, q_content) 
tidy_text[1:5]

#Removing stopwords
tidy_text <- tidy_text %>% anti_join(stop_words)

#Sorting by frequency of tokens
tidy_text %>% count(word, sort = TRUE)

#Visualizing sorted tokens with min freq of 2000
library(ggplot2)
tidy_text %>% 
  count(word, sort = TRUE) %>%
  filter( n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#Stemmng the orginal data
library(SnowballC)
tidy_text <- pcd %>% unnest_tokens(word, q_content) %>% mutate(word = wordStem(word)) 

#Removing stopwords
tidy_text <- tidy_text %>% anti_join(stop_words)

#Visualizing sorted tokens with min freq of 4000 after stemming&remving stopwords
tidy_text %>% 
  count(word, sort = TRUE) %>%
  filter( n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#Visualization of wordcloud with min token frequency of 200
library(wordcloud)
tidy_text %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200,random.order = FALSE))

##color-coded word cloud based on sentiment
#In functions such as comparison.cloud() we may need to turn the data frame into a matrix with reshape2’s acast(). 
#Let’s do the sentiment analysis to tag +ve and -ve words using an inner join, then plot the most common positive and negative words. 
library(reshape2)
tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)
#----- repeating above steps for column 'answers -----#
#Loading libraries “dplyr” and “tidytext” to tokenize column answers
library(dplyr)
library(tidytext)
tidy_text <- pcd %>% unnest_tokens(word, answers) 
tidy_text[1:5]

#Removing stopwords
tidy_text <- tidy_text %>% anti_join(stop_words)

#Sorting by frequency of tokens
tidy_text %>% count(word, sort = TRUE)

#Visualizing sorted tokens with min freq of 2000
library(ggplot2)
tidy_text %>% 
  count(word, sort = TRUE) %>%
  filter( n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#Stemmng the orginal data
library(SnowballC)
tidy_text <- pcd %>% unnest_tokens(word, answers) %>% mutate(word = wordStem(word)) 

#Removing stopwords
tidy_text <- tidy_text %>% anti_join(stop_words)

#Visualizing sorted tokens with min freq of 4000 after stemming&remving stopwords
tidy_text %>% 
  count(word, sort = TRUE) %>%
  filter( n > 6000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#Visualization of wordcloud with min token frequency of 200
library(wordcloud)
tidy_text %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200,random.order = FALSE))

##color-coded word cloud based on sentiment
#In functions such as comparison.cloud() we may need to turn the data frame into a matrix with reshape2’s acast(). 
#Let’s do the sentiment analysis to tag +ve and -ve words using an inner join, then plot the most common positive and negative words. 
library(reshape2)
tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

#code to perform topic-modeling on column 'q_content'
library(RTextTools)
library(tm)
library(wordcloud)
library(topicmodels)
library(slam)
data <- pcd[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$q_content), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 2) # k is the number of topics to be found.

#Using the tidytext package for extracting the per-topic-per-word probabilities
library(tidytext)
lda_td <- tidy(lda)
lda_td

#Visualization to find the 10 terms that are most common within each topic
library(ggplot2)
library(dplyr)

top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
#After trying different values of k, we finalize k=2

#code to perform topic-modeling on column 'answers':
library(RTextTools)
library(tm)
library(wordcloud)
library(topicmodels)
library(slam)
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$answers), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 14) # k is the number of topics to be found.

#Using the tidytext package for extracting the per-topic-per-word probabilities
library(tidytext)
lda_td <- tidy(lda)
lda_td

#Visualization to find the 10 terms that are most common within each topic
library(ggplot2)
library(dplyr)

top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#After trying different values of k, we finalize k=14

