#Reading psychcentral data
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
install.packages("SnowballC", repos = "https://cran.r-project.org")
library(SnowballC)
tidy_text <- pcd %>% unnest_tokens(word, q_content) %>% mutate(word = wordStem(word)) 

