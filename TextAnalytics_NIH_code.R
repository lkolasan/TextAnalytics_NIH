pcd <- fread("psychcentral_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
str(pcd)
library(dplyr)
library(tidytext)
tidy_text <- pcd %>% unnest_tokens(word, q_content) 
tidy_text[1:5]
tidy_text <- tidy_text %>% anti_join(stop_words)
tidy_text %>% count(word, sort = TRUE)
library(ggplot2)
tidy_text %>% 
  count(word, sort = TRUE) %>%
  filter( n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()
  
install.packages("SnowballC", repos = "https://cran.r-project.org")

