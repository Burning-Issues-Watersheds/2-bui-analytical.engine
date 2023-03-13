###############################################################################
# TEXT ANALYSIS WILDFIRES COMMUNITY PAPER SURVEY ANSWERS
################################################################################

## Author: Francisco J. Guerrero
## Date Start: Feb 28 2023

## Loading required packages

librarian::shelf(plyr, tidytext, tidyverse,
                 widyr,igraph, ggraph,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr)

## Loading the dataset & Tokenization

### Our dataset

aad <- read_csv("assets/data/wildfires_survey_all_answers.csv")


### Tokenization

aat <- aad %>% group_by(question) %>% 
  unnest_tokens(word,answers)%>%
  anti_join(stop_words, by = "word")%>%
  filter(str_detect(word,"[:alpha:]"))%>%
  distinct() %>% 
  count(word, sort = FALSE) %>% 
  mutate(length = nchar(word)) %>% 
  with(wordcloud(word,n,scale = c(2,.001)))
  # with(wordcloud(word,n, scale = c(4, .1)))
         
aat %>% group_by(question) %>% 
  mutate(freq = count(word,sort = FALSE))
         