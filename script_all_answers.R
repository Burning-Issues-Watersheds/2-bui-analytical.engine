require(librarian)
librarian::shelf(plyr, tidytext, tidyverse,
                 widyr,igraph, ggraph,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr,
                 ggwordcloud,tm)

t_df <- as_tibble(read_csv("../1-bui-knowledge.base/assets/data/wildfires_survey_all_answers.csv",show_col_types = FALSE))
t_df_wa <- filter(t_df,question!="research-a")
t_df_wa$question <- factor(t_df_wa$question, levels = c("pressing-q","roadblocks","pathways"))

# Cleaning the data set
ans_dat <- t_df_wa %>%
  mutate(answers=removeNumbers(answers))%>%
  mutate(answers=gsub(paste0('\\b',tm::stopwords("english"), '\\b', 
                              collapse = '|'), '', answers)) 

###############################################################################
# Tokenization
###############################################################################

tkn_tgt <- "answers" #(token target)

aq_tokens <- ans_dat %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = tkn_tgt, drop = FALSE)%>%
  # anti_join(stop_words, by = "word")%>%
  # filter(str_detect(word,"[:alpha:]"))%>%
  # filter(!str_detect(word, "[:punct:]|[:digit:]")) %>% 
  # removeNumbers(tkn_tgt) %>% 
  rowwise() %>% mutate(word = if_else(word!="data",singularize(word),"data")) %>%
  distinct() %>% 
  group_by(question) %>% 
  count(word, sort = FALSE) %>% 
  mutate(length = nchar(word)) 

head(aq_tokens)
summary(aq_tokens)

# Tokenization
ans_tokens <- ans_dat %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = answers, drop = FALSE)%>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>% 
  rowwise() %>% mutate(word = if_else(word!="data",singularize(word),"data")) %>%
  distinct() %>% 
  group_by(question) %>% 
  count(word, sort = TRUE) %>% 
  mutate(length = nchar(word)) 


# Using ggwordclouds:
# https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

ans_wofire <- filter(ans_tokens,word!="fire")
ans_wofire <- filter(ans_wofire,word!="wildfire")
ans_wofire <- filter(ans_wofire,word!="al")
ans_wofire <- filter(ans_wofire,word!="et")

p <- ggplot(filter(ans_wofire,n>3), 
            aes(label = word, 
                size = n, 
                color = question)) +
  geom_text_wordcloud(area_corr_power = 1) +
  scale_color_manual(values = c("darkorchid4","darkorange3","green4"))+
  scale_radius(range = c(0, 20),
               limits = c(0, NA)) +
  facet_wrap(~question, ncol = 3) 
p



# Using ggwordclouds:
# https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

aq_wofire <- filter(aq_tokens,word!="fire")
aq_wofire <- filter(aq_wofire,word!="wildfire")
aq_wofire <- filter(aq_wofire,word!="al")
aq_wofire <- filter(aq_wofire,word!="et")

p <- ggplot(filter(aq_wofire,n>2), 
            aes(label = word, 
                size = n, 
                color = question)) +
  geom_text_wordcloud(area_corr_power = 1) +
  scale_color_manual(values = c("darkorchid4","darkorange3","green4"))+
  scale_radius(range = c(0, 20),
               limits = c(0, NA)) +
  facet_wrap(~question, ncol = 3) 
p

# Titles

pub_cmp <- "answers"

ans_tokens <- pub_dat %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = pub_cmp, drop = FALSE)%>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>% 
  rowwise() %>% mutate(word = if_else(word!="data",singularize(word),"data")) %>%
  distinct() %>% 
  group_by(year) %>% 
  count(word, sort = TRUE) %>% 
  mutate(length = nchar(word)) 

# Word clouds
p1 <- ggplot(pub_tokens, 
             aes(x = year,
                 y = n,
                 label = word, 
                 size = n, 
                 color = as.factor(year))) +
  geom_text_wordcloud(area_corr_power = 1) +
  # scale_y_log10()+
  scale_radius(range = c(0, 20),
               limits = c(0, NA))+
  theme_minimal()
p1

# n-grams

gram_l = 2
n_gram <- paste(gram_l,"gram",sep='-')

a <- seq(1:gram_l)
b <- rep("word",times=gram_l)
columns <- paste(b,a,sep = '')

ans_ngrams <- t_df_wa %>%
  ungroup() %>%
  filter(str_detect(pub_cmp,"[:alpha:]")) %>%
  unnest_tokens(n_gram, pub_cmp, token = "ngrams", n = gram_l) %>%
  filter(!str_detect(n_gram, "[:punct:]|[:digit:]")) %>% 
  filter(!n_gram %in% c(stop_words$word)) %>%
  separate(n_gram, columns, sep = " ", remove = FALSE) %>%
  count(across(all_of(columns), ~.x), sort = TRUE) %>%
  mutate(rank = row_number(),
         total = sum(n),
         t_freq = n/total)
head(pub_ngrams)


# Tri-grams

# Pressing Questions
t_pq <- filter(t_df_wa,question=="pressing-q")

pq_trigrams <- t_pq%>%
  ungroup() %>%
  filter(str_detect(answers,"[:alpha:]"))%>%
  unnest_tokens(trigram, answers, token = "ngrams", n = 3) %>% 
  separate(trigram,c("word1", "word2","word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  # group_by(question) %>% 
  count(word1, word2, word3,sort = TRUE) %>% 
  mutate(rank = row_number(),
         total=sum(n),
         t_freq = n/total)
head(pq_trigrams)


pq_trigrams %>% 
  filter(rank < 20) %>% 
  unite(trigram, word1, word2, word3, sep = " ") %>% 
  ggplot(aes(t_freq, fct_reorder(trigram, t_freq), fill = t_freq)) +
  scale_fill_gradient(low= "violet", high= "darkorchid4")+
  geom_col(show.legend = FALSE) +
  labs(x = "Frequency", y = NULL)

trigram_graph <- pq_trigrams %>%
  filter(rank < 101) %>%
  graph_from_data_frame()
trigram_graph


l <- layout_with_fr(trigram_graph)
e <- get.edgelist(trigram_graph,names=FALSE)
m <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(trigram_graph))
deg <- degree(trigram_graph,mode="all")
fsize <- degree(trigram_graph, mode= "all")

#png(filename=paste("assets/NetworkAnalysis_words_",Sys.Date(),".png", sep = ""), res = 100)

plot(trigram_graph,
     layout=m, 
     edge.arrow.size =.05,
     vertex.color = "pink", 
     vertex.size =500,
     vertex.frame.color="deeppink",
     vertex.label.color="black", 
     vertex.label.cex=fsize/2,
     vertex.label.dist=0.6,
     edge.curve = 0.75,
     edge.color="skyblue",
     edge.label.family="Arial", 
     rescale=F, 
     axes = FALSE, 
     ylim = c(-90,90), 
     xlim = c(-60,130),
     asp =0)

plot(trigram_graph,layout=m, edge.arrow.size =.05,vertex.color = "pink", vertex.size =deg*150,vertex.frame.color="deeppink",vertex.label.color="black", vertex.label.cex=0.55,vertex.label.dist=0.8,edge.curve = 0.75,edge.color="skyblue",edge.label.family="Arial", rescale=F, axes = FALSE, ylim = c(-50,90), xlim = c(-55,120), asp =0)
