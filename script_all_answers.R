require(librarian)
librarian::shelf(plyr, tidytext, tidyverse,
                 widyr,igraph, ggraph,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr,
                 ggwordcloud)

t_df <- as_tibble(read_csv("../1-bui-knowledge.base/assets/data/wildfires_survey_all_answers.csv",show_col_types = FALSE))
t_df_wa <- filter(t_df,question!="research-a")
t_df_wa$question <- factor(t_df_wa$question, levels = c("pressing-q","roadblocks","pathways"))

aq_tokens <- t_df_wa %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = answers, drop = FALSE)%>%
  anti_join(stop_words, by = "word")%>%
  filter(str_detect(word,"[:alpha:]"))%>%
  rowwise() %>% mutate(word = if_else(word!="data",singularize(word),"data")) %>%
  distinct() %>% 
  group_by(question) %>% 
  count(word, sort = FALSE) %>% 
  mutate(length = nchar(word)) 

head(aq_tokens)
summary(aq_tokens)

# Using ggwordclouds:
# https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

aq_wofire <- filter(aq_tokens,word!="fire")
aq_wofire <- filter(aq_wofire,word!="wildfire")
aq_wofire <- filter(aq_wofire,word!="al")

p <- ggplot(filter(aq_wofire,n>2), 
            aes(label = word, 
                size = n, 
                color = question)) +
  geom_text_wordcloud(area_corr_power = 1) +
  scale_radius(range = c(0, 20),
               limits = c(0, NA)) +
  facet_wrap(~question, ncol = 3) 
p


# Bi-grams

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
  geom_col(show.legend = FALSE) +
  labs(x = "Frequency", y = NULL)

trigram_graph <- pq_trigrams %>%
  filter(rank < 101) %>%
  graph_from_data_frame()
trigram_graph

set.seed(2017)

# a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
# 
# ggraph(bigram_graph, layout = "fr") +
#   geom_edge_link(show.legend = FALSE,
#                  arrow = a, end_cap = circle(.07, 'inches')) +
#   geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
#                  arrow = a, end_cap = circle(.035, 'inches')) +
#   geom_node_point(color = "blue", size = 3) +
#   geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
#   theme_void()
# V(bigram_graph)$size <- V(bigram_graph)$t_freq*10
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
