###############################################################################
# Text Analysis and visualization from reference collection in Zotero
# Reference provided by survey respondents to the Wildfires & Watersheds survey
# by Myers-Pigg et al., 20xx
################################################################################

#Cleaning memory resources
gc()

#loading required packages
librarian::shelf(dplyr, tidytext, tidyverse,
                 widyr,igraph, ggraph, plotly,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr,
                 ggwordcloud,tm,scales, ggrepel, ggplotify,zoo,
                 htmlwidgets, htmltools, visNetwork)


# Import and Export paths
assets_pubs <- "../1-bui-knowledge.base/assets/data/raw" 
figures <- "../1-bui-knowledge.base/assets/figs" 

t_df <- as_tibble(read_csv(paste(assets_pubs,"survey_answers_zotero_refs.csv",sep='/'),
                           show_col_types = FALSE))

q_tp <- as_tibble(read_csv(paste(assets_pubs,"question_type_references.csv", sep = '/'),
                           show_col_types = FALSE))

# For our analyses below, we won't need DOIs, PMIDs or arXiv IDs. We will focus
# on Title, Abstract, Authors, Journal, Year. To keep column names formatting
# simple, I use lowercase and spaces are added with (_) when needed.

# In some cases is possible that the abstracts for the papers are not retrieved. 
# In such cases,the value returned would be NA (which is read as a character string and not as
# the typical NA). It could also happen that the abstracts could be partially
# retrieved. To filter these cases out, we will add another column to the data
# frame to count the character length of each abstract, and remove those that are
# less than 20 characters long. Finally, we will add a sequential id and make it
# a factor for visualization purposes.

t_df_c <- t_df %>% 
  distinct() %>% 
  select(Key, Title,`Abstract Note`,Author,`Publication Title`,`Publication Year`) %>% 
  rename(key = Key,
         title = Title,
         abstract = `Abstract Note`,
         authors = Author,
         journal = `Publication Title`,
         year = `Publication Year`) %>% 
  mutate(abstract_lenght = nchar(abstract)) %>% 
  filter(abstract_lenght > 20)%>% 
  mutate(id = seq(from =1, to= nrow(.),by=1)) %>% 
  mutate(id = factor(id))

t_df_c

# Abstracts per question types:

abs_q_type <- q_tp %>% 
  select(`unique-id`,
         `question-type`,
         key) %>% 
  rename(unique_id = `unique-id`,
         question_type =`question-type`) %>% 
  merge(.,
        t_df_c,
        by = "key",
        all.x = TRUE) %>% 
  filter(is.na(id)==FALSE)


# In this case we have abstracts retrieved for all 142 publication items.

# To make sure all words are processed correctly, we need to do some additional 
# cleaning on the text data. That includes unnesting each publication component 
# (pub_comp) into tokens (i.e. single words), performing the cleaning tasks 
# (i.e. singularizing,removing punctuations and digits, removing stop words (a,
# an, by, ...)), and finally putting the tokens back together (i.e. nesting)

# How to unnest and nest text data in using tidytext? check this post: 
# https://stackoverflow.com/questions/49118539/opposite-of-unnest-tokens-in-r

# Choosing analysis level

# You can choose the publication component (pub_comp) you want to focus your 
# analysis on. In this case, our options are title or abstract. We will analyze
# abstracts


# Preparing the data

pub_comp = "abstract"

pub_dat<- dplyr::select(abs_q_type, question_type, id, authors, year, journal, all_of(pub_comp)) %>%
  rename(pub_comp_words = all_of(pub_comp)) %>% 
  unnest_tokens(output = word, input = pub_comp_words, drop = FALSE) %>% 
  mutate(word = str_to_lower(word),
         word = singularize(word)) %>%  
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>% 
  anti_join(stop_words, by = "word") %>% 
  nest(data = word) %>% 
  mutate(!!paste0(pub_comp) := map_chr(map(data, unlist), paste, collapse = " ")) %>% 
  select(-c(pub_comp_words,data))



# Conceptual maps from n-grams

gram_l = 4
breath = 250
time_window = 1990

n_gram <- paste(gram_l,"gram",sep='-')

a <- seq(1:gram_l)
b <- rep("word",times=gram_l)
columns <- paste(b,a,sep = '')

# Concept Oriented Networks

pub_ngrams <- pub_dat %>%
  filter(question_type == "roadblocks") %>% 
  ungroup() %>%
  unnest_tokens(n_gram, pub_comp, token = "ngrams", n = gram_l) %>%
  separate(n_gram, columns, sep = " ", remove = FALSE) %>%
  count(across(all_of(columns), ~.x), sort = TRUE) %>%
  mutate(rank = row_number(),
         total = sum(n),
         t_freq = n/total)
head(pub_ngrams)

# Create the graph using igraph
ngram_graph <- pub_ngrams %>%
  filter(rank < breath) %>%
  graph_from_data_frame()


# Creating the network as an interactive html object to be embedded into the
# Quarto website

# Create a data frame for nodes
node_df <- data.frame(id = V(ngram_graph)$name, 
                      # size = 15, 
                      label = V(ngram_graph)$name)

# Calculate the number of edges pointing at each node
in_degree <- igraph::degree(ngram_graph, mode = "in")
node_df$value <- in_degree


# Create a data frame for edges
edge_df <- data.frame(from = as.character(get.edgelist(ngram_graph)[,1]), 
                      to = as.character(get.edgelist(ngram_graph)[,2]))

# Create a visNetwork object
network <- visNetwork(nodes = node_df, edges = edge_df,
                      width = "100%", height = "600px") %>%
  
  # Add physics layout and stabilization
  visPhysics(stabilization = TRUE) %>%
  
  # Add labels for nodes
  visNodes(label = "label", title = "title", font = list(size = 20)) %>%
  
  # Customize edges
  visEdges(arrows = "to") %>%
  
  # Add a tooltip
  visInteraction(hover = TRUE)

# Display the network in the RStudio Viewer pane
network

# Generate the HTML code for the network visualization
html_code <- as.character(tags$div(id = "network-container", network))

# Save the network as an HTML file
html_file <- "../3-bui-production.hub/roadblocks_abstract_network.html"
saveWidget(network, file = html_file)

# Save the HTML code to a file
html_file <- "network.html"
writeLines(html_code, con = html_file) 







# Tokenization

# Our first step to analyze the selected publication component, is to break it 
# into individual words (or tokens) and store the tokens in a new data frame for 
# (pub_tokens). There are several packages you could use to tokenize a piece of 
# text, here we will use the tidytext package for most of our analysis. 

# What the following chunk of code does is: 1) call the pub_dat dataset, 2) break the 
# chunk of (nested) text into tokens (output = word) by using the function unnest_tokens(),
# 3) eliminating duplicated words with distinct(), 4) grouping the tokens (word) by years,
# 5) calculating the frequency of a given token in each year -count(), and adding a new
# column with the numnber of characters -nchar for each token, so we can filter monosyllabes.

pub_tokens <- filter(pub_dat, question_type == "pathways") %>%
  unnest_tokens(output = word, input = pub_comp, drop = FALSE) %>%
  distinct() %>%
  group_by(year) %>%
  count(word, sort = TRUE)%>%
  rename(word_freq = n) %>% 
  summarise(tot_freq = sum(word_freq)) %>%
  left_join(
    .,
    pub_dat %>%filter(question_type == "pathways") %>% 
  unnest_tokens(output = word, input = pub_comp, drop = FALSE) %>%
  distinct() %>%
  group_by(year, word) %>%
  count(sort = TRUE) %>%
  rename(word_freq = n) %>% 
  rowwise() %>% dplyr::summarise(max_freq = max(word_freq),groups = word,word_freq) %>% 
  mutate(length = nchar(word)) %>%
  filter(length > 2) %>%
  group_by(year) %>%
  mutate(
    n_rel = word_freq / sum(word_freq),
    csm_rel = cumsum(n_rel),
    pst_rel = (word_freq-1) + csm_rel)%>%
    ungroup(),
  by = "year"
  )
#source: https://chat.openai.com/auth/login?next=/chat/f817b62b-ac51-4121-bad6-f6f0fd8c7d90 



# Time Indexed Wordcloud

res_plot = 0.85
span_y <- res_plot * nrow(pub_tokens)
set.seed(27)

p <- pub_tokens[1:span_y,] %>% 
  ggplot(aes(x = year, y = pst_rel, color = -year, size = word_freq, label = word)) +
  geom_text(aes(size = word_freq),check_overlap = TRUE, hjust = 0)+
  scale_color_viridis_c(option = "turbo")+
  # scale_color_gradient2(low = "#fde725", high = "lightblue")+
  scale_radius(range = c(3,6))+
  scale_y_log10()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "snow"),
        axis.ticks.length.x = unit(0.15,"cm"),
        axis.ticks.x = element_line(colour = "black"))
p


#source: https://ggplot2.tidyverse.org/reference/position_jitter.html



################################################################################  
# Time Oriented Networks - Extremely complex for abstracts
################################################################################
pub_time_ngrams <- pub_dat %>%
  ungroup() %>%
  unnest_tokens(n_gram, pub_comp, token = "ngrams", n = gram_l) %>%
  group_by(year) %>%
  count(n_gram, sort = TRUE) %>%
  mutate(rank = row_number(),
         total = sum(n),
         t_freq = n/total)
head(pub_time_ngrams)

# Create the graph using igraph
ngram_time_graph <- pub_time_ngrams %>%
  filter(rank < breath) %>%
  filter(year > time_window) %>%
  # filter(is.na(n_gram)==TRUE) %>% 
  graph_from_data_frame()

library(visNetwork)

# Create a data frame for nodes
node_tdf <- data.frame(id = V(ngram_time_graph)$name, 
                      size = 10, 
                      label = V(ngram_time_graph)$name)

# Create a data frame for edges
edge_tdf <- data.frame(from = as.character(get.edgelist(ngram_time_graph)[,1]), 
                      to = as.character(get.edgelist(ngram_time_graph)[,2]))

# Create a visNetwork object
visNetwork(nodes = node_tdf, edges = edge_tdf, 
           width = "100%", height = "600px") %>%
  
  # Add physics layout and stabilization
  visPhysics(stabilization = TRUE) %>%
  
  # Add labels for nodes
  visNodes(label = "label", title = "title", font = list(size = 15)) %>%
  
  # Customize edges
  visEdges(arrows = "to") %>%
  
  # Add a tooltip
  visInteraction(hover = TRUE) 


# source: https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html
# Plotting networks with igraph:
# https://kateto.net/netscix2016.html

################################################################################

