###############################################################################
# Text Analysis and visualization from reference collection in Zotero
# Reference provided by survey respondents to the Wildfires & Watersheds survey
# by Myers-Pigg et al., 20xx
################################################################################

#Code author: Francisco J. Guerrero


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
figures <- "../3-bui-production.hub" 

references_raw_dat <- as_tibble(read_csv(paste(assets_pubs,"survey_answers_zotero_refs.csv",sep='/'),
                           show_col_types = FALSE))
head(references_raw_dat)

question_type_raw_dat <- as_tibble(read_csv(paste(assets_pubs,"question_type_references.csv", sep = '/'),
                           show_col_types = FALSE))
head(question_type_raw_dat)

################################################################################
# Data preparation
###############################################################################

# 1. Data Assembly

# For our analyses below, we won't need DOIs, PMIDs or arXiv IDs. We will focus
# on Title, Abstract, Authors, Journal, Year. To keep column names formatting
# simple, I use lowercase and spaces are added with (_) when needed.

# In some cases is possible that the abstracts for the papers are not retrieved. 
# In such cases,the value returned would be NA (which is read as a character string 
# and not as the typical NA). It could also happen that the abstracts could be 
# partially retrieved. To filter these cases out, we will add another column to 
# the dataframe to count the character length of each abstract, and remove those 
# that are less than 20 characters long. Finally, we will add a sequential id and 
# make it a factor for visualization purposes.

references_dat <- references_raw_dat %>% 
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

head(references_dat)

# In this case we have abstracts retrieved for all 142 publication items.

# Now, we do some minor reformatting on the `question_type_raw_dat` dataset in 
# in preparation to be merged with the `reference_dat` dataset.  

question_type_dat <- question_type_raw_dat %>% 
  select(`unique-id`,
         `question-type`,
         key) %>% 
  rename(unique_id = `unique-id`,
         question_type =`question-type`)

# Then, we merge the `question_type_dat`, with the `references_dat` which contains 
# all the metadata about the publications. In that way, we can subset the entire 
# reference dataset according to question types.

refereces_question_type_dat <- question_type_dat %>% 
  merge(.,
        references_dat,
        by = "key",
        all.x = TRUE) %>% 
  filter(is.na(id)==FALSE)

head(refereces_question_type_dat)

# 2. Data cleaning and tokenization

# Choosing analysis level

# You can choose the publication component (pub_comp) you want to focus your 
# analysis on. In this case, our options are title or abstract. We will analyze
# abstracts

pub_comp = "abstract"

# Tidying text and tokenization

# Our first step to analyze the selected publication component, is to make sure that 
# our text data is tidy. Tidy text data is all in lower case, singular forms, 
# with no punctuation, numeric characters or symbols. These cleaning process could be
# done on each abstract as a whole, but we found that it was most efficient to tokenize
# the abstracts (i.e., break them into individual words) and save the tokens in a new
# data frame. We then clean all the tokens and reassmenble the 'tokenized' abstracts
# back into "clean" paragprahps.

# There are several packages you could use to tokenize a piece of text, here we 
# will use the `tidytext` package for most of our analysis. 

# How to unnest and nest text data in using tidytext? check this post: 
# https://stackoverflow.com/questions/49118539/opposite-of-unnest-tokens-in-r

# 1) Define the new data frame, `pub_dat`, as a subset of the `abs_q_type` dataset 
# and rename the column `abstract` to `pub_comp_words`, so it can be used as a 
# generic variable in downstream analysis:

pub_dat<- dplyr::select(refereces_question_type_dat, 
                        question_type,
                        id, 
                        authors, 
                        year, 
                        journal, 
                        all_of(pub_comp)) %>%
 rename(pub_comp_words = all_of(pub_comp)) %>% 
  
# 2) Break the chunk of (nested) text into tokens (output = word) by using the 
# function unnest_tokens(). We set the argument `drop` to TRUE so the original 
# column with the complete abstracts is removed:
  
  unnest_tokens(input = pub_comp_words, 
                output = word, 
                drop = TRUE) %>%  
  
# 3) We make sure that all of the words are in lower caps and in their singular form
# with the functions `str_to_lower()` and `singularize()`:
  
  mutate(word = str_to_lower(word),
         word = singularize(word)) %>% 
  
# 4) We remove punctuation, numeric characters, and `stop-words` including articles 
# and prepositions (e.g., the, a, as, etc.) with `filter()` and `antijoin()`:
  
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>% 
  anti_join(stop_words, by = "word") %>% 
  
# 5) We reassemble the abstracts back by nesting (`nest()`) the tokens. This operation
# stores the tokens as lists associated to each abstract entry:
  
  nest(data = word) %>%  
  
# 6) We finally unlist the tokens into tidy paragraphs, which, alike the original 
# abstract text, are free from special characters, punctuations, etc.:
  
  mutate(!!paste0(pub_comp) := map_chr(map(data, unlist), paste, collapse = " ")) 

head(pub_dat)

################################################################################
# Conceptual maps from text analysis
################################################################################

###############################################################################
# Parameter definitions


# We start defining the minimum number of word-chunks into which the text is going
# to be broken into (like tidy-words if gram_l = 1 or tidy sentences if gram_l >2.
gram_l = 4

# We then define the bottom of the frequency ranking to be included in the network
breath = 250

# Finally we select the type of question to be represented in the network
question = "roadblocks"

###############################################################################
# Creating the dataset for network analysis: 


# Defining column names for the dataset
n_gram <- paste(gram_l,"gram",sep='-')
a <- seq(1:gram_l)
b <- rep("word",times=gram_l)
columns <- paste(b,a,sep = '')

# 1) Define the new data frame, `pub_ngrams`, as a subset of the `pub_dat` by 
# filtering by question type (e.g., pressing, pathways, roadblocks)
# generic variable in dowstream analysis:

pub_ngrams <- pub_dat %>%
  filter(question_type == question) %>% 

# 2) We break the entire text units from `pub_comp` into chunks (`tokens`) of 
# length = `gram_l` and store the output in the column `n_gram`:
  
  unnest_tokens(n_gram, pub_comp, token = "ngrams", n = gram_l) %>%

# 3) We now separate the tokens into columns of single words:
  
  separate(n_gram, columns, sep = " ", remove = FALSE) %>%
  
# 4) Finally, we count the frequency of each word across all the columns and 
# assign each word a rank and calculate its global frequency across all the 
# abstracts in our sample:
  
  count(across(all_of(columns), ~.x), sort = TRUE) %>%
  mutate(rank = row_number(),
         total = sum(n),
         t_freq = n/total)

head(pub_ngrams)

###############################################################################
# Creating network graph

graph_type <- "network"

# Create the graph using igraph
ngram_graph <- pub_ngrams %>%
  filter(rank < breath) %>%
  graph_from_data_frame()

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
  visEdges(arrows = "to") #%>%
  # 
  # # Add a tooltip
  # visInteraction(hover = TRUE)

# Display the network in the RStudio Viewer pane
network

###############################################################################
# Creating the network as an interactive html object to be embedded into the
# Quarto website

# Generate the HTML code for the network visualization
html_code <- as.character(tags$div(id = "network-container", network))

# Save the network as an HTML file
file_extension <- "html"
file_type <- paste(graph_type, file_extension, sep = '.')
file_name <- paste(question, pub_comp, file_type, sep = '_')
html_file <- paste(figures, file_name, sep = '/')

# Write the HTML code to the file
writeLines(html_code, con = html_file)

# Display a message confirming the file has been saved
cat("HTML file saved:", html_file, "\n")


################################################################################








################################################################################
# Bonus code
################################################################################
# Time-oriented wordclouds
################################################################################

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

