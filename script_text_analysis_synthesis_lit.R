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

references_raw_dat <- as_tibble(read_csv(paste(assets_pubs,"zotero_synthesis_papers.csv",sep='/'),
                                         show_col_types = FALSE))
head(references_raw_dat)

question_type_raw_dat <- as_tibble(read_csv(paste(assets_pubs,"zotero_synthesis_literature_answers.csv", sep = '/'),
                                            show_col_types = FALSE))
head(question_type_raw_dat)

###############################################################################
# Parameter definitions
###############################################################################

# Choosing analysis level

# Select the publication component (pub_comp) you want to focus your 
# analysis on. In this case, our options are `title` or `abstract`. We will 
#analyze abstracts

pub_comp = "answers"

# Select the number of word chunks into which the text is going
# to be broken into (like tidy words if gram_l = 1 or tidy sentences if gram_l >2).
gram_l = 4

# Select the bottom of the frequency ranking for words included in 
# the network (more than 500 could be overwhelmingly complex). The idea here is to 
# avoid an excess of word combinations with frequencies = 1
breath = 250

# Select the type of question to be represented in the network, the 
# options can be seen as: 
print(unique(question_type_raw_dat$question_type))
question = "pressing_questions"


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

# In this case we have abstracts retrieved for all 6 publication items.


# Now, we do some minor reformatting on the `question_type_raw_dat` dataset in 
# in preparation to be merged with the `reference_dat` dataset. We remove references
# within parenthesis

# Regular expression pattern to match any text within parentheses containing a number
pattern <- "\\s*\\([^\\)]+\\)"

# Define a custom function to clean the text
clean_text <- function(text) {
  cleaned_text <- str_replace_all(text, pattern, "")
  return(cleaned_text)
}

# Use the custom function with mutate to create a new cleaned column
question_type_dat <- question_type_raw_dat %>%
  mutate(cleaned_answers = clean_text(answers)) %>% 
  select(-answers) %>% 
  rename(answers = cleaned_answers)

question_type_dat$answers


# Then, we merge the `question_type_dat`, with the `references_dat` which contains 
# all the metadata about the publications. In that way, we can subset the entire 
# reference dataset according to question types.

references_question_type_dat <- question_type_dat %>% 
  merge(.,
        references_dat,
        by = "key",
        all.x = TRUE) %>% 
  filter(is.na(id)==FALSE)

head(references_question_type_dat)

# 2. Data cleaning and tokenization

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

pub_dat<- dplyr::select(references_question_type_dat, 
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
         word = if_else(word == "data",
                        word,
                        singularize(word))) %>% 
  
  # 4) We remove punctuation, numeric characters, and `stop-words` including articles 
  # and prepositions (e.g., the, a, as, etc.) with `filter()` and `antijoin()`:
  
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>% 
  anti_join(stop_words, by = "word") %>% 
  
  # 5) We reassemble the abstracts back by nesting (`nest()`) the tokens. This operation
  # stores the tokens as lists associated to each abstract entry:
  
  nest(data = word) %>%  
  
  # 6) We now unlist the tokens into tidy paragraphs, which, alike the original 
  # abstract text, are free from special characters, punctuations, etc.:
  
  mutate(!!paste0(pub_comp) := map_chr(map(data, unlist), paste, collapse = " ")) %>% 
  
  # 7) For simplicity, we will use the word fire across the entire analysis, so its most
  # common synonym, "wildfire" will be replaced across the dataset
  mutate(!!paste0(pub_comp) := str_replace_all(!!sym(paste0(pub_comp)), "wildfire", "fire"))

head(pub_dat)

################################################################################
# Conceptual maps from text analysis
################################################################################
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
  
  # 4) We count the frequency of each word across all the columns and 
  # assign each word a rank and calculate its global frequency across all the 
  # abstracts in our sample:
  
  count(across(all_of(columns), ~.x), sort = TRUE) %>%
  mutate(rank = row_number(),
         total = sum(n),
         t_freq = n/total) %>% 
  # 5) We remove NA values, so they are not included in the ne
  
  drop_na()

head(pub_ngrams)

###############################################################################
# Creating network graph

# Create the graph using igraph
ngram_graph <- pub_ngrams %>%
  filter(rank < breath) %>%
  graph_from_data_frame()

# Apply the force-directed layout algorithm
layout <- layout_with_fr(ngram_graph)

# Create a data frame for nodes
node_df <- data.frame(id = V(ngram_graph)$name, 
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
  visPhysics(stabilization = FALSE) %>%
  
  # Add labels for nodes
  visNodes(label = "label", title = "title", font = list(size = 20)) %>%
  
  # Customize edges
  visEdges(arrows = "to") %>%
  
  # Add a tooltip
  visInteraction(hover = FALSE) %>% 
  
  # Add navigation buttons
  visInteraction(navigationButtons = TRUE)

# Display the network in the RStudio Viewer pane
network

###############################################################################
# Creating the network as an interactive html object to be embedded into the
# Quarto website

# Generate the HTML code for the network visualization
# Generate the HTML code for the network visualization
html_code <- as.character(tags$div(id = "network-container", network))

# Create the widget file name including `question` and `pub_comp`
widget_file <- paste(figures, paste("synthesis_lit",question, pub_comp, "interactive.html", sep = "_"), sep = "/")
saveWidget(network, file = widget_file)

# Modify the HTML code to include the widget file name
html_code <- gsub("network\\.html", paste("synthesis_lit",question, pub_comp, "interactive.html", sep = "_"), html_code)

# Save the modified HTML code to a file
html_file <- paste(figures, paste("synthesis_lit",question, pub_comp, "network.html", sep = "_"), sep = "/")
writeLines(html_code, con = html_file)

# Display a message confirming the files have been saved
cat("HTML widget saved:", widget_file, "\n")
cat("HTML code saved:", html_file, "\n")


################################################################################



