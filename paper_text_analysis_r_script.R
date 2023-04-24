###############################################################################
#Text Analysis of a Research Article
###############################################################################

# Setting up libraries
require(librarian)
librarian::shelf(dplyr, tidytext, tidyverse,stringr,stringi,
                 widyr,igraph, ggraph,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr, pdftools)

# # Downloading the file
# download.file("https://www.nature.com/articles/s41467-021-22747-3.pdf", 
#               destfile = "~/GitHub/burn_iss-website/assets/wq_burning1.pdf", mode = "wb")

# Reading our file
ppr <- pdf_text("assets/wq_burning.pdf")
info <- pdf_info("assets/wq_burning.pdf")
ppr_txt0 <- unlist(ppr)
ppr_txt <- data.frame(line = 1:8, text = ppr_txt0)

# Extracting pdf metadata and removing it from text body

sbj <- unlist(strsplit(info$keys$Subject, " "))
dmn <- unlist(strsplit(info$keys$`CrossMarkDomains[1]`," "))
ctr <- unlist(strsplit(info$keys$Creator, " "))
doi <- info$keys$doi
cdm <- info$keys$`CrossMarkDomains[2]`

# Cleaning pdf file
ppr_txt$text <- sub(paste(sbj, collapse = '|'),'',ppr_txt)
ppr_txt$text <- sub(paste(dmn, collapse = '|'),'',ppr_txt)
ppr_txt$text <- sub(paste(ctr, collapse = '|'),'',ppr_txt)
# ppr_txt$text <- sub(paste(doi),'',ppr_txt)#long exec time
# ppr_txt$text <- sub(paste(cdm, collapse = '|'),'',ppr_txt)#long exec time


#Word cloud

ppr_cln <- ppr_txt%>% 
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(str_detect(word,"[:alpha:]")) %>% 
  count(word, sort = TRUE) %>% 
  mutate(length = nchar(word)) %>% 
  select(word, n) %>% 
  mutate(rank = row_number(),
         total=sum(n),
         t_freq = n/total)

ppr_cln %>% with(wordcloud(word,n,max.words = 400))








  

