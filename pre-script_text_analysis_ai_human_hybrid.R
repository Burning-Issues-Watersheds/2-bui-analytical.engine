gc()

librarian::shelf(dplyr, tidytext, tidyverse,
                 widyr,igraph, ggraph, plotly,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr,
                 ggwordcloud,tm,scales, ggrepel, ggplotify,zoo,
                 htmlwidgets, htmltools)


# Import and Export paths
assets_pubs <- "../1-bui-knowledge.base/assets/data/raw" 
figures <- "../1-bui-knowledge.base/assets/figs" 


ai_t_df <- read.table(paste(assets_pubs,"chatgpt_pressing_questions_synthesis.txt",sep='/'))
dat <- read_delim("../1-bui-knowledge.base/assets/data/raw/chatgpt_pressing_questions_synthesis.txt")
ai_t_df