################################################################################
# Bibliography matching

librarian::shelf(tidyverse)

# Internal paths

raw_data <- "../1-bui-knowledge.base/assets/data"

drv_dat <- read_csv(paste(raw_data,"references_text_analysis.csv", sep = '/' ),
                    show_col_types = FALSE)
zot_dat <- read_csv(paste(raw_data,"zotero_related_papers.csv", sep = '/'),
                    show_col_types = FALSE)


zot_dat1 <- zot_dat %>% 
  select(Key,
         `Item Type`,
         Author,
         `Publication Year`,
         Title,
         `Abstract Note`) %>% 
  rename(key = Key,
         item_type = `Item Type`,
         author = Author,
         year = `Publication Year`,
         title = Title,
         abstract = `Abstract Note`)

drv_dat1 <- drv_dat %>% 
  select(`question-type`,
         reference) %>% 
  rename(question_type = `question-type`,
         citation = reference) %>% 
  na.omit()
  
