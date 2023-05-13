################################################################################
# Bibliography matching

librarian::shelf(tidyverse,
                 stringr,
                 fuzzyjoin)

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

pattern <- "(?<=\\.)\\s*(?=[^\\(]*$)"

drv_dat1 <- drv_dat %>% 
  select(`question-type`,
         reference) %>% 
  rename(question_type = `question-type`,
         citation = reference) %>% 
  mutate(title = str_extract(citation, pattern))

# Merging the two data frames with Fuzzy Join

library(fuzzyjoin)

# Assuming your first data frame is called 'df1' and the second is called 'df2'
# and the columns with the matching values are called 'a' and 'b', respectively
threshold <- 0.75 # Setting a threshold for similarity
merged_df <- stringdist_left_join(zot_dat1, drv_dat1, by = c("title", "citation"), max_dist = threshold, distance_col = "distance")

threshold <- 0.75 # Setting a threshold for similarity
merged_df <- stringdist_left_join(zot_dat1, drv_dat1, by = c("title", "citation"), max_dist = threshold, distance_col = "distance")


# Filtering based on threshold
merged_df <- merged_df[merged_df$distance <= threshold, ]

# Removing the 'distance' column
merged_df$distance <- NULL


