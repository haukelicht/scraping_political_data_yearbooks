# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
# Title:  Analyze ECPR Political Data Yearbooks for sections/paragraphs containing 
#          scandal-related terms
# Author: Hauke Licht
# Date:   2019-06-26
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(quanteda)

# create corpus of sections in ECPR political data yearboks ---

pdy_article_content <- read_rds(file.path("data", "political_data_yearbooks.RData"))

pdy_article_content %>% 
  group_by(section) %>% 
  summarise(n = n_distinct(doc_id))

pdy_sections <- pdy_article_content %>% 
  group_by(doc_id, section_nr, section, paragraph_nr, country, data_year) %>% 
  summarise(text = paste(sentence, collapse = "\n")) %>% 
  group_by(doc_id, section_nr, section, country, data_year) %>% 
  summarise(text = paste(text, collapse = "\n\n")) %>% 
  ungroup() %>% 
  mutate(
    article_id = doc_id
    , doc_id = sprintf("%s_sec%03d", article_id, section_nr)
  )


pdy_df <- read_rds(file.path("data", "pdy_articles.RData")) %>%
  mutate(
    publication_date = dmy(publication_date)
    , doc_id = gsub("\\.[a-z]+$", "", basename(article_local_path))
  ) %>% 
  select(-article_local_path) %>% 
  right_join(pdy_article_content, by = "doc_id") %>% 
  mutate(
    article_id = doc_id
    , section_id = sprintf("%s.%03d", article_id, section_nr)
    , paragraph_id = sprintf("%s.%03d.%03d", article_id, section_nr, paragraph_nr)
    , sentence_id = sprintf("%s.%03d.%03d.%03d", article_id, section_nr, paragraph_nr, sentence_nr)
  ) 

pdy_corpus <- quanteda::corpus(pdy_df, docid_field = "sentence_id", text_field = "sentence")


# parse text a data ----

# define dictionary of scandal-related keywords
keywords <- c(
  "^affairs?$"
  , "allegation"
  , "alleged"
  , "brib"
  , "^corruption"
  , "^fraud"
  , "impropr"
  , "misconduct"
  , "scandal"
  # , "siphoning[ -]off"
  , "slander"
  , "sleaze"
  , "^trials?"
  , "^revelations?$"
  , "vote-?buy"
  , "wronging"
)

dict <- dictionary(list(scandal = keywords))

# find paragraphs containing scandal-related keywords

# test
kwic(pdy_corpus, pattern = "^revelations?$", valuetype = "regex", case_insensitive = FALSE)

# apply dictionary at paragraph level
pdy_scandals_df <- pdy_corpus %>% 
  dfm() %>% 
  dfm_weight("prop") %>% 
  dfm_lookup(dict) %>%
  convert("data.frame") %>% 
  as_tibble()

# how many affected sentences
pdy_scandals_df %>% 
  group_by(scandal > 0) %>% 
  summarise(n = n())

pdy_df_scandals <- pdy_df %>% 
  left_join(pdy_scandals_df, by = c("sentence_id" = "document")) %>% 
  mutate(
    has_scandal_term_sen = scandal > 0
  ) %>% 
  group_by(paragraph_id) %>% 
  mutate(
    max_scandal_par = max(scandal)
    , has_scandal_term_par = max_scandal_par > 0
  ) %>% 
  group_by(section_id) %>% 
  mutate(
    max_scandal_sec = max(scandal)
    , has_scandal_term_sec = max_scandal_sec > 0
  ) %>% 
  group_by(doc_id) %>% 
  mutate(
    max_scandal_doc = max(scandal)
    , has_scandal_term_doc = max_scandal_doc > 0
  ) %>% 
  ungroup()


pdy_df_scandals



# how many affected documents?
pdy_df_scandals %>% 
  group_by(has_scandal_term_doc) %>% 
  summarise(n = n_distinct(doc_id))

# how many affected sections?
pdy_df_scandals %>% 
  group_by(has_scandal_term_sec) %>% 
  summarise(n = n_distinct(section_id))

# how many affected paragraphs?
pdy_df_scandals %>% 
  group_by(has_scandal_term_par) %>% 
  summarise(n = n_distinct(paragraph_id))

# how many affected sections?
pdy_df_scandals %>% 
  group_by(has_scandal_term_sen) %>% 
  summarise(n = n_distinct(sentence_id))



