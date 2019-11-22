# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
# Title:  Scrape election reports	
# Author: Hauke Licht
# Date:   2019-06-26
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----
library(rvest)
library(urltools)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)

# helpers 
get_redirected_url <- function(url){
  
  resp <- httr::GET(url)
  # extract status 
  stat <- map_int(resp$all_headers, "status")
  
  # check status and return
  if(any(stat >= 300 & stat < 400)){
    url <- resp$url
  }
  
  return(url)
}



# ECPR Political Data Yearbooks ----

# get country yearbook links
pdy_url <- get_redirected_url(url = "https://onlinelibrary.wiley.com/journal/20478852")

# parse URL
pdy_url_parsed <- url_parse(pdy_url)

resp <- read_html(pdy_url)

pdy_country_links <- resp %>% 
  html_nodes(xpath = '//*[@id="main-content"]/div/div/section/div/div/div/div[1]/div[2]') %>% 
  html_nodes("a") %>% 
  {tibble(
    name = html_text(.)
    , link = html_attr(., "href")
  )} %>% 
  mutate(
    country = case_when(
      name == "UK" ~ "United Kingdom"
      , name == "USA " ~ "United States"
      , TRUE ~ name 
    )
  )


countries <- read_csv(file.path("data", "countries.csv"))[,-1]

idxs <- which(pdy_country_links$country %in% countries$country_name)

# get country yearbook article links
pdy_country_article_links <- map2_df(
  pdy_country_links$name[idxs]
  , pdy_country_links$link[idxs]
  , function(
    ctr
    , path
    , url.base = paste0(pdy_url_parsed$scheme, "://", pdy_url_parsed$domain)
  ) {
    
    dflt_out <- tibble(
      link = link
      , country = ctr
      , publish_year = NA_integer_
      , data_year = NA_integer_
      , source_url = NA_character_
    )
    
    if(!grepl(url.base, path))
      path <- paste0(url.base, path)

    resp <- tryCatch(
        read_html(path)
        , error = function(err) err
      )
    
    if (inherits(resp, "error")) 
      return(dflt_out)
    
    tryCatch(
      resp %>% 
        html_nodes("div.main-content") %>% 
        html_nodes("table") %>% 
        html_nodes("a") %>% 
        {tibble(
          name = trimws(html_text(.))
          , link = html_attr(., "href")
        )} %>% 
        filter(grepl("^\\d{4}\\sPolitical", name)) %>%
        mutate(
          country = ctr
          , publish_year = as.integer(gsub("^(\\d{4}).+", "\\1", name))
          , data_year = publish_year-1L
          , source_url = gsub("/(pdf|full)$", "", link)
          , source_url = gsub("%\\d{2}", "", source_url)
          , source_url = case_when(
            grepl("^/doi/", link) ~ paste0(url.base, gsub("^/doi/(abs/)?([a-z\\d\\./-]+)$", "/doi/\\2", link, perl = TRUE))
            , grepl("^http://dx\\.doi\\.org/", link) ~ paste0(url.base, gsub("^http://dx\\.doi\\.org/(.+)$", "/doi/\\1", link, perl = TRUE))
            , grepl(paste0("^", url.base, "/doi/abs/"), link) ~ sub("/abs/", "/", link)
            , TRUE ~ link
          )
        ) %>% 
        select(-name)
      # return default out if error
      , error = function(err) dflt_out
    )
  }
) %>% 
  mutate(
    country = case_when(
      country == "UK" ~ "United Kingdom"
      , country == "USA " ~ "United States"
      , TRUE ~ country 
    )
  )


# any no URL?
pdy_country_article_links %>% 
  filter(is.na(source_url))


# check what articles already downloaded
pdy_existing <- list.files(file.path("webdata")) %>% 
  tibble::enframe(name = NULL) %>% 
  tidyr::extract(value, c("country", "data_year"), regex = "([a-z_]+)_(\\d{4}).+", convert = TRUE) %>% 
  mutate(country = str_to_title(gsub("_", " ", country))) 

# read and save article content
pdy_articles <- pdy_country_article_links %>% 
  filter(!is.na(source_url)) %>% 
  filter(data_year > 2009) %>%
  # # do not get daat for year books that have already been downloaded
  # anti_join(pdy_existing) %>% 
  filter(country %in% countries$country_name) %>% 
  select(-link) %>% 
  pmap_dfr(function(
      country
      , publish_year
      , data_year
      , source_url
      , url.base = paste0(pdy_url_parsed$scheme, "://", pdy_url_parsed$domain)
    ){
      
      out <- tibble(
        source_url = source_url
        , doi = NA_character_
        , title = NA_character_
        , authors = vector("list", 1L)
        , publication_date = NA_character_
        , article_local_path = NA_character_
      )
      
      if(!grepl(url.base, source_url))
        source_url <- paste0(url.base, source_url)
      
      resp <- tryCatch(
        read_html(source_url)
        , error = function(err) err
      )
      
      if (inherits(resp, "error")) 
        return(out)
      
      out$doi <- tryCatch(
        resp %>% 
          html_node("a.epub-doi") %>% 
          html_attr("href") %>% 
          sub(x = ., pattern = "^https://doi\\.org/(.+)$", replacement = "\\1") 
        , error = function(err) NA_character_
      )
      
      out$title <- tryCatch(
        resp %>% 
          html_node("h2.citation__title") %>% 
          html_text()
        , error = function(err) NA_character_
      )
      
      out$authors <- tryCatch(
        resp %>% 
          html_nodes("a.author-name") %>% 
          {tibble(
            name = stringr::str_to_title(html_nodes(., "span") %>% html_text())
            , link = paste0(url.base, html_attr(., "href"))
          )} %>% 
          unique() %>% 
          split(f = .$name) %>% 
          set_names(nm = NULL) %>% 
          list()
        , error = function(err) vector("list", 1L)
      )
      
      out$publication_date <- tryCatch(
        resp %>% 
          html_nodes("span.epub-date") %>% 
          html_text()
        , error = function(err) NA_character_
      )
      
      content <- tryCatch(
        resp %>% 
          html_nodes("article") %>% 
          html_nodes("section.article-section__full")
        , error = function(err) err
      )
      
      if (inherits(content, "error") || length(content) == 0)
        return(out)
      
      
      file_name <- file.path(".", "webdata", sprintf("%s.html", tolower(paste0(gsub("\\s+", "_", country), "_", data_year))))
      
      # save to disk
      write_html(content, file_name, options = "format") 
      
      out$article_local_path <- file_name
      
      return(out)
    }
  )



pdy_articles

write_rds(pdy_articles, file.path("data", "pdy_articles.RData"))


pdy_article_content <- list.files("webdata", full.names = T, pattern = "*.html") %>% 
  map_df(function(file.path) {
    # read from disk
    read_html(file.path, encoding = "UTF-8") %>% 
      # get (sub)sections
      html_nodes(xpath = paste0("//div[@class='article-section__", c("sub-content']", "content']"), collapse = "|")) %>% 
      map_df(function(node){
        tibble(
          # get section header
          section = html_node(node, css = "[class^=article-section]") %>%
            html_text() %>%
            trimws()
          # get section content
          , text = html_nodes(node, "p") %>% 
            # note: string conversion automatically gets rid of all html tags 
            # (and converts them to plain-style text)
            html_text() %>% 
            gsub(x = ., pattern = "\\n", replacement = "<newpar>") %>% 
            paste(collapse = " ") %>% 
            trimws()
        )
      }) %>% 
      # numerate sections
      mutate(section_nr = row_number()) %>% 
      # split sections by paragraph into separate rows
      separate_rows(text, sep = "<newpar>") %>% 
      mutate_at(vars(text), trimws) %>% 
      # numerate paragraphs
      group_by(section) %>% 
      mutate(paragraph_nr = row_number()) %>% 
      # split paragraphs by sentences into separate rows
      # separate_rows(text, sep = "(?<=[.!?])\\s+(?=[A-Z0-9])") %>% 
      separate_rows(text, sep = "(?<=[.!?])\\s+(?=[A-Z])") %>% 
      filter(text != "") %>% 
      # numerate sentences within rows
      group_by(section) %>% 
      mutate(sentence_nr = row_number()) %>% 
      ungroup() %>% 
      mutate(
        doc_id = gsub(".+/([a-z_]+)_(\\d{4})\\.[a-z]+$", "\\1_\\2", file.path)
        , country = stringr::str_to_title(gsub("_", " ", sub("( \\d{4})$", "", doc_id)))
        , data_year = stringr::str_to_title(sub(".+(\\d{4})$", "\\1", doc_id))
      ) %>% 
      select(
        doc_id
        , section_nr
        , section
        , paragraph_nr
        , sentence_nr
        , sentence = text
        , country
        , data_year
      )
  }) %>% 
  mutate_at(vars(data_year), as.integer)


write_rds(pdy_article_content, file.path("data", "political_data_yearbooks.RData"))
