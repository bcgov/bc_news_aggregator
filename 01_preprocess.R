library(tidyRSS)
library(tidyverse)
library(rvest)
library(here)
library(conflicted)
conflicts_prefer(dplyr::filter)
#constants-----------------------------------------
feeds <- c("https://www.cbc.ca/cmlink/rss-canada-britishcolumbia",
           "https://bcbusiness.ca/feed",
           "https://globalnews.ca/bc/feed",
           "https://news.gov.bc.ca/feed"
           )

title_pattern <- "\\b(Deepwater\\s+Port\\s+Development|Container\\s+Terminal\\s+Expansion|Major\\s+Logistics\\s+Hub|Intermodal\\s+Freight\\s+Corridor|Solar\\s+Farm|Wind\\s+Farm|Hydrogen\\s+Hub|Nuclear\\s+Power\\s+Plant\\s+Construction|LNG|Liquefied\\s+Natural\\s+Gas|Carbon\\s+Capture\\s+and\\s+Storage|CCS\\s+Project|Open\\s+Pit|Mine|Mining|Critical\\s+Minerals\\s+Project|Integrated\\s+Steel\\s+Mill|Oil\\s+Sands|Desalination|Water\\s+Transfer|Reservoir|Flood\\s+Control|Rail\\s+Line|Tunnel|Airport\\s+Terminal|Gigafactory|Semiconductor|Data\\s+Center)\\b"

sentence_pattern <- "\\b(trillion|billion|million|employment|workers|jobs)\\b"

#functions-------------------
get_article_text <- function(url) {
  tryCatch({#this allows reading of url to fail gracefully
    page <- read_html(url)
    text <- page %>% html_nodes("p") %>% html_text() %>% paste(collapse = " ")
    return(text)
  }, error = function(e) {
    return(NA)
  })
}
extract_sentences <- function(text, pattern) {
  sentences <- str_split(text, "(?<=[.!?])\\s+", simplify = FALSE)[[1]]
  sentences <- sentences[str_detect(sentences, pattern)]
}
tidyfeed_wrapper <- function(feed){
  df <- tidyfeed(feed)
  if (!"item_category" %in% names(df)) {
    df$item_category <- NA_character_
  }
  df$item_category <- map_chr(df$item_category, ~{
    if (is.null(.x) || length(.x) == 0) "" else paste(.x, collapse = ", ")
  })
  df
}
#retrieve data-----------------------------

projects_bc <- tibble(feed = feeds) %>%
  mutate(result = map(feed, tidyfeed_wrapper)) %>%
  unnest(result)|>
  filter(grepl(title_pattern, item_title, ignore.case = TRUE)) %>%
  select(feed, item_title, item_link, item_pub_date)|>
  mutate(full_text=map(item_link, get_article_text))|>
  filter(!is.na(full_text))|>
  mutate(key_details=map(full_text, extract_sentences, sentence_pattern))

#if previously scraped exists, read it, otherwise (i.e. first time) create empty

if(file.exists(here("data","scraped.rds"))){
  previous <- read_rds(here("data","scraped.rds"))
}else{
  previous <- tibble("feed"=NA_character_,
                     "item_title"=NA_character_,
                     "item_link"=NA_character_,
                     "item_pub_date"=NA_Date_,
                     "full_text"=list(),
                     "key_details"=list())
}
#avoid adding duplicates------------------------
to_append <- anti_join(projects_bc, previous, by = c("item_title", "item_pub_date"))
#write both previous and new articles to disk
previous|>
  filter(!is.na(item_title))|> #filter out the empty table from first run of script.
  bind_rows(to_append)|>
  write_rds(here::here("data", "scraped.rds"))


