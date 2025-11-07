library(tidyRSS)
library(tidyverse)
library(rvest)
library(here)
library(conflicted)
conflicts_prefer(dplyr::filter)
#functions-------------------
get_article_text <- function(url) {
  tryCatch({
    page <- read_html(url)
    text <- page %>% html_nodes("p") %>% html_text() %>% paste(collapse = " ")
    return(text)
  }, error = function(e) {
    return(NA)
  })
}
extract_sentences <- function(text) {
  # Split into sentences using punctuation followed by space
  sentences <- str_split(text, "(?<=[.!?])\\s+", simplify = FALSE)[[1]]
  sentence_keywords <- c("trillion","billion","million","employment","workers","jobs")
  sentence_pattern <- paste0("\\b(", paste0(sentence_keywords, "s?", collapse = "|"), ")\\b")
  sentences <- sentences[str_detect(sentences, sentence_pattern)]
  sentences <- head(sentences, -1) #get rid of source info
}

#pattern to look for in title

title_keywords <- c(
  "Port", "Logistics", "Deepwater", "Container", "Terminal",
  "Intermodal", "Freight", "Energy", "Utility", "Solar", "Wind",
  "Hydrogen", "Nuclear", "LNG", "Transmission", "Pipeline",
  "Liquefied", "Natural", "Gas", "CCS", "Mine", "Mining",
  "Critical", "Mineral", "Dam", "Reservoir", "Tunnel", "Giga"
)

title_pattern <- paste0("\\b(", paste0(title_keywords, "s?", collapse = "|"), ")\\b")

#get new data-----------------------------

projects_bc <- tibble(feed = c(
  "https://www.cbc.ca/cmlink/rss-canada-britishcolumbia",
  "https://bcbusiness.ca/feed",
  "https://globalnews.ca/bc/feed",
  "https://news.gov.bc.ca/feed"
)) %>%
  mutate(result = map(feed, ~{
    df <- tidyfeed(.x)

    # Ensure column exists
    if (!"item_category" %in% names(df)) {
      df$item_category <- NA_character_
    }

    # Ensure item_category is character
    df$item_category <- map_chr(df$item_category, ~{
      if (is.null(.x) || length(.x) == 0) "" else paste(.x, collapse = ", ")
    })

    df
  })) %>%
  unnest(result)|>
  filter(grepl(title_pattern, item_title, ignore.case = TRUE)) %>%
  select(feed, item_title, item_link, item_pub_date)|>
  mutate(full_text=map(item_link, get_article_text))|>
  filter(!is.na(full_text))|>
  mutate(key_details=map(full_text, extract_sentences))

#if scraped exists, read it, otherwise create empty---------------------

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

to_append <- anti_join(projects_bc, previous, by = c("item_title", "item_pub_date"))

previous|>
  filter(!is.na(item_title))|>
  bind_rows(to_append)|>
  write_rds(here::here("data", "scraped.rds"))


