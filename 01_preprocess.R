library(tidyverse)
library(httr)
library(rvest)
library(stringi)
library(jsonlite)
library(here)
#constants-----------------------
#Free teer of https://newsapi.org/ only allows 100 queries... get a bunch of keys for development
#api_key <- "5b6bbdb71cee445482b86c36349a0776" #rich.p.martin
api_key <- "8163843a603d4703b5e8c6a38a6040ee" #richard.martin@gov.bc.ca
#api_key <- "c7e22e0d769342c2a7503d2f90480795" #psfs.forecast.coop@gmail.com
#api_key <- "63ce890f43384082b0fed1c8b3256d78" #sophia

blacklist_sources=c("Biztoc.com")
stopwords <- c("forum","comments","email","journalist","accessible","captioning","diversity","opinions")
regions <- c('"British Columbia"', '"B.C."', '"BC"')

search_for <- readxl::read_excel(here("data","mpi_dataset_q1_2025.xlsx"), sheet = "mpi_dataset_q1_2025")|>
  filter(ESTIMATED_COST>=1000)|>
  select(PROJECT_NAME)|>
  mutate(query = paste0('"', PROJECT_NAME, '" AND (', paste(regions, collapse = " OR "), ')'))|>
  select(query)

#functions------------------------
get_article_text <- function(url) {
  tryCatch({
    page <- read_html(url)

    # Attempt to target main article content
    article_nodes <- page %>%
      html_nodes("article p, .article-body p, .story-body p")  # common selectors

    if(length(article_nodes) == 0) {
      article_nodes <- page %>% html_nodes("p")  # fallback
    }

    paragraphs <- article_nodes %>% html_text() %>% str_squish()

    # Keep substantial paragraphs only
    paragraphs <- paragraphs[nchar(paragraphs) > 80]

    # Keep paragraphs where at least 60% of tokens are words or numbers
    paragraphs <- paragraphs[sapply(paragraphs, function(p) {
      tokens <- str_split(p, "\\s+")[[1]]
      ratio <- sum(str_detect(tokens, "^[A-Za-z0-9]+$")) / length(tokens)
      ratio > 0.6
    })]

    # Collapse into a single text, preserving punctuation
    cleaned_text <- paste(paragraphs, collapse = " ")

    return(cleaned_text)
  }, error = function(e) NA)
}

extract_sentences <- function(text, stopwords) {
  sentences <- str_split(text, "(?<=[.!?])\\s+", simplify = FALSE)[[1]]
  trimmed <- str_trim(sentences)  # remove leading/trailing spaces
  long <- trimmed[str_count(trimmed, "\\S+") > 10] #ignore short sentences
  pattern <- paste0("\\b(", paste(stopwords, collapse = "|"), ")\\b") #filter out stopwords
  long[!str_detect(str_to_lower(long), pattern)]
}
normalize_text <- function(x) {
  x %>%
    stri_trans_general("latin-ascii") %>%  # convert accented letters to ASCII
    gsub("\u2018|\u2019|\u201A|\u201B", "'", .) %>%  # smart single quotes → '
    gsub("\u201C|\u201D|\u201E", '"', .) %>%        # smart double quotes → "
    gsub("\u00A0", " ", .) %>%                      # non-breaking space → regular space
    gsub("[\r\n]+", " ", .) %>%                     # normalize newlines
    trimws()                                        # remove leading/trailing spaces
}

get_urls <- function(query) {
  message("Fetching: ", query)

  res <- GET(
    "https://newsapi.org/v2/everything",
    query = list(
      q = query,
      language = "en",
      sortBy = "relevancy",
      pageSize = 10,
      apiKey = api_key
    )
  )

  Sys.sleep(1.2)  # rate limit buffer

  if (status_code(res) != 200) {
    warning("Request failed for query: ", query)
    return(NULL)
  }

  dat <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

  if (is.null(dat$articles) || length(dat$articles) == 0) return(NULL)

  # Coerce safely to data.frame — automatically recycles or fills missing
  df <- as.data.frame(dat$articles, stringsAsFactors = FALSE)

  # Ensure expected columns exist
  needed <- c("source.name", "author", "title", "description", "url", "publishedAt")
  for (col in needed) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

  tibble(
    query = query,
    source = df$source.name,
    title = df$title,
    url = df$url,
    publishedAt = df$publishedAt
  )
}

#get_urls_safe <- safely(get_urls, otherwise = NULL, quiet = TRUE)

new_urls <- search_for|>
  mutate(results=map(query, get_urls))|>
  select(results)|>
  unnest(results)

if(nrow(new_urls)==0) stop("no new urls to scrape")

possibly_already_scraped <- new_urls|>
  mutate(title=normalize_text(title))|>#possible multiple copies of same titled article.
  group_by(title)|>
  filter(publishedAt==max(publishedAt),
         ! source %in% blacklist_sources
         ) #choose the most recent of each title

#need to compare to urls already scraped to only scrape new................

previously_scraped <- read_rds("scraped.rds")

new_scrape <- anti_join(possibly_already_scraped, previously_scraped, by="url")|>
  mutate(text=map(url, get_article_text),
         text=map(text, extract_sentences, stopwords),
         text = lapply(text, unlist)) %>%
  ungroup()|>
  subset(lengths(text) > 0)|>
  filter(!is.na(text))

 if(nrow(new_scrape)>0){
   bind_rows(previously_scraped, new_scrape)|>
     write_rds("scraped.rds")
   rsconnect::deployApp(
     appDir = "bc_news_dashboard.Rmd",
     appName = "bc_news_dashboard",
     account = "richard-martin"
   )
}

