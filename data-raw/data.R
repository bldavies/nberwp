# DATA.R
#
# This script downloads, parses, collates and exports data on NBER working papers.
#
# Ben Davies
# September 2019

# Load packages
library(dplyr)
library(readr)
library(tidyr)

# Locate raw data
raw_data_dir <- 'data-raw/nberwo'

# Define function for downloading raw data
download_raw_data <- function(year) {
  base_url <- 'https://www.nber.org/RePEc/nbr/nberwo/'
  year_url <- sprintf(paste0(base_url, 'nberwo%d.rdf'), year)
  download.file(year_url, sprintf(paste0(raw_data_dir, '/nberwo%d.rdf'), year))
}

# Download raw data
years <- 1973 : 2018
years_needed <- paste0('nberwo', years, '.rdf')
years_missing <- years[!(years_needed %in% dir(raw_data_dir))]
lapply(years_missing, download_raw_data)

# Import raw data
raw_data <- dir(raw_data_dir, '*.rdf', full.name = TRUE) %>%
  lapply(function(x) tibble(file = x, line = read_lines(x))) %>%
  bind_rows()

# Process data
data <- raw_data %>%
  filter(substr(line, 1, 1) != '#') %>%
  mutate(entry = cumsum(line == '')) %>%
  filter(line != '') %>%
  mutate(text = sub('^([A-Za-z\\-]+):(.*)$', '\\1#@#\\2', line)) %>%
  filter(grepl('#@#', text) & !grepl('File-Restriction', line)) %>%
  separate(text, c('key', 'value'), sep = '#@#') %>%
  mutate(key = tolower(gsub('-', '_', key)),
         value = trimws(value)) %>%
  select(entry, key, value)

# Define function for removing HTML tags, replacing non-ASCII characters
# with ASCII equivalents, and squishing whitespace
clean_text <- function(x) {
  subfun <- function(x, pattern, y) gsub(pattern, y, x, perl = TRUE)
  x %>%
    sapply(function(x) paste0('<p>', x, '</p>')) %>%
    sapply(function(x) paste(rvest::html_text(xml2::read_html(x)), collapse = ' ')) %>%
    iconv('', 'ASCII', sub = 'byte') %>%
    subfun('<c3><a0>', 'a') %>%  # grave
    subfun('<c3><a1>', 'a') %>%  # acute
    subfun('<c3><a9>', 'e') %>%  # acute
    subfun('<c3><ad>', 'i') %>%  # acute
    subfun('<c3><b1>', 'n') %>%  # tilde
    subfun('<e2><80><90>', '-') %>%
    subfun('<e2><80><93>', '--') %>%
    subfun('<e2><80><94>', '---') %>%
    subfun('<e2><80><98>', '\'') %>%
    subfun('<e2><80><99>', '\'') %>%
    subfun('<e2><80><9c>', '\"') %>%
    subfun('<e2><80><9d>', '\"') %>%
    subfun('<ef><ac><80>', 'ff') %>%
    subfun('<ef><bb><bf>', '') %>%
    stringr::str_squish()
}

# Collate working paper information
bad_numbers <- c(156, 7255, 7436, 13800, 21929)
papers <- data %>%
  filter(key %in% c('number', 'creation_date', 'title')) %>%
  spread(key, value) %>%
  separate(creation_date, c('year', 'month'), sep = '-') %>%
  mutate_at(c('year', 'month', 'number'), as.integer) %>%
  filter(!(number %in% bad_numbers)) %>%
  mutate(title = clean_text(title)) %>%
  select(number, year, month, title) %>%
  arrange(number)

# Export data
write_csv(papers, 'data-raw/papers.csv')
save(papers, file = 'data/papers.rda', version = 2, compress = 'bzip2')

# Save session info
options(width = 80)
write_lines(capture.output(sessioninfo::session_info()), 'data-raw/data.log')
