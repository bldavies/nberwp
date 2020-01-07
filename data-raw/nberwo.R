# NBERWO.R
#
# This script downloads and parses raw data on NBER working papers.
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

# Export data
write_csv(data, 'data-raw/nberwo.csv')

# Save session info
options(width = 80)
write_lines(capture.output(sessioninfo::session_info()), 'data-raw/nberwo.log')