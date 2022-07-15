# REPEC.R
#
# This script downloads and parses raw data on historical, technical, and
# general working papers from the NBER RePEc index.
#
# Ben Davies
# July 2022

# Load packages
library(bldr)
library(dplyr)
library(readr)
library(tidyr)

# Initialize output directory
outdir = paste0('data-raw/repec/')
if (!dir.exists(outdir)) dir.create(outdir)

# Specify series
series = c('nberhi', 'nberte', 'nberwo')
series_years = list()
series_years[['nberhi']] = 1989:2001
series_years[['nberte']] = 1979:2007
series_years[['nberwo']] = 1973:2021

# Iterate over series
for (s in series) {

  # Initialize output directory
  outdir = paste0('data-raw/repec/', s)
  if (!dir.exists(outdir)) dir.create(outdir)

  # Define function for downloading raw data
  download_raw_data = function(year) {
    base_url = paste0('https://www.nber.org/RePEc/nbr/', s, '/')
    year_url = sprintf(paste0(base_url, paste0(s, '%d.rdf')), year)
    download.file(year_url, sprintf(paste0(outdir, paste0('/', s, '%d.rdf')), year))
  }

  # Download raw data
  years = series_years[[s]]
  years_needed = paste0(s, years, '.rdf')
  years_missing = years[!(years_needed %in% dir(outdir))]
  lapply(years_missing, download_raw_data)

  # Import raw data
  indata = dir(outdir, '*.rdf', full.name = TRUE) %>%
    lapply(function(x) tibble(file = x, line = read_lines(x))) %>%
    bind_rows()

  # Process data
  outdata = indata %>%
    filter(substr(line, 1, 1) != '#') %>%
    mutate(entry = cumsum(line == '')) %>%
    filter(line != '') %>%
    mutate(text = sub('^([A-Za-z\\-]+):(.*)$', '\\1#@#\\2', line)) %>%
    filter(grepl('#@#', text) & !grepl('File-Restriction', line)) %>%
    separate(text, c('key', 'value'), sep = '#@#') %>%
    mutate(key = tolower(gsub('-', '_', key)),
           value = trimws(value)) %>%
    filter(!key %in% c('file_format', 'file_url', 'handle', 'order_url', 'price', 'publication_status', 'template_type')) %>%
    select(entry, key, value)

  # Export data
  write_csv(outdata, paste0(outdir, '.csv'))

}

# Save session info
save_session_info('data-raw/repec.log')
