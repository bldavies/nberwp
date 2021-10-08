# AUTHORS.R
#
# This script exports a table of author attributes.
#
# Ben Davies
# October 2021

# Load packages
library(bldr)
library(dplyr)
library(readr)
library(tidyr)

# Import intermediate data
author_names = read_csv('data-raw/author_names.csv')
author_sexes = read_csv('data-raw/author_sexes.csv')

# Combine data
authors = author_names %>%
  left_join(author_sexes) %>%
  rename(sex_source = source)

# Export combined data
write_csv(authors, 'data-raw/authors.csv')
save(authors, file = 'data/authors.rda', version = 2, compress = 'bzip2')

# Save session info
save_session_info('data-raw/authors.log')
