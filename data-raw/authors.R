# AUTHORS.R
#
# This script exports a table of author attributes.
#
# Ben Davies
# March 2022

# Load packages
library(bldr)
library(dplyr)
library(readr)
library(tidyr)

# Import intermediate data
author_names = read_csv('data-raw/author_names.csv')
author_genders = read_csv('data-raw/author_genders.csv')

# Combine data
authors = author_names %>%
  left_join(author_genders) %>%
  rename(female_source = source)

# Export combined data
write_csv(authors, 'data-raw/authors.csv')
save(authors, file = 'data/authors.rda', version = 2, compress = 'bzip2')

# Save session info
save_session_info('data-raw/authors.log')
