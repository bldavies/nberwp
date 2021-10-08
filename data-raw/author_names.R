# AUTHOR_NAMES.R
#
# This script exports a table of author names.
#
# Ben Davies
# October 2021

# Load packages
library(bldr)
library(dplyr)
library(readr)
library(tidyr)

# Import helper functions
source('data-raw/helpers.R')

# Import disambiguated paper-author-(user)name correspondences
disambiguated_correspondences = read_csv('data-raw/disambiguated_correspondences.csv')

# Prepare table of author names
author_names = disambiguated_correspondences %>%
  group_by(author, user_nber, user_repec) %>%
  summarise(name = name[which.max(nchar(name))]) %>%
  ungroup() %>%
  select(author, name, user_nber, user_repec) %>%
  sort_by_author()

# Assert that author IDs are unique
if (nrow(author_names) != n_distinct(author_names$author)) {
  stop('Author IDs are not unique')
}

# Export data
write_csv(author_names, 'data-raw/author_names.csv')

# Save session info
save_session_info('data-raw/author_names.log')
