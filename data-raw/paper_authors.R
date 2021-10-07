# PAPER_AUTHORS.R
#
# This script exports a table of paper-author correspondences.
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

# Prepare table of paper-author correspondences
paper_authors = disambiguated_correspondences %>%
  select(paper, author) %>%
  distinct() %>%
  sort_by_author() %>%
  sort_by_paper()

# Export data
write_csv(paper_authors, 'data-raw/paper_authors.csv')
save(paper_authors, file = 'data/paper_authors.rda', version = 2, compress = 'bzip2')

# Save session info
save_session_info('data-raw/paper_authors.log')
