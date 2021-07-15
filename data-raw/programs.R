# PROGRAMS.R
#
# This script exports tables of program attributes and of paper-program correspondences.
#
# Ben Davies
# July 2021

# Load packages
library(bldr)
library(data.table)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Import helper functions
source('data-raw/helpers.R')

# Import raw metadata
paper_programs_raw = fread('data-raw/metadata/working_papers_programs.tab', quote = '')

# Import working paper attributes
papers = read_csv('data-raw/papers.csv')

# Create paper-program crosswalk
paper_programs = paper_programs_raw %>%
  as_tibble() %>%
  select(paper, program) %>%
  filter(grepl('^w[0-9]+', paper)) %>%
  semi_join(mutate(papers, paper = with_prefix(paper, 'w'))) %>%  # ! To be edited !
  distinct() %>%
  arrange(program) %>%
  sort_by_paper() %>%
  mutate(paper = as.integer(sub('^w', '', paper)))  # ! To be deleted !

# Export paper-program crosswalk
write_csv(paper_programs, 'data-raw/paper_programs.csv')
save(paper_programs, file = 'data/paper_programs.rda', version = 2, compress = 'bzip2')

# Export program descriptions
programs = read_csv('data-raw/programs.csv')
save(programs, file = 'data/programs.rda', version = 2, compress = 'bzip2')

# Save session info
save_session_info('data-raw/programs.log')
