# PROGRAMS.R
#
# This script exports a table of paper-program correspondences.
#
# Ben Davies
# March 2021

# Load packages
library(bldr)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Import parsed raw data
nberwo = read_csv('data-raw/nberwo.csv')

# Create paper-program crosswalk
programs = nberwo %>%
  filter(key %in% c('note', 'number')) %>%
  spread(key, value) %>%
  drop_na() %>%
  mutate(paper = as.integer(number),
         program = str_split(note, '\\s+')) %>%
  unnest('program') %>%
  distinct(paper, program) %>%
  arrange(paper, program)

# Export paper-program crosswalk
write_csv(programs, 'data-raw/programs.csv')
save(programs, file = 'data/programs.rda', version = 2, compress = 'bzip2')

# Export program descriptions
program_descriptions = read_csv('data-raw/program_descriptions.csv')
save(program_descriptions, file = 'data/program_descriptions.rda', version = 2, compress = 'bzip2')

# Save session info
save_session_info('data-raw/programs.log')
