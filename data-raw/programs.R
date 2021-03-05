# PROGRAMS.R
#
# This script exports a table of paper-program correspondences.
#
# Ben Davies
# March 2021

# Load packages
library(bldr)
library(dplyr)
library(haven)
library(readr)
library(stringr)
library(tidyr)

# Import raw metadata
prog = read_dta('data-raw/metadata/prog.dta')

# Import working paper attributes
papers = read_csv('data-raw/papers.csv')

# Create paper-program crosswalk
programs = prog %>%
  mutate(paper = as.integer(sub('^w', '', paper))) %>%
  semi_join(papers) %>%
  distinct() %>%
  arrange(paper, program)

# Export paper-program crosswalk
write_csv(programs, 'data-raw/programs.csv')
save(programs, file = 'data/programs.rda', version = 2, compress = 'bzip2')

# Export program descriptions
program_descriptions = read_csv('data-raw/program_descriptions.csv')
save(program_descriptions, file = 'data/program_descriptions.rda', version = 2, compress = 'bzip2')

# Save session info
save_session_info('data-raw/programs.log')
