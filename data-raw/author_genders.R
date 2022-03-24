# AUTHOR_GENDERS.R
#
# This script exports a table of author genders and associated information.
#
# Ben Davies
# March 2022


# Initialization ----

# Load packages
library(babynames)
library(bldr)
library(dplyr)
library(readr)
library(tidyr)

# Import helper functions
source('data-raw/helpers.R')

# Import author attributes
author_names = read_csv('data-raw/author_names.csv')

# Import data on Facebook forename frequencies
facebook = read_csv('data-raw/genders/facebook.csv')

# Import data constructed manually
genders_via_nber = read_csv('data-raw/genders/via_nber.csv')
genders_via_name = read_csv('data-raw/genders/via_name.csv')


# Data construction ----

# Extract probabilities from SSA baby name data
probabilities_ssa = babynames::babynames %>%
  filter(year %in% 1940:1995) %>%
  group_by(forename = name) %>%
  summarise(p_female = sum(n * (sex == 'F')) / sum(n)) %>%
  ungroup() %>%
  mutate(forename = tolower(forename))

# Extract probabilities from Facebook data
probabilities_fb = facebook %>%
  mutate(p_female = females / (males + females)) %>%
  select(forename, p_female)

# Define function for performing manual overrides
override_with = function(d1, d2) {
  d1 %>%
    left_join(d2) %>%
    mutate(override = !is.na(female) & !is.na(new_female),
           source = case_when(override ~ -source,
                              !is.na(new_female) ~ 0,
                              T ~ source),
           female = ifelse(!is.na(new_female), new_female, female)) %>%
    select(-new_female)
}

# Process estimates
estimates = author_names %>%
  mutate(forename = case_when(grepl('^[A-Za-z]{2}', name) ~ sub('^([A-Za-z-]+) .*', '\\1', name),
                              grepl('^[A-Z] .* .*', name) ~ sub('^[A-Z] ([A-Za-z]+) .*', '\\1', name),
                              T ~ ''),
         forename = tolower(forename)) %>%
  {left_join(.,
    bind_rows(
      mutate(probabilities_ssa, source = 1),
      mutate(probabilities_fb, source = 2)
    ) %>%
      filter(forename %in% .$forename) %>%
      filter(abs(p_female - 0.5) > 0.45) %>%
      mutate(female = round(p_female)) %>%
      select(-p_female) %>%
      group_by(forename) %>%
      slice_min(source) %>%
      ungroup()
  )} %>%
  # Override with data constructed manually
  override_with(genders_via_nber) %>%
  override_with(genders_via_name)


# Finishing up ----

# Prepare data
author_genders = estimates %>%
  select(author, female, source) %>%
  sort_by_author()

# Export data
write_csv(author_genders, 'data-raw/author_genders.csv')

# Save session info
save_session_info('data-raw/author_genders.log')
