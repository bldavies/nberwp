# FACEBOOK.R
#
# This script creates a cleaned copy of Tang et al.'s (2011) data on male and
# female forenames among Facebook users.
#
# Ben Davies
# July 2021


# Load packages
library(bldr)
library(dplyr)
library(readr)

# Define function for replacing non-ASCII characters with their ASCII equivalents
replace_non_ascii = function(x) {
  subfun = function(x, pattern, y) gsub(pattern, y, x, perl = TRUE)
  x %>%
    subfun('À', 'A') %>%
    subfun('Á', 'A') %>%
    subfun('Ã', 'A') %>%
    subfun('Å', 'A') %>%
    subfun('Ä', 'A') %>%
    subfun('Ạ', 'A') %>%
    subfun('Ç', 'C') %>%
    subfun('Ď', 'D') %>%
    subfun('È', 'E') %>%
    subfun('É', 'E') %>%
    subfun('Ê', 'E') %>%
    subfun('Ė', 'E') %>%
    subfun('Ë', 'E') %>%
    subfun('Ğ', 'G') %>%
    subfun('Í', 'I') %>%
    subfun('Ī', 'I') %>%
    subfun('İ', 'I') %>%
    subfun('Ï', 'I') %>%
    subfun('Ĩ', 'I') %>%
    subfun('Ń', 'N') %>%
    subfun('Ň', 'N') %>%
    subfun('Ñ', 'N') %>%
    subfun('Ó', 'O') %>%
    subfun('Ô', 'O') %>%
    subfun('Ồ', 'O') %>%
    subfun('Ö', 'O') %>%
    subfun('Ř', 'R') %>%
    subfun('Ş', 'S') %>%
    subfun('Š', 'S') %>%
    subfun('Ú', 'U') %>%
    subfun('Ü', 'U') %>%
    subfun('Û', 'U') %>%
    subfun('Ż', 'Z')
}

# Download raw data
indata = read_csv('https://sites.google.com/site/facebooknamelist/namelist/firstname.csv')

# Process data
outdata = indata %>%
  `names<-`(c('forename', 'males', 'females', 'total')) %>%
  mutate(forename = tolower(replace_non_ascii(forename)))

# Save data
write_csv(outdata, 'data-raw/sexes/facebook.csv')

# Save session info
save_session_info('data-raw/sexes/facebook.log')
