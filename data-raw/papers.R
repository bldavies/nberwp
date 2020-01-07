# PAPERS.R
#
# This script generates data/papers.rda.
#
# Ben Davies
# September 2019

# Load packages
library(dplyr)
library(readr)
library(tidyr)

# Import parsed raw data
data <- read_csv('data-raw/nberwo.csv')

# Define function for removing HTML tags, replacing non-ASCII characters
# with ASCII equivalents, and squishing whitespace
clean_text <- function(x) {
  subfun <- function(x, pattern, y) gsub(pattern, y, x, perl = TRUE)
  x %>%
    sapply(function(x) paste0('<p>', x, '</p>')) %>%
    sapply(function(x) paste(rvest::html_text(xml2::read_html(x)), collapse = ' ')) %>%
    iconv('', 'ASCII', sub = 'byte') %>%
    subfun('<c3><a0>', 'a') %>%  # grave
    subfun('<c3><a1>', 'a') %>%  # acute
    subfun('<c3><a9>', 'e') %>%  # acute
    subfun('<c3><ad>', 'i') %>%  # acute
    subfun('<c3><af>', 'i') %>%  # diaresis
    subfun('<c3><b1>', 'n') %>%  # tilde
    subfun('<e2><80><90>', '-') %>%
    subfun('<e2><80><93>', '--') %>%
    subfun('<e2><80><94>', '---') %>%
    subfun('<e2><80><98>', '\'') %>%
    subfun('<e2><80><99>', '\'') %>%
    subfun('<e2><80><9c>', '\"') %>%
    subfun('<e2><80><9d>', '\"') %>%
    subfun('<ef><ac><80>', 'ff') %>%
    subfun('<ef><bb><bf>', '') %>%
    stringr::str_squish()
}

# Define function for fixing title-specific errors
fix_title <- function(x) {
  subfun <- function(x, pattern, y) gsub(pattern, y, x, perl = TRUE)
  x %>%
    subfun('nS', 'n S') %>%  # 138
    subfun('fL', 'f L') %>%  # 158
    subfun('lA', 'l A') %>%  # 171
    subfun('mC', 'm C') %>%  # 179
    subfun('nW', 'n W') %>%  # 305
    subfun('dE', 'd E') %>%  # 336, 4809
    subfun('lT', 'l T') %>%  # 337, 570
    subfun('gC', 'g C') %>%  # 677
    subfun('ldA', 'ld A') %>%  # 732
    subfun('eE', 'e E') %>%  # 949, 1850, 3440
    subfun('yF', 'y F') %>%  # 993
    subfun('eC', 'e C') %>%  # 1063
    subfun('tI', 't I') %>%  # 1348
    subfun('sD', 's D') %>%  # 1348
    subfun('onT', 'on T') %>%  # 1396
    subfun('tR', 't R') %>%  # 1480, 1508
    subfun('eF', 'e F') %>%  # 1484, 4492
    subfun('fI', 'f I') %>%  # 1425, 1655
    subfun('eI', 'e I') %>%  # 1460, 3332
    subfun('fU', 'f U') %>%  # 1463
    subfun('nC', 'n C') %>%  # 1513
    subfun('rM', 'r M') %>%  # 1559, 6753
    subfun('fF', 'f F') %>%  # 1586
    subfun('nV', 'n V') %>%  # 1623
    subfun('tM', 't M') %>%  # 1642
    subfun('eP', 'e P') %>%  # 1650, 4596, 5235
    subfun('tyC', 'ty C') %>%  # 1736
    subfun('nE', 'n E') %>%  # 1763
    subfun('eM', 'e M') %>%  # 1783, 1903
    subfun('lP', 'l P') %>%  # 1969
    subfun('dL', 'd L') %>%  # 1981
    subfun('lD', 'l D') %>%  # 2012, 2682
    subfun('rE', 'r E') %>%  # 2102
    subfun('eR', 'e R') %>%  # 2162, 7385
    subfun('dO', 'd O') %>%  # 2173, 7224
    subfun('sU', 's U') %>%  # 2215, 6263
    subfun('dT', 'd T') %>%  # 2375, 2624
    subfun('rP', 'r P') %>%  # 2543
    subfun('mE', 'm E') %>%  # 2583
    subfun('lB', 'l B') %>%  # 3560
    subfun('dC', 'd C') %>%  # 3583
    subfun('sF', 's F') %>%  # 4270
    subfun('yH', 'y H') %>%  # 4460
    subfun('usS', 'us S') %>%  # 4731
    subfun('hC', 'h C') %>%  # 4786
    subfun('dF', 'd F') %>%  # 4800
    subfun('sT', 's T') %>%  # 4870
    subfun('gI', 'g I') %>%  # 5060
    subfun('hG', 'h G') %>%  # 5170
    subfun('yP', 'y P') %>%  # 5362
    subfun('eS', 'e S') %>%  # 5364, 6732, 24938
    subfun('yM', 'y M') %>%  # 5474
    subfun('DoB', 'Do B') %>%  # 5695
    subfun('QuUality', 'Quality') %>%  # 6753
    subfun('lC', 'l C') %>%  # 7386
    subfun('nA', 'n A') %>%  # 7493
    subfun('n: b', 'n\" b')  # 7493
}

# Collate working paper information
bad_numbers <- c(156, 7255, 7436, 13800, 21929)
papers <- data %>%
  filter(key %in% c('number', 'creation_date', 'title')) %>%
  spread(key, value) %>%
  separate(creation_date, c('year', 'month'), sep = '-') %>%
  mutate_at(c('year', 'month', 'number'), as.integer) %>%
  filter(!(number %in% bad_numbers)) %>%
  mutate(title = clean_text(title),
         title = fix_title(title)) %>%
  select(number, year, month, title) %>%
  arrange(number)

# Export data
write_csv(papers, 'data-raw/papers.csv')
save(papers, file = 'data/papers.rda', version = 2, compress = 'bzip2')

# Save session info
options(width = 80)
write_lines(capture.output(sessioninfo::session_info()), 'data-raw/papers.log')
