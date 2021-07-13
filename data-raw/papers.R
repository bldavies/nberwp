# PAPERS.R
#
# This script exports a table of working paper attributes.
#
# Ben Davies
# July 2021

# Load packages
library(bldr)
library(data.table)
library(dplyr)
library(readr)
library(rvest)
library(stringr)
library(tidyr)
library(xml2)

# Import raw metadata
papers_raw = fread('data-raw/metadata/working_papers.tab', quote = '', encoding = 'Latin-1')

# Set boundary issue date
max_issue_date = '2021-02-28'

# Define function for removing known parenthetical notes
remove_parenthetical_notes = function(x) {
  subfun = function(x, pattern, y) gsub(pattern, y, x, ignore.case = T, perl = TRUE)
  x %>%
    subfun('\\(long version\\)', '') %>%  # 14662, 15660, 15881
    subfun('\\(part.*\\)$', '') %>%  # 3018, 3344
    subfun('\\(rev.*\\)', '') %>%  # 337, 409, 414, 508, 584, 660, 942
    subfun('\\(see also.*\\)', '') %>%  # 3131, 3132
    subfun('\\(series.*ble\\)', '')  # 8467
}

# Define function for removing HTML tags, replacing non-ASCII characters
# with ASCII equivalents, forcing spaces after colons, and squishing whitespace
clean_text = function(x) {
  subfun = function(x, pattern, y) gsub(pattern, y, x, perl = TRUE)
  x %>%
    subfun('<.*?>(.*)</.*?>', '\\1') %>%
    subfun('à', 'a') %>%
    subfun('á', 'a') %>%
    subfun('é', 'e') %>%
    subfun('í', 'i') %>%
    subfun('ï', 'i') %>%
    subfun('ñ', 'n') %>%
    subfun('‘', '\'') %>%
    subfun('’', '\'') %>%
    subfun('“', '"') %>%
    subfun('”', '"') %>%
    subfun('‐', '-') %>%
    subfun('–', '--') %>%
    subfun('—', '---') %>%
    subfun(':([A-Za-z0-9])', ': \\1') %>%
    stringr::str_squish()
}

# Define function for fixing title-specific errors
fix_title = function(x) {
  subfun = function(x, pattern, y) gsub(pattern, y, x, perl = TRUE)
  x %>%
    subfun('THe', 'The') %>%  # 86
    subfun('nS', 'n S') %>%  # 138
    subfun('fL', 'f L') %>%  # 158
    subfun('lA', 'l A') %>%  # 171
    subfun('mC', 'm C') %>%  # 179
    subfun('nW', 'n W') %>%  # 305
    subfun('dE', 'd E') %>%  # 336, 4809
    subfun('lT', 'l T') %>%  # 337, 570
    subfun('n:A', 'n: A') %>%  # 579, 9799
    subfun('e3', 'es') %>%  # 646
    subfun('gC', 'g C') %>%  # 677
    subfun('ldA', 'ld A') %>%  # 732
    subfun('eE', 'e E') %>%  # 949, 1850, 3440
    subfun('yF', 'y F') %>%  # 993
    subfun('eC', 'e C') %>%  # 1063
    subfun(',P', ', P') %>%  # 1106
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
    subfun('RUn', 'Run') %>%  # 1931
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
    subfun('TAx', 'Tax') %>%  # 3074
    subfun('OBs', 'Obs') %>%  # 3132
    subfun('BAr', 'Bar') %>%  # 3188
    subfun('acor', 'acro') %>%  # 3344
    subfun('lB', 'l B') %>%  # 3560
    subfun('dC', 'd C') %>%  # 3583
    subfun('sF', 's F') %>%  # 4270
    subfun('yH', 'y H') %>%  # 4460
    subfun('e1', 'e 1') %>%  # 4496
    subfun('usS', 'us S') %>%  # 4731
    subfun('ationa ', 'ations ') %>%  # 4754, 10565
    subfun('hC', 'h C') %>%  # 4786
    subfun('dF', 'd F') %>%  # 4800
    subfun('sT', 's T') %>%  # 4870
    subfun('gI', 'g I') %>%  # 5060
    subfun('hG', 'h G') %>%  # 5170
    subfun('yP', 'y P') %>%  # 5362
    subfun('eS', 'e S') %>%  # 5364, 6732, 24938
    subfun('yM', 'y M') %>%  # 5474
    subfun('DoB', 'Do B') %>%  # 5695
    subfun(',I', ', I') %>%  # 6223
    subfun('lyis', 'lysis') %>%  # 6310
    subfun('SYs', 'Sys') %>%  # 6436
    subfun('QuUality', 'Quality') %>%  # 6753
    subfun('lC', 'l C') %>%  # 7386
    subfun('nA', 'n A') %>%  # 7493
    subfun('n: b', 'n\" b') %>%  # 7493
    subfun('SHe', 'She') %>%  # 8060
    subfun(',a', ', a') %>%  # 9614
    subfun('s1', 's') %>%  # 10447
    subfun('mis ', 'mic ') %>%  # 11470
    subfun('N 1$', 'N+1') %>%  # 11713
    subfun('jA', 'ja') %>%  # 12393
    subfun('\\%u2019', '\'') %>%  # 12396
    subfun(',2004', ', 2004') %>%  # 12607
    subfun(',1980', ', 1980') %>%  # 15274
    subfun('ter\\?Cyc', 'ter-Cyc') %>%  # 18062
    subfun('A-az', 'iaz') %>%  # 21350
    subfun('^\\?', '') %>%  # 21802
    subfun('E\\?ect', 'Effect') %>%  # 22950
    subfun('dor\\?Uni', 'dor-Uni') %>%  # 23018
    subfun('^q\\?$', 'q5') %>%  # 24709
    subfun(',W', ', W') %>%  # 25311
    subfun('Di\\?er', 'Differ') %>%  # 25380, 26375
    subfun('Pro\\?t', 'Profit') %>%  # 26027
    subfun('\\?E', '? E') %>%  # 26268
    subfun(' o\\? ', ' off ')  %>%  # 26624
    subfun('i\\?I', 'i-I') %>%  # 27175
    subfun('DEg', 'Deg') %>%  # 27239
    subfun(' R\\? ', ' R ') %>%  # 27632
    subfun('R\\?1', 'R<=1')  # 28093
}

# Collate working paper information
bad_numbers = c(156, 623, 2432, 7044, 7255, 7436, 7565, 8649, 9101, 9694, 13410, 13800, 21929, 28460, 28473)
papers = papers_raw %>%
  as_tibble() %>%
  filter(grepl('^w[0-9]', paper)) %>%
  filter(issue_date <= max_issue_date) %>%
  mutate(paper = as.integer(sub('^w', '', paper)),
         year = as.integer(substr(issue_date, 1, 4)),
         month = as.integer(substr(issue_date, 6, 7))) %>%
  select(paper, year, month, title) %>%
  mutate(title = remove_parenthetical_notes(title),
         title = clean_text(title),
         title = fix_title(title)) %>%
  filter(paper > 0) %>%
  filter(!paper %in% bad_numbers) %>%
  arrange(paper)

# Export data
write_csv(papers, 'data-raw/papers.csv')
save(papers, file = 'data/papers.rda', version = 2, compress = 'bzip2')

# Save session info
save_session_info('data-raw/papers.log')
