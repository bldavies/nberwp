# AUTHORS.R
#
# This script exports a table of paper-author correspondences.
#
# Ben Davies
# March 2021


# Initialization ----

# Load packages
library(bldr)
library(dplyr)
library(igraph)
library(readr)
library(stringdist)
library(stringr)
library(tidyr)

# Import raw data
auths = read_tsv('data-raw/metadata/auths.txt')
author_user = read_tsv('data-raw/metadata/author_user.txt')
nberwo = read_csv('data-raw/nberwo.csv')

# Import processed data
papers = read_csv('data-raw/papers.csv')
programs = read_csv('data-raw/programs.csv')


# Raw data cleaning ----

# Define function for replacing non-ASCII chracters with their ASCII equivalents
replace_non_ascii = function(x) {
  subfun = function(x, pattern, y) gsub(pattern, y, x, perl = TRUE)
  x %>%
    subfun('’', '\'') %>%
    subfun('à', 'a') %>%
    subfun('Á', 'A') %>%
    subfun('á', 'a') %>%
    subfun('ã', 'a') %>%
    subfun('ä', 'a') %>%
    subfun('â', 'a') %>%
    subfun('å', 'a') %>%
    subfun('Å', 'A') %>%
    subfun('ć', 'c') %>%
    subfun('Č', 'C') %>%
    subfun('č', 'c') %>%
    subfun('Ç', 'C') %>%
    subfun('ç', 'c') %>%
    subfun('ð', 'd') %>%
    subfun('đ', 'd') %>%
    subfun('è', 'e') %>%
    subfun('É', 'E') %>%
    subfun('é', 'e') %>%
    subfun('ë', 'e') %>%
    subfun('ê', 'e') %>%
    subfun('ě', 'e') %>%
    subfun('ę', 'e') %>%
    subfun('ğ', 'g') %>%
    subfun('ǧ', 'g') %>%
    subfun('ı', 'i') %>%
    subfun('í', 'i') %>%
    subfun('ï', 'i') %>%
    subfun('î', 'i') %>%
    subfun('Ł', 'L') %>%
    subfun('ł', 'l') %>%
    subfun('Ľ', 'L') %>%
    subfun('ń', 'n') %>%
    subfun('ñ', 'n') %>%
    subfun('Ò', 'O') %>%
    subfun('ò', 'o') %>%
    subfun('Ó', 'O') %>%
    subfun('ó', 'o') %>%
    subfun('ő', 'o') %>%
    subfun('õ', 'o') %>%
    subfun('Ö', 'O') %>%
    subfun('ö', 'o') %>%
    subfun('ô', 'o') %>%
    subfun('Ø', 'O') %>%
    subfun('ø', 'o') %>%
    subfun('ř', 'r') %>%
    subfun('Š', 'S') %>%
    subfun('š', 's') %>%
    subfun('Ṣ', 'S') %>%
    subfun('Ş', 'S') %>%
    subfun('ş', 's') %>%
    subfun('ț', 't') %>%
    subfun('ú', 'u') %>%
    subfun('Ü', 'U') %>%
    subfun('ü', 'u') %>%
    subfun('û', 'u') %>%
    subfun('ÿ', 'y') %>%
    subfun('æ', 'ae') %>%
    subfun('Þ', 'Th') %>%
    subfun(' ⓡ', '')
}

# Define function for cleaning author names
clean_name = function(x) {
  subfun = function(x, pattern, y) gsub(pattern, y, x, perl = TRUE)
  x %>%
    subfun(' Jr', '') %>%
    subfun('[,]+', '') %>%
    subfun('Bertl', 'Bertil') %>%
    subfun('Bobie', 'Bodie') %>%
    subfun('Caballlero', 'Caballero') %>%
    subfun('Cogaj', 'Cogan') %>%
    subfun('C[*]', 'C') %>%
    subfun('C -L|L -C', 'L-C') %>%
    subfun('C -Y', 'C-Y') %>%
    subfun('Feldstean', 'Feldstein') %>%
    subfun('Fullterton', 'Fullerton') %>%
    subfun('Giles', 'Gilles') %>%
    subfun('Haijime', 'Hajime') %>%
    subfun('Nagataki', 'Nagatake') %>%
    subfun('Romaine', 'Romain') %>%
    subfun('Siata', 'Saita') %>%
    subfun('Stiflitx', 'Stiglitz') %>%
    subfun('Stratman$', 'Stratmann') %>%
    subfun('Tuillo', 'Tullio') %>%
    subfun('wickstrom', 'Wickstrom')
}

# Extract raw authorship data with NBER user names
authors_raw_nber = auths %>%
  bind_cols(author_user) %>%
  select(paper = paper...1, name, user_nber = author_user) %>%
  filter(grepl('^w[0-9]+$', paper)) %>%
  mutate(paper = as.integer(sub('^w', '', paper))) %>%
  semi_join(papers) %>%
  # Clean author names
  mutate(name = replace_non_ascii(name),
         name = gsub('\\.', ' ', name),
         name = str_squish(name),
         name = clean_name(name),
         name = ifelse(name == 'NULL', NA, name),
         user_nber = ifelse(user_nber == 'NULL', NA, user_nber)) %>%
  distinct() %>%
  # Delete 1:m name:user matches
  add_count(paper, user_nber) %>%
  mutate(user_nber = replace(user_nber, n > 1, NA)) %>%
  select(-n) %>%
  # Do some manual editing
  filter(name != 'M B Landrum J Newhouse') %>%
  bind_rows(tribble(
    ~paper, ~name, ~user_nber,
    7760, 'Mary Beth Landrum', 'MaryBeth_Landrum',
    7760, 'Joseph P Newhouse', 'joseph_newhouse',
    22701, 'Andrei Shleifer', 'andrei_shleifer'
  ))

# Extract raw authorship data with RePEc user names
authors_raw_repec = nberwo %>%
  filter(key %in% c('number', 'author_name', 'author_person')) %>%
  group_by(entry) %>%
  mutate(paper = as.integer(value[which(key == 'number')]),
         pos = cumsum(key == 'author_name')) %>%
  ungroup() %>%
  filter(key != 'number') %>%
  select(-entry) %>%
  spread(key, value) %>%
  semi_join(papers) %>%
  select(paper, name = author_name, user_repec = author_person) %>%
  # Clean author names
  mutate(name = replace_non_ascii(name),
         name = gsub('\\.', ' ', name),
         name = str_squish(name),
         name = clean_name(name),
         name = ifelse(name == 'NULL', NA, name)) %>%
  distinct()

# Define function for asserting NBER:RePEc user name correspondences is m:1
assert_many_to_one = function(x) {
  t = x %>%
    drop_na() %>%
    distinct(user_nber, user_repec) %>%
    count(user_nber, sort = T) %>%
    {max(.$n)}
  if (t > 1) {
    stop('NBER:RePEc user name correspondence is not m:1')
  }
  x
}

# Define function for reassigning NBER user names
reassign_manually = function(x) {
  x = replace(x, x == 'elizabeth_richardson', 'elizabeth_richardson_vigdor')
}

# Merge raw authorship data, treating NBER records as ground truth
authors_raw = authors_raw_nber %>%
  left_join(authors_raw_repec) %>%
  arrange(paper, name) %>%
  # Do manual reassignments and catch known RePEc user name mis-codings
  mutate(user_nber = reassign_manually(user_nber)) %>%
  mutate(user_repec = replace(user_repec, user_nber %in% c('george_wu', 'ye_qi'), NA)) %>%
  # Assert that NBER:RePEc user name correspondences are m:1
  assert_many_to_one() %>%
  # Collapse m:1 NBER:RePEc matches to 1:1
  group_by(user_repec) %>%
  mutate(user_nber = replace(user_nber, !is.na(user_repec), first(user_nber))) %>%
  ungroup() %>%
  # Assign new IDs for next stage of disambiguation process
  group_by(user_nber) %>%
  arrange(paper) %>%
  mutate(id = ifelse(!is.na(user_nber), row_number(), 1)) %>%
  ungroup() %>%
  arrange(user_nber, paper) %>%
  mutate(id = cumsum(id == 1),
         no_nber = is.na(user_nber))


# Fuzzy matching initialization ----

# Define function for computing string distances, ignoring non-alpha characters
get_distance = function(x, y, ...) {
  func = function(s) tolower(gsub('[^[:alpha:]]', '', s))
  stringdist(func(x), func(y), ...)
}

# Define function for testing sub-string containment
test_substrings = function(x, y) {
  abs(nchar(x) - nchar(y)) == stringdist(x, y, 'lcs')
}

# Define function for counting sub-string intersections
count_name_intersections = function(x, y) {
  n = length(x)
  sapply(1:n, function(i) {
    xs = strsplit(gsub('\\s+', ' ', gsub('[^[:alpha:]]', ' ', x[i])), ' ')[[1]]
    ys = strsplit(gsub('\\s+', ' ', gsub('[^[:alpha:]]', ' ', y[i])), ' ')[[1]]
    xs_matches = sum(sapply(xs, function(s) any(grepl(s, ys))))
    ys_matches = sum(sapply(ys, function(s) any(grepl(s, xs))))
    max(xs_matches, ys_matches)
  })
}

# Define function for extracting ID reassignments
#
# Idea: matches form adjacent pairs in a network, the components of which
# correspond to sets of IDs linked by (possibly asymmetric) matches
extract_reassignments = function(x) {
  x %>%
    select(id.x, id.y) %>%
    graph_from_data_frame(directed = F) %>%
    components() %>%
    {tibble(from_id = as.integer(names(.$membership)), component = .$membership)} %>%
    group_by(component) %>%
    mutate(to_id = min(from_id)) %>%
    ungroup() %>%
    select(from_id, to_id) %>%
    filter(from_id != to_id)
}


# Matching on common co-authors ----

# Identify unique ID-name pairs
id_names = authors_raw %>%
  distinct(id, name)

# Identify unique ID-paper pairs
id_papers = authors_raw %>%
  distinct(id, paper)

# Extract ID reassignments based on common co-author matching
reassignments_cc = authors_raw %>%
  filter(is.na(user_nber)) %>%
  select(id.x = id, paper) %>%
  # Determine co-authors
  left_join(id_papers) %>%
  select(id.x, cc_id = id) %>%
  filter(id.x != cc_id) %>%
  distinct() %>%
  # Determine co-authors' papers
  left_join(id_papers, by = c('cc_id' = 'id')) %>%
  select(id.x, cc_paper = paper) %>%
  distinct() %>%
  # Determine match candidates (authors with whom x shares common co-author)
  left_join(id_papers, by = c('cc_paper' = 'paper')) %>%
  select(id.x, id.y = id) %>%
  filter(id.x != id.y) %>%
  distinct() %>%
  # Apply name similarity criteria
  left_join(id_names, by = c('id.x' = 'id')) %>%
  left_join(id_names, by = c('id.y' = 'id')) %>%
  mutate(cosine = get_distance(name.x, name.y, 'cosine')) %>%
  filter(cosine < 0.05) %>%
  mutate(t = test_substrings(name.x, name.y)) %>%
  mutate(ni = count_name_intersections(name.x, name.y)) %>%
  filter(t | ni >= 3) %>%  # Checked manually
  # Extract reassignments
  extract_reassignments()

# Apply reassignments
authors_post_cc = authors_raw %>%
  left_join(reassignments_cc, by = c('id' = 'from_id')) %>%
  mutate(id = ifelse(!is.na(to_id), to_id, id)) %>%
  select(-to_id) %>%
  group_by(id) %>%
  arrange(user_nber) %>%
  mutate(user_nber = first(user_nber)) %>%
  ungroup()


# Matching on common programs ----

# Update unique ID-name pairs
id_names = authors_post_cc %>%
  distinct(id, name)

# Identify unique ID-program pairs
id_programs = authors_post_cc %>%
  select(id, paper) %>%
  left_join(programs) %>%
  distinct(id, program)

# Extract ID reassignments based on common program matching
reassignments_programs = authors_post_cc %>%
  filter(is.na(user_nber)) %>%
  distinct(id) %>%
  # Determine match candidates (authors who published in same program as x)
  left_join(id_programs) %>%
  drop_na() %>%
  left_join(id_programs, by = 'program') %>%
  filter(id.x != id.y) %>%
  distinct(id.x, id.y) %>%
  # Apply name similarity criteria
  left_join(id_names, by = c('id.x' = 'id')) %>%
  left_join(id_names, by = c('id.y' = 'id')) %>%
  mutate(cosine = get_distance(name.x, name.y, 'cosine')) %>%
  filter(cosine < 0.05) %>%
  mutate(ni = count_name_intersections(name.x, name.y)) %>%
  mutate(lv = get_distance(name.x, name.y, 'lv')) %>%
  filter(cosine < 0.01 | ni >= 3 | (ni == 2 & lv <= 4)) %>%  # Checked manually
  # Extract reassignments
  extract_reassignments()

# Apply reassignments
authors_post_programs = authors_post_cc %>%
  left_join(reassignments_programs, by = c('id' = 'from_id')) %>%
  mutate(id = ifelse(!is.na(to_id), to_id, id)) %>%
  select(-to_id) %>%
  group_by(id) %>%
  arrange(user_nber) %>%
  mutate(user_nber = first(user_nber)) %>%
  ungroup()


# Finishing up ----

# Prepare paper-author correspondences
authors = authors_post_programs %>%
  group_by(id) %>%
  mutate(name = name[which.max(nchar(name))]) %>%
  ungroup() %>%
  select(paper, author = name) %>%
  arrange(paper, author)

# Export data
write_csv(authors, 'data-raw/authors.csv')
save(authors, file = 'data/authors.rda', version = 2, compress = 'bzip2')

# Save session info
save_session_info('data-raw/authors.log')
