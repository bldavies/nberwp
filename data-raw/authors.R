# AUTHORS.R
#
# This script exports tables of author attributes and paper-author correspondences.
#
# Ben Davies
# July 2021


# Initialization ----

# Load packages
library(bldr)
library(data.table)
library(dplyr)
library(igraph)
library(readr)
library(stringdist)
library(stringr)
library(tidyr)

# Import helper functions
source('data-raw/helpers.R')

# Import raw data
paper_authors_raw = fread('data-raw/metadata/working_papers_authors.tab', quote = '', encoding = 'Latin-1')
nberhi = read_csv('data-raw/repec/nberhi.csv')
nberte = read_csv('data-raw/repec/nberte.csv')
nberwo = read_csv('data-raw/repec/nberwo.csv')

# Import processed data
papers = read_csv('data-raw/papers.csv') %>%
  separate(paper, c('series', 'number'), sep = 1, convert = T) %>%
  arrange(year, month, desc(series), number) %>%
  mutate(paper_id = row_number(),
         number = with_prefix(number, '')) %>%
  unite(paper, c('series', 'number'), sep = '')
paper_programs = read_csv('data-raw/paper_programs.csv')


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
    subfun(' \\?$', '') %>%
    subfun(' Jr', '') %>%
    subfun('[,]+', '') %>%
    subfun('Abdulkadiro\\?lu', 'Abdulkadiroglu') %>%
    subfun('Arbatl\\?', 'Arbatli') %>%
    subfun('Aydo\\?an', 'Aydogan') %>%
    subfun('Ay\\?egul', 'Aysegul') %>%
    subfun('Bertl', 'Bertil') %>%
    subfun('Bobie', 'Bodie') %>%
    subfun('Bora\\?an', 'Boragan') %>%
    subfun('Borovi\\?k', 'Borovick') %>%
    subfun('Caballlero', 'Caballero') %>%
    subfun('Ca\\?lar', 'Caglar') %>%
    subfun('Cakmakl\\?', 'Cakmakli') %>%
    subfun('\\?apkun', 'Capkun') %>%
    subfun('Cemalc\\?lar', 'Cemalcilar') %>%
    subfun('Chi\\?u', 'Chihu') %>%
    subfun('\\?ihak', 'Cihak') %>%
    subfun('Clarez', 'Clare') %>%
    subfun('Cogaj', 'Cogan') %>%
    subfun('Co\\?ar', 'Cosar') %>%
    subfun('C[*]', 'C') %>%
    subfun('C -L|L -C', 'L-C') %>%
    subfun('C -Y', 'C-Y') %>%
    subfun('Dean Karlin', 'Dean Karlan') %>%
    subfun('Demirgu\\?', 'Demirguc') %>%
    subfun('Den Haan', 'den Haan') %>%
    subfun('Dzieli\\?ski', 'Dzielinski') %>%
    subfun('^Dr ', '') %>%
    subfun('Feldstean', 'Feldstein') %>%
    subfun('Fullterton', 'Fullerton') %>%
    subfun('Giles', 'Gilles') %>%
    subfun('Gombovi\\?', 'Gombovic') %>%
    subfun('Goval', 'Goyal') %>%
    subfun('Gune\\?', 'Gunes') %>%
    subfun('Gwilyn', 'Gwylim') %>%
    subfun('Haijime', 'Hajime') %>%
    subfun('Has\\?i\\?', 'Hascic') %>%
    subfun('Ivkovi\\?', 'Ivkovich') %>%
    subfun('Jan\\?okova', 'Jancokova') %>%
    subfun('Jaroci\\?ski', 'Jarocinski') %>%
    subfun('Josef Konings', 'Jozef Konings') %>%
    subfun('Kapi\\?ka', 'Kapicka') %>%
    subfun('K\\?sac\\?ko\\?lu', 'Kisacikoglu') %>%
    subfun('Kova\\?ik', 'Kovarik') %>%
    subfun('\\?ubos', 'Lubos') %>%
    subfun('\\?ukasz', 'Lukasz') %>%
    subfun('Ma\\?kowiak', 'Mackowiak') %>%
    subfun('Mat\\?jka', 'Matejka') %>%
    subfun('Micha\\?', 'Michal') %>%
    subfun('Nagataki', 'Nagatake') %>%
    subfun('P\\?nar', 'Pinar') %>%
    subfun(' Rebin$', ' Rubin') %>%
    subfun('R\\)chard', 'Richard') %>%
    subfun('Romaine', 'Romain') %>%
    subfun('\\? Pelin', 'S Pelin') %>%
    subfun('Sa\\?lam', 'Saglam') %>%
    subfun('\\?ahin', 'Sahin') %>%
    subfun('\\?ebnem', 'Sebnem') %>%
    subfun('Sedla\\?ek', 'Sedlacek') %>%
    subfun('Siata', 'Saita') %>%
    subfun('Sr\\?jan', 'Srdjan') %>%
    subfun('St\\?pan', 'Stepan') %>%
    subfun('Stiflitx', 'Stiglitz') %>%
    subfun('Stratman$', 'Stratmann') %>%
    subfun('Sz\\?ke', 'Szoke') %>%
    subfun('Tuillo', 'Tullio') %>%
    subfun('U\\?ur', 'Ugur') %>%
    subfun('wickstrom', 'Wickstrom') %>%
    subfun('Wi\\?cek', 'Wiecek') %>%
    subfun('Ye\\?ilbayraktar', 'Yesilbayraktar') %>%
    subfun('Ye\\?ilta\\?', 'Yesiltas') %>%
    subfun('Ye\\?im', 'Yesim') %>%
    subfun('Y\\?ld\\?r\\?m', 'Yildirim') %>%
    subfun('Y\\?lmaz', 'Yilmaz')
}

# Extract raw authorship data with NBER user names
authors_raw_nber = paper_authors_raw %>%
  as_tibble() %>%
  arrange(paper, order_num) %>%
  select(paper, name, user_nber = author_user) %>%
  filter(grepl('^(h|t|w)[0-9]', paper)) %>%
  semi_join(papers) %>%
  # Clean author names
  mutate(name = replace_non_ascii(name),
         name = gsub('\\.', ' ', name),
         name = str_squish(name),
         name = clean_name(name),
         name = ifelse(name == 'NULL', NA, name),
         user_nber = ifelse(user_nber %in% c('', 'NULL'), NA, user_nber)) %>%
  distinct() %>%
  # Delete 1:m name:user matches
  add_count(paper, user_nber) %>%
  mutate(user_nber = replace(user_nber, n > 1, NA)) %>%
  select(-n)

# Extract raw authorship data with RePEc user names
authors_raw_repec = bind_rows(
  mutate(nberhi, series = 'h'),
  mutate(nberte, series = 't'),
  mutate(nberwo, series = 'w')
) %>%
  filter(key %in% c('number', 'author_name', 'author_person')) %>%
  group_by(series, entry) %>%
  mutate(paper = paste0(series, value[which(key == 'number')]),
         pos = cumsum(key == 'author_name')) %>%
  ungroup() %>%
  filter(key != 'number') %>%
  select(-entry, -series) %>%
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

# Define function for importing disambiguation files
import_disamb = function(s) read_csv(paste0('data-raw/disambiguation/', s, '.csv'))

# Define function for adding known missing NBER and RePEc user names, and catching known user name mis-codings
replace_users_manually = function(d) {
  nber_replacements_via_nber = import_disamb('nber_replacements_via_nber')
  nber_replacements_via_name = import_disamb('nber_replacements_via_name')
  repec_replacements_via_nber = import_disamb('repec_replacements_via_nber')
  repec_replacements_via_name = import_disamb('repec_replacements_via_name')
  d %>%
    # Replace NBER user names via outdated/incorrectly distinct NBER user names
    left_join(mutate(nber_replacements_via_nber, to_edit = T)) %>%
    mutate(user_nber = ifelse(!is.na(to_edit), new_user_nber, user_nber)) %>%
    select(-new_user_nber, -to_edit) %>%
    # Replace NBER user names via name-paper pairs
    left_join(mutate(nber_replacements_via_name, to_edit = T)) %>%
    mutate(user_nber = ifelse(!is.na(to_edit), new_user_nber, user_nber)) %>%
    select(-new_user_nber, -to_edit) %>%
    # Replace RePEc IDs via NBER user names
    left_join(mutate(repec_replacements_via_nber, to_edit = T)) %>%
    mutate(user_repec = ifelse(!is.na(to_edit), new_user_repec, user_repec)) %>%
    select(-new_user_repec, -to_edit) %>%
    # Replace RePEc IDs via name-paper pairs
    left_join(mutate(repec_replacements_via_name, to_edit = T)) %>%
    mutate(user_repec = ifelse(!is.na(to_edit), new_user_repec, user_repec)) %>%
    select(-new_user_repec, -to_edit)
}

# Import manual merges for authors with no NBER or RePEc profiles
manual_merges = import_disamb('manual_merges')

# Merge raw authorship data, treating NBER records as ground truth
authors_raw = authors_raw_nber %>%
  # Do some manual editing
  filter(name != 'M B Landrum J Newhouse') %>%
  filter(!(paper == 'w0073' & name == 'Mark A Satterthwaite')) %>%
  filter(!(paper == 'w4371' & name == 'Michael J White')) %>%
  filter(!(paper == 'w22701' & user_nber == 'andrei_shleifer')) %>%
  bind_rows(tribble(
    ~paper, ~name, ~user_nber,
    'w4371', 'Michelle J White', 'michelle_white',
    'w7760', 'Mary Beth Landrum', 'MaryBeth_Landrum',
    'w7760', 'Joseph P Newhouse', 'joseph_newhouse'
  )) %>%
  # Merge in RePEc data
  left_join(authors_raw_repec) %>%
  arrange(paper, name) %>%
  # Manually replace NBER user names and RePEc IDs
  replace_users_manually() %>%
  # Assert that NBER:RePEc user name correspondence are m:1
  assert_many2one(user_nber, user_repec) %>%
  # Collapse m:1 NBER:RePEc matches to 1:1
  group_by(user_repec) %>%
  mutate(user_nber = replace(user_nber, !is.na(user_repec), first(user_nber[!is.na(user_nber)]))) %>%
  group_by(user_nber) %>%
  mutate(user_repec = replace(user_repec, !is.na(user_nber), first(user_repec[!is.na(user_repec)]))) %>%
  ungroup() %>%
  assert_one2one(user_repec, user_nber) %>%
  # Assign new IDs for next stage of disambiguation process
  left_join(papers) %>%
  group_by(paper_id) %>%
  mutate(id = 100 * paper_id + row_number(),
         no_nber = is.na(user_nber),
         no_repec = is.na(user_repec)) %>%
  group_by(user_nber) %>%
  mutate(id = ifelse(!no_nber, min(id), id)) %>%
  group_by(user_repec) %>%
  mutate(id = ifelse(!no_repec, min(id), id)) %>%
  # Manually merge authors with no NBER or RePEc profiles
  left_join(manual_merges) %>%
  group_by(merge_id) %>%
  mutate(id = replace(id, !is.na(merge_id), min(id))) %>%
  ungroup()


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

# Import excluded matches
excluded_matches = import_disamb('excluded_matches')


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
  anti_join(excluded_matches) %>%
  # Extract reassignments
  extract_reassignments()

# Apply reassignments
authors_post_cc = authors_raw %>%
  left_join(reassignments_cc, by = c('id' = 'from_id')) %>%
  mutate(id = ifelse(!is.na(to_id), to_id, id)) %>%
  select(-to_id) %>%
  group_by(id) %>%
  arrange(user_nber) %>%
  mutate(user_nber = first(user_nber),
         user_repec = first(user_repec)) %>%
  ungroup()


# Matching on common programs ----

# Update unique ID-name pairs
id_names = authors_post_cc %>%
  distinct(id, name)

# Identify unique ID-program pairs
id_programs = authors_post_cc %>%
  select(id, paper) %>%
  left_join(paper_programs) %>%
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
  anti_join(excluded_matches) %>%
  # Extract reassignments
  extract_reassignments()

# Apply reassignments
authors_post_programs = authors_post_cc %>%
  left_join(reassignments_programs, by = c('id' = 'from_id')) %>%
  mutate(id = ifelse(!is.na(to_id), to_id, id)) %>%
  select(-to_id) %>%
  group_by(id) %>%
  arrange(user_nber) %>%
  mutate(user_nber = first(user_nber),
         user_repec = first(user_repec)) %>%
  ungroup()


# Finishing up ----

# Prepare table of author attributes
authors = authors_post_programs %>%
  group_by(id) %>%
  mutate(name = name[which.max(nchar(name))]) %>%
  slice_min(paper_id) %>%
  ungroup() %>%
  mutate(author = paste(paper, id %% 100, sep = '.')) %>%
  select(author, name, user_nber, user_repec) %>%
  sort_by_author()

# Assert that author IDs are unique
if (nrow(authors) != n_distinct(authors$author)) {
  stop('Author IDs are not unique')
}

# Prepare paper-author correspondences
paper_authors = authors_post_programs %>%
  group_by(id) %>%
  mutate(min_paper = paper[which.min(paper_id)]) %>%
  ungroup() %>%
  mutate(author = paste(min_paper, id %% 100, sep = '.')) %>%
  select(paper, author) %>%
  distinct() %>%
  sort_by_author() %>%
  sort_by_paper()

# Export data
write_csv(authors, 'data-raw/authors.csv')
save(authors, file = 'data/authors.rda', version = 2, compress = 'bzip2')
write_csv(paper_authors, 'data-raw/paper_authors.csv')
save(paper_authors, file = 'data/paper_authors.rda', version = 2, compress = 'bzip2')

# Save session info
save_session_info('data-raw/authors.log')
