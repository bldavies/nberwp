# AUTHORS.R
#
# This script exports a table of paper-author correspondenes.
#
# Ben Davies
# January 2020

# Load packages
library(dplyr)
library(igraph)
library(readr)
library(stringdist)
library(stringr)
library(tidyr)

# Import data
data <- read_csv('data-raw/nberwo.csv')
papers <- read_csv('data-raw/papers.csv')

# Define function for replacing non-ASCII characters with ASCII equivalents
replace_non_ascii <- function(x) {
  subfun <- function(x, pattern, y) gsub(pattern, y, x, perl = TRUE)
  x %>%
    iconv('', 'ASCII', sub = 'byte') %>%
    subfun('<e2><80><99>', '\'') %>%
    subfun('<e2><93><a1>', '') %>%  # (r)
    subfun('<e1><b9><a2>', 'S') %>%  # dot below
    subfun('<c3><81>', 'A') %>%  # acute
    subfun('<c3><85>', 'A') %>%  # ring above
    subfun('<c3><87>', 'C') %>%  # cedilla
    subfun('<c3><89>', 'E') %>%  # acute
    subfun('<c3><92>', 'O') %>%  # grave
    subfun('<c3><93>', 'O') %>%  # acute
    subfun('<c3><96>', 'O') %>%  # diaresis
    subfun('<c3><9e>', 'Th') %>%  # thorn
    subfun('<c3><9c>', 'U') %>%  # diaresis
    subfun('<c3><a0>', 'a') %>%  # grave
    subfun('<c3><a1>', 'a') %>%  # acute
    subfun('<c3><a2>', 'a') %>%  # circumflex
    subfun('<c3><a3>', 'a') %>%  # tilde
    subfun('<c3><a4>', 'a') %>%  # diaresis
    subfun('<c3><a5>', 'a') %>%  # ring above
    subfun('<c3><a6>', 'ae') %>%
    subfun('<c3><a7>', 'c') %>%  # cedilla
    subfun('<c3><a8>', 'e') %>%  # grave
    subfun('<c3><a9>', 'e') %>%  # acute
    subfun('<c3><aa>', 'e') %>%  # circumflex
    subfun('<c3><ab>', 'e') %>%  # diaresis
    subfun('<c3><ad>', 'i') %>%  # acute
    subfun('<c3><ae>', 'i') %>%  # circumflex
    subfun('<c3><af>', 'i') %>%  # diaresis
    subfun('<c3><b0>', 'd') %>%  # eth
    subfun('<c3><b1>', 'n') %>%  # tilde
    subfun('<c3><b2>', 'o') %>%  # grave
    subfun('<c3><b3>', 'o') %>%  # acute
    subfun('<c3><b4>', 'o') %>%  # circumflex
    subfun('<c3><b6>', 'o') %>%  # diaresis
    subfun('<c3><b8>', 'o') %>%  # stroke
    subfun('<c3><ba>', 'u') %>%  # acute
    subfun('<c3><bb>', 'u') %>%  # circumflex
    subfun('<c3><bc>', 'u') %>%  # diaresis
    subfun('<c3><bf>', 'y') %>%  # diaresis
    subfun('<c4><87>', 'c') %>%  # acute
    subfun('<c4><8c>', 'C') %>%  # caron
    subfun('<c4><8d>', 'c') %>%  # caron
    subfun('<c4><9b>', 'e') %>%  # caron
    subfun('<c4><9f>', 'g') %>%  # caron
    subfun('<c4><b1>', 'i') %>%  # dotless
    subfun('<c5><81>', 'L') %>%  # stroke
    subfun('<c5><82>', 'l') %>%  # stroke
    subfun('<c5><84>', 'n') %>%  # acute
    subfun('<c5><91>', 'o') %>%  # double acute
    subfun('<c5><99>', 'r') %>%  # caron
    subfun('<c5><9e>', 'S') %>%  # cedilla
    subfun('<c5><9f>', 's') %>%  # cedilla
    subfun('<c5><a0>', 'S') %>%  # caron
    subfun('<c5><a1>', 's') %>%  # caron
    subfun('<c7><a7>', 'g') %>%  # caron
    subfun('<c8><9b>', 't')  # comma below
}

# Define function for disambiguating author names
disambiguate_names <- function(df) {

  # Use RePEc IDs
  repec <- df %>%
    filter(!is.na(repec_id)) %>%
    group_by(repec_id) %>%
    mutate(repec = name[which.max(nchar(name))]) %>%
    ungroup() %>%
    filter(stringdist(name, repec, 'cosine') < 0.4)

  # Use pairs of authors two edges apart in co-authorship network
  bip <- df %>%
    distinct(paper, name) %>%
    graph_from_data_frame(directed = F)
  V(bip)$type <- V(bip)$name %in% unique(df$name)
  net <- bip %>%
    bipartite_projection(which = 'true') %>%
    simplify()
  V(net)$id <- seq_along(V(net))
  nb <- neighborhood(net, mindist = 1)
  coauth_list <- vector('list', gorder(net))
  for (i in seq_len(gorder(net))) {
    matches <- unique(names(unlist(nb[nb[[i]]$id])))
    if (length(matches) > 0) {
      coauth_list[[i]] <- tibble(match = matches) %>%
        mutate(name = V(net)$name[i]) %>%
        filter(name != match) %>%
        filter(stringdist(name, match, method = 'cosine') < 0.1)
    }
  }
  coauth <- coauth_list %>%
    bind_rows() %>%
    mutate(coauth = ifelse(nchar(match) > nchar(name), match, name)) %>%
    select(name, coauth)

  # Combine RePEc and co-authorship-based matches
  combined <- df %>%
    left_join(repec) %>%
    left_join(coauth) %>%
    mutate(new = ifelse(!is.na(repec), repec, ifelse(!is.na(coauth), coauth, name)))

  # Compute text similarities among distinct new names
  new_unique <- sort(unique(combined$new))
  similarities_list <- vector('list', length(new_unique))
  for (i in seq_along(new_unique)) {
    cosine <- stringdist(new_unique[i], new_unique, 'cosine')
    matches <- which(cosine < 0.06)
    similarities_list[[i]] <- tibble(
      new = rep(new_unique[i], length(matches)),
      revised = new_unique[matches],
      cosine = cosine[matches]
    ) %>%
      filter(nchar(new) != nchar(revised)) %>%
      mutate(lcs = stringdist(new, revised, 'lcs'),
             is_substring = lcs == abs(nchar(new) - nchar(revised))) %>%
      filter(stringdist(new, revised) <= 2 & is_substring)
  }

  # Return disambiguated data frame
  similarities_list %>%
    bind_rows() %>%
    select(new, revised) %>%
    right_join(combined) %>%
    mutate(revised = ifelse(!is.na(revised), revised, new),
           new = ifelse(nchar(revised) > nchar(new), revised, new)) %>%
    group_by(old_name = name) %>%
    mutate(author = new[which.max(nchar(new))]) %>%
    ungroup() %>%
    select(paper, author) %>%
    arrange(paper, author)
}

# Define function for cleaning known errors in author names
clean_names <- function(x) {
  subfun <- function(x, pattern, y) gsub(pattern, y, x, perl = TRUE)
  x %>%
    subfun('^Haij', 'Haj') %>%
    subfun('^Romaine', 'Romain') %>%
    subfun('Stiflitx', 'Stiglitz')
}

# Extract and clean author-paper pairs
authors <- data %>%
  filter(key %in% c('number', 'author_name', 'author_person')) %>%
  group_by(entry) %>%
  mutate(paper = as.integer(value[which(key == 'number')]),
         author = cumsum(key == 'author_name')) %>%
  ungroup() %>%
  filter(key != 'number') %>%
  select(-entry) %>%
  spread(key, value) %>%
  semi_join(papers) %>%
  arrange(paper, author) %>%
  select(paper, name = author_name, repec_id = author_person) %>%
  # Clean author names
  mutate(name = replace_non_ascii(name),
         name = gsub('\\.', ' ', name),
         name = str_squish(name),
         name = clean_names(name)) %>%
  # Disambiguate author names
  disambiguate_names()

# Export data
write_csv(authors, 'data-raw/authors.csv')
save(authors, file = 'data/authors.rda', version = 2, compress = 'bzip2')

# Save session info
options(width = 80)
write_lines(capture.output(sessioninfo::session_info()), 'data-raw/authors.log')