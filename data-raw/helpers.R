# HELPERS.R
#
# This script defines helpful functions used in other scripts.
#
# Ben Davies
# July 2021

# Define function for adding series prefixes to paper numbers
with_prefix = function(x, p) {
  paste0(p, sprintf('%04d', x))
}

# Define function for sorting tibbles by series-prefixed paper numbers
sort_by_paper = function(d) {
  d %>%
    mutate(paper = sub('([a-z])([0-9]+)', '\\1_\\2', paper)) %>%
    separate(paper, c('series', 'number'), convert = T) %>%
    arrange(desc(series), number) %>%
    mutate(number = with_prefix(number, '')) %>%
    unite(paper, c('series', 'number'), sep = '')
}

# Define function for sorting tibbles by series-prefixed author IDs
sort_by_author = function(d) {
  d %>%
    mutate(author = sub('([a-z])([0-9]+)[.]([0-9]+)', '\\1_\\2_\\3', author)) %>%
    separate(author, c('series', 'number', 'pos'), convert = T) %>%
    arrange(desc(series), number, pos) %>%
    mutate(number = with_prefix(number, ''),
           pos = paste0('.', pos)) %>%
    unite(author, c(series, number, pos), sep = '')
}
