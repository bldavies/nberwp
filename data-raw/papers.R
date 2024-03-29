# PAPERS.R
#
# This script exports a table of working paper attributes.
#
# Ben Davies
# March 2022

# Load packages
library(bldr)
library(data.table)
library(dplyr)
library(readr)
library(rvest)
library(stringr)
library(tidyr)
library(xml2)

# Import helper functions
source('data-raw/helpers.R')

# Import raw metadata
papers_raw = fread('data-raw/metadata/working_papers.tab', quote = '', encoding = 'Latin-1')
outlets_raw = fread('data-raw/metadata/working_papers_published.tab', quote = '', encoding = 'Latin-1')

# Set boundary issue date
max_issue_date = '2021-12-31'

# Define function for removing known parenthetical notes
remove_parenthetical_notes = function(x) {
  subfun = function(x, pattern, y) gsub(pattern, y, x, ignore.case = T, perl = TRUE)
  x %>%
    subfun('\\(expanded version\\)', '') %>%  # 8387, 12024
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
    subfun('ô', 'o') %>%
    subfun('`', '\'') %>%
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
    subfun(' -A', ' - A') %>%  # 13
    subfun('Useof', 'Use of') %>%  # 66
    subfun('THe', 'The') %>%  # 86
    subfun('nS', 'n S') %>%  # 138
    subfun('fL', 'f L') %>%  # 158
    subfun('Rosetak', 'Rosepack') %>%  # 165
    subfun('ome, Cau', 'ome Cau') %>%  # 167
    subfun('lA', 'l A') %>%  # 171
    subfun('mC', 'm C') %>%  # 179
    subfun('nW', 'n W') %>%  # 305
    subfun('dE', 'd E') %>%  # 336, 4809
    subfun('lT', 'l T') %>%  # 337, 570
    subfun('Investigationof', 'Investigation of') %>%  # 344
    subfun('Regimeswith', 'Regimes with') %>%  # 500
    subfun('Systsems', 'Systems') %>%  # 574
    subfun('n:A', 'n: A') %>%  # 579, 9799
    subfun('e3', 'es') %>%  # 646
    subfun('gC', 'g C') %>%  # 677
    subfun('ldA', 'ld A') %>%  # 732
    subfun('Markats', 'Markets') %>%  # 843
    subfun('Discrepion', 'Discretion') %>%  # 889
    subfun('eE', 'e E') %>%  # 949, 1850, 3440
    subfun('yF', 'y F') %>%  # 993
    subfun('eC', 'e C') %>%  # 1063
    subfun('Macro- econ', 'Macroecon') %>%  # 1162
    subfun('Commentand', 'Comment and') %>%  # 1200
    subfun('Japaneseand', 'Japanese and') %>%  # 1264
    subfun(',P', ', P') %>%  # 1106
    subfun('Determinationand', 'Determination and') %>%  # 1112
    subfun('Mult-Div', 'Multi-Div') %>%  # 1213
    subfun('tI', 't I') %>%  # 1348
    subfun('sD', 's D') %>%  # 1348
    subfun('Fctry Assoc[.] w/Gains in Effcny[?]: Evdnc[.]', 'Factory Associated with Gains in Efficiency? Evidence') %>%  # 1386
    subfun('Mnfctr[.]', 'Manufacturing') %>%  # 1386
    subfun('onT', 'on T') %>%  # 1396
    subfun('nge- ', 'nge') %>%  # 1407, 5429
    subfun('fI', 'f I') %>%  # 1425, 1655
    subfun('eI', 'e I') %>%  # 1460, 3332
    subfun('fU', 'f U') %>%  # 1463
    subfun('tR', 't R') %>%  # 1480, 1508
    subfun('eF', 'e F') %>%  # 1484, 4492
    subfun('nC', 'n C') %>%  # 1513
    subfun('Developedand', 'Developed and') %>%  # 1528
    subfun('rM', 'r M') %>%  # 1559, 6753
    subfun('Compenstation', 'Compensation') %>%  # 1578
    subfun('Quardractic', 'Quadratic') %>%  # 1581
    subfun('fF', 'f F') %>%  # 1586
    subfun('nV', 'n V') %>%  # 1623
    subfun('tM', 't M') %>%  # 1642
    subfun('eP', 'e P') %>%  # 1650, 4596, 5235
    subfun('Comparisonof', 'Comparison of') %>%  # 1735
    subfun('tyC', 'ty C') %>%  # 1736
    subfun('nE', 'n E') %>%  # 1763
    subfun('eM', 'e M') %>%  # 1783, 1903
    subfun('Competative', 'Competitive') %>%  # 1897
    subfun('RUn', 'Run') %>%  # 1931
    subfun('Reasse ssment', 'Reassessment') %>%  # 1949
    subfun('lP', 'l P') %>%  # 1969
    subfun('dL', 'd L') %>%  # 1981
    subfun('lD', 'l D') %>%  # 2012, 2682
    subfun('Exchage', 'Exchange') %>%  # 2017
    subfun('forthe', 'for the') %>%  # 2069
    subfun('rE', 'r E') %>%  # 2102
    subfun('eR', 'e R') %>%  # 2162, 7385
    subfun('dO', 'd O') %>%  # 2173, 7224
    subfun('sU', 's U') %>%  # 2215, 6263
    subfun('Choiceby', 'Choice by') %>%  # 2292
    subfun('inthe', 'in the') %>%  # 2337
    subfun('dT', 'd T') %>%  # 2375, 2624
    subfun('Fina([a-z]+)al', 'Financial') %>%  # 2376, 3116, 9286
    subfun('Macroeocnomics', 'Macroeconomics') %>%  # 2473
    subfun('rP', 'r P') %>%  # 2543
    subfun('mE', 'm E') %>%  # 2583
    subfun('sure- ', 'sure') %>%  # 2950
    subfun('Commited', 'Committed') %>%  # 3005
    subfun('Produc- tivity', 'Productivity') %>%  # 3026
    subfun('Exhange', 'Exchange') %>%  # 3067
    subfun('TAx', 'Tax') %>%  # 3074
    subfun('Endogeous', 'Endogenous') %>%  # 3085
    subfun('Reinve stment', 'Reinvestment') %>%  # 3093
    subfun('Ebolution', 'Evolution') %>%  # 3104
    subfun('folio- ', 'folio-') %>%  # 3114
    subfun('OBs', 'Obs') %>%  # 3132
    subfun('BAr', 'Bar') %>%  # 3188
    subfun('Comparive', 'Comparative') %>%  # 3194
    subfun('Correlationswith', 'Correlations with') %>%  # 3249
    subfun('Inve([a-z]+)ment', 'Investment') %>%  # 3249, 10337
    subfun('acor', 'acro') %>%  # 3344
    subfun('Competiton', 'Competition') %>%  # 3344
    subfun('Genral', 'General') %>%  # 3356
    subfun('vs.Living', 'vs. Living') %>%  # 3559
    subfun('lB', 'l B') %>%  # 3560
    subfun('dC', 'd C') %>%  # 3583
    subfun('Com-mon', 'Common') %>%  # 3637
    subfun('Thrift, [.][.][.]$', 'Thrift, Public Debt, Capital Taxation and Policy Towards Human Capital Formation') %>%  # 3637
    subfun('Endo-genous', 'Endogenous') %>%  # 3671
    subfun('Cross- Sec', 'Cross-Sec') %>%  # 3806
    subfun('ti- cal', 'tical') %>%  # 3811
    subfun('Mid- Cen', 'Mid-Cen') %>%  # 3817
    subfun('Engogenous', 'Endogenous') %>%  # 4026
    subfun('Elderly-', 'Elderly -') %>%  # 4182
    subfun('Studyof', 'Study of') %>%  # 4211
    subfun('sF', 's F') %>%  # 4270
    subfun('Infaliton', 'Inflation') %>%  # 4319
    subfun('Effciency', 'Efficiency') %>%  # 4381
    subfun('Persoanl', 'Personal') %>%  # 4391
    subfun('yH', 'y H') %>%  # 4460
    subfun('e1', 'e 1') %>%  # 4496
    subfun('Firm- Level', 'Firm-Level') %>%  # 4540
    subfun('Convicton', 'Conviction') %>%  # 4551
    subfun('"Household.*Bag,"', 'Household Demand for Garbage and Recycling Collection with the Start of a Price per Bag') %>%  # 4670
    subfun('usS', 'us S') %>%  # 4731
    subfun('ationa ', 'ations ') %>%  # 4754, 10565
    subfun('hC', 'h C') %>%  # 4786
    subfun('dF', 'd F') %>%  # 4800
    subfun('Quantative', 'Quantitative') %>%  # 4819
    subfun('sT', 's T') %>%  # 4870
    subfun('fromthe', 'from the') %>%  #w5058, h0072, h0085
    subfun('gI', 'g I') %>%  # 5060
    subfun('hG', 'h G') %>%  # 5170
    subfun('Imigration', 'Immigration') %>%  # 5185
    subfun('1890- ', '1890-') %>%  # 5314
    subfun('yP', 'y P') %>%  # 5362
    subfun('eS', 'e S') %>%  # 5364, 6732, 24938
    subfun('Means- Tested', 'Means-Tested') %>%  # 5372
    subfun('der- City', 'der-City') %>%  # 5425
    subfun('yM', 'y M') %>%  # 5474
    subfun('DoB', 'Do B') %>%  # 5695
    subfun('Flowof', 'Flow of') %>%  # 5712
    subfun('Emp irical', 'Empirical') %>%  # w5771
    subfun('Endgenous', 'Endogenous') %>%  # 5851
    subfun('Acccoun', 'Accounts Data') %>%  # 5884
    subfun('Masschusetts', 'Massachusetts') %>%  # 5957
    subfun(',I', ', I') %>%  # 6223
    subfun('lyis', 'lysis') %>%  # 6310
    subfun('SYs', 'Sys') %>%  # 6436
    subfun('Complimentarity', 'Complementarity') %>%  # 6600
    subfun('QuUality', 'Quality') %>%  # 6753
    subfun('Endogeneously', 'Endogenously') %>%  # 7060
    subfun('Equilibriumin', 'Equilibrium in') %>%  # 7026
    subfun('Hunderd', 'Hundred') %>%  # 7195
    subfun('lC', 'l C') %>%  # 7386
    subfun('nA', 'n A') %>%  # 7493
    subfun('n: b', 'n\" b') %>%  # 7493
    subfun('Comparision', 'Comparison') %>%  # 7599
    subfun('SHe', 'She') %>%  # 8060
    subfun('Commerical', 'Commercial') %>%  # 8060
    subfun(',a', ', a') %>%  # 9614
    subfun('Inequiality', 'Inequality') %>%  # 9830
    subfun('Nonaddative', 'Nonadditive') %>%  # 9895
    subfun('Commision', 'Commission') %>%  # 10030
    subfun('Complemetarity', 'Complementarity') %>%  # 10350
    subfun('s1', 's') %>%  # 10447
    subfun('Self_Control', 'Self Control') %>%  # 10819
    subfun('mis ', 'mic ') %>%  # 11470
    subfun('Collpase', 'Collapse') %>%  # 11153
    subfun('N 1$', 'N+1') %>%  # 11713
    subfun('contracs', 'contracts') %>%  # 11804
    subfun('jA', 'ja') %>%  # 12393
    subfun('\\%u2019', '\'') %>%  # 12396
    subfun(',2004', ', 2004') %>%  # 12607
    subfun(',1980', ', 1980') %>%  # 15274
    subfun('ter\\?Cyc', 'ter-Cyc') %>%  # 18062
    subfun('China -US', 'China-US') %>%  # 19898
    subfun('A-az', 'iaz') %>%  # 21350
    subfun('^\\?', '') %>%  # 21802
    subfun('E\\?ect', 'Effect') %>%  # 22950
    subfun('dor\\?Uni', 'dor-Uni') %>%  # 23018
    subfun('CO\\? Em', 'CO2 Em') %>%  # 24293, 26086, 26537
    subfun('^q\\?$', 'q5') %>%  # 24709
    subfun('Emit CO\\?', 'Emit CO2') %>%  # 25063
    subfun(',W', ', W') %>%  # 25311
    subfun('Di\\?er', 'Differ') %>%  # 25380, 26375
    subfun('Pro\\?t', 'Profit') %>%  # 26027
    subfun('\\?E', '? E') %>%  # 26268
    subfun('Employess', 'Employees')  %>%  # 26272
    subfun(' o\\? ', ' off ')  %>%  # 26624
    subfun('i\\?I', 'i-I') %>%  # 27175
    subfun('DEg', 'Deg') %>%  # 27239
    subfun('ChinaÂ', 'China') %>%  # 27502
    subfun(' R\\? ', ' R ') %>%  # 27632
    subfun('R\\?1', 'R<=1') %>%  # 28093
    subfun('Ce MENT', 'CeMENT') %>%  # 28727
    subfun('CO\\? Fert', 'CO2 Fert') %>%  # 29320
    subfun('of-the- ', 'of-the-') %>%  # h0073
    subfun('Rela-tion', 'Relation') %>%  # h0079
    subfun('tothe', 'to the') %>%  # h0080
    subfun('nalExp', 'nal Exp') %>%  # t0005
    subfun('Structral', 'Structural') %>%  # t0031
    subfun('recent development$', 'recent developments') %>%  # t0034
    subfun('tioWhe', 'tio Whe') %>%  # t0069
    subfun('ashDixits:"(.*)[.]$', 'ash Dixit\'s "\\1"') %>%  # t0077
    subfun('denti- ', 'denti') %>%  # t0106
    subfun('JTPA Exp$', 'JTPA Experiment') %>%  # t0149
    subfun('Cleaningthe', 'Cleaning the') %>%  # t0160
    subfun('ri-zz', 'riz') %>%  # t0168
    subfun('Quasi- ', 'Quasi-') %>%  # t0170
    subfun('Norwegian [.][.][.]$', 'Norwegian Vocational Rehabilitation Programs') %>%  # t0262
    subfun('and Application in', 'and Applications in')  # t0281
}

# Define function for matching patterns using Perl regular expressions
check_text = function(x, y) {
  grepl(x, y, ignore.case = T, perl = T)
}

# Extract outlets
journal_false_negatives = c(
  with_prefix(c(1826, 2095, 2297, 2313, 3478, 3609, 3912, 7126, 8243, 9439,
                12220, 13192, 13931, 14570, 14572, 16641, 16795, 16887, 17169, 18745, 19054, 19284,
                20341, 20397, 21465, 22908, 26707, 27290, 27362, 27364, 27547, 28454, 28801), 'w')
)
top5_false_positives = c(
  with_prefix(c(2107, 5777, 5805, 6762, 16247), 'w')
)
outlets = outlets_raw %>%
  as_tibble() %>%
  filter(grepl('^(h|t|w)[0-9]', paper)) %>%
  mutate(
    type = replace(type, paper %in% journal_false_negatives, 'journal'),
    AER  = check_text('(?<!Latin )American Economic Review(?!.*?Insights)', published_text) | grepl('aecrev', journal_abbrev) | journal_abbrev == 'AER',
    AER  = AER & !check_text('Papers and Proceed', published_text),
    ECTA = check_text('Econometrica', published_text) | grepl('emetrp', journal_abbrev),
    JPE  = check_text('(?<!(Scottish|European) )Journal of Political Economy', published_text) | grepl('jpolec', journal_abbrev),
    QJE  = check_text('Quarterly Journal of Economics', published_text) | grepl('qjecon', journal_abbrev) | journal_abbrev == 'QJE',
    RES  = check_text('Review of Economic Studies', published_text) | grepl('restud', journal_abbrev),
    top5 = (AER | ECTA | JPE | QJE | RES) & !paper %in% top5_false_positives
  ) %>%
  filter(type %in% c('book', 'journal')) %>%
  group_by(paper) %>%
  summarise(outlet = case_when(sum(top5) > 0 ~ 1,
                               sum(type == 'journal') > 0 ~ 2,
                               T ~ 3)) %>%
  ungroup()

# Collate working paper information
excluded_papers = c(
  with_prefix(c(125), 'h'),
  with_prefix(c(306, 331, 332, 335, 336, 337, 338, 345), 't'),
  with_prefix(c(156, 623, 2432, 7044, 7255, 7436, 7565, 8649, 9101, 9694, 13410, 13800, 21929), 'w')
)
papers = papers_raw %>%
  as_tibble() %>%
  filter(grepl('^(h|t|w)[0-9]', paper)) %>%
  filter(issue_date <= max_issue_date) %>%
  mutate(year = as.integer(substr(issue_date, 1, 4)),
         month = as.integer(substr(issue_date, 6, 7))) %>%
  select(paper, year, month, title) %>%
  left_join(outlets) %>%
  mutate(title = remove_parenthetical_notes(title),
         title = clean_text(title),
         title = fix_title(title),
         title = if_else(paper %in% c('w9635', 'w9793', 'w12110'), title, sub('^"(.*)"$', '\\1', title))) %>%
  filter(paper != 'w0000') %>%
  filter(!paper %in% excluded_papers) %>%
  sort_by_paper()

# Export data
write_csv(papers, 'data-raw/papers.csv')
save(papers, file = 'data/papers.rda', version = 2, compress = 'bzip2')

# Save session info
save_session_info('data-raw/papers.log')
