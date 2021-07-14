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

# Import raw data
paper_authors_raw = fread('data-raw/metadata/working_papers_authors.tab', quote = '', encoding = 'Latin-1')
nberwo = read_csv('data-raw/nberwo.csv')

# Import processed data
papers = read_csv('data-raw/papers.csv')
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
    subfun('Dzieli\\?ski', 'Dzielinski') %>%
    subfun('^Dr ', '') %>%
    subfun('Feldstean', 'Feldstein') %>%
    subfun('Fullterton', 'Fullerton') %>%
    subfun('Giles', 'Gilles') %>%
    subfun('Gombovi\\?', 'Gombovic') %>%
    subfun('Gune\\?', 'Gunes') %>%
    subfun('Gwilyn', 'Gwylim') %>%
    subfun('Haijime', 'Hajime') %>%
    subfun('Has\\?i\\?', 'Hascic') %>%
    subfun('Ivkovi\\?', 'Ivkovich') %>%
    subfun('Jan\\?okova', 'Jancokova') %>%
    subfun('Jaroci\\?ski', 'Jarocinski') %>%
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
  filter(grepl('^w[0-9]+$', paper)) %>%
  mutate(paper = as.integer(sub('^w', '', paper))) %>%
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

# Define function for reassigning NBER user names
reassign_nber_manually = function(x) {
  x = replace(x, x == 'albert_n._link', 'albert_link')
  x = replace(x, x == 'alison_delrossi', 'alison_del_rossi')
  x = replace(x, x == 'andresrodriguez', 'andres_rodriguez')
  x = replace(x, x == 'benjamin__bennett_1', 'benjamin_bennett')
  x = replace(x, x == 'benjamin_remy', 'benjamin_chabot')
  x = replace(x, x == 'chen_lin_1', 'chen_lin')
  x = replace(x, x == 'cheryl_long_1', 'cheryl_long')
  x = replace(x, x == 'chitra_jayanty_1', 'chitra_jayanty')
  x = replace(x, x == 'christopher_hansman_1', 'christopher_hansman')
  x = replace(x, x == 'dan_goldhaber_1', 'dan_goldhaber')
  x = replace(x, x == 'dana__kiku', 'dana_kiku')
  x = replace(x, x == 'daniel__jones', 'dbjones')
  x = replace(x, x == 'daniel_chen', 'daniel_l_chen')
  x = replace(x, x == 'dean.hyslop', 'dean_hyslop')
  x = replace(x, x == 'elizabeth_richardson', 'elizabeth_richardson_vigdor')
  x = replace(x, x == 'emiko_usui', 'emiko')
  x = replace(x, x == 'florian_hoffman', 'florian_hoffmann')
  x = replace(x, x == 'jacopo_perego', 'jacopo_perego_1')
  x = replace(x, x == 'jorginho84', 'jorge__rodriguez')
  x = replace(x, x == 'giorgo_sertsios_1', 'giorgo_sertsios')
  x = replace(x, x == 'hangbai', 'hang_bai')
  x = replace(x, x == 'helen_f_ladd', 'helen_ladd')
  x = replace(x, x == 'greve', 'jane_greve_1')
  x = replace(x, x == 'james_liang', 'james_liang_1')
  x = replace(x, x == 'jason_lee', 'jason_lee_1')
  x = replace(x, x == 'jing_li_2', 'jing_li_1')
  x = replace(x, x == 'john__marshall', 'john_marshall1')
  x = replace(x, x == 'julia_wang', 'julia_shu-huah__wang')
  x = replace(x, x == 'julien_prat', 'julien_prat_1')
  x = replace(x, x == 'kasper_nielsen', 'kasper_m_nielsen')
  x = replace(x, x == 'klaus_prettner_1', 'klaus_prettner')
  x = replace(x, x == 'kendall_jake', 'jake_kendall')
  x = replace(x, x == 'lai_wei', 'lai_wei_1')
  x = replace(x, x == 'lasse_brune', 'lasse_brune_1')
  x = replace(x, x == 'liangliang_jiang', 'liangliang_jiang_1')
  x = replace(x, x == 'luis_brites_pereira_1', 'luis_pereira')
  x = replace(x, x == 'mark_r._trusheim', 'mark_trusheim')
  x = replace(x, x == 'michael_p_keane', 'michael_p_keane_1')
  x = replace(x, x == 'mishka1', 'michael_katz')
  x = replace(x, x == 'oren_levintal', 'oren_levintal_1')
  x = replace(x, x == 'peter_landry', 'peter_landry_1')
  x = replace(x, x == 'peter_shirley', 'peter_shirley_1')
  x = replace(x, x == 'phenix.hf', 'feng_huang')
  x = replace(x, x == 'primofrank', 'francisco_rodriguez')
  x = replace(x, x == 'roger_clemmons', 'roger_clemmons_1')
  x = replace(x, x == 'roger__moon_', 'hyungsik_moon')
  x = replace(x, x == 'stephen_l_ross', 'steven_ross')
  x = replace(x, x == 'valerio__leone_sciabolazza', 'sciabolazza')
  x = replace(x, x == 'yildirim', 'muhammed_yildirim')
  x = replace(x, x == 'zhangliang999', 'liang_zhang')
}

# Specify manual merges for authors with no NBER user name
manual_merges_nonber = tribble(
  ~paper, ~name, ~merge_id,
  7425, 'Brett Katzman', 'brett_katzman',
  8844, 'Brett Katzman', 'brett_katzman',
  471, 'Burton G Malkiel', 'burton_malkiel',
  576, 'Burton G Malkiel', 'burton_malkiel',
  700, 'Burton G Malkiel', 'burton_malkiel',
  7590, 'Burton G Malkiel', 'burton_malkiel',
  3215, 'Catherine Jackson', 'catherine_jackson',
  3624, 'Catherine A Jackson', 'catherine_jackson',
  189, 'David E Coleman', 'david_coleman',
  195, 'David E Coleman', 'david_coleman',
  63, 'Forrest D Nelson', 'forrest_nelson',
  68, 'Forrest D Nelson', 'forrest_nelson',
  70, 'Forrest D Nelson', 'forrest_nelson',
  96, 'Forrest D Nelson', 'forrest_nelson',
  3113, 'Glenn T Sueyoshi', 'glenn_sueyoshi',
  6175, 'Glenn T Sueyoshi', 'glenn_sueyoshi',
  69, 'G S Maddala', 'gs_maddala',
  70, 'G S Maddala', 'gs_maddala',
  96, 'G S Maddala', 'gs_maddala',
  4404, 'John H Boyd', 'john_boyd',
  5045, 'John H Boyd', 'john_boyd',
  8092, 'John H Boyd', 'john_boyd',
  4880, 'John R Penrod', 'john_penrod',
  6435, 'John R Penrod', 'john_penrod',
  3311, 'K C Chan', 'kc_chan',
  4074, 'K C Chan', 'kc_chan',
  4743, 'K C Chan', 'kc_chan',
  139, 'Kenneth E Warner', 'kenneth_warner',
  7047, 'Kenneth E Warner', 'kenneth_warner',
  7, 'Lewis C Solmon', 'lewis_solmon',
  14, 'Lewis C Solmon', 'lewis_solmon',
  480, 'Mark Gersovitz', 'mark_gersovitz',
  972, 'Mark Gersovitz', 'mark_gersovitz',
  1894, 'Mark Gersovitz', 'mark_gersovitz',
  2204, 'Mark Gersovitz', 'mark_gersovitz',
  7575, 'Paras Mehta', 'paras_mehta',
  7952, 'Paras Mehta', 'paras_mehta',
  2956, 'Paul J Seguin', 'paul_seguin',
  5179, 'Paul J Seguin', 'paul_seguin',
  10, 'Paul W Holland', 'paul_holland',
  11, 'Paul W Holland', 'paul_holland',
  44, 'Paul W Holland', 'paul_holland',
  58, 'Paul W Holland', 'paul_holland',
  189, 'Paul W Holland', 'paul_holland',
  3330, 'S Rao Aiyagari', 'aiyagari',
  3481, 'S Rao Aiyagari', 'aiyagari',
  6747, 'S Rao Aiyagari', 'aiyagari',
  94, 'Vinay Dharmadhikari', 'vinay_dharmadhikari',
  100, 'Vinay Dharmadhikari', 'vinay_dharmadhikari'
)

# Merge raw authorship data, treating NBER records as ground truth
authors_raw = authors_raw_nber %>%
  # Do some manual editing
  filter(name != 'M B Landrum J Newhouse') %>%
  filter(!(paper == 73 & name == 'Mark A Satterthwaite')) %>%
  filter(!(paper == 4371 & name == 'Michael J White')) %>%
  filter(!(paper == 22701 & user_nber == 'andrei_shleifer')) %>%
  bind_rows(tribble(
    ~paper, ~name, ~user_nber,
    4371, 'Michelle J White', 'michelle_white',
    7760, 'Mary Beth Landrum', 'MaryBeth_Landrum',
    7760, 'Joseph P Newhouse', 'joseph_newhouse'
  )) %>%
  # Merge in RePEc data
  left_join(authors_raw_repec) %>%
  arrange(paper, name) %>%
  # Do manual reassignments, add known missing NBER and RePEc user names, and catch known RePEc user name mis-codings
  mutate(user_nber = reassign_nber_manually(user_nber),
         user_nber = replace(user_nber, paper == 4515 & name == 'Caroline M Betts', 'caroline_betts'),
         user_nber = replace(user_nber, paper %in% c(3370, 6568) & name == 'Changyong Rhee', 'changyong_rhee'),
         user_nber = replace(user_nber, paper %in% c(9940, 10600) & name == 'Christopher Blattman', 'christopher_blattman'),
         user_nber = replace(user_nber, paper %in% c(6860, 7832) & name == 'Daniel Altman', 'altman'),
         user_nber = replace(user_nber, paper == 4669 & name == 'Daniel D Goldhaber', 'dan_goldhaber'),
         user_nber = replace(user_nber, paper %in% c(8185, 9130) & name == 'Dahlia K Remler', 'dahlia_remler'),
         user_nber = replace(user_nber, paper %in% 1267:5180 & name == 'David C Ling', 'david_ling'),
         user_nber = replace(user_nber, paper == 5116 & name == 'David N Beede', 'david_beede'),
         user_nber = replace(user_nber, paper %in% c(6744, 10148) & name == 'Dina Mayzlin', 'dina_mayzlin'),
         user_nber = replace(user_nber, paper == 132 & name == 'Donald O Parsons', 'donald_parsons'),
         user_nber = replace(user_nber, paper %in% c(1121, 4571) & name == 'Gikas A Hardouvelis', 'gikas_hardouvelis'),
         user_nber = replace(user_nber, paper == 9207 & name == 'Gwilym Pryce', 'gwilym_pryce'),
         user_nber = replace(user_nber, paper %in% c(376, 6870, 10730) & name == 'J Peter Neary', 'peter_neary'),
         user_nber = replace(user_nber, paper %in% c(6944, 7210) & name == 'Jack Porter', 'jack__porter'),
         user_nber = replace(user_nber, paper == 10347 & name == 'Jessica Tjornhom Donohue', 'jessica_donohue'),
         user_nber = replace(user_nber, paper == 4822 & name == 'John A Rizzo', 'john_rizzo'),
         user_nber = replace(user_nber, paper == 8394 & name == 'John P Conley', 'john_p_conley'),
         user_nber = replace(user_nber, paper == 5542 & name == 'Jonathan P Caulkins', 'jonathan_caulkins'),
         user_nber = replace(user_nber, paper == 9645 & name == 'Koen Schoors', 'koen_schoors'),
         user_nber = replace(user_nber, paper == 10527 & name == 'Larry Samuelson', 'larry_samuelson'),
         user_nber = replace(user_nber, paper %in% c(270, 271) & name == 'Lawrence W Kenny', 'lawrence_kenny'),
         user_nber = replace(user_nber, paper == 12356 & name == 'Kasper M Nielsen', 'kasper_m_nielsen'),
         user_nber = replace(user_nber, paper %in% c(5626, 5656, 6321) & name == 'Kenneth R Troske', 'kenneth_troske'),
         user_nber = replace(user_nber, paper %in% c(6997, 7003) & name == 'Kimberly Bayard', 'kimberly_bayard'),
         user_nber = replace(user_nber, paper == 2526 & name == 'Mark M Pitt', 'mark_pitt'),
         user_nber = replace(user_nber, paper %in% c(5774, 6621) & name == 'Mark Wilhelm', 'mark_ottoni-wilhelm'),
         user_nber = replace(user_nber, paper %in% 4779:7762 & name == 'Mary E Lovely', 'mary_lovely'),
         user_nber = replace(user_nber, paper %in% 7070:10748 & name == 'Matthew F Mitchell', 'matthew_mitchell'),
         user_nber = replace(user_nber, paper == 9579 & name == 'Matthew O Jackson', 'matthew_jackson'),
         user_nber = replace(user_nber, paper %in% c(3453, 5136) & name == 'Maxim Boycko', 'maxim_boycko'),
         user_nber = replace(user_nber, paper == 2241 & name == 'Michael C Burda', 'michael_burda'),
         user_nber = replace(user_nber, paper == 9207 & name == 'Michael White', 'Michant'),
         user_nber = replace(user_nber, paper == 9248 & name == 'N Edward Coulson', 'edward_coulson'),
         user_nber = replace(user_nber, paper == 7962 & name == 'Neil Gandal', 'neil_gandal'),
         user_nber = replace(user_nber, paper %in% c(9615, 12951) & name == 'Nuno Sousa Pereira', 'nuno_pereira'),
         user_nber = replace(user_nber, paper == 628 & name == 'Paul Strebel', 'paul_strebel'),
         user_nber = replace(user_nber, paper %in% c(4525, 7981) & name == 'Peter E Rossi', 'peter_rossi'),
         user_nber = replace(user_nber, paper <= 8126 & name == 'Richard A Posner', 'richard_posner'),
         user_nber = replace(user_nber, paper %in% c(3, 26) & name == 'Swarnjit S Arora', 'swarnjit_arora'),
         user_nber = replace(user_nber, paper == 10928 & name == 'Thomas Stratmann', 'thomas_stratmann'),
         user_nber = replace(user_nber, paper == 3954 & name == 'William F Samuelson', 'wsamuels'),
         user_repec = replace(user_repec, paper == 4728 & name == 'David C Mare', 'pma565'),
         user_repec = replace(user_repec, paper %in% c(2196, 6622) & name == 'David E M Sappington', 'psa323'),
         user_repec = replace(user_repec, paper %in% c(5179, 9262) & name == 'Dennis R Capozza', 'pca514'),
         user_repec = replace(user_repec, paper %in% 5385:8581 & grepl('Ramstetter', name), 'pra758'),
         user_repec = replace(user_repec, paper %in% c(1121, 4571) & name == 'Gikas A Hardouvelis', 'pha554'),
         user_repec = replace(user_repec, paper %in% c(6138, 7584) & name == 'Jody Overland', 'pov8'),
         user_repec = replace(user_repec, paper %in% c(3847, 8984) & name == 'Joram Mayshar', 'pma2277'),
         user_repec = replace(user_repec, paper %in% 4:185 & name == 'Lee A Lillard', 'pli669'),
         user_repec = replace(user_repec, paper %in% 6374:7980 & name == 'Mark Rider', 'pri188'),
         user_repec = replace(user_repec, paper %in% c(3020, 4172) & grepl('Stone', name), 'pst156'),
         user_repec = replace(user_repec, paper %in% c(863, 1890) & name == 'Peter R Hartley', 'pha1073'),
         user_repec = replace(user_repec, user_nber %in% c('george_wu', 'ye_qi'), NA),
         user_repec = replace(user_repec, user_nber == 'athene_laws', 'pla779'),
         user_repec = replace(user_repec, user_nber == 'bennett_mccallum', 'pmc4'),
         user_repec = replace(user_repec, user_nber == 'bldavies', 'pda777'),
         user_repec = replace(user_repec, user_nber == 'gregory_mankiw', 'pma131'),
         user_repec = replace(user_repec, user_nber == 'isabelle_sin', 'psi661'),
         user_repec = replace(user_repec, user_nber == 'james_heckman', 'phe22'),
         user_repec = replace(user_repec, user_nber == 'joseph_stiglitz', 'pst33'),
         user_repec = replace(user_repec, user_nber %in% c('laura_liu1', 'lauraliuyu'), 'pli1251'),
         user_repec = replace(user_repec, user_nber == 'lesoxley', 'pox4'),
         user_repec = replace(user_repec, user_nber == 'luigi_zingales', 'pzi101'),
         user_repec = replace(user_repec, user_nber == 'maxim_boycko', 'pbo352'),
         user_repec = replace(user_repec, user_nber == 'petr_sedlacek', 'pse304'),
         user_repec = replace(user_repec, user_nber == 'rene_stulz', 'pst226'),
         user_repec = replace(user_repec, user_nber == 'robert_hall', 'pha128'),
         user_repec = replace(user_repec, user_nber == 'robert_macculloch', 'pma305'),
         user_repec = replace(user_repec, user_nber == 'russell_cooper', 'pco940'),
         user_repec = replace(user_repec, user_nber == 'sergio_rebelo', 'pre4'),
         user_repec = replace(user_repec, user_nber == 'stephen_ross', 'pro1047'),
         user_repec = replace(user_repec, user_nber == 'takatoshi_ito', 'pit3'),
         user_repec = replace(user_repec, name == 'Kay Porter', NA),
         user_repec = replace(user_repec, name == 'Muhammed A Yildirim', NA)) %>%
  # Assert that NBER:RePEc user name correspondence are m:1
  assert_many2one(user_nber, user_repec) %>%
  # Collapse m:1 NBER:RePEc matches to 1:1
  group_by(user_repec) %>%
  mutate(user_nber = replace(user_nber, !is.na(user_repec), first(user_nber))) %>%
  group_by(user_nber) %>%
  mutate(user_repec = replace(user_repec, !is.na(user_nber), first(user_repec[!is.na(user_repec)]))) %>%
  ungroup() %>%
  assert_one2one(user_repec, user_nber) %>%
  # Assign new IDs for next stage of disambiguation process
  group_by(paper) %>%
  mutate(id = 100 * paper + row_number(),
         no_nber = is.na(user_nber),
         no_repec = is.na(user_repec)) %>%
  group_by(user_nber) %>%
  mutate(id = ifelse(!no_nber, min(id), id)) %>%
  group_by(user_repec) %>%
  mutate(id = ifelse(!no_repec, min(id), id)) %>%
  # Manually merge authors with no NBER user name
  left_join(manual_merges_nonber) %>%
  group_by(merge_id) %>%
  mutate(id = replace(id, !is.na(merge_id), min(id))) %>%
  ungroup() %>%
  arrange(user_nber, paper)


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
  ungroup() %>%
  select(author = id, name, user_nber, user_repec) %>%
  distinct() %>%
  arrange(author)

# Assert that author IDs are unique
if (nrow(authors) != n_distinct(authors$author)) {
  stop('Author IDs are not unique')
}

# Prepare paper-author correspondences
paper_authors = authors_post_programs %>%
  select(paper, author = id) %>%
  distinct() %>%
  arrange(paper, author)

# Export data
write_csv(authors, 'data-raw/authors.csv')
save(authors, file = 'data/authors.rda', version = 2, compress = 'bzip2')
write_csv(paper_authors, 'data-raw/paper_authors.csv')
save(paper_authors, file = 'data/paper_authors.rda', version = 2, compress = 'bzip2')

# Save session info
save_session_info('data-raw/authors.log')
