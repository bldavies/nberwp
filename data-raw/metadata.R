# METADATA.R
#
# This script downloads metadata for the NBER working paper series.
#
# Ben Davies
# March 2021

# Load packages
library(bldr)

# Initialize output directory
outdir = 'data-raw/metadata/'
if (!dir.exists(outdir)) dir.create(outdir)

# Download files
files = paste0(c('author_user', 'auths', 'date', 'jel', 'prog', 'title'), '.dta')
for (f in files) {
  download.file(paste0('http://www2.nber.org/wp_metadata/', f), paste0(outdir, f))
}

# Save session info
save_session_info('data-raw/metadata.log')
