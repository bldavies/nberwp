# METADATA.R
#
# This script downloads metadata for the NBER working paper series.
#
# Ben Davies
# July 2021

# Load packages
library(bldr)

# Set timeout limit
options(timeout = max(300, getOption('timeout')))

# Initialize output directory
outdir = 'data-raw/metadata/'
if (!dir.exists(outdir)) dir.create(outdir)

# Download files
files = paste0('working_papers', c('', '_authors', '_programs'), '.tab')
for (f in files) {
  download.file(paste0('https://data.nber.org/nber-wp-logs/', f), paste0(outdir, f))
}

# Save session info
save_session_info('data-raw/metadata.log')
