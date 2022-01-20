This folder contains code and data used to create the tables in `data/`.
I create these tables using the following procedure:

1. Run `metadata.R` and `repec.R` to download the latest raw metadata.
2. Run `papers.R` to construct the table `papers`.
3. Run `programs.R` to construct the tables `programs` and `paper_programs`.
4. Run `disambiguated_correspondences.R`, then `paper_authors.R` to construct the table `paper_authors`.
5. Run `author_names.R`, then `sexes/facebook.R`, then `author_sexes.R`, then `authors.R` to construct the table `authors`.

I run each R script in a fresh instance of `nberwp.Rproj`.
