# Descriptive statistics of input variables for LCA analysis
source("code/00_packages_functions.R")

# Load Death data
source("code/01_data_preparation.R")

# dat_LCA
lca_year <-
  dat_LCA %>%
  filter(Age >= 70) %>%
  ungroup() %>%
  dplyr::select(acute, care_clinic, care1, care0,
                hosp_median, out_median, DeathYear, LopNr)

# Annual number of deaths
an_num <- lca_year %>% count(DeathYear)
an_num

# Care status at 1 year
care1 <-
  lca_year %>%
  count(DeathYear, care1) %>%
  pivot_wider(id_cols = care1, names_from = DeathYear, values_from = n)

care1

# Care status at death
care0 <-
  lca_year %>%
  count(DeathYear, care0) %>%
  pivot_wider(id_cols = care0, names_from = DeathYear, values_from = n)

care0

# in
hosp <-
  lca_year %>%
  count(DeathYear, hosp_median) %>%
  pivot_wider(id_cols = hosp_median, names_from = DeathYear, values_from = n)
hosp

# out
out <-
  lca_year %>%
  count(DeathYear, out_median) %>%
  pivot_wider(id_cols = out_median, names_from = DeathYear, values_from = n)
out

# acute
acute <-
  lca_year %>%
  count(DeathYear, acute) %>%
  pivot_wider(id_cols = acute, names_from = DeathYear, values_from = n)


# clinic
clinic <-
  lca_year %>%
  count(DeathYear, care_clinic) %>%
  pivot_wider(id_cols = care_clinic, names_from = DeathYear, values_from = n)


# write.table(rbind(an_num$n,
#                         as.matrix(care1[,paste(2015:2020)]),
#                         as.matrix(care0[,paste(2015:2020)]),
#                         as.matrix(hosp[,paste(2015:2020)]),
#                         as.matrix(out[,paste(2015:2020)]),
#                         as.matrix(acute[,paste(2015:2020)]),
#                         as.matrix(clinic[,paste(2015:2020)])),
#             file="data_inter/table1_numbers.txt",
#             row.names = FALSE)
# 
# write.table(apply(rbind(an_num$n,
#                         as.matrix(care1[,paste(2015:2020)]),
#                         as.matrix(care0[,paste(2015:2020)]),
#                         as.matrix(hosp[,paste(2015:2020)]),
#                         as.matrix(out[,paste(2015:2020)]),
#                         as.matrix(acute[,paste(2015:2020)]),
#                         as.matrix(clinic[,paste(2015:2020)])),
#                   MARGIN = 2, FUN = function(x) round(x/x[1]*100, 0)),
#             file="data_inter/table1_percent.txt",
#             row.names = FALSE)
