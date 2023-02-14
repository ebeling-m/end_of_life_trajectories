# LCA analysis to determine death types
source("code/00_packages_functions.R")

# Load Death data
source("code/01_data_preparation.R")
# dat_LCA

lca_mod <- list()

for(i in 2018:2020){
lca_year <- 
  dat_LCA %>%
  ungroup() %>% 
  filter(DeathYear == i & Age >= 70) %>% 
  dplyr::select(acute, care_clinic, care1, care0, hosp_median, out_median)

func <- as.matrix(lca_year)~1
lca_mod[[i-2017]] <- poLCA(func, nclass = 6, maxiter = 1000000, 
              nrep = 5, data = lca_year, graphs = FALSE)
print(i)
}

save(lca_mod, file = "data_inter/lca_fits.RData")
# load("data_inter/lca_fits.RData")
# lca_mod
