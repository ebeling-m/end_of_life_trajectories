# Data preparation for descrptives LCA
source("code/00_packages_functions.R")

# Load data
var_types <- c("n", "n", "n", "n", "c", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", 
               "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", 
               "n", "n", "c")

dat <- 
  read_delim("U:/deathPathways/data/oneYearPD_data_20210914.txt", delim = " ", 
                  col_types = as.list(var_types)) %>% 
  dplyr::select(LopNr, hhCh, hhSp, RurUrb, care1, care0, martSt, migBack, placeD, Age, 
         Edu, CoD, DeathYear, hospD, nV, hhSize, sex)



# Load MVO add on 
mvo <- 
  read_delim("U:/deathPathways/data/mvo_new_20220216.txt", delim = " ") %>%
  rename(care_clinic = care)

dat <- 
  dat %>% 
  left_join(mvo)

# Variables in LCA
# acute, care1, care0, care_clinic, 
# median hospital, median outpatient, 
# mvo above 2

# create variables
dat_LCA <- 
  dat %>%
  dplyr::select(LopNr, acute, care_clinic, psycho,care1, care0, hospD, nV, n, DeathYear, Age) %>% 
  # edit NA variables related to hospital care
  mutate(acute = ifelse(is.na(acute), 1, acute),
         care_clinic = ifelse(is.na(care_clinic), 1, care_clinic),
         n = ifelse(is.na(n), 0, n)) %>%
  # 3927 individuals have no care information, they will be omitted
  filter(!is.na(care1)) %>% 
  # scale down hospital days to 365 (higher number arises from stays starting >12 months prior death)
  mutate(hospD = ifelse(hospD > 365, 365, hospD)) %>% 
  mutate(hosp_median = as.numeric(cut(hospD, breaks = quantile(hospD, probs = c(0, 1/3, 2/3, 1)), 
                           labels = 1:3, include.lowest = TRUE)), 
         out_median = as.numeric(cut(nV, breaks = quantile(nV, probs = c(0, 1/3, 2/3, 1)), 
                          labels = 1:3, include.lowest = TRUE)), 
         mvo_median = ifelse(n > median(n), 2, 1))

# save(dat_LCA, file = "data_inter/data_LCA.RData")
# calculate cut off for medical care terciles
dat %>%
  dplyr::select(LopNr, acute, care_clinic, psycho,care1, care0, hospD, nV, n, DeathYear, Age) %>% 
  # edit NA variables related to hospital care
  mutate(acute = ifelse(is.na(acute), 1, acute),
         care_clinic = ifelse(is.na(care_clinic), 1, care_clinic),
         psycho = ifelse(is.na(psycho), 1, psycho),
         n = ifelse(is.na(n), 0, n)) %>%
  # 3927 individuals have no care information, they will be omitted for the moment
  filter(!is.na(care1)) %>% 
  # scale down hospital days to 365 (higher number arises from stays starting >12 months prior death)
  mutate(hospD = ifelse(hospD > 365, 365, hospD)) %>%
  summarise(hosp_low = quantile(hospD, probs = 1/3),
            hosp_mid = quantile(hospD, probs = 2/3),
            out_low = quantile(nV, probs = 1/3),
            out_mid = quantile(nV, probs = 2/3))


# How are the missing cases distributed 
apply(table(dat$DeathYear, is.na(dat$care1)), MARGIN = 1, FUN = function(x) x/sum(x))
