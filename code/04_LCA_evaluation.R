# LCA analysis to determine death types
source("code/00_packages_functions.R")

# Load data
load("data_inter/lca_fits.RData")
# 
# lca_mod

# Plot class probabilities 
class_prob <- pop_dist <-list()

for(i in 1:3){
  class_prob[[i]] <- bind_rows(
    tibble(as.data.frame.table(lca_mod[[i]]$probs$acute), Year = i, indi = "acute"),
    tibble(as.data.frame.table(lca_mod[[i]]$probs$care_clinic), Year = i, indi = "c_c"),
    tibble(as.data.frame.table(lca_mod[[i]]$probs$care1), Year = i, indi = "c_1"),
    tibble(as.data.frame.table(lca_mod[[i]]$probs$care0), Year = i, indi = "c_0"),
    tibble(as.data.frame.table(lca_mod[[i]]$probs$hosp_median), Year = i, indi = "in"),
    tibble(as.data.frame.table(lca_mod[[i]]$probs$out_median), Year = i, indi = "out"))
  pop_dist[[i]] <- 
    bind_rows(tibble(Pop = lca_mod[[i]]$P, Year = i, Var1 = paste(1:6)))
  }

class_prob <- bind_rows(class_prob)
pop_dist <- bind_rows(pop_dist)

# Identify common classes across years
class_prob <- 
  class_prob %>% 
  mutate(Var2 = mapvalues(as.character(Var2), from = c("Pr(1)", "Pr(2)", "Pr(3)"), 
                          to = 1:3), 
         Var1 = mapvalues(as.character(Var1), 
                          from = c("class 1: ", "class 2: ", "class 3: ",
                                   "class 4: ", "class 5: ", "class 6: "), 
                          to = 1:6), 
         Year = as.numeric(mapvalues(Year, from = 1:3, to = 2018:2020))) %>%
  pivot_wider(id_cols = c(Var1, Year), names_from = c(indi, Var2), values_from = Freq) %>% 
  rename(acute_N = acute_1, acute_Y = acute_2,
         cc_N = c_c_1, cc_Y = c_c_2, 
         gero1_NC = c_1_1, gero1_HC = c_1_2, gero1_CH = c_1_3,
         gero0_NC = c_0_1, gero0_HC = c_0_2, gero0_CH = c_0_3,
         )

# Note that classes chnage each time a new LCA model is estimated
class_prob %>% arrange(desc(gero1_CH), desc(in_1), Year)

# 2018: 3 sudden death, 6 terminally ill, 5 impaired sev prog, 2 impaired, 4 dependent, 1 dependent sev prog 
# 2019: 6 sudden death, 5 terminally ill, 1 impaired sev prog, 4 impaired, 2 dependent, 3 dependent sev prog
# 2020: 6 sudden death, 4 terminally ill, 2 impaired sev prog, 1 impaired, 5 dependent, 3 dependent sev prog


# Relabel classes (Wanrning: recode works only with the current LCA results)

# 1 Terminal ill
# 2 Sudden death
# 3 Impaired severe progression
# 4 Impaired
# 5 Dependent severe progression
# 6 Dependent

# 2018: 6 - 1, 3 - 2, 5 - 3, 2 - 4, 1 - 5, 4 - 6
# 2019: 5 - 1, 6 - 2, 1 - 3, 4 - 4, 3 - 5, 2 - 6 
# 2020: 4 - 1, 6 - 2, 2 - 3, 1 - 4, 3 - 5, 5 - 6

class_prob <- 
  class_prob %>% 
  filter(Year > 2) %>% 
  mutate(type = case_when(Year == 2018 ~ mapvalues(Var1, 
                                                from = c(6, 3, 5, 2, 1, 4), 
                                                to = 1:6),
                          Year == 2019 ~ mapvalues(Var1, 
                                                from = c(5, 6, 1, 4, 3, 2), 
                                                to = 1:6),
                          Year == 2020 ~ mapvalues(Var1, 
                                                from = c(4, 6, 2, 1, 3, 5), 
                                                to = 1:6))) %>% 
  left_join(pop_dist)

# Evaluation of prediction performance
# Load Death data
source("code/01_data_preparation.R")
years <- 2018:2020
pred_prob_mat <- list()
class_dat <- list()

i <- 1

for(i in 1:3){
lca_year <- 
    dat_LCA %>%
    ungroup() %>% 
    filter(DeathYear == years[i] & Age >= 70) %>%
    dplyr::select(acute, care_clinic, care1, care0, hosp_median, out_median)

post_year <- 
  tibble(class = lca_mod[[i]]$predclass,
         as.data.frame(poLCA.posterior(lca_mod[[i]], y=as.matrix(lca_year)))) %>% 
                 mutate(type = case_when(i == 1 ~ mapvalues(class, 
                                                            from = c(6, 3, 5, 2, 1, 4), 
                                                            to = 1:6),
                                         i == 2 ~ mapvalues(class, 
                                                            from = c(5, 6, 1, 4, 3, 2), 
                                                            to = 1:6),
                                         i == 3 ~ mapvalues(class, 
                                                            from = c(4, 6, 2, 1, 3, 5), 
                                                            to = 1:6)),
                        id = 1:nrow(lca_year)) %>% 
  pivot_longer(cols = V1:V6) %>% 
  mutate(orig_class = substr(name, 2,2),
         orig_class_re = case_when(i == 1 ~ mapvalues(orig_class,
                                                      from = c(6, 3, 5, 2, 1, 4), 
                                                      to = 1:6),
                                   i == 2 ~ mapvalues(orig_class, 
                                                      from = c(5, 6, 1, 4, 3, 2), 
                                                      to = 1:6),
                                   i == 3 ~ mapvalues(orig_class, 
                                                      from = c(4, 6, 2, 1, 3, 5), 
                                                      to = 1:6))) %>% 
  pivot_wider(id_cols = c(class, type, id), 
              names_from = orig_class_re, values_from = value)



pred_prob_mat[[i]] <- round(post_year %>% group_by(type) %>% summarise(V1 = mean(`1`),
                                             V2 = mean(`2`),
                                             V3 = mean(`3`),
                                             V4 = mean(`4`),
                                             V5 = mean(`5`),
                                             V6 = mean(`6`)),2)

}

# xtable::xtable(pred_prob_mat[[3]])
# xtable::xtable(pred_prob_mat[[4]])
# xtable::xtable(pred_prob_mat[[5]])

# Extract lopnr and class in one data
lop_class <- list()
for(i in 1:3){
  lca_year <- 
    dat_LCA %>%
    ungroup() %>% 
    filter(DeathYear == years[i] & Age >= 70) %>%
    dplyr::select(LopNr)
  
  lop_class[[i]] <- 
    tibble(class = lca_mod[[i]]$predclass,
           LopNr = lca_year$LopNr) %>%
    mutate(type = case_when(i == 1 ~ mapvalues(class, 
                                               from = c(6, 3, 5, 2, 1, 4), 
                                               to = 1:6),
                            i == 2 ~ mapvalues(class, 
                                               from = c(5, 6, 1, 4, 3, 2), 
                                               to = 1:6),
                            i == 3 ~ mapvalues(class, 
                                               from = c(4, 6, 2, 1, 3, 5), 
                                               to = 1:6)))
}

lop_class <- 
  lop_class %>% 
  bind_rows()

save(lop_class, file = "data_inter/lopnr_predClass.RData")

