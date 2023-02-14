# LCA analysis to determine death types
source("code/A_00_packages_functions.R")

# Load Death data
source("code/01_data_preparation.R")
# dat_LCA

# Run estimation about optimal death numbert of classes 
lca_dat_bic_est <- 
  dat_LCA %>%
  ungroup() %>% 
  filter(DeathYear == 2018 & Age >= 70) %>% 
  dplyr::select(acute, care_clinic, care1, care0, hosp_median, out_median)

# est_cases <- sample(x=1:nrow(lca_dat_bic_est), size = 5000)
# func <- as.matrix(lca_dat_bic_est)[est_cases,]~1
func <- as.matrix(lca_dat_bic_est)~1
bic.list <- aic.list <- bic_mod <- list()

for(i in 5:7){
  # bic_mod[[i]] <- poLCA(func, nclass = i, maxiter = 1000000, 
  #                  data = lca_dat_bic_est[est_cases,], nrep = 5)
  bic_mod[[i]] <- poLCA(func, nclass = i, maxiter = 1000000, 
                        data = lca_dat_bic_est, nrep = 5)
  bic.list[[i]] <- bic_mod[[i]]$bic
  aic.list[[i]] <- bic_mod[[i]]$aic
  try(if(bic.list[[i]] >  bic.list[[i-1]] & i > 1) stop("minimum BIC reached"))
}

bic_mod[[5]]$probs

diff(unlist(bic.list))/bic.list[[5]]
diff(unlist(aic.list))

# check posterior probabilities
c5 <- 
  tibble(class = bic_mod[[5]]$predclass,
         as.data.frame(poLCA.posterior(bic_mod[[5]], 
                                       y=as.matrix(lca_dat_bic_est)))) %>% 
  group_by(class) %>% 
  summarise(V1= mean(V1),
            V2= mean(V2),
            V3= mean(V3),
            V4= mean(V4),
            V5= mean(V5))

c6 <- 
  tibble(class = bic_mod[[6]]$predclass,
         as.data.frame(poLCA.posterior(bic_mod[[6]], 
                                       y=as.matrix(lca_dat_bic_est)))) %>% 
  group_by(class) %>% 
  summarise(V1= mean(V1),
            V2= mean(V2),
            V3= mean(V3),
            V4= mean(V4),
            V5= mean(V5),
            V6= mean(V6))


c7 <- 
  tibble(class = bic_mod[[7]]$predclass,
         as.data.frame(poLCA.posterior(bic_mod[[7]], 
                                       y=as.matrix(lca_dat_bic_est)))) %>% 
  group_by(class) %>% 
  summarise(V1= mean(V1),
            V2= mean(V2),
            V3= mean(V3),
            V4= mean(V4),
            V5= mean(V5),
            V6= mean(V6),
            V7= mean(V7))

diag(as.matrix(c5[,-1]))
diag(as.matrix(c6[,-1]))
diag(as.matrix(c7[,-1]))

round(as.matrix(c5[,-1]),2)
round(as.matrix(c6[,-1]),2)
round(as.matrix(c7[,-1]),2)

pred_prob_mat[[i]] <- round(test %>% group_by(type) %>% summarise(V1 = mean(`1`),
                                                                  V2 = mean(`2`),
                                                                  V3 = mean(`3`),
                                                                  V4 = mean(`4`),
                                                                  V5 = mean(`5`),
                                                                  V6 = mean(`6`)),2)

}




# Plot class probabilities 
class_prob <- pop_dist <-list()
for(i in 1:5){
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
# 6 Classes indentified as best fitting model 
# Fit model with 6 class BIC improves only marginally after that
