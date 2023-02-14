# LCA analysis to determine death types
source("code/A_00_packages_functions.R")

# Load Death data
load("data_inter/data_LCA.RData")
# dat_LCA

# Run estimation about optimal death numbert of classes 
lca_dat_bic_est <- 
  dat_LCA %>%
  ungroup() %>% 
  filter(DeathYear == 2018) %>% 
  dplyr::select(acute, care_clinic, care1, care0, hosp_median, out_median, mvo_median)

est_cases <- sample(x=1:nrow(lca_dat_bic_est), size = 5000)
func <- as.matrix(lca_dat_bic_est)[est_cases,]~1

bic.list <- aic.list <- list()
for(i in 1:10){
bic_mod <- poLCA(func, nclass = i, maxiter = 1000000, 
                 data = lca_dat_bic_est[est_cases,], nrep = 5)
bic.list[[i]] <- bic_mod$bic
aic.list[[i]] <- bic_mod$aic
try(if(bic.list[[i]] >  bic.list[[i-1]] & i > 1) stop("minimum BIC reached"))
}

# 6 Classes indentified as best fitting model 
# Fit model with 6 class BIC improves only marginally after that


# Fit LCA
# 2016
lca_2016 <- 
  dat_LCA %>%
  ungroup() %>% 
  filter(DeathYear == 2016) %>% 
  dplyr::select(acute, care_clinic, care1, care0, hosp_median, out_median, mvo_median)

table(lca_2016$hosp_median, lca_2016$out_median)

func <- as.matrix(lca_2016)~1
mod_16 <- poLCA(func, nclass = 6, maxiter = 1000000, 
              nrep = 15, data = lca_2016, graphs = TRUE)



?poLCA.entropy(mod_16)

names(mod_16)

test <- tibble(class = mod_16$predclass, 
               as.data.frame(poLCA.posterior(mod_16, y=as.matrix(lca_2016))))

round(test %>% group_by(class) %>% summarise(V1 = mean(V1),
                                       V2 = mean(V2),
                                       V3 = mean(V3),
                                       V4 = mean(V4),
                                       V5 = mean(V5),
                                       V6 = mean(V6)),2)

mean(diag(poLCA.posterior(mod_16, y=as.matrix(lca_2016))))