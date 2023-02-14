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
pop_dist <- 
  bind_rows(pop_dist) %>% 
  mutate(Year = as.numeric(mapvalues(Year, from = 1:3, to = 2018:2020)))
  

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


# Table Supplemental Materials
class_prob1 <- 
  class_prob %>% 
  dplyr::select(Year, type, Pop, 
                gero1_CH, gero1_HC, gero1_NC,
                gero0_CH, gero0_HC, gero0_NC,
                in_3, in_2, in_1,
                out_3, out_2, out_1, 
                acute_Y, acute_N, 
                cc_Y, cc_N)

mean_p_tab <- matrix(NA, ncol = 6, nrow = 16)

for(i in 1:6){
  t_ill <- class_prob1 %>% filter(type == i)
  mean_p_tab[,i] <- apply(t_ill[,c(-1,-2,-3)], MARGIN = 2, FUN = mean)
}

# Store table and extract also Latex table
# write.table(mean_p_tab, file = "data_output/LCA_prob_estimates.txt", row.names = FALSE)
# install.packages("xtable")
# xtable::xtable(mean_p_tab, digits = 2)



pdf("figures/types_table.pdf", family = "Times", width = 10, height = 8)

par(mar = c(1, 2, 4, 1) + 0.1, xpd = TRUE)
plot(x=0, y=0, typ = "n", bty = "n", xlab = NA, ylab = NA, 
     xaxt = "n", yaxt = "n", xlim = c(0.75,8), ylim = c(0,16))

# Cell grid
segments(x0=1,y0=0:16, x1=8,y1=0:16, col = "lightgray")

var_value <- rev(c("Care home", "Home care", "No care",
  "Care home", "Home care", "No care",
  "High", "Medium", "Low",
  "High", "Medium", "Low",
  "Yes", "No",
  "Yes", "No"))
for(i in 1:16){
text(x=1.5, y=i-1, pos = 3, var_value[i])
}

text(x=0.55, y=16, "Elder care", cex = 1.1)
text(x=0.55, y=10, "Medical care", cex = 1.1)
text(x=0.7, y=14.5, "One year\nprior death")
text(x=0.7, y=11.5, "At death")
text(x=0.7, y=8.5, "Inpatient\ncare")
text(x=0.7, y=5.5, "Outpatient\ncare")
text(x=0.7, y=3, "Acute\ncare")
text(x=0.7, y=1, "Clinical\ncare")

text(x = seq(2.5, 7.5, by = 1), y = 17, pos = 3, 
     c("Terminally ill", "Sudden death", 
  "Impaired\nsevere progression", "Impaired", 
  "Dependent\nsevere progression", "Dependent"))

# Population shares
pop_share <- 
  class_prob1 %>% 
  group_by(type) %>% 
  summarise(mean_p = round(mean(Pop)*100,0))
  # summarise(min_p = round(min(Pop)*100,0), max_p = round(max(Pop)*100,0))

text(x = 1.5, y = 16, pos = 3, "% on all deaths\n(mean 2018-20)")
text(x = 5, y = 18, pos = 3, "Type of end-of-life trajectory", cex = 1.3)

text(x = seq(2.5, 7.5, by = 1), y = 16, pos = 3, 
     paste(pop_share$mean_p,"%", sep = ""))


class_prob1 <- 
  class_prob %>% 
  dplyr::select(Year, type, Pop, 
                gero1_CH, gero1_HC, gero1_NC,
                gero0_CH, gero0_HC, gero0_NC,
                in_3, in_2, in_1,
                out_3, out_2, out_1, 
                acute_Y, acute_N, 
                cc_Y, cc_N)

# Terminal ill
for(i in 1:6){
  
  t_ill <- class_prob1 %>% filter(type == i)
  mean_p <- apply(t_ill[,c(-1,-2,-3)], MARGIN = 2, FUN = mean)
  col_p <- rev(as.character(cut(mean_p, breaks = c(0, 1/3, 2/3, 1),
                                
             labels = c('#fff7bc','#fec44f','#d95f0e'),
             include.lowest = TRUE)))
  xx <- c(i+1, i+1, i+2, i+2)
  for(j in 1:16){
    yy <- c(j-1, j, j, j-1)
    polygon(x=xx, y=yy, col=col_p[j], border = col_p[j])
  }
}

segments(x0=2:8,y0=0, x1=2:8,y1=16, col = "lightgray", lwd = 1.5)
segments(x0=1,y0=c(10, 16), x1=8,y1=c(10, 16), 
         col = gray(0.2), lwd = 3)
segments(x0=1,y0=c(2, 4, 7, 13), x1=8,y1=c(2, 4, 7, 13), 
         col = "darkgray", lwd = 3, lty = 1)
text(x=3.5, y=-0.5, "Probabilities:", pos = 4)
legend(y=0, x = 4.4, legend = c("< 0.33", "0.33-0.66", "> 0.66"), 
       col = c('#fff7bc','#fec44f','#d95f0e'), horiz = TRUE, bty = "n", 
       pch = 15)
dev.off()









