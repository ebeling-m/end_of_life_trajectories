# LCA analysis to determine death types
source("code/00_packages_functions.R")

# Load data and merge predicted class
source("code/01_data_preparation.R")

load("data_inter/lopnr_predClass.RData")
# dat_LCA

dat_death <- 
  dat %>% 
  left_join(lop_class) %>% 
  filter(DeathYear > 2017 & !is.na(type))

# Calculate proportions of deaths by year 
d_prop <- 
  dat_death %>% 
  count(type, DeathYear) %>% 
  group_by(DeathYear) %>% 
  mutate(t_d = sum(n),
         p = round(n/t_d*100,0)) %>% 
  pivot_wider(id_cols = DeathYear, names_from = type, values_from = p)

# Calculate age distribution
age_dist <- 
  dat_death %>% 
  group_by(type, sex) %>% 
  summarise(median_age = quantile(Age, probs = 0.5),
            iqr1_age = quantile(Age, probs = 0.25),
            iqr2_age = quantile(Age, probs = 0.75)) %>% 
  arrange(sex, median_age)

# Plot age dist
col_type <- adjustcolor(c('#a6cee3','#1f78b4','#b2df8a',
                          '#33a02c','#fb9a99','#e31a1c'),
                        alpha.f = 0.8)
names(col_type) <- 1:6
type_names <- c("Terminally ill", "Sudden death", "Impaired severe progression",
                "Impaired", "Dependent severe progression", "Dependent")
names(type_names) <- 1:6

# Figure distribution over age
# Distribution of deaths over age 

counts_age <- 
  dat_death %>% 
  mutate(age_int = as.integer(Age), 
         age_int = ifelse(age_int >= 105, 105, age_int)) %>% 
  count(sex, type, age_int) %>%
  arrange(sex, age_int, type) %>%
  group_by(sex, age_int) %>% 
  mutate(n_cum = cumsum(n)) %>% 
  filter(age_int >= 70) %>%
  ungroup() %>% 
  mutate(col_type = mapvalues(type, 
                              from = 1:6, 
                              to = col_type))


pdf("figures/death_distribution_age.pdf", width = 14, 
    pointsize = 16, height = 14)

layout(rbind(1, 2))

par(mar = c(2,4,3,1))
plot(x=1, y=1, ylim = c(0,7000), xlim = c(69, 106), xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i", bty = "n", xlab = NA, 
     ylab = NA)

segments(x0=seq(70, 105, by = 5), y0=0, 
         x1=seq(70, 105, by = 5), y1=7000,
         col = "darkgray")

# segments(x0=seq(72.5, 102.5, by = 5), y0=0, 
#          x1=seq(72.5, 102.5, by = 5), y1=7000,
#          col = "darkgray", lwd = 0.5)

segments(x0=69, y0=seq(0, 7000, by = 1000), 
         x1=106, y1=seq(0, 7000, by = 1000),
         col = "darkgray")

segments(x0=69, y0=seq(500, 6500, by = 1000), 
         x1=106, y1=seq(500, 6500, by = 1000),
         col = "darkgray", lwd = 0.5)

axis(1, at = seq(70, 105, by = 5), labels = TRUE, lwd = 3)
# axis(1, at = seq(72.5, 102.5, by = 5), labels = FALSE, lwd = 1)

axis(2, at = seq(0, 7000, by = 1000), labels = TRUE, lwd = 3)
axis(2, at = seq(500, 6500, by = 1000), labels = FALSE, lwd = 1)

# mtext("Age", 1, line = 2, cex = 1.5)
mtext("Death", 2, line = 2, cex = 1.5)
mtext("Women", 3, line = -1.3, cex = 1.5, adj = 0.01)

xx <- c(-0.5,-0.5, 0.5, 0.5)
for(i in 70:105){
  plotDat <- 
    counts_age %>% 
    filter(sex == 2 & age_int == i)
  for(j in 1:6){
    if(j == 1){
      yy <- c(0, rep(plotDat$n_cum[j], 2), 0)
    }else{
      yy <- c(plotDat$n_cum[j-1], rep(plotDat$n_cum[j], 2), plotDat$n_cum[j-1])
    }
    polygon(x=xx+i, y=yy, col = col_type[j], border = "white")
  }
}
age_dist
legend("topright", 
       legend = c("Terminally ill (Median age: 80.2)", "Sudden death (81.0)", "Impaired severe progression (87.0)",
                  "Impaired (89.4)", "Dependent severe progression (89.3)", "Dependent (91.0)"), 
       col = col_type, bty = "n", cex = 1, pch = 15)

# Men 
par(mar = c(4,4,1,1))
plot(x=1, y=1, ylim = c(0,7000), xlim = c(69, 106), xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i", bty = "n", xlab = NA, 
     ylab = NA)

segments(x0=seq(70, 105, by = 5), y0=0, 
         x1=seq(70, 105, by = 5), y1=7000,
         col = "darkgray")

# segments(x0=seq(72.5, 102.5, by = 5), y0=0, 
#          x1=seq(72.5, 102.5, by = 5), y1=7000,
#          col = "darkgray", lwd = 0.5)

segments(x0=69, y0=seq(0, 7000, by = 1000), 
         x1=106, y1=seq(0, 7000, by = 1000),
         col = "darkgray")

segments(x0=69, y0=seq(500, 6500, by = 1000), 
         x1=106, y1=seq(500, 6500, by = 1000),
         col = "darkgray", lwd = 0.5)

axis(1, at = seq(70, 105, by = 5), labels = TRUE, lwd = 3)
# axis(1, at = seq(72.5, 102.5, by = 5), labels = FALSE, lwd = 1)

axis(2, at = seq(0, 7000, by = 1000), labels = TRUE, lwd = 3)
axis(2, at = seq(500, 6500, by = 1000), labels = FALSE, lwd = 1)

mtext("Age", 1, line = 2, cex = 1.5)
mtext("Death", 2, line = 2, cex = 1.5)
mtext("Men", 3, line = -1.3, cex = 1.5, adj = 0.01)

xx <- c(-0.5,-0.5, 0.5, 0.5)
for(i in 70:105){
  plotDat <- 
    counts_age %>% 
    filter(sex == 1 & age_int == i)
  for(j in 1:6){
    if(j == 1){
      yy <- c(0, rep(plotDat$n_cum[j], 2), 0)
    }else{
      yy <- c(plotDat$n_cum[j-1], rep(plotDat$n_cum[j], 2), plotDat$n_cum[j-1])
    }
    polygon(x=xx+i, y=yy, col = col_type[j], border = "white")
  }
}
legend("topright", 
       legend = c("Terminally ill (80.5)", "Sudden death (79.5)", "Impaired severe progression (85.7)",
                  "Impaired (87.4)", "Dependent severe progression (86.8)", "Depedent (87.8)"), 
       col = col_type, bty = "n", cex = 1, pch = 15)
dev.off()

# Cause of death
#####################################

cod <- 
  dat_death %>% 
  mutate(cod_c = substr(CoD, 1,1), 
         cod_c = ifelse(substr(CoD, 1,3) == "R54", "F", cod_c),
         cod_c = ifelse(substr(CoD, 1,3) %in% paste("R",95:99, sep = ""),
                        "ZZ", cod_c), 
         cod_c = ifelse(substr(CoD, 1,3) %in% 
                          paste("D",c(c("00", "01", "02", "03", "04" , "05" , "06", "07", "08", "09"), 10:48), sep = ""),
                        "C", cod_c), 
         cod_c = ifelse(cod_c %in% c("C", "F", "G", "I", "J", "ZZ"), cod_c, "X")) %>% 
  count(sex, type, cod_c) %>%
  group_by(sex, type) %>% 
  mutate(tot_d = sum(n), 
         p = n / tot_d*100, 
         cod_c_num = mapvalues(cod_c, 
                               from = c("C", "F", "G", "I", "J", "ZZ", "X"),
                               to = 1:7)) %>% 
  arrange(sex, type, cod_c_num) %>% 
  group_by(sex, type) %>% 
  mutate(p_cum = cumsum(p))


col_cod <- adjustcolor(
  c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628'), 
  alpha.f = 0.8)
col_cod <- adjustcolor(
  c('#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58'),
  alpha.f = 1)


pdf("figures/cod_death_types.pdf", pointsize = 16, width = 14, height = 14)
layout(rbind(1, 2))
par(mar = c(3,1,2,4))
par(xpd = TRUE)

plot(x=1, y=1, ylim = c(0,6), xlim = c(0, 120), xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i", bty = "n", xlab = NA, 
     ylab = NA, typ = "n")


segments(x0=seq(0, 100, by = 10), y0=0, 
         x1=seq(0, 100, by = 10), y1=6,
         col = "darkgray")

segments(x0=seq(5, 95, by = 10), y0=0, 
         x1=seq(5, 95, by = 10), y1=6,
         col = "darkgray", lwd = 0.5)

axis(1, at = seq(0, 100, by = 10), 
     labels = paste(seq(0, 100, by = 10), "%", sep = ""), 
     lwd = 3)
axis(1, at = seq(5, 95, by = 10), 
     labels = FALSE, lwd = 1)

yy <- 1:6
names(yy) <- 1:6
for(i in 1:6){
  plotDat <- 
    cod %>% 
    filter(sex == 2 & type == i)
  for(j in 1:7){
    if(j == 1){
      xx <- c(0, 0, rep(plotDat$p_cum[j], 2))
    }else{
      xx <- c(rep(plotDat$p_cum[j-1], 2), rep(plotDat$p_cum[j], 2))
    }
    y <- c(yy[paste(i)]-0.75, rep(yy[paste(i)]-0.25, 2), yy[paste(i)]-0.75)
    polygon(x=xx, y=y, col = col_cod[j], border = "white")
  }
}

text(x=0, y=yy,
     c("Terminally ill", "Sudden death", "Impaired severe progression",
       "Impaired", "Dependent severe progression", "Dependent"),
     font = 2, pos = 4)

legend(x=100, y=4.5, 
       legend = c("Cancer", "Mental & behavioral\ndisorders", "Nervous system diseases",
                  "Cardiovascular diseases", "Respiratory diseases", "Unknown cause", 
                  "Other"), bty = "n", col = col_cod, pch = 15)
mtext("Women", 3, line = 0, adj = 0.8, cex = 1.4)
# mtext("Percentage on all death", 1, line = 2.5, cex = 1.2, adj = 0.4)

# Men
par(mar = c(4,1,1,4))
par(xpd = TRUE)
plot(x=1, y=1, ylim = c(0,6), xlim = c(0, 120), xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i", bty = "n", xlab = NA, 
     ylab = NA, typ = "n")


segments(x0=seq(0, 100, by = 10), y0=0, 
         x1=seq(0, 100, by = 10), y1=6,
         col = "darkgray")

segments(x0=seq(5, 95, by = 10), y0=0, 
         x1=seq(5, 95, by = 10), y1=6,
         col = "darkgray", lwd = 0.5)

axis(1, at = seq(0, 100, by = 10), 
     labels = paste(seq(0, 100, by = 10), "%", sep = ""), 
     lwd = 3)
axis(1, at = seq(5, 95, by = 10), 
     labels = FALSE, lwd = 1)

yy <- 1:6
names(yy) <- 1:6
for(i in 1:6){
  plotDat <- 
    cod %>% 
    filter(sex == 1 & type == i)
  for(j in 1:7){
    if(j == 1){
      xx <- c(0, 0, rep(plotDat$p_cum[j], 2))
    }else{
      xx <- c(rep(plotDat$p_cum[j-1], 2), rep(plotDat$p_cum[j], 2))
    }
    y <- c(yy[paste(i)]-0.75, rep(yy[paste(i)]-0.25, 2), yy[paste(i)]-0.75)
    polygon(x=xx, y=y, col = col_cod[j], border = "white")
  }
}

text(x=0, y=yy,
     c("Terminally ill", "Sudden death", "Impaired severe progression",
       "Impaired", "Dependent severe progression", "Dependent"),
     font = 2, pos = 4)

legend(x=100, y=4.5, 
       legend = c("Cancer", "Mental & behavioral\ndisorders", "Nervous system diseases",
                  "Cardiovascular diseases", "Respiratory diseases", "Unknown cause", 
                  "Other"), bty = "n", col = col_cod, pch = 15)
mtext("Men", 3, line = 0, adj = 0.8, cex = 1.4)
mtext("Percentage on all death", 1, line = 2.5, cex = 1.2, adj = 0.4)
dev.off()


# Supplemental Figures
###########################################################
##########################################################

# Distribution by sex 

sex_counts <- 
  dat_death %>% 
  count(sex, type)

pdf("figures/sex_dist.pdf", pointsize = 14)
par(mar = c(3,3,2,2), xpd = TRUE)
plot(x=1, y=1, ylim = c(0,6), xlim = c(-40000, 40000), xaxt = "n",
     yaxt = "n", xaxs = "i", yaxs = "i", bty = "n", xlab = NA, 
     ylab = NA, typ = "n")

segments(x0=seq(-40000, 40000, by = 10000), y0=0, 
         x1=seq(-40000, 40000, by = 10000), y1=6,
         col = "darkgray")

segments(x0=seq(-35000, 35000, by = 10000), y0=0, 
         x1=seq(-35000, 35000, by = 10000), y1=6,
         col = "darkgray", lwd = 0.5)

axis(1, at = seq(-40000, 40000, by = 10000), 
     labels = c(seq(40000, 0, by = -10000),
                seq(10000, 40000, by = 10000)), 
     lwd = 3, pos = 0.05)
axis(1, at = seq(-35000, 35000, by = 10000), 
     labels = FALSE, 
     lwd = 1, pos = 0.05)

yy <- 1:6
names(yy) <- 1:6
for(i in 1:6){
  women <- 
    sex_counts %>% 
    filter(type == i & sex == 2)
  
  men <- 
    sex_counts %>% 
    filter(type == i & sex == 1)
  
  polygon(x=c(0, rep(-men$n, 2), 0), y=c(rep(yy[paste(i)]-0.75, 2), 
                                         rep(yy[paste(i)]-0.25, 2)),
          col =adjustcolor("steelblue", alpha.f = 0.8), border = "white")
  
  polygon(x=c(0, rep(women$n, 2), 0), y=c(rep(yy[paste(i)]-0.75, 2), 
                                         rep(yy[paste(i)]-0.25, 2)),
          col =adjustcolor("tomato", alpha.f = 0.8), border = "white")
  
}

men <- 
  sex_counts %>% 
  filter(sex == 1)
xx <- -men$n
text(x=-41000, y=yy,
     c("Terminally ill", "Sudden death", "Impaired severe progression",
       "Impaired", "Dependent severe progression", "Dependent"),
     font = 2, pos = 4)
# mtext("Men", 3, adj = 0.25, font = 2)
# mtext("Women", 3, adj = 0.75, font = 2)
dev.off()



