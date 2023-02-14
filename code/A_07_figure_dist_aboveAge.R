# LCA analysis to determine death types
source("code/A_00_packages_functions.R")

# Load data and merge predicted class
var_types <- c("n", "n", "n", "n", "c", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", 
               "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", 
               "n", "n", "c")

dat <- 
  read_delim("C:/deathPathways/data/oneYearPD_data_20210914.txt", delim = " ", 
             col_types = as.list(var_types))

# Load MVO add on 
mvo <- 
  read_delim("C:/deathPathways/data/mvo_new_20220216.txt", delim = " ") %>%
  rename(care_clinic = care)

load("data_inter/lopnr_predClass.RData")

dat <- 
  dat %>%
  left_join(mvo) %>% 
  left_join(lop_class)


agg_death <- 
  dat %>% 
  filter(Age >= 70 & DeathYear >= 2018 & !is.na(type)) %>% 
  mutate(Age = as.integer(Age)) %>% 
  count(Age, sex, type) 

# Above Age 70
agg_70 <-
  agg_death %>% 
  group_by(sex, type) %>% 
  summarise(n_70 = sum(n)) %>%
  group_by(sex) %>% 
  mutate(tot_70 = sum(n_70),
         p_70 = n_70 / tot_70)

# Above LE M: 81, W: 85
agg_LE <-
  agg_death %>%
  filter((sex == 1 & Age > 80) |
           (sex == 2 & Age > 84)) %>% 
  group_by(sex, type) %>% 
  summarise(n_LE = sum(n)) %>%
  group_by(sex) %>% 
  mutate(tot_LE = sum(n_LE),
         p_LE = n_LE / tot_LE)

# Above Age 90
agg_90 <-
  agg_death %>%
  filter(Age > 90) %>% 
  group_by(sex, type) %>% 
  summarise(n_90 = sum(n)) %>%
  group_by(sex) %>% 
  mutate(tot_90 = sum(n_90),
         p_90 = n_90 / tot_90)


# merge datasets
agg_plot <- 
  agg_90 %>% 
  left_join(agg_70) %>% 
  left_join(agg_LE)

# Plot results
col_type <- adjustcolor(c('#a6cee3','#1f78b4','#b2df8a',
                          '#33a02c','#fb9a99','#e31a1c'), 
                        alpha.f = 0.8)

names(col_type) <- 1:6

type_names <- c("Terminal ill", "Sudden death", "Impaired severe progression",
                "Impaired", "Dependent severe progression", "Depedent")

names(type_names) <- 1:6


dev.off()
pdf("figures/relative_dist_aboveAge.pdf", 
    width = 10, pointsize = 12)
plot(x=1, y=1, typ = "n", bty = "n", xaxt = "n", yaxt = "n", xaxs = "i",
     yaxs = "i", xlab = NA, ylab = NA, xlim = c(0, 8), ylim = c(0, 1.1))

plot_fct <- function(s_x=0,e_x=2){
yy <- seq(0,1, by=0.2)
segments(x0=e_x, y0=yy, x1=s_x, y1=yy, 
         col = "lightgray")
segments(x0=s_x:e_x, y0=0, x1=s_x:e_x, y1=1, 
         col = "lightgray")
text(x=s_x + 0.5, y = 1, "Women", pos = 3, cex = 1.2)
text(x=s_x + 1.5, y = 1, "Men", pos = 3, cex = 1.2)

}

# Above Age 70
plot_fct(s_x=0,e_x=2)

men <- 
  agg_plot %>%
  filter(sex == 1)
p_cum_men <- cumsum(men$p_70)

women <- 
  agg_plot %>%
  filter(sex == 2)
p_cum_women <- cumsum(women$p_70)

xx_w <- c(0.2, 0.2, 0.8, 0.8)
xx_m <- c(0.2, 0.2, 0.8, 0.8)+1

for(i in 1:6){
if(i == 1){
  yy_w <- c(0, rep(p_cum_women[i],2), 0)
  yy_m <- c(0, rep(p_cum_men[i],2), 0)
}else{
  yy_w <- c(p_cum_women[i-1], rep(p_cum_women[i],2), 
            p_cum_women[i-1])
  yy_m <- c(p_cum_men[i-1], rep(p_cum_men[i],2), 
            p_cum_men[i-1])
}  
polygon(x=xx_w, y=yy_w, col = col_type[i], 
        border = "white")
polygon(x=xx_m, y=yy_m, col = col_type[i], 
        border = "white")
}

# Above LE
s_x <- 3
s_e <- 5
plot_fct(s_x=3,e_x=5)

men <- 
  agg_plot %>%
  filter(sex == 1)
p_cum_men <- cumsum(men$p_LE)

women <- 
  agg_plot %>%
  filter(sex == 2)
p_cum_women <- cumsum(women$p_LE)

xx_w <- c(0.2, 0.2, 0.8, 0.8)+s_x
xx_m <- c(0.2, 0.2, 0.8, 0.8)+s_e-1

for(i in 1:6){
  if(i == 1){
    yy_w <- c(0, rep(p_cum_women[i],2), 0)
    yy_m <- c(0, rep(p_cum_men[i],2), 0)
  }else{
    yy_w <- c(p_cum_women[i-1], rep(p_cum_women[i],2), 
              p_cum_women[i-1])
    yy_m <- c(p_cum_men[i-1], rep(p_cum_men[i],2), 
              p_cum_men[i-1])
  }  
  polygon(x=xx_w, y=yy_w, col = col_type[i], 
          border = "white")
  polygon(x=xx_m, y=yy_m, col = col_type[i], 
          border = "white")
}

# Age 90
s_x <- 6
s_e <- 8
plot_fct(s_x=6,e_x=8)

men <- 
  agg_plot %>%
  filter(sex == 1)
p_cum_men <- cumsum(men$p_90)

women <- 
  agg_plot %>%
  filter(sex == 2)
p_cum_women <- cumsum(women$p_90)

xx_w <- c(0.2, 0.2, 0.8, 0.8)+s_x
xx_m <- c(0.2, 0.2, 0.8, 0.8)+s_e-1

for(i in 1:6){
  if(i == 1){
    yy_w <- c(0, rep(p_cum_women[i],2), 0)
    yy_m <- c(0, rep(p_cum_men[i],2), 0)
  }else{
    yy_w <- c(p_cum_women[i-1], rep(p_cum_women[i],2), 
              p_cum_women[i-1])
    yy_m <- c(p_cum_men[i-1], rep(p_cum_men[i],2), 
              p_cum_men[i-1])
  }  
  polygon(x=xx_w, y=yy_w, col = col_type[i], 
          border = "white")
  polygon(x=xx_m, y=yy_m, col = col_type[i], 
          border = "white")
}

# Text 
par(xpd = TRUE)
text(x=1, y=1.1, "Above Age 70", pos = 3, cex = 1.3)
text(x=4, y=1.1, "Above\nLife Expectancy", pos = 3, cex = 1.3)
text(x=7, y=1.1, "Above Age 90", pos = 3, cex = 1.3)

lines(y=-0.1, x = 0, pch = 15, col = col_type[1], typ = "p", cex = 1.3)
text(y=-0.1, x = 0, pos = 4, type_names[1], cex = 1.2)

lines(y=-0.15, x = 0, pch = 15, col = col_type[2], typ = "p", cex = 1.3)
text(y=-0.15, x = 0, pos = 4, type_names[2], cex = 1.2)

lines(y=-0.1, x = 2.5, pch = 15, col = col_type[3], typ = "p", cex = 1.3)
text(y=-0.1, x = 2.5, pos = 4, type_names[3], cex = 1.2)

lines(y=-0.15, x = 2.5, pch = 15, col = col_type[4], typ = "p", cex = 1.3)
text(y=-0.15, x = 2.5, pos = 4, type_names[4], cex = 1.2)

lines(y=-0.1, x = 5, pch = 15, col = col_type[5], typ = "p", cex = 1.3)
text(y=-0.1, x = 5, pos = 4, type_names[5], cex = 1.2)

lines(y=-0.15, x = 5, pch = 15, col = col_type[6], typ = "p", cex = 1.3)
text(y=-0.15, x = 5, pos = 4, type_names[6], cex = 1.2)

par(las = 1)
axis(2, at = yy, labels = paste(round(yy*100, 0), "%"), lwd = 0,
     pos = 0.1, cex.axis = 1.2)
axis(2, at = yy, labels = paste(round(yy*100, 0), "%"), lwd = 0,
     pos = 3.1, cex.axis = 1.2)
axis(2, at = yy, labels = paste(round(yy*100, 0), "%"), lwd = 0,
     pos = 6.1, cex.axis = 1.2)
dev.off()

