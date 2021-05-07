# prework work
library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(imputeTS)
library(emmeans)

truelove_si<-read_tsv("truelove_si.txt")
truelove_oxy<-read_tsv("truelove_oxy.txt")
truelove_ice<-read_tsv("truelove_ice.txt")
truelove_sun<-read_tsv("truelove_sun.txt")
truelove_summer<-read_tsv("truelove_summer.txt")
ca_db <- read_tsv("ca_lake_db.txt")

## for oxygen, this all works

estimate_oxy_by_date <- function(target_date, target_depth) {
  data_for_date <- truelove_oxy %>%
    filter(date == target_date) %>%
    arrange(depth)
  approx(data_for_date$depth, data_for_date$oxygen_mL, xout = target_depth)$y
}

estimate_oxy_by_depth <- function(target_depth, target_date) {
  data_for_depth <- oxy_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$oxygen_mL, xout = target_date)$y
}

oxy_interp_depth<-crossing(tibble(date = unique(truelove_oxy$date)), 
                           tibble(depth = seq(1, 6, length.out = 24))) %>%
  group_by(date) %>%
  mutate(oxygen_mL = estimate_oxy_by_date(date[1], depth))

oxy_raster <- crossing(tibble(date = seq(ymd("1961-09-10"), ymd("1962-06-17"), by=3)),
                       tibble(depth = unique(oxy_interp_depth$depth))) %>%
  filter(depth >= 1) %>%
  group_by(depth) %>%
  mutate(oxygen_mL = estimate_oxy_by_depth(depth[3], date))

# for ice interpolation to match oxy and silica raster dates every 3 days
truelove_ice<-na_interpolation(truelove_ice)

## inner join this once we have more ice thickness data
# oxy_raster_meta<-inner_join(oxy_raster, truelove_ice, by = "date")
oxy_raster_ice<-left_join(oxy_raster, truelove_ice, by = "date")

# theme(axis.line = element_line(colour = "black"),
  #panel.grid.major = element_blank(),
  #panel.grid.minor = element_blank(),
  #panel.border = element_blank(),
  #panel.background = element_blank(),
  #legend.position = c(0.2, 0.2), legend.key=element_blank())
#is the same as theme_classic() with legend position and removal of grey boxes 
#around the legend symbols

## For silica
estimate_silica_by_date <- function(target_date, target_depth) {
  data_for_date <- truelove_si %>%
    filter(date == target_date) %>%
    arrange(depth)
  approx(data_for_date$depth, data_for_date$silica_uM, xout = target_depth)$y
}

estimate_silica_by_depth <- function(target_depth, target_date) {
  data_for_depth <- silica_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$silica_uM, xout = target_date)$y
}

silica_interp_depth<-crossing(tibble(date = unique(truelove_si$date)), 
                              tibble(depth = seq(1, 6, length.out = 24))) %>%
  group_by(date) %>%
  mutate(silica_uM = estimate_silica_by_date(date[1], depth))

silica_raster<-crossing(tibble(date = seq(ymd("1961-11-21"), ymd("1962-08-12"), by = 3)), 
                        tibble(depth = unique(silica_interp_depth$depth))) %>%
  group_by(depth) %>%
  mutate(silica_uM = estimate_silica_by_depth(depth[3], date))
silica_raster_ice<-left_join(silica_raster, truelove_ice, by ="date")

## Plotting
oxy_legend<-expression(paste("Oxygen (mL ",L^-1,")"))
oxy_grid<-ggplot(oxy_raster_ice, aes(date, depth, fill = oxygen_mL)) +
  geom_raster() +
  scale_fill_gradient2(low = "#0072B2", mid = "#D55E00", high = "#F0E442", midpoint = 7.25, name = "Oxygen\n(mL/L)") +
  geom_point(data = truelove_oxy, aes(x = date, y = depth), size = 1) +
  geom_line(aes(x = date, y = ice_m)) +
  scale_y_reverse() +
  ylim(7, 0) +
  xlab(NULL) +
  scale_x_date(limits = as.Date(c("1961-09-08", "1962-09-30"))) +
  ylab("Depth (m)") +
  coord_cartesian(expand = FALSE) +
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(), 
        legend.position = c(0.947, 0.48), legend.margin=margin(0,0,0,0), legend.box.margin=margin(0,0,0,0),
        plot.margin = unit(c(0.2, 0.01, 0.01, 0.02), "cm"), text = element_text(size = 15, color = "black"))
# another colorblind-friendly option: scale_fill_gradient2(low = "#D55E00", mid = "#F0E442", high = "#009E73", midpoint =
si_legend<-expression(paste("Silica\n(",mu,"M)"))
silica_grid<-ggplot(silica_raster_ice, aes(date, depth, fill = silica_uM)) +
  geom_raster() +
  scale_fill_gradient2(low = "#0072B2", mid = "#D55E00", high = "#F0E442", midpoint = 23.25, name = si_legend) +
  geom_point(data = truelove_si, aes(x = date, y = depth), size = 1) +
  geom_line(aes(x = date, y = ice_m)) +
  scale_y_reverse() +
  ylim(7, 0) +
  scale_x_date(limits = as.Date(c("1961-09-08", "1962-09-30"))) +
  ylab("Depth (m)") +
  xlab(NULL) +
  coord_cartesian(expand = FALSE) +
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(), 
        legend.position = c(0.947, 0.5), legend.margin=margin(0,0,0,0), legend.box.margin=margin(0,0,0,0), 
        plot.margin = unit(c(0.01, 0.01, 0.01, 0.02), "cm"), text = element_text(size = 15, color = "black"))

sun<-ggplot(truelove_sun, aes(x = date, y = day_length)) +
  geom_area(fill = "Yellow") +
  ylim(0, 25) +
  scale_x_date(limits = as.Date(c("1961-09-08", "1962-09-30"))) +
  ylab("Day Length (h)") +
  xlab("Date") +
  coord_cartesian(expand = FALSE) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
        plot.margin = unit(c(0.01, 0.01, 0.01, 0.02), "cm"), text = element_text(size = 15, color = "black"))

pdf("Immerk_Si_O2_sun.pdf", height = 8, width = 7)
ggarrange(oxy_grid, silica_grid, sun, ncol = 1, nrow = 3, 
          align = "v", heights = c(1, 1, 0.5), labels = "AUTO", label.x = c(0.96,0.96,0.96), label.y = c(0.98,0.98,0.98))
dev.off()

## Si and O2 correlations
si.o2<-left_join(truelove_oxy, truelove_si, by = c("date" = "date", "depth" = "depth")) %>%
  drop_na(.)
si.o2<-transform(si.o2, depth = as.character(depth))


label_o2 = expression(paste("Dissolved Oxygen (ml L"^"-1"))
label_si = expression(paste("Silicate (",mu,"M)"))

my.formula<-y~x

pdf("Immerk_Si_O2_corr.pdf", height = 4, width = 6)

ggplot(si.o2, aes(x=oxygen_mL, y=silica_uM)) +
stat_smooth(method = lm, formula = y~x) +
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE,
label.x = 0.7, label.y = 0.9) +
geom_point(aes(color=depth), size = 3) +
scale_color_manual(values = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"), name = "Depth (m)") +
xlab(label_o2) +
ylab(label_si) +
theme_classic()

dev.off()

ca_db <- read_tsv("ca_lake_db.txt")
summary(ca_db$o2_rate)
mean(ca_db$o2_rate)
sd(ca_db$o2_rate)

#Summer only data

pdf("Immerk_Si_summer.pdf", height = 4, width = 6)
ggplot(truelove_summer, aes(x=Si, y=depth)) +
  geom_point(aes(color=date), size = 3) +
  geom_path(aes(color=date), linetype = 2) +
  #scale_color_manual(values = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"), name = "Date") +
  scale_color_manual(name=NULL,
                     values=c("Black", "grey40", "blue", "green", "green4"),
                     breaks=c("5June", "17June", "18July", "2August", "21August"),
                     labels=c("5Jun1962", "17Jun1962", "18Jul1962", "2Aug1962", "21Aug1962")) +
  xlim(c(0,50)) +
  scale_x_continuous(position = "top", limits = c(0,40)) +
  xlab(label_si) +
  ylab("Depth (m)") +
  ylim(c(7,0)) +
  theme_classic()

dev.off()

## Si and O2 correlations
si.o2<-left_join(truelove_oxy, truelove_si, by = c("date" = "date", "depth" = "depth")) %>%
  drop_na(.)
si.o2<-transform(si.o2, depth = as.character(depth))


label_o2 = expression(paste("Dissolved Oxygen (ml L"^"-1"))
label_si = expression(paste("Silicate (",mu,"M)"))

my.formula<-y~x

pdf("Immerk_Si_O2_corr.pdf", height = 4, width = 6)

ggplot(si.o2, aes(x=oxygen_mL, y=silica_uM)) +
  stat_smooth(method = lm, formula = y~x) +
  stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE,
               label.x = 0.7, label.y = 0.9) +
  geom_point(aes(color=depth), size = 3) +
  scale_color_manual(values = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"), name = "Depth (m)") +
  xlab(label_o2) +
  ylab(label_si) +
  theme_classic()

dev.off()

#Canadian Lakes database

summary(ca_db$o2_rate)
mean(ca_db$o2_rate)
sd(ca_db$o2_rate)

## Si production/consumption rates

label_time = expression(paste("Date"))
label_int_si = expression(paste("Silicate (mmoles ", m^2,")"))
label_int_oxy = expression(paste("Oxygen (L ", m^2,")"))

my.formula<-y~x

si_int <- ggplot(truelove_int, aes(x=date)) +
  geom_point(aes(y=total_si), size = 3, shape = 16) +
  stat_smooth(data = truelove_int %>% filter(date < '1962-05-09'), method = lm, formula = my.formula, aes(y = total_si), color = "#0072B2") +
  stat_poly_eq(formula = my.formula, data = truelove_int %>% filter(date < '1962-05-09'),
               aes(y = total_si, label = paste(..eq.label.., sep = "~~~")), parse = TRUE, label.x = 0.15, label.y = 0.7) +
  stat_cor(data = truelove_int %>% filter(date < '1962-05-09'), 
           aes(y = total_si, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = -3000, label.y = 95) +
  
  geom_point(aes(y=total_si), size = 3, shape = 16) +
  stat_smooth(data = truelove_int %>% filter(date > '1962-05-09'), method = lm, formula = my.formula, aes(y = total_si), color = "#eb071e") +
  stat_poly_eq(formula = my.formula, data = truelove_int %>% filter(date > '1962-05-09'),
               aes(y = total_si, label = paste(..eq.label.., sep = "~~~")), parse = TRUE, label.x = 0.78, label.y = 0.5) +
  stat_cor(data = truelove_int %>% filter(date > '1962-05-09'), 
           aes(y = total_si, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = -2850, label.y = 65) +
  ylim(c(0,150)) +
  xlab(NULL) +
  ylab(label_int_si) +
  xlab(label_time) +
  geom_vline(xintercept = as.numeric(ymd("1962-06-16")), linetype = 4, color = "black") +
  theme(axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_blank(), panel.background = element_blank(), 
  axis.title = element_text(size=14, color = "black"), axis.text = element_text(size=14, color = "black"),
  legend.position = "none")

total_oxy_int <- ggplot(truelove_int, aes(x=date)) +
  geom_point(aes(y=total_oxy), size = 3, shape = 17) +
  geom_smooth(aes(y=total_oxy), color = "black", data = truelove_int %>% filter(!date == '1962-06-17')) +
  scale_color_manual(values = c("#78B7C5")) +
  ylim(c(5,40)) +
  ylab(label_int_oxy) +
  xlab(NULL) +
  geom_vline(xintercept = as.numeric(ymd("1962-06-16")), linetype = 4, color = "black") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(), 
        axis.title = element_text(size=14, color = "black"), axis.text = element_text(size=14, color = "black"))

oxy_int <- ggplot(truelove_int, aes(x=date)) +
  geom_point(aes(y=top1_oxy), size = 3, shape = 16) +
  stat_smooth(method = lm, formula = my.formula, aes(y = top1_oxy), color = "#F0E442") +
  stat_poly_eq(formula = my.formula, aes(y = top1_oxy, label = paste(..eq.label.., sep = "~~~")), parse = TRUE, label.x = 0.1, label.y = 0.999) +
  stat_cor(aes(y = top1_oxy, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = -3015, label.y = 11.6) +
  
  geom_point(aes(y=surface_oxy), size = 5, shape = 18) +
  stat_smooth(data = truelove_int %>% filter(date < '1962-03-05'), method = lm, formula = my.formula, aes(y = surface_oxy), color = "#D55E00") +
  stat_poly_eq(formula = my.formula, data = truelove_int %>% filter(date < '1962-03-05'),
               aes(y = surface_oxy, label = paste(..eq.label.., sep = "~~~")), parse = TRUE, label.x = 0.64, label.y = 0.88) +
  stat_cor(data = truelove_int %>% filter(date < '1962-03-05'), 
           aes(y = surface_oxy, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = -2910, label.y = 9.9) +
  
  geom_point(aes(y=surface_oxy), size = 5, shape = 18) +
  stat_smooth(data = truelove_int %>% filter(date > '1962-03-04' & date < '1962-06-16'), method = lm, formula = my.formula, aes(y = surface_oxy), color = "#D55E00") +
  stat_poly_eq(formula = my.formula, data = truelove_int %>% filter(date > '1962-03-04' & date < '1962-06-16'),
               aes(y = surface_oxy, label = paste(..eq.label.., sep = "~~~")), parse = TRUE, label.x = 0.79, label.y = 0.75) +
  stat_cor(data = truelove_int %>% filter(date > '1962-03-04' & date < '1962-06-16'), 
           aes(y = surface_oxy, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = -2850, label.y = 8.25) +
  
  geom_point(aes(y=deep_oxy), size = 3, shape = 15, data = truelove_int %>% filter(date < '1962-03-05')) +
  stat_smooth(method = lm, formula = my.formula, aes(y = deep_oxy), data = truelove_int %>% filter(date < '1962-03-05'), color = "#eb071e") +
  stat_poly_eq(data = truelove_int %>% filter(date < '1962-03-05'), formula = my.formula, 
               aes(y = deep_oxy, label = paste(..eq.label.., sep = "~~~")), parse = TRUE, label.x = 0.23, label.y = 0.34) +
  stat_cor(data = truelove_int %>% filter(date < '1962-03-05'), 
           aes(y = deep_oxy, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = -2970, label.y = 3.5) +
  
  geom_point(aes(y=deep_oxy), size = 3, shape = 15) +
  stat_smooth(method = lm, formula = my.formula, aes(y = deep_oxy), data = truelove_int %>% filter(date > '1962-03-04' & date < '1962-06-16'), color = "#eb071e") +
  stat_poly_eq(data = truelove_int %>% filter(date > '1962-03-04' & date < '1962-06-16'), formula = my.formula, 
               aes(y = deep_oxy, label = paste(..eq.label.., sep = "~~~")), parse = TRUE, label.x = 0.79, label.y = 0.25) +
  stat_cor(data = truelove_int %>% filter(date > '1962-03-04' & date < '1962-06-16'), 
           aes(y = deep_oxy, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = -2855, label.y = 2.3) +
  scale_color_manual(values = c("#78B7C5")) +
  ylim(c(0,12)) +
  ylab(label_int_oxy) +
  xlab(NULL) +
  geom_vline(xintercept = as.numeric(ymd("1962-06-16")), linetype = 4, color = "black") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(), 
        axis.title = element_text(size=14, color = "black"), axis.text = element_text(size=14, color = "black"))

pdf("Immerk_rates.pdf", height = 11, width = 7)
ggarrange(total_oxy_int, oxy_int, si_int, ncol = 1, nrow = 3, align = c("v"), labels = "AUTO", label.x = c(0.96,0.96,0.96), label.y = c(0.98,0.98,0.98))
dev.off()

y_surface_early <- filter(truelove_int, date < '1962-03-04')$surface_oxy
y_surface_late <- filter(truelove_int, date > '1962-03-04' & date < '1962-06-16')$surface_oxy
y_deep_early <- filter(truelove_int, date < '1962-03-04')$deep_oxy
y_deep_late <- filter(truelove_int, date > '1962-03-04' & date < '1962-06-16')$deep_oxy
date_total <- filter(truelove_int, date < '1962-06-06')$date
date_early <- filter(truelove_int, date < '1962-03-04')$date
date_late <- filter(truelove_int, date > '1962-03-04' & date < '1962-06-16')$date

surface_early <- data.frame(x=date_early,y=y_surface_early,f='surface_early')
surface_late <- data.frame(x=date_late,y=y_surface_late,f='surface_late')
deep_early <- data.frame(x=date_early,y=y_deep_early,f='deep_early')
deep_late <- data.frame(x=date_late,y=y_deep_late,f='deep_late')

surface_dat <- rbind(surface_early, surface_late)
deep_dat <- rbind(deep_early, deep_late)
early_dat <- rbind(surface_early, deep_early)
late_dat <- rbind(surface_late, deep_late)

surface_model <- lm(y~x*f,data = surface_dat)
deep_model <- lm(y~x*f,data = deep_dat)
early_model <- lm(y~x*f,data = early_dat)
late_model <- lm(y~x*f,data = late_dat)

summary(surface_model)
anova(surface_model)
summary(deep_model)
anova(deep_model)
summary(early_model)
anova(early_model)
summary(late_model)
anova(late_model)


summary(model)
anova(model)
par(mfrow = c(2,2))
plot(model)
anova(lm(y~x*f,data = dat))
ggplot(data=dat,aes(x=x,y=y,group=f))+geom_point(aes(colour=f))+
  geom_abline(intercept=-76.286,slope=-0.02822)+
  geom_abline(intercept=(-76.286-10.677), slope=(-.02822-.004515))+
  geom_abline(intercept=(-76.286+58.4774), slope=(-.02822+0.018995))+
  geom_abline(intercept=(-76.286+40.1771), slope=(-.02822+0.012826))

oxy_lines <- emtrends(model, "x", var = "f")
pairs(oxy_lines)