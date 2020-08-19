# prework work
library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(imputeTS)

truelove_si<-read_tsv("truelove_si.txt")
truelove_oxy<-read_tsv("truelove_oxy.txt")
truelove_ice<-read_tsv("truelove_ice.txt")
truelove_sun<-read_tsv("truelove_sun.txt")

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

# oxy_raster_meta<-inner_join(oxy_raster, truelove_ice, by = "date")
oxy_raster_ice<-left_join(oxy_raster, truelove_ice, by = "date")

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

silica_raster<-crossing(tibble(date = seq(ymd("1961-11-21"), ymd("1962-08-02"), by = 3)), 
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
  scale_x_date(limits = as.Date(c("1961-09-08", "1962-09-20"))) +
  ylab("Depth (m)") +
  coord_cartesian(expand = FALSE) +
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(), 
        legend.position = c(0.947, 0.5), legend.margin=margin(0,0,0,0), legend.box.margin=margin(0,0,0,0),
        plot.margin = unit(c(0.2, 0.01, 0.01, 0.02), "cm"))
# another colorblind-friendly option: scale_fill_gradient2(low = "#D55E00", mid = "#F0E442", high = "#009E73", midpoint =
si_legend<-expression(paste("Silica\n(",mu,"M)"))
silica_grid<-ggplot(silica_raster_ice, aes(date, depth, fill = silica_uM)) +
  geom_raster() +
  scale_fill_gradient2(low = "#0072B2", mid = "#D55E00", high = "#F0E442", midpoint = 23.25, name = si_legend) +
  geom_point(data = truelove_si, aes(x = date, y = depth), size = 1) +
  geom_line(aes(x = date, y = ice_m)) +
  scale_y_reverse() +
  ylim(7, 0) +
  scale_x_date(limits = as.Date(c("1961-09-08", "1962-09-20"))) +
  ylab("Depth (m)") +
  xlab(NULL) +
  coord_cartesian(expand = FALSE) +
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(), 
        legend.position = c(0.947, 0.5), legend.margin=margin(0,0,0,0), legend.box.margin=margin(0,0,0,0), 
        plot.margin = unit(c(0.01, 0.01, 0.01, 0.02), "cm"))

sun<-ggplot(truelove_sun, aes(x = date, y = day_length)) +
  geom_area(fill = "Yellow") +
  ylim(0, 25) +
  scale_x_date(limits = as.Date(c("1961-09-08", "1962-09-20"))) +
  ylab("Day Length (h)") +
  xlab("Date") +
  coord_cartesian(expand = FALSE) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
        plot.margin = unit(c(0.01, 0.01, 0.01, 0.02), "cm"))

pdf("Immerk_Si_O2.pdf", height = 8, width = 7)
ggarrange(oxy_grid, silica_grid, sun, ncol = 1, nrow = 3, 
          align = "v", heights = c(1, 1, 0.5))
dev.off()

## Si and O2 correlations
si.o2<-left_join(truelove_oxy, truelove_si, by = c("date" = "date", "depth" = "depth")) %>%
  drop_na(.)
si.o2<-transform(si.o2, depth = as.character(depth))


label_o2 = expression(paste("Dissolved Oxygen (ml L"^"-1"))
label_si = expression(paste("Silica (",mu,"M)"))

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
