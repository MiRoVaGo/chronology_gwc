#Required Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(reshape2)
library(tidyverse)
#Load Data
gwc_data <- read_csv('gwc_chronology.csv')
gwc_data$Author <- factor(gwc_data$Author, levels = gwc_data$Author)
#Bar Plots
p01 <- ggplot(data = gwc_data, aes(x = Author, y = P_L)) +
  geom_bar(stat="identity") +
  labs(x = NULL, y = expression(paste("Precipitation over land in ", 10^3, km^3, "/", year))) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 150), expand = c(0, 0)) +
  theme(legend.position = c(0.5, 0.92), legend.text = element_text(size = 16), legend.direction = "horizontal", panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 18), axis.text.x = element_text(angle = 90, size = 10, hjust = 1, vjust = 0.5), axis.ticks.x = element_blank())

p02 <- ggplot(data = gwc_data, aes(x = Author, y = ET)) +
  geom_bar(stat="identity") +
  labs(x = NULL, y = expression(paste("Evapotranspiration in ", 10^3, km^3, "/", year))) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  theme(legend.position = c(0.5, 0.92), legend.text = element_text(size = 16), legend.direction = "horizontal", panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 18), axis.text.x = element_text(angle = 90, size = 10, hjust = 1, vjust = 0.5), axis.ticks.x = element_blank())

p03 <- ggplot(data = gwc_data, aes(x = Author, y = Q)) +
  geom_bar(stat="identity") +
  labs(x = NULL, y = expression(paste("Runoff in ", 10^3, km^3, "/", year))) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  theme(legend.position = c(0.5, 0.92), legend.text = element_text(size = 16), legend.direction = "horizontal", panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 18), axis.text.x = element_text(angle = 90, size = 10, hjust = 1, vjust = 0.5), axis.ticks.x = element_blank())

p04 <- ggplot(data = gwc_data, aes(x = Author, y = P_O)) +
  geom_bar(stat="identity") +
  labs(x = NULL, y = expression(paste("Precipitation over the ocean in ", 10^3, km^3, "/", year))) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 500), expand = c(0, 0)) +
  theme(legend.position = c(0.5, 0.92), legend.text = element_text(size = 16), legend.direction = "horizontal", panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 18), axis.text.x = element_text(angle = 90, size = 10, hjust = 1, vjust = 0.5), axis.ticks.x = element_blank())

p05 <- ggplot(data = gwc_data, aes(x = Author, y = E)) +
  geom_bar(stat="identity") +
  labs(x = NULL, y = expression(paste("Evaporation over the ocean in ", 10^3, km^3, "/", year))) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 550), expand = c(0, 0)) +
  theme(legend.position = c(0.5, 0.92), legend.text = element_text(size = 16), legend.direction = "horizontal", panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 18), axis.text.x = element_text(angle = 90, size = 10, hjust = 1, vjust = 0.5), axis.ticks.x = element_blank())

p06 <- ggplot(data = gwc_data, aes(x = Author, y = P_TOT)) +
  geom_bar(stat="identity") +
  labs(x = NULL, y = expression(paste("Global Precipitation in ", 10^3, km^3, "/", year))) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 600), expand = c(0, 0)) +
  theme(legend.position = c(0.5, 0.92), legend.text = element_text(size = 16), legend.direction = "horizontal", panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 18), axis.text.x = element_text(angle = 90, size = 10, hjust = 1, vjust = 0.5), axis.ticks.x = element_blank())

p07 <- ggplot(data = gwc_data, aes(x = Author, y = E_TOT)) +
  geom_bar(stat="identity") +
  labs(x = NULL, y = expression(paste("Global Evapotranspiration in ", 10^3, km^3, "/", year))) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 600), expand = c(0, 0)) +
  theme(legend.position = c(0.5, 0.92), legend.text = element_text(size = 16), legend.direction = "horizontal", panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 18), axis.text.x = element_text(angle = 90, size = 10, hjust = 1, vjust = 0.5), axis.ticks.x = element_blank())

#Box Plot
gwc_means <- aggregate(value ~  variable, gwc_data[,1:6] %>% melt() %>% na.omit(), mean)

p08 <- ggplot(data = gwc_data[,1:6] %>% melt() %>% na.omit(), aes(x = variable, y = value)) +
  geom_boxplot(fill='#A4A4A4', color="black") +
  stat_summary(aes(color = "black"), fun = mean, geom = "point", color = "darkred", shape = 18, size = 3) +
  geom_text(data = gwc_means, aes(label = round(value, 1), y = value + 107, size =14)) +
  labs(x = NULL, y = expression(paste("Flux in ", 10^3, km^3, "/", year))) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 550), expand = c(0, 0)) +
  theme(legend.position = "none", panel.grid.major.x = element_blank(), axis.text = element_text(size = 16), axis.title = element_text(size = 18))

#Point Plots
land_data <- gwc_data[,1:4] %>% rename(P = P_L) %>% melt() %>% na.omit()
ocean_data <- gwc_data[,c(1,5:6)] %>% rename(P = P_O) %>% melt()
gwc_data$idx <- gwc_data$ET/gwc_data$P_L

p09 <- ggplot(data = land_data, aes(x = Author, y = value, color = variable, group = variable)) +
  geom_line(size = 2, alpha = 0.75) + geom_point(aes(fill = variable), shape = 21, size = 2, stroke = 1, show.legend = FALSE) + 
  labs(color = NULL, x = NULL, y = expression(paste("Land fluxes in ", 10^3, km^3, "/", year))) +
  theme_bw() +
  scale_color_manual(values=c("#1f78b4", "#33a02c", "#a6cee3")) +
  scale_fill_manual(values=c("#1f78b4", "#33a02c", "#a6cee3")) +
  scale_y_continuous(limits = c(0, 150), expand = c(0, 0)) +
  theme(legend.position = c(0.5, 0.92), legend.text = element_text(size = 16), legend.direction = "horizontal", panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 18), axis.text.x = element_text(angle = 90, size = 10, hjust = 1, vjust = 0.5), axis.ticks.x = element_blank())

p10 <- ggplot(data = ocean_data, aes(x = Author, y = value, color = variable, group = variable)) +
  geom_point(aes(fill = variable), shape = 21, size = 2, stroke = 1, show.legend = FALSE) + 
  geom_line(data = ocean_data %>% subset(!is.na(value)), size = 2, alpha = 0.75) + geom_point(aes(fill = variable), shape = 21, size = 2, stroke = 1, show.legend = FALSE) +
  labs(color = NULL, x = NULL, y = expression(paste("Ocean fluxes in ", 10^3, km^3, "/", year))) +
  theme_bw() +
  scale_color_manual(values=c("#1f78b4", "#33a02c")) +
  scale_fill_manual(values=c("#1f78b4", "#33a02c", "#a6cee3")) +
  scale_y_continuous(limits = c(200, 550), expand = c(0, 0)) +
  theme(legend.position = c(0.5, 0.92), legend.text = element_text(size = 16), legend.direction = "horizontal", panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 18), axis.text.x = element_text(angle = 90, size = 10, hjust = 1, vjust = 0.5), axis.ticks.x = element_blank())

p11 <- ggplot(data = gwc_data, aes(x = Author, y = idx, group = 1, color = "#984ea3", fill = "#984ea3")) +
  geom_point(shape = 21, size = 2, stroke = 1, show.legend = FALSE) + 
  geom_line(data = gwc_data[-37,], size = 2, alpha = 0.75) + geom_point(shape = 21, size = 2, stroke = 1, show.legend = FALSE) +
  labs(color = NULL, fill = NULL, x = NULL, y = "Evaporative Index (ET/P)") +
  theme_bw() +
  scale_color_manual(values=c("#984ea3", "#984ea3")) +
  scale_fill_manual(values=c("#984ea3", "#984ea3", "#984ea3")) +
  scale_y_continuous(limits = c(0.5, 0.8), expand = c(0, 0)) +
  theme(legend.position = "none", panel.grid.major.x = element_blank(), axis.title.y = element_text(size = 18), axis.text.y = element_text(size = 16), axis.text.x = element_text(angle = 90, size = 10, hjust = 1, vjust = 0.5), axis.ticks.x = element_blank())

#Density Plots
mean_p <- mean(gwc_data$P_L, na.rm = TRUE)
mean_e <- mean(gwc_data$ET, na.rm = TRUE)
mean_q <- mean(gwc_data$Q, na.rm = TRUE)
mean_po <- mean(gwc_data$P_O, na.rm = TRUE)
mean_eo <- mean(gwc_data$E, na.rm = TRUE)
mean_idx <- mean(gwc_data$idx, na.rm = TRUE)

p12 <- ggplot(data = land_data, aes(x = value, group = variable, fill = variable)) +
  geom_density(alpha = 0.75) +
  geom_vline(aes(xintercept = mean_p), color="black", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = mean_e), color="black", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = mean_q), color="black", linetype="dashed", size=1) +
  labs(fill = NULL, y = "Density", x = expression(paste("Fluxes over land in ", 10^3, km^3, "/", year))) +
  theme_bw() +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#a6cee3", "#31A9B8", "#486B00", "#258039", "#A2C523")) +
  scale_y_continuous(limits = c(0, 0.15), expand = c(0, 0)) +
  scale_x_continuous(limits = c(23.9, 126), expand = c(0, 0)) +
  theme(legend.position = c(0.5, 0.94), legend.text = element_text(size = 16), legend.direction = "horizontal", axis.text.y = element_text(size = 16), axis.text.x = element_text(size = 16), axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18))

p13 <- ggplot(data = ocean_data, aes(x = value, group = variable, fill = variable)) +
  geom_density(alpha = 0.75) +
  geom_vline(aes(xintercept = mean_po), color="black", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = mean_eo), color="black", linetype="dashed", size=1) +
  labs(fill = NULL, y = "Density", x = expression(paste("Fluxes over the ocean in ", 10^3, km^3, "/", year))) +
  theme_bw() +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#a6cee3", "#31A9B8", "#486B00", "#258039", "#A2C523")) +
  scale_y_continuous(limits = c(0, 0.035), expand = c(0, 0)) +
  scale_x_continuous(limits = c(242, 505), expand = c(0, 0)) +
  theme(legend.position = c(0.65, 0.97), legend.text = element_text(size = 16), legend.direction = "horizontal", axis.text.y = element_text(size = 16), axis.text.x = element_text(size = 16), axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18))

p14 <- ggplot(data = gwc_data[,"idx"], aes(x = idx, fill = idx)) +
  geom_density(alpha = 0.75, fill =  "#984ea3")+
  geom_vline(aes(xintercept = mean_idx), color="black", linetype="dashed", size=1) +
  labs(fill = NULL, y = "Density", x = "Evaporative Index (ET/P)") +
  theme_bw() +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(limits = c(0, 9), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0.5, 0.8), expand = c(0, 0)) +
  theme(legend.position = c(0.5, 0.94), legend.text = element_text(size = 16), legend.direction = "horizontal", axis.text.y = element_text(size = 16), axis.text.x = element_text(size = 16), axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18))

#Density Plots From Baumgartner and Reichel (1972) onwards
land_data_2 <- gwc_data[c(24:47),1:4] %>% rename(P = P_L) %>% melt() %>% na.omit()
ocean_data_2 <- gwc_data[c(24:47),c(1,5:6)] %>% rename(P = P_O) %>% melt() %>% na.omit()
mean_p_2 <- mean(land_data_2$value[land_data_2$variable == "P"])
mean_e_2 <- mean(land_data_2$value[land_data_2$variable == "ET"])
mean_q_2 <- mean(land_data_2$value[land_data_2$variable == "Q"])
mean_po_2 <- mean(ocean_data_2$value[ocean_data_2$variable == "P"])
mean_eo_2 <- mean(ocean_data_2$value[ocean_data_2$variable == "E"])

p15 <- ggplot(data = land_data_2, aes(x = value, group = variable, fill = variable)) +
  geom_density(alpha = 0.75) +
  geom_vline(aes(xintercept = mean_p_2), color="black", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = mean_e_2), color="black", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = mean_q_2), color="black", linetype="dashed", size=1) +
  labs(fill = NULL, y = "Density", x = expression(paste("Fluxes over land in ", 10^3, km^3, "/", year))) +
  theme_bw() +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#a6cee3", "#31A9B8", "#486B00", "#258039", "#A2C523")) +
  scale_y_continuous(limits = c(0, 0.15), expand = c(0, 0)) +
  scale_x_continuous(limits = c(23.9, 126), expand = c(0, 0)) +
  theme(legend.position = c(0.5, 0.94), legend.text = element_text(size = 16), legend.direction = "horizontal", axis.text.y = element_text(size = 16), axis.text.x = element_text(size = 16), axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18))

p16 <- ggplot(data = ocean_data_2, aes(x = value, group = variable, fill = variable)) +
  geom_density(alpha = 0.75) +
  geom_vline(aes(xintercept = mean_po_2), color="black", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = mean_eo_2), color="black", linetype="dashed", size=1) +
  labs(fill = NULL, y = "Density", x = expression(paste("Fluxes over the ocean in ", 10^3, km^3, "/", year))) +
  theme_bw() +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#a6cee3", "#31A9B8", "#486B00", "#258039", "#A2C523")) +
  scale_y_continuous(limits = c(0, 0.035), expand = c(0, 0)) +
  scale_x_continuous(limits = c(242, 505), expand = c(0, 0)) +
  theme(legend.position = c(0.65, 0.97), legend.text = element_text(size = 16), legend.direction = "horizontal", axis.text.y = element_text(size = 16), axis.text.x = element_text(size = 16), axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18))
