library(readxl)
env_data <- read_excel("data/FinalMetrics_Summary.xlsx")
View(env_data)

library(ggplot2)

env_data <- env_data |>
     mutate(Ub_pred = Ub_avrg)

env_data$WaveEnergyLevel <- as.factor(env_data$WaveEnergyLevel)

# Reorder the levels of the Reef variable
env_data$Reef <- factor(env_data$Reef, levels = c("Heron", "Davies", "Moore"))


Ub_pred<-ggplot(data=env_data, aes(y = median_speed, x = Ub_avrg, color=WaveEnergyLevel)) + 
  geom_point(aes(y = Ub_pred)) + 
  scale_color_manual(values = c("darkgreen", "lightgreen", "yellow","orange", "red")) +
  ylab("In-situ current (m/s)")+
  xlab("Predicted current (m/s)") +
  theme_bw()+
  ylim(0,0.8) +
  xlim(0,0.8) +
  facet_wrap(~Reef)

Ub_pred1<-ggplot(data=env_data, aes(y = Ub_avrg, x = WaveEnergyLevel, color=WaveEnergyLevel)) + 
  geom_point(aes(y = Ub_pred)) + 
  scale_x_discrete(limits=c("1","2","3", "4", "5"))+
  scale_color_manual(values = c("darkgreen", "lightgreen", "yellow","orange", "red")) +
  ylab("In-situ current (m/s)")+
  xlab("Predicted current (m/s)") +
  theme_bw()+
  ylim(0,0.8) +
  xlim(0,0.8) +
  facet_wrap(~Reef)

Ub_pred1



Ub_avrg<-ggplot(data=env_data, aes(y = median_speed, x = Ub_avrg, color=WaveEnergyLevel)) + 
  geom_point() +
  geom_line(aes(y=Ub_pred), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("darkgreen", "lightgreen", "yellow","orange", "red")) + 
  ylab("In-situ current (m/s)")+
  xlab("Predicted current (m/s)") +
  theme_bw()+
  ylim(0,0.8) +
  xlim(0,0.8) +
  facet_wrap(~Reef)

Ub_pred
Ub_avrg

ggsave("Ub_pred.png", plot=Ub_pred, width = 10, height = 6, dpi = 600)

ggsave("Ub_avrg.png", plot=Ub_avrg, width = 10, height = 6, dpi = 600)



ggplot(Fix, aes(x=Site, y=Meansurvival, shape=Orientation, color=Habitat))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin=Meansurvival-se, ymax=Meansurvival+se), width=0, linewidth=0.6) +
  scale_x_discrete(limits=c("Clam","Humpy","Home", "Mazie", "Shelving", "Halfway"))+
  facet_wrap(~Species, labeller = labeller(Species = new_labels))+
  scale_color_manual(values = c("#50C878", "#FF7518")) +
  theme_bw()+
  ylab("Proportion Survival")+
  theme(axis.text = element_text( angle = 90))


Ub_avrg


library(tidyr)
env_data <- env_data |>
  mutate(Ub_pred = Ub_avrg)
str(env_data)  


heron_Spp_summary <- heron_Spp_t3 |>
  filter(!is.na(SurvDev_Spp)) |>
  group_by(Reef,WaveEnergyLevel, Site,Spp) |>
  summarise(SurvivalNDevices = sum(SurvDev_Spp, na.rm=T),
              DevTot = 25,
              Perc_SurvDev = sum(SurvDev_Spp, na.rm=T)/25)

data_survival$SurvDev <- as.numeric(data_survival$SurvDev)
summary_survival_Dev <- data_survival |>
  filter(!is.na(SurvDev)) |>
  group_by(Reef, Site, WaveEnergyLevel, Census, ExpDay, Spp) |>
  summarise(SurvivalNDevices = sum(SurvDev, na.rm=T),
            DevTot =25,
            Perc_SurvDev = sum(SurvDev, na.rm =T)/25)

sum_surv <- summary_survival_Dev |>
  filter(!Reef == "Heron",
         Census == "t6")

sum_surv <- rbind(sum_surv, heron_Spp_summary)

# Reorder the levels of the Reef variable
sum_surv$Reef <- factor(sum_surv$Reef, levels = c("Moore", "Davies", "Heron"))

plot2c <- ggplot(sum_surv, aes(x = Reef, y = Perc_SurvDev, color = Spp)) +
  geom_boxplot() +
  labs(title = "Mean Survival final census (yield)",
       x = "Reef",
       y = "% Survival") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),  # Adjust font size of axis text
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))  # Adjust font size of axis titles) +


plot2c
ggsave("Fig2c - Mean survival final census Reef Spp-yield.jpeg", plot=plot2c, width = 8, height = 6, dpi = 300)

#################
str(heron_Spp_t3)

plot_env <- ggplot(environment, aes(x=Site), color = Reef) + 
  geom_col(aes(y=Ub_avrg)) +
  labs(title = "Heron Reef",
       x = "Site",
       y = "average bottom stress (m/s)") + 
  theme_minimal() +
  facet_wrap(~ Reef)
plot_env
