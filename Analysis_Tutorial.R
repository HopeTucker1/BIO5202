#Clean data of NA values
all_data_clean <- na.omit(all_data_at)
view(all_data_clean)

#Ocular Distance Data Point Plot
library(tidyverse)
library(ggplot2)
library(ggthemes)
"Ocular Distance Point Data" <- ggplot(all_data_clean, aes(x = beaker, y = ocular_distance_mm)) +
  geom_point(aes(color = treatment)) +
  labs(color = "Treatment", 
       title = "The Effect of Rotenone and Paraquat
       on Ocular Distance",
       x = "Beaker Treatment", y = "Ocular Distance (mm)") +
   scale_color_manual(
     labels = c("paraquat"= "Paraquat",
                "rotenone" = "Rotenone",
                "rotenone_paraquat" = "Rotenone + Paraquat"),
     values = c("paraquat" = "deepskyblue", 
                "rotenone" = "darkgoldenrod1", 
                "rotenone_paraquat" = "chartreuse4")) +
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 14)) +
  theme_bw()
#Saving Ocular Distance Data Point Plot
ggsave(filename = "OcularDistancePointData.png",
       plot = last_plot(), 
       width = 4, 
       height = 3, 
       units = "in") 

#Length Data Point Plot
"Length Point Data" <- ggplot(all_data_clean, aes(x = beaker, y = length_mm)) +
  geom_point(aes(color = treatment), na.rm = TRUE) +
  labs(color = "Treatment", 
       title = "The Effect of Rotenone and Paraquat 
       on Length",
       x = "Beaker Treatment", y = "Length (mm)") +
  scale_color_manual(
    labels = c("paraquat"= "Paraquat",
               "rotenone" = "Rotenone",
               "rotenone_paraquat" = "Rotenone + Paraquat"),
    values = c("paraquat" = "deepskyblue",
               "rotenone" = "darkgoldenrod1",
               "rotenone_paraquat" = "chartreuse4")) +
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 14)) +
  theme_bw()
#Saving Length Data Point Plot
ggsave(filename = "LengthPointData.png",
       plot = last_plot(), 
       width = 4, 
       height = 3, 
       units = "in") 

#Mean Ocular Distance Data Bar Plot
#Summarize Data
mean_sd_ocular <- all_data_clean |>
  group_by(beaker, treatment) |>
  summarise(
    mean_ocular_distance = mean(ocular_distance_mm),
    sd_ocular_distance = sd(ocular_distance_mm),
    .groups = "drop"
  )
view(mean_sd_ocular)
#Plot Data
"Average Ocular Distance Data" <- ggplot(mean_sd_ocular, aes(x = beaker, y = mean_ocular_distance, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_ocular_distance - sd_ocular_distance,
                    ymax = mean_ocular_distance + sd_ocular_distance),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(
    title = "Mean Ocular Distance across Rotenone and Paraquat Treatments",
    subtitle = "With Standard Deviation Error Bars",
    x = "Beaker", y = "Ocular Distance (mm)") +
  scale_fill_manual(
    name = "Treatment",
    labels = c("paraquat" = "Paraquat",
               "rotenone" = "Rotenone",
               "rotenone_paraquat" = "Rotenone + Paraquat"),
    values = c("paraquat" = "deepskyblue",
               "rotenone" = "darkgoldenrod1",
               "rotenone_paraquat" = "chartreuse4")) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
#Saving Average Ocular Distance
ggsave(filename = "AverageOcularDistanceData.png",
       plot = last_plot(), 
       width = 9, 
       height = 7, 
       units = "in") 

#Mean Length Data Bar Plot
#Summarize Data
mean_sd_length <- all_data_clean |>
  group_by(beaker, treatment) |>
  summarise(
    mean_length = mean(length_mm),
    sd_length = sd(length_mm),
    .groups = "drop"
  )
view(mean_sd_length)
#Plot Data
"Average Length Data" <- ggplot(mean_sd_length, aes(x = beaker, y = mean_length, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_length - sd_length,
                    ymax = mean_length + sd_length),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(
    title = "Mean Length across Rotenone and Paraquat Treatments",
    subtitle = "With Standard Deviation Error Bars",
    x = "Beaker", y = "Length (mm)") +
  scale_fill_manual(
    name = "Treatment",
    labels = c("paraquat" = "Paraquat",
               "rotenone" = "Rotenone",
               "rotenone_paraquat" = "Rotenone + Paraquat"),
    values = c("paraquat" = "deepskyblue",
               "rotenone" = "darkgoldenrod1",
               "rotenone_paraquat" = "chartreuse4")) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
#Saving Average Ocular Distance
ggsave(filename = "AverageLengthData.png",
       plot = last_plot(), 
       width = 9, 
       height = 7, 
       units = "in")