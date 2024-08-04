library(tidyverse)
library(readxl)
library(ggpubr)

Toxin<- read_xlsx("C:\\Users\\DELL\\Documents\\Git in R\\Toxicology\\Data\\Working sheet.xlsx",
                  sheet = 'Sheet1')

view(Toxin)
summary(Toxin)

Toxin$Class <- factor(Toxin$Class,
                      levels = c("Atrazine", "Atrazine + Vitamin"))
Toxin$Treatment <- factor(Toxin$Treatment, levels = c("Control" , "Control + VT",
                                                      "AT1", "AT2", "AT3",
                                                      "AT1 + VT",
                                                      "AT2 + VT",
                                                      "AT3 + VT"))

Toxin$Week <- factor(Toxin$Week,
                     labels = c("Week 1", "Week 2", "Week 3", "Week 4"))
levels(Toxin$Week)

Toxin$Replicate <- factor(Toxin$Replicate)

levels(Toxin$Replicate)   # Replicate levels

# ----------- control only ------------------------------------------------------------------
Control_Toxin <- Toxin %>%
  filter(Treatment == "Control")


control_plot <- ggplot(Control_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Control",
    x = "Log(Length)",
    y = "Log(Weight)"
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.3, label.y = 1.5  # Adjust the position of the equation
  )
control_plot
# --------------- Control + VT ----------------------------------------------------

ControlVT_Toxin <- Toxin %>%
  filter(Treatment == "Control + VT")


Control_Vit_plot<- ggplot(ControlVT_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "CVT",
    x = "Log(Length)",
    y = "Log(Weight)"
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.55, label.y = 2  # Adjust the position of the equation
  )
Control_Vit_plot

# --------------- AT1 --------------------------------------------------------------------
AT1_Toxin <- Toxin %>%
  filter(Treatment == "AT1")

AT1_plot <- ggplot(AT1_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "AT1",
    x = "Log(Length)",
    y = "Log(Weight)"
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.4, label.y = 1.7  # Adjust the position of the equation
  )
AT1_plot

# --------------- AT2 -------------------------------------------------------------------------


AT2_Toxin <- Toxin %>%
  filter(Treatment == "AT2")

AT2_Plot <- ggplot(AT2_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "AT2",
    x = "Log(Length)",
    y = "Log(Weight)"
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.3, label.y = 1.6  # Adjust the position of the equation
  )
AT2_Plot

# --------------- AT3 -----------------------------------------------------------------
AT3_Toxin <- Toxin %>%
  filter(Treatment == "AT3")

AT3_plot <- ggplot(AT3_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "AT3",
    x = "Log(Length)",
    y = "Log(Weight)"
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.32, label.y = 1.6  # Adjust the position of the equation
  )
AT3_plot

# -----------------------------------------------------------------------------
AT1_VT1_Toxin <- Toxin %>%
  filter(Treatment == "AT1 + VT")


ATV1_Plot <- ggplot(AT1_VT1_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "AT1 + VT",
    x = "Log(Length)",
    y = "Log(Weight)"
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.33, label.y = 1.6  # Adjust the position of the equation
  )
ATV1_Plot

# -----------------------------------------------------------------------------
AT2_VT1_Toxin <- Toxin %>%
  filter(Treatment == "AT2 + VT")

ATV2_Plot <- ggplot(AT2_VT1_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "AT2 + VT",
    x = "Log(Length)",
    y = "Log(Weight)"
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.2, label.y = 1.4  # Adjust the position of the equation
  )
ATV2_Plot

# ------------------------------------------------------------------------------------

AT3_VT1_Toxin <- Toxin %>%
  filter(Treatment == "AT3 + VT")

ATV3_Plot<- ggplot(AT3_VT1_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "AT3 + VT",
    x = "Log(Length)",
    y = "Log(Weight)"
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.3, label.y = 1.6 # Adjust the position of the equation
  )
ATV3_Plot


####################################################################################################







