library(tidyverse)
library(lme4)
library(lmerTest)
install.packages("MuMIn")
library(MuMIn)
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

Control_LM <- lmer(log(Weight) ~ log(Length) + (1| Replicate),
                   data = Control_Toxin)
summary(Control_LM)

r2_Control_LM <- r.squaredGLMM(Control_LM)
print(r2_Control_LM)
# R2C = The proportion of variance explained by both the fixed and random effects



control_plot <- ggplot(Control_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Control",
    x = " ",
    y = "Log(Weight)"
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.23, label.y = 1.5  # Adjust the position of the equation
  )

# --------------- Control + VT ----------------------------------------------------

ControlVT_Toxin <- Toxin %>%
  filter(Treatment == "Control + VT")

ControlVT_LM <- lmer(log(Weight) ~ log(Length) + (1| Week),
                   data = ControlVT_Toxin)
summary(ControlVT_LM)

r2_ControlVT_LM <- r.squaredGLMM(ControlVT_LM)
print(r2_ControlVT_LM)

Control_Vit_plot<- ggplot(ControlVT_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Control + VT",
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
    label.x = 2.45, label.y = 2  # Adjust the position of the equation
  )


# --------------- AT1 --------------------------------------------------------------------
AT1_Toxin <- Toxin %>%
  filter(Treatment == "AT1")

AT1_LM <- lmer(log(Weight) ~ log(Length) + (1| Replicate),
               data = AT1_Toxin)
summary(AT1_LM)

r2_AT1_LM <- r.squaredGLMM(AT1_LM)
r2_AT1_LM

AT1_plot <- ggplot(AT1_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "AT1",
    x = "",
    y = ""
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.3, label.y = 1.7  # Adjust the position of the equation
  )

# --------------- AT2 -------------------------------------------------------------------------


AT2_Toxin <- Toxin %>%
  filter(Treatment == "AT2")

AT2_LM <- lmer(log(Weight) ~ log(Length) + (1| Replicate),
               data = AT2_Toxin)
summary(AT2_LM)

r2_AT2_LM <- r.squaredGLMM(AT2_LM)
r2_AT2_LM

lm_at2 <- lm(log(Weight) ~ log(Length),
             data = AT2_Toxin)
summary(lm_at2)

AT2_Plot <- ggplot(AT2_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "AT2",
    x = "",
    y = ""
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.2, label.y = 1.6  # Adjust the position of the equation
  )


# --------------- AT3 -----------------------------------------------------------------
AT3_Toxin <- Toxin %>%
  filter(Treatment == "AT3")

AT3_LM <- lmer(log(Weight) ~ log(Length) + ( 1| Replicate),
               data = AT3_Toxin)
summary(AT3_LM)

r2_AT3_LM <- r.squaredGLMM(AT3_LM)
r2_AT3_LM

AT3_plot <- ggplot(AT3_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "AT3",
    x = "",
    y = ""
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.22, label.y = 1.6  # Adjust the position of the equation
  )


# -----------------------------------------------------------------------------
AT1_VT1_Toxin <- Toxin %>%
filter(Treatment == "AT1 + VT")

AT1_VT1_LM <- lmer(log(Weight) ~ log(Length) + (1| Week),
                     data = AT1_VT1_Toxin)
summary(AT1_VT1_LM)

r2_AT1_VT1_LM <- r.squaredGLMM(AT1_VT1_LM)
print(r2_AT1_VT1_LM)

ATV1_Plot <- ggplot(AT1_VT1_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "AT1 + VT",
    x = "Log(Length)",
    y = ""
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.23, label.y = 1.6  # Adjust the position of the equation
  )

# -----------------------------------------------------------------------------
AT2_VT1_Toxin <- Toxin %>%
  filter(Treatment == "AT2 + VT")

AT2_VT1_LM <- lmer(log(Weight) ~ log(Length) + (1| Replicate),
                   data = AT2_VT1_Toxin)
summary(AT2_VT1_LM)

r2_AT2_VT1_LM <- r.squaredGLMM(AT2_VT1_LM)
print(r2_AT2_VT1_LM)

ATV2_Plot <- ggplot(AT2_VT1_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "AT2 + VT",
    x = "Log(Length)",
    y = ""
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.1, label.y = 1.4  # Adjust the position of the equation
  )


# ------------------------------------------------------------------------------------

 AT3_VT1_Toxin <- Toxin %>%
  filter(Treatment == "AT3 + VT")

AT3_VT1_LM <- lmer(log(Weight) ~ log(Length) + (1| Week),
                   data = AT3_VT1_Toxin)
summary(AT3_VT1_LM)

r2_AT3_VT1_LM <- r.squaredGLMM(AT3_VT1_LM)
print(r2_AT3_VT1_LM)

ATV3_Plot<- ggplot(AT3_VT1_Toxin, aes(x = log(Length), y = log(Weight))) +
  geom_point(aes(color = as.factor(Replicate)), size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "AT3 + VT",
    x = "Log(Length)",
    y = ""
  ) +
  guides(color = "none") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_regline_equation(
    aes(label = paste( ..rr.label.., ..eq.label.., sep = "~~~~")),
    label.x = 2.22, label.y = 1.6 # Adjust the position of the equation
  )



####################################################################################################

install.packages("patchwork")
library(patchwork)


combined_plot <- control_plot +
  AT1_plot + AT2_Plot + AT3_plot +
  Control_Vit_plot +ATV1_Plot +
  ATV2_Plot + ATV3_Plot +
  plot_layout(nrow = 2, ncol = 4)
combined_plot













