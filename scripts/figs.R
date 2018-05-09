
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lme4)
library(here)

# rm(list = ls(all = TRUE))

dataset <- read_csv(here("data", "P_SP.csv"))


data <- dataset %>%
  filter(., do == "AGC")

datab <- data %>%
  filter(., group == "b")
datab3 <- datab %>%
  filter(., item != "spAGCInan4") %>%
  filter(., item != "spAGCInan5")


fig1 <- data %>%
  mutate(., group = recode(group, `b` = "Bilinguals", `m` = "Monolinguals")) %>%
  ggplot(., aes(x=item, y=response)) +
  geom_point(color = 'white', size = 2) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  stat_summary(fun.y = mean, geom = 'point', color = 'blue', size = 3, 
               position = position_dodge(width = 0.5)) + 
  scale_shape(name = "") +
  facet_grid(~ group) +
  labs(title = 'Figure 1. Production of DOM in agentivity \nconstraint contexts across groups.')

fig1

fig2 <- datab3 %>%
  ggplot(., aes(x=mintsp, y=response)) +
  geom_point() + 
  geom_smooth(method = lm) +
  labs(title = 'Figure 2. Production of DOM as a function of \nMiNT score in bilinguals.')

fig2

dataPron <- dataset %>%
  filter(., do == "Pron") %>%
  filter(., animacy == "Inan")

dataPronb <- dataPron %>%
  filter(., group == "b")
head(dataPronb)

fig3 <- dataPron %>%
  ggplot(., aes(x=item, y=response)) +
  geom_point(color = 'white', size = 2) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  stat_summary(fun.y = mean, geom = 'point', color = 'blue', size = 3, 
               position = position_dodge(width = 0.5)) + 
  scale_shape(name = "") +
  facet_grid(~ group) +
  labs(title = 'Figure 3. Production of DOM in inanimate \ndemonstrative pronoun contexts across groups.')

fig3

fig4 <- dataPronb %>%
  ggplot(., aes(x=education, y=response)) +
  geom_point() + 
  geom_smooth(method = lm) +
  labs(title = 'Figure 4. Production of DOM as a function of \nlevel of education in bilinguals.')

fig4

fig5 <- dataPronb %>%
  ggplot(., aes(x=roday, y=response)) +
  geom_point() + 
  geom_smooth(method = lm) +
  labs(title = 'Figure 5. Production of DOM as a function of \namount of Romanian spoken daily in bilinguals.')

fig5

mod_mint3 <- glm(response ~ mintsp, data = datab3, family = binomial(link = "logit"))
summary(mod_mint3)

mod_educationP <- glm(response ~ education, data = dataPronb, family = binomial(link = "logit"))
summary(mod_educationP)

mod_rodayP <- glm(response ~ roday, data = dataPronb, family = binomial(link = "logit"))
summary(mod_rodayP)