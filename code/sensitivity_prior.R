library(rethinking)
library(qgraph)
library(tidyverse)
library(rstan)
library(ggpubr)
rm(list = ls()) 

### Load dataset 
load('../data/proactive_behaviors.RData') 
exclude_behaviors <- c("praising", "request for money", "request for permission")
df <- proactive_behaviors[!(proactive_behaviors$Behavior %in% exclude_behaviors),
                          c("Behavior", "Initiator_ID", "Recipient_ID", 
                            "Initiator_Gender", "Recipient_Gender", 
                            "Date_Obs", "Init_Age_At_Obs", "Recp_Age_At_Obs", 
                            "Init_House", "Recp_House")]
df[rowSums(is.na(df)) > 0,]
# removing rows with unknown age
df <- df[-which(is.na(df$Init_Age_At_Obs)), ]
df[rowSums(is.na(df)) > 0,]
df<-droplevels.data.frame(df)
unique(df$Behavior)

df$same_household <- ifelse(df$Init_House == df$Recp_House, 1, 0)

# set 'ownership assertion' to be the reference category
df$Behavior[which(df$Behavior=='ownership assertion')] <- 'Z'

init_id_map <- 1:length(unique(df$Initiator_ID))
names(init_id_map) <- unique(df$Initiator_ID)

recp_id_map <- 1:length(unique(df$Recipient_ID))
names(recp_id_map) <- unique(df$Recipient_ID)

init_house_id_map <- 1:length(unique(df$Init_House))
names(init_house_id_map) <- unique(df$Init_House)

recp_house_id_map <- 1:length(unique(df$Recp_House))
names(recp_house_id_map) <- unique(df$Recp_House)

# preprocessing per stan requirement
N <- nrow(df) ## Number of observations

df$y <- coerce_index(df$Behavior) ## Renaming response variable
categories <- unique(df[,c("Behavior", "y")])
categories <- categories[order(categories$y),]
K <- max(df$y) ## Number of response categories

df$init_id <- init_id_map[df$Initiator_ID] ## Index of observed individuals, TODO reorder the index
df$recp_id <- recp_id_map[df$Recipient_ID]
N_init_id <- max(df$init_id) ## Number of observed individuals
N_recp_id <- max(df$recp_id)

df$init_house_id <- init_house_id_map[df$Init_House] ## Index of observed households, TODO reorder the index
df$recp_house_id <- recp_house_id_map[df$Recp_House]
N_init_house <- max(df$init_house_id) ## Number of observed individuals
N_recp_house <- max(df$recp_house_id)

df$init_g <- ifelse(df$Initiator_Gender=='F', 1, 0)
df$recp_g <- ifelse(df$Recipient_Gender=='F', 1, 0)
df$init_age_z <- (df$Init_Age_At_Obs-mean(df$Init_Age_At_Obs))/sd(df$Init_Age_At_Obs)
df$recp_age_z <- (df$Recp_Age_At_Obs-mean(df$Recp_Age_At_Obs))/sd(df$Recp_Age_At_Obs)

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

categories$Behavior <- c("(1) Comforting", "(2) Dominating", "(3) Helping", 
                         "(4) Leading", "(5) Dirty Looks", "(6) Physical Aggression", 
                         "(7) Requesting for Access", "(8) Requesting for Comfort",
                         "(9) Requesting for Help", "(10) Requesting for Sharing", 
                         "(11) Scolding", "(12) Sharing", "(13) Supporting Opinions",
                         "(14) Taking Away", "(15) Tattling", "(16) Aggressive Teasing",
                         "(17) Playful Teasing", "(18) Verbal Aggression", 
                         "(19) Ownership Assertion")
categories$Behavior <- factor(categories$Behavior, 
                              levels = c("(1) Comforting", "(2) Dominating", "(3) Helping", 
                                         "(4) Leading", "(5) Dirty Looks", "(6) Physical Aggression", 
                                         "(7) Requesting for Access", "(8) Requesting for Comfort",
                                         "(9) Requesting for Help", "(10) Requesting for Sharing", 
                                         "(11) Scolding", "(12) Sharing", "(13) Supporting Opinions",
                                         "(14) Taking Away", "(15) Tattling", "(16) Aggressive Teasing",
                                         "(17) Playful Teasing", "(18) Verbal Aggression", 
                                         "(19) Ownership Assertion"))

################### mfit_i #######################################
load('../output/models/model_i_0610_sd_2.RData')
mfit_2 <- mfit_i
load('../output/models/model_i_0614_sd_3.RData')
mfit_3 <- mfit_i
load('../output/models/model_i_0614_sd_4.RData')
mfit_4 <- mfit_i
load('../output/models/model_i_0614_sd_5.RData')
mfit_5 <- mfit_i

tiff("../output/figure/sensitivity_test/posterior_intercepts_i_sd.tiff", units="in", width=6, height=6, res=300)
p1 <- stan_plot(mfit_2, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,2)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-8,4), 
                     breaks = seq(-8,4,2))

p2 <- stan_plot(mfit_3, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,3)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-8,4), 
                     breaks = seq(-8,4,2))
p3 <- stan_plot(mfit_4, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,4)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-8,4), 
                     breaks = seq(-8,4,2))

p4 <- stan_plot(mfit_5, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,5)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-8,4), 
                     breaks = seq(-8,4,2))

plot <- ggarrange(p1,p2,p3,p4, ncol=2, nrow=2)

annotate_figure(plot, top = text_grob("Posterior intercepts in model mfit_i", 
                                      color = "black", face = "bold", size = 9))
dev.off()

rm(mfit_2, mfit_3, mfit_4, mfit_5)

################### mfit_ih #######################################
load('../output/models/model_ih_0610_sd_2.RData')
mfit_2 <- mfit_ih
load('../output/models/model_ih_0614_sd_3.RData')
mfit_3 <- mfit_ih
load('../output/models/model_ih_0614_sd_4.RData')
mfit_4 <- mfit_ih
load('../output/models/model_ih_0614_sd_5.RData')
mfit_5 <- mfit_ih

tiff("../output/figure/sensitivity_test/posterior_intercepts_ih_sd.tiff", units="in", width=6, height=6, res=300)
p1 <- stan_plot(mfit_2, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,2)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-8,4), 
                     breaks = seq(-8,4,2))

p2 <- stan_plot(mfit_3, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,3)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-8,4), 
                     breaks = seq(-8,4,2))
p3 <- stan_plot(mfit_4, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,4)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-8,4), 
                     breaks = seq(-8,4,2))

p4 <- stan_plot(mfit_5, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,5)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-8,4), 
                     breaks = seq(-8,4,2))

plot <- ggarrange(p1,p2,p3,p4, ncol=2, nrow=2)

annotate_figure(plot, top = text_grob("Posterior intercepts in model mfit_ih", 
                                      color = "black", face = "bold", size = 9))
dev.off()

rm(mfit_ih, mfit_2, mfit_3, mfit_4, mfit_5)

################### mfit_iF #######################################
load('../output/models/model_iF_0610_sd_2.RData')
mfit_2 <- mfit_iF
load('../output/models/model_iF_0614_sd_3.RData')
mfit_3 <- mfit_iF
load('../output/models/model_iF_0614_sd_4.RData')
mfit_4 <- mfit_iF
load('../output/models/model_iF_0614_sd_5.RData')
mfit_5 <- mfit_iF

tiff("../output/figure/sensitivity_test/posterior_intercepts_iF_sd.tiff", units="in", width=6, height=6, res=300)
p1 <- stan_plot(mfit_2, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,2)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-11,4), 
                     breaks = seq(-11,4,2))

p2 <- stan_plot(mfit_3, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,3)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-11,4), 
                     breaks = seq(-11,4,2))
p3 <- stan_plot(mfit_4, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,4)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-11,4), 
                     breaks = seq(-11,4,2))

p4 <- stan_plot(mfit_5, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,5)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-11,4), 
                     breaks = seq(-11,4,2))

plot <- ggarrange(p1,p2,p3,p4, ncol=2, nrow=2)

annotate_figure(plot, top = text_grob("Posterior Intercepts in Model mfit_iF", 
                                      color = "black", face = "bold", size = 9))
dev.off()

tiff("../output/figure/sensitivity_test/posterior_fixeff_iF_sd.tiff", units="in", width=10, height=8, res=300)
p1 <- stan_plot(mfit_2, point_est = "median", 
                pars = c("bA","bB", "bI", "bR", "bAI", "bBR", "bH"),
                show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,2)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 3),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-6,5), 
                     breaks = seq(-6,5,2))

p2 <- stan_plot(mfit_3, point_est = "median", pars = c("bA","bB", "bI", "bR", "bAI", "bBR", "bH"), show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,3)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 3),
        axis.text.x = element_text(size = 7))+
  scale_x_continuous(limits = c(-6,5), 
                     breaks = seq(-6,5,2))

p3 <- stan_plot(mfit_4, point_est = "median", 
                pars = c("bA","bB", "bI", "bR", "bAI", "bBR", "bH"),
                show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,4)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 3),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-6,5), 
                     breaks = seq(-6,5,2))

p4 <- stan_plot(mfit_5, point_est = "median", pars = c("bA","bB", "bI", "bR", "bAI", "bBR", "bH"), show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,5)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 3),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-6,5), 
                     breaks = seq(-6,5,2))

plot<- ggarrange(p1,p2,p3,p4, ncol=4, nrow=1)

annotate_figure(plot, top = text_grob("Posterior Coefficients for Fixed Effects in Model mfit_iF", 
                                      color = "black", face = "bold", size = 12))
dev.off()

rm(mfit_iF, mfit_2, mfit_3, mfit_4, mfit_5)




################### mfit_ihF #######################################
load('../output/models/model_ihF_0610_sd_2.RData')
mfit_2 <- mfit_ihF
load('../output/models/model_ihF_0614_sd_3.RData')
mfit_3 <- mfit_ihF
load('../output/models/model_ihF_0614_sd_4.RData')
mfit_4 <- mfit_ihF
load('../output/models/model_ihF_0614_sd_5.RData')
mfit_5 <- mfit_ihF

tiff("../output/figure/sensitivity_test/posterior_intercepts_ihF_sd.tiff", units="in", width=6, height=6, res=300)
p1 <- stan_plot(mfit_2, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,2)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-11,4), 
                     breaks = seq(-11,4,2))

p2 <- stan_plot(mfit_3, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,3)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-11,4), 
                     breaks = seq(-11,4,2))
p3 <- stan_plot(mfit_4, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,4)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-11,4), 
                     breaks = seq(-11,4,2))

p4 <- stan_plot(mfit_5, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,5)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-11,4), 
                     breaks = seq(-11,4,2))

plot <- ggarrange(p1,p2,p3,p4, ncol=2, nrow=2)

annotate_figure(plot, top = text_grob("Posterior Intercepts in Model mfit_ihF", 
                                      color = "black", face = "bold", size = 9))
dev.off()

tiff("../output/figure/sensitivity_test/posterior_fixeff_ihF_sd.tiff", units="in", width=10, height=8, res=300)
p1 <- stan_plot(mfit_2, point_est = "median", 
                pars = c("bA","bB", "bI", "bR", "bAI", "bBR", "bH"),
                show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,2)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 3),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-7,5), 
                     breaks = seq(-7,5,2))

p2 <- stan_plot(mfit_3, point_est = "median", pars = c("bA","bB", "bI", "bR", "bAI", "bBR", "bH"), show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,3)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 3),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-7,5), 
                     breaks = seq(-7,5,2))

p3 <- stan_plot(mfit_4, point_est = "median", 
                pars = c("bA","bB", "bI", "bR", "bAI", "bBR", "bH"),
                show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,4)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 3),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-7,5), 
                     breaks = seq(-7,5,2))

p4 <- stan_plot(mfit_5, point_est = "median", pars = c("bA","bB", "bI", "bR", "bAI", "bBR", "bH"), show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "Prior: Normal(0,5)") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 3),
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous(limits = c(-7,5), 
                     breaks = seq(-7,5,2))

plot<- ggarrange(p1,p2,p3,p4, ncol=4, nrow=1)

annotate_figure(plot, top = text_grob("Posterior Coefficients for Fixed Effects in Model mfit_ihF", 
                                      color = "black", face = "bold", size = 12))
dev.off()

rm(mfit_ihF, mfit_2, mfit_3, mfit_4, mfit_5)





load('../output/models/model_i_0614_sd_5_lkj_0.5.RData')
mfit_05 <- mfit_i
load('../output/models/model_i_0614_sd_5_lkj_1.RData')
mfit_1 <- mfit_i

params <- precis(mfit_05, depth = 3)

######################## HMC Diagnostics #######################################
check_hmc_diagnostics(mfit_i)
check_hmc_diagnostics(mfit_ih)
check_hmc_diagnostics(mfit_iF)
check_hmc_diagnostics(mfit_ihF)

############ Model comparison using WAIC #######################################
models <- compare(mfit_i, mfit_ih, mfit_iF, mfit_ihF)
write.csv(models, "../output/WAIC.csv", row.names=T)
plot(models)

## Load csv files of the R outputs #######
rst_i <- read.csv("../output/model_i_random_effect.csv")
rst_i_corr <- read.csv("../output/model_i_corr.csv")

rst_iF <- read.csv("../output/model_iF_random_effect.csv")
rst_iF_corr <- read.csv("../output/model_iF_corr.csv")

rst_ih <- read.csv("../output/model_ih_random_effect.csv")
rst_ih_corr <- read.csv("../output/model_ih_corr.csv")

rst_ihF <- read.csv("../output/model_ihF_random_effect.csv")
rst_ihF_corr <- read.csv("../output/model_ihF_corr.csv")
