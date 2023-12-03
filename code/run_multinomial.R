## initialize
library(rstan)
library(rethinking)
library(chron)
library(tidyverse)


options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

rm(list = ls())
## data
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

write.csv(df, '../data/proactive-multinomial-model-data.csv')
nrow(df)
table(df$Behavior)

# 
# # df <- read.csv('data/multinomial-model-data.csv')
# 
# ############### Model i #################################
# # model with only individual random effects
# i_dat_list <- list(
#   K = K,
#   N = N,
#   N_init_id = N_init_id,
#   N_recp_id = N_recp_id,
#   y = df$y,
#   init_id = df$init_id,
#   recp_id = df$recp_id
# )
# 
# model_code_i <- "
# data{
#     int<lower=1> N;
#     int<lower=1> N_init_id;
#     int<lower=1> N_recp_id;
#     int<lower=1> y[N];
#     int<lower=1> init_id[N];
#     int<lower=1> recp_id[N];
#     int<lower=1> K;
# }
# parameters{      
#     real a[K-1];                // intercepts for each behavior
#     matrix[K-1,N_init_id] z_init_id;      // matrix of standardized random effects of initiator 
#     vector<lower=0>[K-1] sigma_init_id;   // stddev of random effects
#     cholesky_factor_corr[K-1] L_Rho_init_id;         // correlation matrix of random effects
#     matrix[K-1,N_recp_id] z_recp_id;      // matrix of standardized random effects of recipient
#     vector<lower=0>[K-1] sigma_recp_id;   // stddev of random effects
#     cholesky_factor_corr[K-1] L_Rho_recp_id;         // correlation matrix of random effects
# }
# transformed parameters{
#     matrix[N_init_id,K-1] v_init_id;      // matrix of scaled random effects
#     matrix[N_recp_id,K-1] v_recp_id;   
#     v_init_id = (diag_pre_multiply(sigma_init_id,L_Rho_init_id) * z_init_id)';
#     v_recp_id = (diag_pre_multiply(sigma_recp_id,L_Rho_recp_id) * z_recp_id)';
# }
# model{
#     
#     // priors
#     a ~ normal(0,1);
# 
#     // hyper-prior
#     to_vector(z_init_id) ~ normal(0,1);
#     sigma_init_id ~ exponential(1);
#     L_Rho_init_id ~ lkj_corr_cholesky(2);
#     
#     to_vector(z_recp_id) ~ normal(0,1);
#     sigma_recp_id ~ exponential(1);
#     L_Rho_recp_id ~ lkj_corr_cholesky(2);
# 
#     // likelihood
#     for ( i in 1:N ) {
#         vector[K] p;
#         for ( k in 1:(K-1) ) 
#             p[k] = a[k] + v_init_id[init_id[i], k] + v_recp_id[recp_id[i], k];
#         p[K] = 0;
#         y[i] ~ categorical_logit( p );
#     }
# }
# generated quantities{
#     matrix[K-1,K-1] Rho_init_id;
#     matrix[K-1,K-1] Rho_recp_id;
#     vector[N] log_lik;
#     Rho_init_id = L_Rho_init_id * L_Rho_init_id';
#     Rho_recp_id = L_Rho_recp_id * L_Rho_recp_id';
#     
#     for ( i in 1:N ) {
#         vector[K] p;
#         for ( k in 1:(K-1) ) 
#           p[k] = a[k] + v_init_id[init_id[i], k] + v_recp_id[recp_id[i], k];
#         p[K] = 0;
#         log_lik[i] = categorical_logit_lpmf( y[i] | p );
#     }
# }
# "
# 
# # model
# start_i <- list(
#   a = rep(0,K-1),
#   sigma_init_id = rep(1,K-1),
#   L_Rho_init_id = diag(K-1),
#   z_init_id = matrix(0,nrow=K-1,ncol=N_init_id),
#   sigma_recp_id = rep(1,K-1),
#   L_Rho_recp_id = diag(K-1),
#   z_recp_id = matrix(0,nrow=K-1,ncol=N_recp_id)
# )
# 
# n_chains_i <- 3
# init_i <- list()
# for ( i in 1:n_chains_i ) init_i[[i]] <- start_i
# 
# mfit_i <- stan( model_code = model_code_i, data = i_dat_list, chains = n_chains_i, 
#                 cores = n_chains_i, warmup = 5000, iter = 10000, init = init_i, 
#                 control = list(adapt_delta = 0.99))
# 
# save(mfit_i, file = '../output/model_i_0522.RData')
# 
# # ################ Model iF ################################
# 
# # model data with random individual effects and fixed effects
# iF_dat_list <- list(
#   K = K,
#   N = N,
#   N_init_id = N_init_id,
#   N_recp_id = N_recp_id,
#   y = df$y,
#   init_id = df$init_id,
#   recp_id = df$recp_id,
#   init_age_z = df$init_age_z,
#   recp_age_z = df$recp_age_z,
#   init_g = df$init_g,
#   recp_g = df$recp_g,
#   same_house = df$same_household
# )
# 
# 
# model_code_iF <- "
# data{
#     int<lower=1> N;
#     int<lower=1> N_init_id;
#     int<lower=1> N_recp_id;
#     int<lower=1> y[N];
#     int<lower=1> init_id[N];
#     int<lower=1> recp_id[N];
#     real init_age_z[N];
#     real recp_age_z[N];
#     int<lower=0,upper=1> init_g[N];
#     int<lower=0,upper=1> recp_g[N];
#     int<lower=0,upper=1> same_house[N];
#     int<lower=1> K;
# }
# parameters{
#     real a[K-1];                // intercepts for each behavior
#     real bA[K-1];				// fixed effect for initiator age
#     real bB[K-1];				// fixed effect for recipient age
#     real bI[K-1];				// fixed effect for initiator gender
#     real bR[K-1];				// fixed effect for recipient gender
#     real bAI[K-1];      // fixed effect for interaction between initiator age and sex
#     real bBR[K-1];      // fixed effect for interaction between recipient age and sex
#     real bH[K-1];       // fixed effect for same household
#     matrix[K-1,N_init_id] z_init_id;      // matrix of standardized random effects of initiator 
#     vector<lower=0>[K-1] sigma_init_id;   // stddev of random effects
#     cholesky_factor_corr[K-1] L_Rho_init_id;         // correlation matrix of random effects
#     matrix[K-1,N_recp_id] z_recp_id;      // matrix of standardized random effects of recipient
#     vector<lower=0>[K-1] sigma_recp_id;   // stddev of random effects
#     cholesky_factor_corr[K-1] L_Rho_recp_id;         // correlation matrix of random effects
# }
# transformed parameters{
#     matrix[N_init_id,K-1] v_init_id;      // matrix of scaled random effects
#     matrix[N_recp_id,K-1] v_recp_id;   
#     v_init_id = (diag_pre_multiply(sigma_init_id,L_Rho_init_id) * z_init_id)';
#     v_recp_id = (diag_pre_multiply(sigma_recp_id,L_Rho_recp_id) * z_recp_id)';
# }
# model{
#   
#      // priors
#     a ~ normal(0,1);
#     bA ~ normal(0,1);
#     bB ~ normal(0,1);
#     bI ~ normal(0,1);
#     bR ~ normal(0,1);
#     bAI ~ normal(0,1);
#     bBR ~ normal(0,1);
#     bH ~ normal(0,1);
# 
#     // hyper-prior
#     to_vector(z_init_id) ~ normal(0,1);
#     sigma_init_id ~ exponential(1);
#     L_Rho_init_id ~ lkj_corr_cholesky(2);
#     
#     to_vector(z_recp_id) ~ normal(0,1);
#     sigma_recp_id ~ exponential(1);
#     L_Rho_recp_id ~ lkj_corr_cholesky(2);
# 
#     // likelihood
#     for ( i in 1:N ) {
#         vector[K] p;
#         for ( k in 1:(K-1) ) 
#             p[k] = a[k] 
#             + bA[k] * init_age_z[i] 
#             + bB[k] * recp_age_z[i] 
#             + bI[k] * init_g[i] 
#             + bR[k] * recp_g[i] 
#             + bAI[k] * init_age_z[i] * init_g[i]
#             + bBR[k] * recp_age_z[i] * recp_g[i]
#             + bH[k] * same_house[i] 
#             + v_init_id[init_id[i],k] 
#             + v_recp_id[recp_id[i],k];
#         p[K] = 0;
#         y[i] ~ categorical_logit( p );
#     }
# }
# generated quantities{
#     matrix[K-1,K-1] Rho_init_id;
#     matrix[K-1,K-1] Rho_recp_id;
#     vector[N] log_lik;
#     Rho_init_id = L_Rho_init_id * L_Rho_init_id';
#     Rho_recp_id = L_Rho_recp_id * L_Rho_recp_id';
#     
#     for ( i in 1:N ) {
#         vector[K] p;
#         for ( k in 1:(K-1) ) 
#             p[k] = a[k] 
#             + bA[k] * init_age_z[i] 
#             + bB[k] * recp_age_z[i] 
#             + bI[k] * init_g[i] 
#             + bR[k] * recp_g[i] 
#             + bAI[k] * init_age_z[i] * init_g[i]
#             + bBR[k] * recp_age_z[i] * recp_g[i]
#             + bH[k] * same_house[i] 
#             + v_init_id[init_id[i],k] 
#             + v_recp_id[recp_id[i],k];
#         p[K] = 0;
#         log_lik[i] = categorical_logit_lpmf( y[i] | p );
#     }
# }
# "
# # model
# start_iF <- list(
#   a = rep(0,K-1),
#   bA = rep(0,K-1),
#   bB = rep(0,K-1),
#   bI = rep(0,K-1),
#   bR = rep(0,K-1),
#   bAI = rep(0,K-1),
#   bBR = rep(0,K-1),
#   bH = rep(0,K-1),
#   sigma_init_id = rep(1,K-1),
#   L_Rho_init_id = diag(K-1),
#   z_init_id = matrix(0,nrow=K-1,ncol=N_init_id),
#   sigma_recp_id = rep(1,K-1),
#   L_Rho_recp_id = diag(K-1),
#   z_recp_id = matrix(0,nrow=K-1,ncol=N_recp_id)
# )
# 
# n_chains_iF <- 3
# init_iF <- list()
# for ( i in 1:n_chains_iF ) init_iF[[i]] <- start_iF
# 
# mfit_iF <- stan( model_code = model_code_iF, data = iF_dat_list, chains = n_chains_iF, 
#                  cores = n_chains_iF, warmup = 5000, iter = 10000, init = init_iF,
#                  control = list(adapt_delta = 0.99))
# 
# save(mfit_iF, file = '../output/model_iF_0522.RData')

############# Model ih ###################################
# 
# print("ih")
# ih_dat_list <- list(
#   K = K,
#   N = N,
#   N_init_id = N_init_id,
#   N_recp_id = N_recp_id,
#   N_init_house = N_init_house,
#   N_recp_house = N_recp_house,
#   y = df$y,
#   init_id = df$init_id,
#   recp_id = df$recp_id,
#   init_house_id = df$init_house_id,
#   recp_house_id = df$recp_house_id
# )
# 
# model_code_ih <- "
# data{
#     int<lower=1> N;
#     int<lower=1> N_init_id;
#     int<lower=1> N_recp_id;
#     int<lower=1> N_init_house;
#     int<lower=1> N_recp_house;
#     int<lower=1> y[N];
#     int<lower=1> init_id[N];
#     int<lower=1> recp_id[N];
#     int<lower=1> init_house_id[N];
#     int<lower=1> recp_house_id[N];
#     int<lower=1> K;
# }
# parameters{
#     real a[K-1];                // intercepts for each behavior
#     matrix[K-1,N_init_id] z_init_id;      // matrix of standardized random effects of initiator
#     vector<lower=0>[K-1] sigma_init_id;   // stddev of random effects
#     cholesky_factor_corr[K-1] L_Rho_init_id;         // correlation matrix of random effects
#     matrix[K-1,N_recp_id] z_recp_id;      // matrix of standardized random effects of recipient
#     vector<lower=0>[K-1] sigma_recp_id;   // stddev of random effects
#     cholesky_factor_corr[K-1] L_Rho_recp_id;         // correlation matrix of random effects
#     matrix[K-1,N_init_house] z_init_house;      // matrix of standardized random effects of initiator household
#     vector<lower=0>[K-1] sigma_init_house;   // stddev of random effects
#     cholesky_factor_corr[K-1] L_Rho_init_house;         // correlation matrix of random effects
#     matrix[K-1,N_recp_house] z_recp_house;      // matrix of standardized random effects of recipent household
#     vector<lower=0>[K-1] sigma_recp_house;   // stddev of random effects
#     cholesky_factor_corr[K-1] L_Rho_recp_house;         // correlation matrix of random effects
# }
# transformed parameters{
#     matrix[N_init_id,K-1] v_init_id;      // matrix of scaled random effects
#     matrix[N_recp_id,K-1] v_recp_id;
#     matrix[N_init_house,K-1] v_init_house;
#     matrix[N_recp_house,K-1] v_recp_house;
#     v_init_id = (diag_pre_multiply(sigma_init_id,L_Rho_init_id) * z_init_id)';
#     v_recp_id = (diag_pre_multiply(sigma_recp_id,L_Rho_recp_id) * z_recp_id)';
#     v_init_house = (diag_pre_multiply(sigma_init_house,L_Rho_init_house) * z_init_house)';
#     v_recp_house = (diag_pre_multiply(sigma_recp_house,L_Rho_recp_house) * z_recp_house)';
# }
# model{
#     // priors
#     a ~ normal(0,1);
# 
#     // hyper-prior
#     to_vector(z_init_id) ~ normal(0,1);
#     sigma_init_id ~ exponential(1);
#     L_Rho_init_id ~ lkj_corr_cholesky(2);
# 
#     to_vector(z_recp_id) ~ normal(0,1);
#     sigma_recp_id ~ exponential(1);
#     L_Rho_recp_id ~ lkj_corr_cholesky(2);
# 
#     to_vector(z_init_house) ~ normal(0,1);
#     sigma_init_house ~ exponential(1);
#     L_Rho_init_house ~ lkj_corr_cholesky(2);
# 
#     to_vector(z_recp_house) ~ normal(0,1);
#     sigma_recp_house ~ exponential(1);
#     L_Rho_recp_house ~ lkj_corr_cholesky(2);
# 
#     // likelihood
#     for ( i in 1:N ) {
#         vector[K] p;
#         for ( k in 1:(K-1) )
#             p[k] = a[k] + v_init_id[init_id[i], k] + v_recp_id[recp_id[i], k] + v_init_house[init_house_id[i],k] + v_recp_house[recp_house_id[i],k];
#         p[K] = 0;
#         y[i] ~ categorical_logit( p );
#     }
# }
# generated quantities{
#     matrix[K-1,K-1] Rho_init_id;
#     matrix[K-1,K-1] Rho_recp_id;
#     matrix[K-1,K-1] Rho_init_house;
#     matrix[K-1,K-1] Rho_recp_house;
#     vector[N] log_lik;
#     Rho_init_id = L_Rho_init_id * L_Rho_init_id';
#     Rho_recp_id = L_Rho_recp_id * L_Rho_recp_id';
#     Rho_init_house = L_Rho_init_house * L_Rho_init_house';
#     Rho_recp_house = L_Rho_recp_house * L_Rho_recp_house';
# 
#     for ( i in 1:N ) {
#         vector[K] p;
#         for ( k in 1:(K-1) )
#           p[k] = a[k] + v_init_id[init_id[i], k] + v_recp_id[recp_id[i], k] + v_init_house[init_house_id[i],k] + v_recp_house[recp_house_id[i],k];
#         p[K] = 0;
#         log_lik[i] = categorical_logit_lpmf( y[i] | p );
#     }
# }
# "
# 
# start_ih <- list(
#   a = rep(0,K-1),
#   sigma_init_id = rep(1,K-1),
#   L_Rho_init_id = diag(K-1),
#   z_init_id = matrix(0,nrow=K-1,ncol=N_init_id),
#   sigma_recp_id = rep(1,K-1),
#   L_Rho_recp_id = diag(K-1),
#   z_recp_id = matrix(0,nrow=K-1,ncol=N_recp_id),
#   sigma_init_house = rep(1,K-1),
#   L_Rho_init_house = diag(K-1),
#   z_init_house = matrix(0,nrow=K-1,ncol=N_init_house),
#   sigma_recp_house = rep(1,K-1),
#   L_Rho_recp_house = diag(K-1),
#   z_recp_house = matrix(0,nrow=K-1,ncol=N_recp_house)
# )
# 
# n_chains_ih <- 3
# init_ih <- list()
# for ( i in 1:n_chains_ih ) init_ih[[i]] <- start_ih
# 
# mfit_ih <- stan(model_code = model_code_ih, data = ih_dat_list, chains = n_chains_ih,
#                 cores = n_chains_ih, warmup = 5000, iter = 10000, init = init_ih,
#                 control = list(adapt_delta = 0.99))
# save(mfit_ih, file = '../output/model_ih_0522.RData')


############### Model ihF #################################
ihF_dat_list <- list(
  K = K,
  N = N,
  N_init_id = N_init_id,
  N_recp_id = N_recp_id,
  N_init_house = N_init_house,
  N_recp_house = N_recp_house,
  y = df$y,
  init_id = df$init_id,
  recp_id = df$recp_id,
  init_house_id = df$init_house_id,
  recp_house_id = df$recp_house_id,
  init_age_z = df$init_age_z,
  recp_age_z = df$recp_age_z,
  init_g = df$init_g,
  recp_g = df$recp_g,
  same_house = df$same_household
)

model_code_ihF <- "
data{
    int<lower=1> N;
    int<lower=1> N_init_id;
    int<lower=1> N_recp_id;
    int<lower=1> N_init_house;
    int<lower=1> N_recp_house;
    int<lower=1> y[N];
    int<lower=1> init_id[N];
    int<lower=1> recp_id[N];
    int<lower=1> init_house_id[N];
    int<lower=1> recp_house_id[N];
    real init_age_z[N];
    real recp_age_z[N];
    int<lower=0,upper=1> init_g[N];
    int<lower=0,upper=1> recp_g[N];
    int<lower=0,upper=1> same_house[N];
    int<lower=1> K;
}
parameters{
    real a[K-1];                // intercepts for each behavior
    real bA[K-1];				// fixed effect for initiator age
    real bB[K-1];				// fixed effect for recipient age
    real bI[K-1];				// fixed effect for initiator gender
    real bR[K-1];				// fixed effect for recipient gender
    real bAI[K-1];      // fixed effect for interaction between initiator age and sex
    real bBR[K-1];      // fixed effect for interaction between recipient age and sex
    real bH[K-1];       // fixed effect for same household
    matrix[K-1,N_init_id] z_init_id;      // matrix of standardized random effects of initiator
    vector<lower=0>[K-1] sigma_init_id;   // stddev of random effects
    cholesky_factor_corr[K-1] L_Rho_init_id;         // correlation matrix of random effects
    matrix[K-1,N_recp_id] z_recp_id;      // matrix of standardized random effects of recipient
    vector<lower=0>[K-1] sigma_recp_id;   // stddev of random effects
    cholesky_factor_corr[K-1] L_Rho_recp_id;         // correlation matrix of random effects
    matrix[K-1,N_init_house] z_init_house;      // matrix of standardized random effects of initiator household
    vector<lower=0>[K-1] sigma_init_house;   // stddev of random effects
    cholesky_factor_corr[K-1] L_Rho_init_house;         // correlation matrix of random effects
    matrix[K-1,N_recp_house] z_recp_house;      // matrix of standardized random effects of recipent household
    vector<lower=0>[K-1] sigma_recp_house;   // stddev of random effects
    cholesky_factor_corr[K-1] L_Rho_recp_house;         // correlation matrix of random effects
}
transformed parameters{
    matrix[N_init_id,K-1] v_init_id;      // matrix of scaled random effects
    matrix[N_recp_id,K-1] v_recp_id;
    matrix[N_init_house,K-1] v_init_house;
    matrix[N_recp_house,K-1] v_recp_house;
    v_init_id = (diag_pre_multiply(sigma_init_id,L_Rho_init_id) * z_init_id)';
    v_recp_id = (diag_pre_multiply(sigma_recp_id,L_Rho_recp_id) * z_recp_id)';
    v_init_house = (diag_pre_multiply(sigma_init_house,L_Rho_init_house) * z_init_house)';
    v_recp_house = (diag_pre_multiply(sigma_recp_house,L_Rho_recp_house) * z_recp_house)';
}
model{
    // priors
    a ~ normal(0,1);
    bA ~ normal(0,1);
    bB ~ normal(0,1);
    bI ~ normal(0,1);
    bR ~ normal(0,1);
    bAI ~ normal(0,1);
    bBR ~ normal(0,1);
    bH ~ normal(0,1);

    // hyper-prior
    to_vector(z_init_id) ~ normal(0,1);
    sigma_init_id ~ exponential(1);
    L_Rho_init_id ~ lkj_corr_cholesky(2);

    to_vector(z_recp_id) ~ normal(0,1);
    sigma_recp_id ~ exponential(1);
    L_Rho_recp_id ~ lkj_corr_cholesky(2);

    to_vector(z_init_house) ~ normal(0,1);
    sigma_init_house ~ exponential(1);
    L_Rho_init_house ~ lkj_corr_cholesky(2);

    to_vector(z_recp_house) ~ normal(0,1);
    sigma_recp_house ~ exponential(1);
    L_Rho_recp_house ~ lkj_corr_cholesky(2);

    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) )
            p[k] = a[k] 
            + bA[k] * init_age_z[i] 
            + bB[k] * recp_age_z[i] 
            + bI[k] * init_g[i] 
            + bR[k] * recp_g[i] 
            + bAI[k] * init_age_z[i] * init_g[i]
            + bBR[k] * recp_age_z[i] * recp_g[i]
            + bH[k] * same_house[i] 
            + v_init_id[init_id[i], k] 
            + v_recp_id[recp_id[i], k] 
            + v_init_house[init_house_id[i],k] 
            + v_recp_house[recp_house_id[i],k];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}
generated quantities{
    matrix[K-1,K-1] Rho_init_id;
    matrix[K-1,K-1] Rho_recp_id;
    matrix[K-1,K-1] Rho_init_house;
    matrix[K-1,K-1] Rho_recp_house;
    vector[N] log_lik;
    Rho_init_id = L_Rho_init_id * L_Rho_init_id';
    Rho_recp_id = L_Rho_recp_id * L_Rho_recp_id';
    Rho_init_house = L_Rho_init_house * L_Rho_init_house';
    Rho_recp_house = L_Rho_recp_house * L_Rho_recp_house';

    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) )
          p[k] = a[k] 
          + bA[k] * init_age_z[i] 
          + bB[k] * recp_age_z[i] 
          + bI[k] * init_g[i] 
          + bR[k] * recp_g[i] 
          + bAI[k] * init_age_z[i] * init_g[i]
          + bBR[k] * recp_age_z[i] * recp_g[i]
          + bH[k] * same_house[i] 
          + v_init_id[init_id[i], k] 
          + v_recp_id[recp_id[i], k]
          + v_init_house[init_house_id[i],k]
          + v_recp_house[recp_house_id[i],k];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
"


start_ihF <- list(
  a = rep(0,K-1),
  bA = rep(0,K-1),
  bB = rep(0,K-1),
  bI = rep(0,K-1),
  bR = rep(0,K-1),
  bAI = rep(0,K-1),
  bBR = rep(0,K-1),
  bH = rep(0,K-1),
  sigma_init_id = rep(1,K-1),
  L_Rho_init_id = diag(K-1),
  z_init_id = matrix(0,nrow=K-1,ncol=N_init_id),
  sigma_recp_id = rep(1,K-1),
  L_Rho_recp_id = diag(K-1),
  z_recp_id = matrix(0,nrow=K-1,ncol=N_recp_id),
  sigma_init_house = rep(1,K-1),
  L_Rho_init_house = diag(K-1),
  z_init_house = matrix(0,nrow=K-1,ncol=N_init_house),
  sigma_recp_house = rep(1,K-1),
  L_Rho_recp_house = diag(K-1),
  z_recp_house = matrix(0,nrow=K-1,ncol=N_recp_house)
)

n_chains_ihF <- 3
init_ihF <- list()
for ( i in 1:n_chains_ihF ) init_ihF[[i]] <- start_ihF

mfit_ihF <- stan( model_code = model_code_ihF, data = ihF_dat_list, chains = n_chains_ihF,
                  cores = n_chains_ihF, warmup = 5000, iter = 10000, init = init_ihF,
                  control = list(adapt_delta = 0.99))
save(mfit_ihF, file = '../output/model_ihF_0522.RData')

# 
# ####### Process results: mfit_i ########
# load('../output/model_i_0522.RData')
# mfit_i_out <- precis(mfit_i, depth = 2, prob = .95, corr = TRUE)
# mfit_i_corr <- precis(mfit_i, depth = 3, prob = .95, corr = TRUE)
# 
# rst_i <- mfit_i_out
# rst_i$combined <- paste0(round(rst_i$mean,2), " (", round(rst_i$sd,2), ")")
# 
# rst_i_corr <- mfit_i_corr
# rst_i_corr$combined <- paste0(round(rst_i_corr$mean,3), " (", round(rst_i_corr$sd,3), ")")
# 
# write.csv(rst_i_corr, "../output/model_i_corr.csv", row.names=T)
# write.csv(rst_i, "../output/model_i_random_effect.csv", row.names=T)
# 
# ####### Process results: mfit_iF ########
# load('../output/model_iF_0522.RData')
# mfit_iF_out <- precis(mfit_iF, depth = 2, prob = .95, corr = TRUE)
# mfit_iF_corr <- precis(mfit_iF, depth = 3, prob = .95, corr = TRUE)
# 
# rst_iF <- mfit_iF_out
# rst_iF$combined <- paste0(round(rst_iF$mean,2), " (", round(rst_iF$sd,2), ")")
# 
# rst_iF_corr <- mfit_iF_corr
# rst_iF_corr$combined <- paste0(round(rst_iF_corr$mean,3), " (", round(rst_iF_corr$sd,3), ")")
# 
# write.csv(rst_iF_corr, "../output/model_iF_corr.csv", row.names=T)
# write.csv(rst_iF, "../output/model_iF_random_effect.csv", row.names=T)
# 
# ####### Process results: mfit_ih ########
# load('../output/model_ih_0522.RData')
# mfit_ih_out <- precis(mfit_ih, depth = 2, prob = .95, corr = TRUE)
# mfit_ih_corr <- precis(mfit_ih, depth = 3, prob = .95, corr = TRUE)
# 
# rst_ih <- mfit_ih_out
# rst_ih$combined <- paste0(round(rst_ih$mean,2), " (", round(rst_ih$sd,2), ")")
# 
# rst_ih_corr <- mfit_ih_corr
# rst_ih_corr$combined <- paste0(round(rst_ih_corr$mean,3), " (", round(rst_ih_corr$sd,3), ")")
# 
# write.csv(rst_ih_corr, "../output/model_ih_corr.csv", row.names=T)
# write.csv(rst_ih, "../output/model_ih_random_effect.csv", row.names=T)

####### Process results: mfit_ihF ########
load('../output/model_ihF_0522.RData')
mfit_ihF_out <- precis(mfit_ihF, depth = 2, prob = .95, corr = TRUE)
mfit_ihF_corr <- precis(mfit_ihF, depth = 3, prob = .95, corr = TRUE)

rst_ihF <- mfit_ihF_out
rst_ihF$combined <- paste0(round(rst_ihF$mean,2), " (", round(rst_ihF$sd,2), ")")

rst_ihF_corr <- mfit_ihF_corr
rst_ihF_corr$combined <- paste0(round(rst_ihF_corr$mean,3), " (", round(rst_ihF_corr$sd,3), ")")

write.csv(rst_ihF_corr, "../output/model_ihF_corr.csv", row.names=T)
write.csv(rst_ihF, "../output/model_ihF_random_effect.csv", row.names=T)
