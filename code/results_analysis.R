library(rethinking)
library(qgraph)
library(tidyverse)
library(rstan)
library(ggpubr)
rm(list = ls())
theme_set(theme_bw())

########## load output RData ######
load('../output/model_i_0522_sd_1.RData')
load('../output/model_iF_0522_sd_1.RData')
load('../output/model_ih_0522_sd_1.RData')
load('../output/model_ihF_0522_sd_1.RData')
rm(post_ih)
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


################### Priors vs Posteriors #######################################
post_i <- extract.samples(mfit_i)
post_iF <- extract.samples(mfit_iF)
post_ih <- extract.samples(mfit_ih)
post_ihF <- extract.samples(mfit_ihF)

tiff("../output/figure/posterior_intercepts.tiff", units="in", width=6, height=6, res=300)
p1 <- stan_plot(mfit_i, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "mfit_i") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7))

p2 <- stan_plot(mfit_ih, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "mfit_ih") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7))

p3 <- stan_plot(mfit_iF, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "mfit_iF") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7))

p4 <- stan_plot(mfit_ihF, point_est = "median", pars = "a", show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "mfit_ihF") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7))

plot<- ggarrange(p1,p2,p3,p4, ncol=2, nrow=2)

annotate_figure(plot, top = text_grob("Posterior Intercepts", 
                                      color = "black", face = "bold", size = 9))
dev.off()


tiff("../output/figure/posterior_fixeff.tiff", units="in", width=6, height=8, res=300)
p1 <- stan_plot(mfit_iF, point_est = "median", 
                pars = c("bA","bB", "bI", "bR", "bAI", "bBR", "bH"),
                show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "mfit_iF") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 3),
        axis.text.x = element_text(size = 7))

p2 <- stan_plot(mfit_ihF, point_est = "median", pars = c("bA","bB", "bI", "bR", "bAI", "bBR", "bH"), show_density = TRUE, fill_color = "maroon") + 
  labs(subtitle = "mfit_ihF") +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 3),
        axis.text.x = element_text(size = 7))
plot<- ggarrange(p1,p2, ncol=2, nrow=1)

annotate_figure(plot, top = text_grob("Posterior Coefficients for Fixed Effects", 
                                      color = "black", face = "bold", size = 9))
dev.off()

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

############ Create heatmaps of correlation ####################################

## heatmap: i correlation #####
rst_i_init_corr <- rst_i_corr[startsWith(rst_i_corr$X,"Rho_init_id"),]
rst_i_recp_corr <- rst_i_corr[startsWith(rst_i_corr$X,"Rho_recp_id"),]
mtx_i_corr <- matrix(0, nrow = K-1 , ncol = K-1)
colnames(mtx_i_corr) <- paste0("(",c(1:(K-1)),")")
rownames(mtx_i_corr) <- paste0("(",c(1:(K-1)),")")
diag(mtx_i_corr) <- 1

tbl_i_corr <- data.frame(matrix(1, nrow = K-1 , ncol = K-1))
colnames(tbl_i_corr) <- paste0("(",c(1:(K-1)),")")
rownames(tbl_i_corr) <- paste0("(",c(1:(K-1)),")")

signif_i <- c()
count <- 0 
for (i in 1:(nrow(mtx_i_corr)-1)) {
  corr_init <- rst_i_init_corr[((K-1)*(i-1)+1):((K-1)*i),]
  mtx_i_corr[(i+1):(K-1),i] <- as.numeric(corr_init$mean[(i+1):(K-1)])
  tbl_i_corr[(i+1):(K-1),i] <- corr_init$combined[(i+1):(K-1)]
  tmp <- corr_init[(i+1):(K-1),]
  for (j in 1:nrow(tmp)) {
    if((tmp$X2.5.[j]<0 & tmp$X97.5.[j]<0)|(tmp$X2.5.[j]>0 & tmp$X97.5.[j]>0)){
      count = count+1
      signif_i[count] <- tmp[j,"X"]
    }
  }
  
  corr_recp <- rst_i_recp_corr[((K-1)*(i-1)+1):((K-1)*i),]
  mtx_i_corr[i,(i+1):(K-1)] <- as.numeric(corr_recp$mean[(i+1):(K-1)])
  tbl_i_corr[i,(i+1):(K-1)] <- corr_recp$combined[(i+1):(K-1)]
  tmp <- corr_recp[(i+1):(K-1),]
  for (j in 1:nrow(tmp)) {
    if((tmp$X2.5.[j]<0 & tmp$X97.5.[j]<0)|(tmp$X2.5.[j]>0 & tmp$X97.5.[j]>0)){
      count = count+1
      signif_i[count] <- tmp[j,"X"]
    }
  }
}
signif_i
rst_i_corr[rst_i_corr$X %in% signif_i, c("X","combined")]

rownames(tbl_i_corr) <- colnames(tbl_i_corr) <- paste0("(",1:18,")")

keep_parentheses_in_new_line <- function(x) {
  gsub(" \\((.*?)\\)", "\n(\\1)", x)
}

tbl_i_corr <- apply(tbl_i_corr, c(1,2), keep_parentheses_in_new_line)
tbl_i_corr <- data.frame(tbl_i_corr)
write.csv(tbl_i_corr, "../output/table/tbl_i_corr.csv", row.names=T)

colnames(mtx_i_corr) <- rownames(mtx_i_corr) <- categories$Behavior[1:18]

tiff("../output/figure/heatmap_i.tiff", units="in", width=5, height=6, res=300)
par(mar=c(7,4,4,2)) 
library(gplots)
my_palette <- colorRampPalette(c("blue", "white", "red"))(n = 300)
col_breaks = c(seq(-1,0,length=151)[-151], 
               0,              
               seq(0,1,length=151)[-1])  
lmat = rbind(c(0, 3, 0), c(2, 1, 0), c(0, 4, 0))
lwid = c(0.6, 4, 0.6)
lhei = c(0.43, 2.5, 0.6)
heatmap_i <- heatmap.2(mtx_i_corr, 
                       density.info="none",  
                       trace="none",   
                       col=my_palette,       
                       breaks=col_breaks,   
                       dendrogram='none',     
                       Rowv=FALSE,
                       Colv=FALSE,
                       cexRow = 1,
                       cexCol = 1,
                       srtCol=90,   
                       adjCol = c(1,0),
                       srtRow=0,
                       adjRow=c(0, 1),
                       key.title = NA,
                       key.par = list(mar=c(4.5,0, 1.8, 5.9),cex.axis = 0.8, cex.lab = 1),
                       symm=F,
                       symkey=T,
                       symbreaks=T, 
                       key.xlab = "Correlation",
                       margins = c(10, 7),
                       lmat = lmat, 
                       lwid = lwid, 
                       lhei = lhei,
                       main="mfit_i")
heatmap_i
dev.off()

tiff("../output/figure/signif_corr_init_i.tiff", units="in", width=5, height=6, res=300)
signif_list <- signif_i[startsWith(signif_i, "Rho_init")]
signif_df <- NULL

for(i in signif_list){
  corr <- rst_i_corr[rst_i_corr$X == i,"mean"]
  numbers <- str_extract_all(i, "\\d+")
  numbers <- as.numeric(numbers[[1]])
  
  signif_df <- rbind(signif_df, c(numbers,corr))
}

signif_df <- as.data.frame(signif_df)
signif_df$V1 <- categories$Behavior[signif_df$V1]
signif_df$V2 <- categories$Behavior[signif_df$V2]
behaviors_signif <- unique(c(signif_df[,1], signif_df[,2]))[order(unique(c(signif_df[,1], signif_df[,2])))]
signif_mtx <- matrix(0, nrow = length(behaviors_signif), ncol = length(behaviors_signif))
colnames(signif_mtx) <- rownames(signif_mtx) <- behaviors_signif
for(i in 1:length(signif_list)){
  signif_mtx[as.character(signif_df[i,1]),as.character(signif_df[i,2])] <- as.numeric(signif_df[i,3])
}

list1 <- c(1,2,6)
list2 <- c(3,4,5,7)
grp <- list(list1, list2)
qgraph(round(signif_mtx,3), 
       title = "(a) Initiator’s individual-level random effects from mfit_i", 
       title.cex = 1, 
       shape="rectangle", borders = F, node.width = 2.1, node.height = 0.9, vTrans = 70, 
       layout="groups", groups = grp, color = c("#c9c9ff", "#ffe0bd"),
       labels = c("(2) Dominating", "(4) Leading", "(6) Physical\nAggression", "(8) Requesting\nComfort",
                  "(9) Requesting\nHelp", "(11) Scolding", "(18) Verbal\nAggression"), 
       label.cex = 0.76, label.scale = F, label.font = 2,
       theme = "Borkulo", arrows = F, curveAll = T, curve = 1, 
       esize = 2, edge.width = 2,
       edge.labels = T, edge.label.cex = 0.95, edge.label.font = 2,
       edge.label.position = 0.4, edge.label.margin = 0.02)

dev.off()



tiff("../output/figure/signif_corr_recp_i.tiff", units="in", width=5, height=5, res=300)
signif_list <- signif_i[startsWith(signif_i, "Rho_recp")]
signif_df <- NULL

for(i in signif_list){
  corr <- rst_i_corr[rst_i_corr$X == i,"mean"]
  numbers <- str_extract_all(i, "\\d+")
  numbers <- as.numeric(numbers[[1]])
  
  signif_df <- rbind(signif_df, c(numbers,corr))
}

signif_df <- as.data.frame(signif_df)
signif_df$V1 <- categories$Behavior[signif_df$V1]
signif_df$V2 <- categories$Behavior[signif_df$V2]
behaviors_signif <- unique(c(signif_df[,1], signif_df[,2]))[order(unique(c(signif_df[,1], signif_df[,2])))]
signif_mtx <- matrix(0, nrow = length(behaviors_signif), ncol = length(behaviors_signif))
colnames(signif_mtx) <- rownames(signif_mtx) <- behaviors_signif
for(i in 1:length(signif_list)){
  signif_mtx[as.character(signif_df[i,1]),as.character(signif_df[i,2])] <- as.numeric(signif_df[i,3])
}

qgraph(signif_mtx, 
       title = "(c) Recipient’s individual-level random effects from mfit_i", 
       title.cex = 1, 
       shape="rectangle", borders = F, node.width = 2.1, node.height = 0.9, vTrans = 70, 
       layout="circle", color = c("#c9c9ff","#c9c9ff","#ffe0bd"),
       labels = c("(1) Comforting", "(2) Dominating", "(7) Requesting\nAccess"), 
       label.cex = 0.76, label.scale = F, label.font = 2,
       theme = "Borkulo", arrows = F, curveAll = F, curve = 1, 
       esize = 2, edge.width = 2,
       edge.labels = T, edge.label.cex = 1, edge.label.font = 2,
       edge.label.position = 0.6, edge.label.margin = 0.02)
dev.off()


## heatmap: iF correlation #####
rst_iF_init_corr <- rst_iF_corr[startsWith(rst_iF_corr$X,"Rho_init_id"),]
rst_iF_recp_corr <- rst_iF_corr[startsWith(rst_iF_corr$X,"Rho_recp_id"),]
mtx_iF_corr <- matrix(0, nrow = K-1 , ncol = K-1)
colnames(mtx_iF_corr) <- paste0("(",c(1:(K-1)),")")
rownames(mtx_iF_corr) <- paste0("(",c(1:(K-1)),")")
diag(mtx_iF_corr) <- 1

tbl_iF_corr <- data.frame(matrix(1, nrow = K-1 , ncol = K-1))
colnames(tbl_iF_corr) <- paste0("(",c(1:(K-1)),")")
rownames(tbl_iF_corr) <- paste0("(",c(1:(K-1)),")")

signif_iF <- c()
count <- 0 
for (i in 1:(nrow(mtx_iF_corr)-1)) {
  corr_init <- rst_iF_init_corr[((K-1)*(i-1)+1):((K-1)*i),]
  mtx_iF_corr[(i+1):(K-1),i] <- as.numeric(corr_init$mean[(i+1):(K-1)])
  tbl_iF_corr[(i+1):(K-1),i] <- corr_init$combined[(i+1):(K-1)]
  tmp <- corr_init[(i+1):(K-1),]
  for (j in 1:nrow(tmp)) {
    if((tmp$X2.5.[j]<0 & tmp$X97.5.[j]<0)|(tmp$X2.5.[j]>0 & tmp$X97.5.[j]>0)){
      count = count+1
      signif_iF[count] <- tmp[j,"X"]
    }
  }
  
  corr_recp <- rst_iF_recp_corr[((K-1)*(i-1)+1):((K-1)*i),]
  mtx_iF_corr[i,(i+1):(K-1)] <- as.numeric(corr_recp$mean[(i+1):(K-1)])
  tbl_iF_corr[i,(i+1):(K-1)] <- corr_recp$combined[(i+1):(K-1)]
  tmp <- corr_recp[(i+1):(K-1),]
  for (j in 1:nrow(tmp)) {
    if((tmp$X2.5.[j]<0 & tmp$X97.5.[j]<0)|(tmp$X2.5.[j]>0 & tmp$X97.5.[j]>0)){
      count = count+1
      signif_iF[count] <- tmp[j,"X"]
    }
  }
}
signif_iF
rownames(tbl_iF_corr) <- colnames(tbl_iF_corr) <- paste0("(",1:18,")")

tbl_iF_corr <- apply(tbl_iF_corr, c(1,2), keep_parentheses_in_new_line)
tbl_iF_corr <- data.frame(tbl_iF_corr)

write.csv(tbl_iF_corr, "../output/table/tbl_iF_corr.csv", row.names=T)

colnames(mtx_iF_corr) <- rownames(mtx_iF_corr) <- categories$Behavior[1:18]

tiff("../output/figure/heatmap_iF.tiff", units="in", width=5, height=6, res=300)
par(mar=c(7,4,4,2)+0.1) 
library(gplots)
my_palette <- colorRampPalette(c("blue", "white", "red"))(n = 300)
col_breaks = c(seq(-1,0,length=151)[-151], 
               0,              
               seq(0,1,length=151)[-1])  
lmat = rbind(c(0, 3, 0), c(2, 1, 0), c(0, 4, 0))
lwid = c(0.6, 4, 0.6)
lhei = c(0.43, 2.5, 0.6)
heatmap_i <- heatmap.2(mtx_iF_corr, 
                       density.info="none",  
                       trace="none",   
                       col=my_palette,       
                       breaks=col_breaks,   
                       dendrogram='none',     
                       Rowv=FALSE,
                       Colv=FALSE,
                       cexRow = 1,
                       cexCol = 1,
                       srtCol=90,   
                       adjCol = c(1,0),
                       srtRow=0,
                       adjRow=c(0, 1),
                       key.title = NA,
                       key.par = list(mar=c(4.5,0, 1.8, 5.9),cex.axis = 0.8, cex.lab = 1),
                       symm=F,
                       symkey=T,
                       symbreaks=T, 
                       key.xlab = "Correlation",
                       margins = c(10, 7),
                       lmat = lmat, 
                       lwid = lwid, 
                       lhei = lhei,
                       main="mfit_iF")
heatmap_i
dev.off()

## heatmap: ih correlation #####
rst_ih_init_corr <- rst_ih_corr[startsWith(rst_ih_corr$X,"Rho_init_id"),]
rst_ih_recp_corr <- rst_ih_corr[startsWith(rst_ih_corr$X,"Rho_recp_id"),]
mtx_ih_corr <- matrix(0, nrow = K-1 , ncol = K-1)
colnames(mtx_ih_corr) <- paste0("(",c(1:(K-1)),")")
rownames(mtx_ih_corr) <- paste0("(",c(1:(K-1)),")")
diag(mtx_ih_corr) <- 1

tbl_ih_corr <- data.frame(matrix(1, nrow = K-1 , ncol = K-1))
colnames(tbl_ih_corr) <- paste0("(",c(1:(K-1)),")")
rownames(tbl_ih_corr) <- paste0("(",c(1:(K-1)),")")

signif_ih <- c()
count <- 0 
for (i in 1:(nrow(mtx_ih_corr)-1)) {
  corr_init <- rst_ih_init_corr[((K-1)*(i-1)+1):((K-1)*i),]
  mtx_ih_corr[(i+1):(K-1),i] <- as.numeric(corr_init$mean[(i+1):(K-1)])
  tbl_ih_corr[(i+1):(K-1),i] <- corr_init$combined[(i+1):(K-1)]
  tmp <- corr_init[(i+1):(K-1),]
  for (j in 1:nrow(tmp)) {
    if((tmp$X2.5.[j]<0 & tmp$X97.5.[j]<0)|(tmp$X2.5.[j]>0 & tmp$X97.5.[j]>0)){
      count = count+1
      signif_ih[count] <- tmp[j,"X"]
    }
  }
  
  corr_recp <- rst_ih_recp_corr[((K-1)*(i-1)+1):((K-1)*i),]
  mtx_ih_corr[i,(i+1):(K-1)] <- as.numeric(corr_recp$mean[(i+1):(K-1)])
  tbl_ih_corr[i,(i+1):(K-1)] <- corr_recp$combined[(i+1):(K-1)]
  tmp <- corr_recp[(i+1):(K-1),]
  for (j in 1:nrow(tmp)) {
    if((tmp$X2.5.[j]<0 & tmp$X97.5.[j]<0)|(tmp$X2.5.[j]>0 & tmp$X97.5.[j]>0)){
      count = count+1
      signif_ih[count] <- tmp[j,"X"]
    }
  }
}
signif_ih
rst_ih_corr[rst_ih_corr$X %in% signif_ih, c("X","combined")]

rownames(tbl_ih_corr) <- colnames(tbl_ih_corr) <- paste0("(",1:18,")")
tbl_ih_corr <- apply(tbl_ih_corr, c(1,2), keep_parentheses_in_new_line)
tbl_ih_corr <- data.frame(tbl_ih_corr)
write.csv(tbl_ih_corr, "../output/table/tbl_ih_corr.csv", row.names=T)

colnames(mtx_ih_corr) <- rownames(mtx_ih_corr) <- categories$Behavior[1:18]

tiff("../output/figure/heatmap_ih.tiff", units="in", width=5, height=6, res=300)
par(mar=c(7,4,4,2)+0.1) 
library(gplots)
my_palette <- colorRampPalette(c("blue", "white", "red"))(n = 300)
col_breaks = c(seq(-1,0,length=151)[-151], 
               0,              
               seq(0,1,length=151)[-1])  
lmat = rbind(c(0, 3, 0), c(2, 1, 0), c(0, 4, 0))
lwid = c(0.6, 4, 0.6)
lhei = c(0.43, 2.5, 0.6)
heatmap_i <- heatmap.2(mtx_ih_corr, 
                       density.info="none",  
                       trace="none",   
                       col=my_palette,       
                       breaks=col_breaks,   
                       dendrogram='none',     
                       Rowv=FALSE,
                       Colv=FALSE,
                       cexRow = 1,
                       cexCol = 1,
                       srtCol=90,   
                       adjCol = c(1,0),
                       srtRow=0,
                       adjRow=c(0, 1),
                       key.title = NA,
                       key.par = list(mar=c(4.5,0, 1.8, 5.9),cex.axis = 0.8, cex.lab = 1),
                       symm=F,
                       symkey=T,
                       symbreaks=T, 
                       key.xlab = "Correlation",
                       margins = c(10, 7),
                       lmat = lmat, 
                       lwid = lwid, 
                       lhei = lhei,
                       main="mfit_ih")
heatmap_i
dev.off()

tiff("../output/figure/signif_corr_init_ih.tiff", units="in", width=5, height=6, res=300)
signif_list <- signif_ih[startsWith(signif_ih, "Rho_init")]
signif_df <- NULL

for(i in signif_list){
  corr <- rst_ih_corr[rst_ih_corr$X == i,"mean"]
  numbers <- str_extract_all(i, "\\d+")
  numbers <- as.numeric(numbers[[1]])
  
  signif_df <- rbind(signif_df, c(numbers,corr))
}

signif_df <- as.data.frame(signif_df)
signif_df$V1 <- categories$Behavior[signif_df$V1]
signif_df$V2 <- categories$Behavior[signif_df$V2]
behaviors_signif <- unique(c(signif_df[,1], signif_df[,2]))[order(unique(c(signif_df[,1], signif_df[,2])))]
signif_mtx <- matrix(0, nrow = length(behaviors_signif), ncol = length(behaviors_signif))
colnames(signif_mtx) <- rownames(signif_mtx) <- behaviors_signif
for(i in 1:length(signif_list)){
  signif_mtx[as.character(signif_df[i,1]),as.character(signif_df[i,2])] <- as.numeric(signif_df[i,3])
}

list1 <- c(1,2,6)
list2 <- c(3,4,5,7)
grp <- list(list1, list2)
qgraph(signif_mtx, 
       title = "(b) Initiator’s individual-level random effects from mfit_ih", 
       title.cex = 1, 
       shape="rectangle", borders = F, node.width = 2.1, node.height = 0.9, vTrans = 70, 
       layout="groups", groups = grp, color = c("#c9c9ff", "#ffe0bd"),
       labels = c("(2) Dominating", "(4) Leading", "(6) Physical\nAggression", "(8) Requesting\nComfort",
                  "(9) Requesting\nHelp", "(11) Scolding", "(18) Verbal\nAggression"), 
       label.cex = 0.76, label.scale = F, label.font = 2,
       theme = "Borkulo", arrows = F, curveAll = T, curve = 1, 
       esize = 2, edge.width = 2,
       edge.labels = T, edge.label.cex = 0.95, edge.label.font = 2,
       edge.label.position = 0.4, edge.label.margin = 0.02)
dev.off()


tiff("../output/figure/signif_corr_recp_ih.tiff", units="in", width=5, height=5, res=300)
signif_list <- signif_ih[startsWith(signif_ih, "Rho_recp")]
signif_df <- NULL

for(i in signif_list){
  corr <- rst_ih_corr[rst_ih_corr$X == i,"mean"]
  numbers <- str_extract_all(i, "\\d+")
  numbers <- as.numeric(numbers[[1]])
  
  signif_df <- rbind(signif_df, c(numbers,corr))
}

signif_df <- as.data.frame(signif_df)
signif_df$V1 <- categories$Behavior[signif_df$V1]
signif_df$V2 <- categories$Behavior[signif_df$V2]
behaviors_signif <- unique(c(signif_df[,1], signif_df[,2]))[order(unique(c(signif_df[,1], signif_df[,2])))]
signif_mtx <- matrix(0, nrow = length(behaviors_signif), ncol = length(behaviors_signif))
colnames(signif_mtx) <- rownames(signif_mtx) <- behaviors_signif
for(i in 1:length(signif_list)){
  signif_mtx[as.character(signif_df[i,1]),as.character(signif_df[i,2])] <- as.numeric(signif_df[i,3])
}

qgraph(signif_mtx, 
       title = "(d) Recipient’s individual-level random effects from mfit_ih", 
       title.cex = 1, 
       shape="rectangle", borders = F, node.width = 2.1, node.height = 0.9, vTrans = 70, 
       layout="circle", color = c("#c9c9ff","#c9c9ff","#ffe0bd"),
       labels = c("(1) Comforting", "(2) Dominating", "(7) Requesting\nAccess"), 
       label.cex = 0.76, label.scale = F, label.font = 2,
       theme = "Borkulo", arrows = F, curveAll = F, curve = 1, 
       esize = 2, edge.width = 2,
       edge.labels = T, edge.label.cex = 1, edge.label.font = 2,
       edge.label.position = 0.6, edge.label.margin = 0.02)
dev.off()


## heatmap: ihF correlation #####
rst_ihF_init_corr <- rst_ihF_corr[startsWith(rst_ihF_corr$X,"Rho_init_id"),]
rst_ihF_recp_corr <- rst_ihF_corr[startsWith(rst_ihF_corr$X,"Rho_recp_id"),]
mtx_ihF_corr <- matrix(0, nrow = K-1 , ncol = K-1)
colnames(mtx_ihF_corr) <- paste0("(",c(1:(K-1)),")")
rownames(mtx_ihF_corr) <- paste0("(",c(1:(K-1)),")")
diag(mtx_ihF_corr) <- 1

tbl_ihF_corr <- data.frame(matrix(1, nrow = K-1 , ncol = K-1))
colnames(tbl_ihF_corr) <- paste0("(",c(1:(K-1)),")")
rownames(tbl_ihF_corr) <- paste0("(",c(1:(K-1)),")")

signif_ihF <- c()
count <- 0 
for (i in 1:(nrow(mtx_ihF_corr)-1)) {
  corr_init <- rst_ihF_init_corr[((K-1)*(i-1)+1):((K-1)*i),]
  mtx_ihF_corr[(i+1):(K-1),i] <- as.numeric(corr_init$mean[(i+1):(K-1)])
  tbl_ihF_corr[(i+1):(K-1),i] <- corr_init$combined[(i+1):(K-1)]
  tmp <- corr_init[(i+1):(K-1),]
  for (j in 1:nrow(tmp)) {
    if((tmp$X2.5.[j]<0 & tmp$X97.5.[j]<0)|(tmp$X2.5.[j]>0 & tmp$X97.5.[j]>0)){
      count = count+1
      signif_ihF[count] <- tmp[j,"X"]
    }
  }
  
  corr_recp <- rst_ihF_recp_corr[((K-1)*(i-1)+1):((K-1)*i),]
  mtx_ihF_corr[i,(i+1):(K-1)] <- as.numeric(corr_recp$mean[(i+1):(K-1)])
  tbl_ihF_corr[i,(i+1):(K-1)] <- corr_recp$combined[(i+1):(K-1)]
  tmp <- corr_recp[(i+1):(K-1),]
  for (j in 1:nrow(tmp)) {
    if((tmp$X2.5.[j]<0 & tmp$X97.5.[j]<0)|(tmp$X2.5.[j]>0 & tmp$X97.5.[j]>0)){
      count = count+1
      signif_ihF[count] <- tmp[j,"X"]
    }
  }
}

signif_ihF
tbl_ihF_corr <- apply(tbl_ihF_corr, c(1,2), keep_parentheses_in_new_line)
tbl_ihF_corr <- data.frame(tbl_ihF_corr)
write.csv(tbl_ihF_corr, "../output/table/tbl_ihF_corr.csv", row.names = TRUE)

colnames(mtx_ihF_corr) <- rownames(mtx_ihF_corr) <- categories$Behavior[1:18]

tiff("../output/figure/heatmap_ihF.tiff", units="in", width=5, height=6, res=300)
par(mar=c(7,4,4,2)+0.1) 
library(gplots)
my_palette <- colorRampPalette(c("blue", "white", "red"))(n = 300)
col_breaks = c(seq(-1,0,length=151)[-151], 
               0,              
               seq(0,1,length=151)[-1])  
lmat = rbind(c(0, 3, 0), c(2, 1, 0), c(0, 4, 0))
lwid = c(0.6, 4, 0.6)
lhei = c(0.43, 2.5, 0.6)
heatmap_i <- heatmap.2(mtx_ihF_corr, 
                       density.info="none",  
                       trace="none",   
                       col=my_palette,       
                       breaks=col_breaks,   
                       dendrogram='none',     
                       Rowv=FALSE,
                       Colv=FALSE,
                       cexRow = 1,
                       cexCol = 1,
                       srtCol=90,   
                       adjCol = c(1,0),
                       srtRow=0,
                       adjRow=c(0, 1),
                       key.title = NA,
                       key.par = list(mar=c(4.5,0, 1.8, 5.9),cex.axis = 0.8, cex.lab = 1),
                       symm=F,
                       symkey=T,
                       symbreaks=T, 
                       key.xlab = "Correlation",
                       margins = c(10, 7),
                       lmat = lmat, 
                       lwid = lwid, 
                       lhei = lhei,
                       main="mfit_ihF")
heatmap_i
dev.off()

par(mfrow=c(1,1))

############ Calculate Predicted probability ###################################
## Predicted probability: mfit_i ########
link.mn_i <- function( data ) {
  K <- dim(post_i$v_init_id)[3] + 1
  ns <- dim(post_i$v_init_id)[1]
  if ( missing(data) ) stop( "BOOM: Need data argument" )
  n <- 1
  
  softmax2 <- function(x) {
    x <- max(x) - x
    exp(-x)/sum(exp(-x))
  }
  
  p <- list()
  
  for ( i in 1:n ) {
    p[[i]] <- sapply( 1:K , function(k) {
      if ( k < K ) {
        ptemp <- post_i$a[,k]
      } else {
        ptemp <- rep(0,ns)
      }
      return(ptemp)
    })
    for ( s in 1:ns ) p[[i]][s,] <- softmax2( p[[i]][s,] )
  }
  return(p)
}

pred_dat <- data.frame(
  init_id = 0
)

p <- link.mn_i ( pred_dat )
p_mean_i <- round(sapply( 1:length(p) , function(i) apply(p[[i]],2,mean) ), 3)

g <- df %>%
  group_by(y) %>%
  summarise(y = n()) %>%
  mutate(freq = round(y / sum(y), 3))
g$pred_i <- p_mean_i

## Predicted probability: mfit_ih ########
link.mn_ih <- function( data ) {
  K <- dim(post_ih$v_init_id)[3] + 1
  ns <- dim(post_ih$v_init_id)[1]
  if ( missing(data) ) stop( "BOOM: Need data argument" )
  n <- 1
  
  softmax2 <- function(x) {
    x <- max(x) - x
    exp(-x)/sum(exp(-x))
  }
  
  p <- list()
  
  for ( i in 1:n ) {
    p[[i]] <- sapply( 1:K , function(k) {
      if ( k < K ) {
        ptemp <- post_ih$a[,k]
      } else {
        ptemp <- rep(0,ns)
      }
      return(ptemp)
    })
    for ( s in 1:ns ) p[[i]][s,] <- softmax2( p[[i]][s,] )
  }
  return(p)
}

pred_dat <- data.frame(
  init_id = 0
)

p <- link.mn_ih ( pred_dat )
p_mean_ih <- round(sapply( 1:length(p) , function(i) apply(p[[i]],2,mean) ), 3)

g$pred_ih <- p_mean_ih

g <- g %>%
  pivot_longer(cols = c(pred_i, pred_ih))
colnames(g)[4] <- "pred"

tiff("../output/figure/pred_vs_obs_prop.tiff", units="in", width=6, height=3, res=300)
ggplot(g, aes(x = freq, y = pred)) +
  geom_point(col = yarrr::transparent("black", trans.val = .5)) + 
  geom_smooth(method="lm", se=F, color = "red", size = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Observed frequency", 
       y="Predicted probability") + 
  theme(axis.title.x = element_text(size=10), 
        axis.title.y= element_text(size=10),
        axis.text = element_text(color = "black")) +
  facet_wrap(~name,
             labeller = labeller(name = c("pred_i" = "mfit_i",
                                          "pred_ih" = "mfit_ih")))
dev.off()

## Predicted probability: mfit_iF ########
link.mn_iF <- function( data ) {
  K <- dim(post_iF$v_init_id)[3] + 1
  ns <- dim(post_iF$v_init_id)[1]
  if ( missing(data) ) stop( "BOOM: Need data argument" )
  n <- seq.length
  
  softmax2 <- function(x) {
    x <- max(x) - x
    exp(-x)/sum(exp(-x))
  }
  
  p <- list()
  
  for ( i in 1:n ) {
    p[[i]] <- sapply( 1:K , function(k) {
      if ( k < K ) {
        ptemp <- post_iF$a[,k] + 
          post_iF$bA[,k] * data$init_age_z[i] + 
          post_iF$bB[,k] * data$recp_age_z[i] + 
          post_iF$bI[,k] * data$init_g[i] + 
          post_iF$bR[,k] * data$recp_g[i] +
          post_iF$bAI[,k] * data$init_age_z[i] * data$init_g[i] +
          post_iF$bBR[,k] * data$recp_age_z[i] * data$recp_g[i] +
          post_iF$bH[,k] * data$same_house[i]
        if ( data$init_id[i]>0 & data$recp_id[i]>0 ) ptemp <- ptemp + post_iF$v_init_id[,data$init_id[i],k] +  post_iF$v_recp_id[,data$recp_id[i],k]
      } else {
        ptemp <- rep(0,ns)
      }
      return(ptemp)
    })
    for ( s in 1:ns ) p[[i]][s,] <- softmax2( p[[i]][s,] )
  }
  return(p)
}

## Predicted probability: mfit_ihF ########

link.mn_ihF <- function( data ) {
  K <- dim(post_ihF$v_init_id)[3] + 1
  ns <- dim(post_ihF$v_init_id)[1]
  if ( missing(data) ) stop( "BOOM: Need data argument" )
  n <- seq.length
  
  softmax2 <- function(x) {
    x <- max(x) - x
    exp(-x)/sum(exp(-x))
  }
  
  p <- list()
  
  for ( i in 1:n ) {
    p[[i]] <- sapply( 1:K , function(k) {
      if ( k < K ) {
        ptemp <- post_ihF$a[,k] + 
          post_ihF$bA[,k] * data$init_age_z[i] + 
          post_ihF$bB[,k] * data$recp_age_z[i] + 
          post_ihF$bI[,k] * data$init_g[i] + 
          post_ihF$bR[,k] * data$recp_g[i] +
          post_ihF$bAI[,k] * data$init_age_z[i] * data$init_g[i] +
          post_ihF$bBR[,k] * data$recp_age_z[i] * data$recp_g[i] +
          post_ihF$bH[,k] * data$same_house[i]
        if ( data$init_id[i]>0 & data$recp_id[i]>0 ) ptemp <- ptemp + post_ihF$v_init_id[,data$init_id[i],k] + post_ihF$v_recp_id[,data$recp_id[i],k]
        if ( data$house_init_id[i]>0 & data$house_recp_id[i]>0) ptemp <- ptemp + post_ihF$v_init_house[,data$house_init_id[i],k] + post_ihF$v_recp_house[,data$house_recp_id[i],k]
      } else {
        ptemp <- rep(0,ns)
      }
      return(ptemp)
    })
    for ( s in 1:ns ) p[[i]][s,] <- softmax2( p[[i]][s,] )
  }
  return(p)
}


############ Create plots for predicted probability with varying covariates ####
## Generating predictions by age of initiator: mfit_iF ####
seq.length <- 100
age_seq <- seq(from= min(df$init_age_z), to= max(df$init_age_z), length.out = seq.length)
preferred.ages <- c(0,2,4,6,8,10,12)
labels.at <- (preferred.ages - mean(df$Init_Age_At_Obs))/sd(df$Init_Age_At_Obs)

pred_dat_mm_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 0, # male
  recp_g = 0, # male
  same_house = 0 # in different household
)

pred_dat_mm_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 0, # male
  recp_g = 0, # male
  same_house = 1 # in same household
)

pred_dat_mf_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 0, # male
  recp_g = 1, # female
  same_house = 0 # in different household
)

pred_dat_mf_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 0, # male
  recp_g = 1, # female
  same_house = 1 # in same household
)

pred_dat_fm_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 1, # female
  recp_g = 0, # male
  same_house = 0 # in different household
)

pred_dat_fm_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 1, # female
  recp_g = 0, # male
  same_house = 1 # in same household
)

pred_dat_ff_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 1, # female
  recp_g = 1, # female
  same_house = 0 # in different household
)

pred_dat_ff_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 1, # female
  recp_g = 1, # female
  same_house = 1 # in same household
)

p_mm_dh <- link.mn_iF ( pred_dat_mm_dh )
p_mm_dh_mean <- sapply( 1:length(p_mm_dh) , function(i) apply(p_mm_dh[[i]],2,mean) )

p_mm_sh <- link.mn_iF ( pred_dat_mm_sh )
p_mm_sh_mean <- sapply( 1:length(p_mm_sh) , function(i) apply(p_mm_sh[[i]],2,mean) )

p_mf_dh <- link.mn_iF ( pred_dat_mf_dh )
p_mf_dh_mean <- sapply( 1:length(p_mf_dh) , function(i) apply(p_mf_dh[[i]],2,mean) )

p_mf_sh <- link.mn_iF ( pred_dat_mf_sh )
p_mf_sh_mean <- sapply( 1:length(p_mf_sh) , function(i) apply(p_mf_sh[[i]],2,mean) )

p_fm_dh <- link.mn_iF ( pred_dat_fm_dh )
p_fm_dh_mean <- sapply( 1:length(p_fm_dh) , function(i) apply(p_fm_dh[[i]],2,mean) )

p_fm_sh <- link.mn_iF ( pred_dat_fm_sh )
p_fm_sh_mean <- sapply( 1:length(p_fm_sh) , function(i) apply(p_fm_sh[[i]],2,mean) )

p_ff_dh <- link.mn_iF ( pred_dat_ff_dh )
p_ff_dh_mean <- sapply( 1:length(p_ff_dh) , function(i) apply(p_ff_dh[[i]],2,mean) )

p_ff_sh <- link.mn_iF ( pred_dat_ff_sh )
p_ff_sh_mean <- sapply( 1:length(p_ff_sh) , function(i) apply(p_ff_sh[[i]],2,mean) )

preferred.ages <- c(0,2,4,6,8,10,12)

p_PI = NULL
for(k in 1:length(categories$Behavior)){
  p_mm_PI <- sapply( 1:length(p_mm_sh) , function(i) PI(p_mm_sh[[i]][,k], prob=0.95) )
  p_mm_PI <- as.data.frame( cbind(t(p_mm_PI), "mm") )
  p_mf_PI <- sapply( 1:length(p_mf_sh) , function(i) PI(p_mf_sh[[i]][,k], prob=0.95) )
  p_mf_PI <- as.data.frame( cbind(t(p_mf_PI), "mf") )
  p_fm_PI <- sapply( 1:length(p_fm_sh) , function(i) PI(p_fm_sh[[i]][,k], prob=0.95) )
  p_fm_PI <- as.data.frame( cbind(t(p_fm_PI), "fm") )
  p_ff_PI <- sapply( 1:length(p_ff_sh) , function(i) PI(p_ff_sh[[i]][,k], prob=0.95) )
  p_ff_PI <- as.data.frame( cbind(t(p_ff_PI), "ff") )
  p_PI_k <- bind_rows(p_mm_PI, p_mf_PI, p_fm_PI, p_ff_PI)
  
  p_PI_k$x <- rep(age_seq, 4)
  p_PI_k$y <- c(p_mm_sh_mean[k,], p_mf_sh_mean[k,], p_fm_sh_mean[k,], p_ff_sh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Same household"
  
  p_PI = rbind(p_PI, p_PI_k)
  
  p_mm_PI <- sapply( 1:length(p_mm_dh) , function(i) PI(p_mm_dh[[i]][,k], prob=0.95) )
  p_mm_PI <- as.data.frame( cbind(t(p_mm_PI), "mm") )
  p_mf_PI <- sapply( 1:length(p_mf_dh) , function(i) PI(p_mf_dh[[i]][,k], prob=0.95) )
  p_mf_PI <- as.data.frame( cbind(t(p_mf_PI), "mf") )
  p_fm_PI <- sapply( 1:length(p_fm_dh) , function(i) PI(p_fm_dh[[i]][,k], prob=0.95) )
  p_fm_PI <- as.data.frame( cbind(t(p_fm_PI), "fm") )
  p_ff_PI <- sapply( 1:length(p_ff_dh) , function(i) PI(p_ff_dh[[i]][,k], prob=0.95) )
  p_ff_PI <- as.data.frame( cbind(t(p_ff_PI), "ff") )
  p_PI_k <- bind_rows(p_mm_PI, p_mf_PI, p_fm_PI, p_ff_PI)
  
  p_PI_k$x <- rep(age_seq, 4)
  p_PI_k$y <- c(p_mm_dh_mean[k,], p_mf_dh_mean[k,], p_fm_dh_mean[k,], p_ff_dh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Different household"
  
  p_PI = rbind(p_PI, p_PI_k)
}

p_PI$`3%` <- as.numeric(p_PI$`3%`)
p_PI$`98%` <- as.numeric(p_PI$`98%`)
# p_PI$category = paste0("(",categories$y[p_PI$category], ") ", categories$Behavior[p_PI$category]) 
# p_PI$category = ifelse(p_PI$category =="(19) Z", "(19) ownership assertion", p_PI$category)
p_PI$category = categories$Behavior[p_PI$category]
p_PI$label_both = paste0(p_PI$category, ", ", p_PI$samehouse)

tiff("../output/figure/predicted_initiator_age_iF.tiff", units="in", width=12, height=15, res=300)
par(mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))

ggplot(p_PI, aes(x = x, y = y, ymin = `3%`, ymax = `98%`, colour = V3, fill = V3)) +
  geom_line(linewidth=0.5) + 
  geom_ribbon(alpha=0.2, linetype = 0) +
  scale_colour_discrete("Sex", breaks=c("ff", "fm", "mf", "mm"),
                        labels=c("Female Initiator, Female Recipient ", 
                                 "Female Initiator, Male Recipient ", 
                                 "Male Initiator, Female Recipient ", 
                                 "Male Initiator, Male Recipient ")) +
  scale_fill_discrete("Sex", breaks=c("ff", "fm", "mf", "mm"),
                      labels=c("Female Initiator, Female Recipient ", 
                               "Female Initiator, Male Recipient ", 
                               "Male Initiator, Female Recipient ", 
                               "Male Initiator, Male Recipient ")) + 
  labs(x = "Initiator's Age (in years)", y = "Predicted Probability",
       color = "Sex", 
       fill = "Sex") +
  scale_x_continuous(breaks=labels.at, labels = preferred.ages) +
  facet_wrap(~factor(label_both, 
                     levels = paste0(rep(c(categories$Behavior), each = 2), ", ", 
                                     rep(c("Same household", "Different household"), 19))), 
             ncol = 4, scales = "free") + 
  theme(strip.text.x = element_text(size = 8),
        legend.position="bottom", legend.box = "vertical",
        legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size=9), 
        legend.text = element_text(size= 9), 
        axis.text = element_text(size= 7.5),
        axis.title.x = element_text(size= 9), axis.title.y= element_text(size= 9)) 

dev.off()


## Generating predictions by age of recipient: mfit_iF ####
seq.length <- 100

age_seq <- seq (from= min(df$recp_age_z), to= max(df$recp_age_z), length.out = seq.length)
preferred.ages <- c(0,2,4,6,8,10,12)
labels.at <- (preferred.ages - mean(df$Recp_Age_At_Obs))/sd(df$Recp_Age_At_Obs)

pred_dat_mm_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 0, # male
  recp_g = 0, # male
  same_house = 0 # in different household
)

pred_dat_mm_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 0, # male
  recp_g = 0, # male
  same_house = 1 # in same household
)

pred_dat_mf_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 0, # male
  recp_g = 1, # female
  same_house = 0 # in different household
)

pred_dat_mf_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 0, # male
  recp_g = 1, # female
  same_house = 1 # in same household
)

pred_dat_fm_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 1, # female
  recp_g = 0, # male
  same_house = 0 # in different household
)

pred_dat_fm_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 1, # female
  recp_g = 0, # male
  same_house = 1 # in same household
)

pred_dat_ff_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 1, # female
  recp_g = 1, # female
  same_house = 0 # in different household
)

pred_dat_ff_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 1, # female
  recp_g = 1, # female
  same_house = 1 # in same household
)

p_mm_dh <- link.mn_iF ( pred_dat_mm_dh )
p_mm_dh_mean <- sapply( 1:length(p_mm_dh) , function(i) apply(p_mm_dh[[i]],2,mean) )

p_mm_sh <- link.mn_iF ( pred_dat_mm_sh )
p_mm_sh_mean <- sapply( 1:length(p_mm_sh) , function(i) apply(p_mm_sh[[i]],2,mean) )

p_mf_dh <- link.mn_iF ( pred_dat_mf_dh )
p_mf_dh_mean <- sapply( 1:length(p_mf_dh) , function(i) apply(p_mf_dh[[i]],2,mean) )

p_mf_sh <- link.mn_iF ( pred_dat_mf_sh )
p_mf_sh_mean <- sapply( 1:length(p_mf_sh) , function(i) apply(p_mf_sh[[i]],2,mean) )

p_fm_dh <- link.mn_iF ( pred_dat_fm_dh )
p_fm_dh_mean <- sapply( 1:length(p_fm_dh) , function(i) apply(p_fm_dh[[i]],2,mean) )

p_fm_sh <- link.mn_iF ( pred_dat_fm_sh )
p_fm_sh_mean <- sapply( 1:length(p_fm_sh) , function(i) apply(p_fm_sh[[i]],2,mean) )

p_ff_dh <- link.mn_iF ( pred_dat_ff_dh )
p_ff_dh_mean <- sapply( 1:length(p_ff_dh) , function(i) apply(p_ff_dh[[i]],2,mean) )

p_ff_sh <- link.mn_iF ( pred_dat_ff_sh )
p_ff_sh_mean <- sapply( 1:length(p_ff_sh) , function(i) apply(p_ff_sh[[i]],2,mean) )

preferred.ages <- c(0,2,4,6,8,10,12)

p_PI = NULL
for(k in 1:length(categories$Behavior)){
  p_mm_PI <- sapply( 1:length(p_mm_sh) , function(i) PI(p_mm_sh[[i]][,k], prob=0.95) )
  p_mm_PI <- as.data.frame( cbind(t(p_mm_PI), "mm") )
  p_mf_PI <- sapply( 1:length(p_mf_sh) , function(i) PI(p_mf_sh[[i]][,k], prob=0.95) )
  p_mf_PI <- as.data.frame( cbind(t(p_mf_PI), "mf") )
  p_fm_PI <- sapply( 1:length(p_fm_sh) , function(i) PI(p_fm_sh[[i]][,k], prob=0.95) )
  p_fm_PI <- as.data.frame( cbind(t(p_fm_PI), "fm") )
  p_ff_PI <- sapply( 1:length(p_ff_sh) , function(i) PI(p_ff_sh[[i]][,k], prob=0.95) )
  p_ff_PI <- as.data.frame( cbind(t(p_ff_PI), "ff") )
  p_PI_k <- bind_rows(p_mm_PI, p_mf_PI, p_fm_PI, p_ff_PI)
  
  p_PI_k$x <- rep(age_seq, 4)
  p_PI_k$y <- c(p_mm_sh_mean[k,], p_mf_sh_mean[k,], p_fm_sh_mean[k,], p_ff_sh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Same household"
  
  p_PI = rbind(p_PI, p_PI_k)
  
  p_mm_PI <- sapply( 1:length(p_mm_dh) , function(i) PI(p_mm_dh[[i]][,k], prob=0.95) )
  p_mm_PI <- as.data.frame( cbind(t(p_mm_PI), "mm") )
  p_mf_PI <- sapply( 1:length(p_mf_dh) , function(i) PI(p_mf_dh[[i]][,k], prob=0.95) )
  p_mf_PI <- as.data.frame( cbind(t(p_mf_PI), "mf") )
  p_fm_PI <- sapply( 1:length(p_fm_dh) , function(i) PI(p_fm_dh[[i]][,k], prob=0.95) )
  p_fm_PI <- as.data.frame( cbind(t(p_fm_PI), "fm") )
  p_ff_PI <- sapply( 1:length(p_ff_dh) , function(i) PI(p_ff_dh[[i]][,k], prob=0.95) )
  p_ff_PI <- as.data.frame( cbind(t(p_ff_PI), "ff") )
  p_PI_k <- bind_rows(p_mm_PI, p_mf_PI, p_fm_PI, p_ff_PI)
  
  p_PI_k$x <- rep(age_seq, 4)
  p_PI_k$y <- c(p_mm_dh_mean[k,], p_mf_dh_mean[k,], p_fm_dh_mean[k,], p_ff_dh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Different household"
  
  p_PI = rbind(p_PI, p_PI_k)
}

p_PI$`3%` <- as.numeric(p_PI$`3%`)
p_PI$`98%` <- as.numeric(p_PI$`98%`)
# p_PI$category = paste0("(",categories$y[p_PI$category], ") ", categories$Behavior[p_PI$category]) 
# p_PI$category = ifelse(p_PI$category =="(19) Z", "(19) ownership assertion", p_PI$category)
p_PI$category = categories$Behavior[p_PI$category]
p_PI$label_both = paste0(p_PI$category, ", ", p_PI$samehouse)

tiff("../output/figure/predicted_recipient_age_iF.tiff", units="in", width=12, height=15, res=300)
par(mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))

ggplot(p_PI, aes(x = x, y = y, ymin = `3%`, ymax = `98%`, colour = V3, fill = V3)) +
  geom_line(linewidth=0.5) + 
  geom_ribbon(alpha=0.2, linetype = 0) +
  scale_colour_discrete("Sex", breaks=c("ff", "fm", "mf", "mm"),
                        labels=c("Female Initiator, Female Recipient ", 
                                 "Female Initiator, Male Recipient ", 
                                 "Male Initiator, Female Recipient ", 
                                 "Male Initiator, Male Recipient ")) +
  scale_fill_discrete("Sex", breaks=c("ff", "fm", "mf", "mm"),
                      labels=c("Female Initiator, Female Recipient ", 
                               "Female Initiator, Male Recipient ", 
                               "Male Initiator, Female Recipient ", 
                               "Male Initiator, Male Recipient ")) + 
  labs(x = "Recipient's Age (in years)", y = "Predicted Probability",
       color = "Sex", 
       fill = "Sex") +
  scale_x_continuous(breaks=labels.at, labels = preferred.ages) +
  facet_wrap(~factor(label_both, 
                     levels = paste0(rep(c(categories$Behavior), each = 2), ", ", 
                                     rep(c("Same household", "Different household"), 19))), 
             ncol = 4, scales = "free") + 

  theme(strip.text.x = element_text(size = 8),
        legend.position="bottom", legend.box = "vertical",
        legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size=9), 
        legend.text = element_text(size = 9), 
        axis.text = element_text(size = 7.5),
        axis.title.x = element_text(size = 9), axis.title.y= element_text(size = 9)) 


dev.off()

## Generating predictions by gender of initiator: mfit_iF ####
seq.length <- 2

pred_Xm_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = c(0,1),
  recp_g = 0, # male
  same_house = 1 # same household
)

pred_Xf_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = c(0,1),
  recp_g = 1, # female
  same_house = 1 # same household
)

pred_Xm_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = c(0,1),
  recp_g = 0, # male
  same_house = 0 # different household
)

pred_Xf_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = c(0,1),
  recp_g = 1, # female
  same_house = 0 # different household
)

p_Xm_sh <- link.mn_iF (pred_Xm_sh)
p_Xm_sh_mean <- sapply( 1:length(p_Xm_sh) , function(i) apply(p_Xm_sh[[i]],2,mean) )

p_Xf_sh <- link.mn_iF (pred_Xf_sh)
p_Xf_sh_mean <- sapply( 1:length(p_Xf_sh) , function(i) apply(p_Xf_sh[[i]],2,mean) )

p_Xm_dh <- link.mn_iF (pred_Xm_dh)
p_Xm_dh_mean <- sapply( 1:length(p_Xm_dh) , function(i) apply(p_Xm_dh[[i]],2,mean) )

p_Xf_dh <- link.mn_iF (pred_Xf_dh)
p_Xf_dh_mean <- sapply( 1:length(p_Xf_dh) , function(i) apply(p_Xf_dh[[i]],2,mean) )

p_PI = NULL
for(k in 1:length(categories$Behavior)){
  # same household
  p_Xm_PI <- sapply( 1:length(p_Xm_sh) , function(i) PI(p_Xm_sh[[i]][,k], prob=0.95) ) 
  p_Xm_PI <- as.data.frame( cbind(t(p_Xm_PI), "Xm") )
  p_Xf_PI <- sapply( 1:length(p_Xf_sh) , function(i) PI(p_Xf_sh[[i]][,k], prob=0.95) ) 
  p_Xf_PI <- as.data.frame( cbind(t(p_Xf_PI), "Xf") )
  
  p_PI_k <- rbind(p_Xm_PI, p_Xf_PI)
  p_PI_k$x <- rep(c("Male", "Female"), 2)
  p_PI_k$y <- c(p_Xm_sh_mean[k,], p_Xf_sh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Same household"
  
  p_PI = rbind(p_PI, p_PI_k)
  
  # different household
  p_Xm_PI <- sapply( 1:length(p_Xm_dh) , function(i) PI(p_Xm_dh[[i]][,k], prob=0.95) ) 
  p_Xm_PI <- as.data.frame( cbind(t(p_Xm_PI), "Xm") )
  p_Xf_PI <- sapply( 1:length(p_Xf_dh) , function(i) PI(p_Xf_dh[[i]][,k], prob=0.95) ) 
  p_Xf_PI <- as.data.frame( cbind(t(p_Xf_PI), "Xf") )
  
  p_PI_k <- rbind(p_Xm_PI, p_Xf_PI)
  p_PI_k$x <- rep(c("Male", "Female"), 2)
  p_PI_k$y <- c(p_Xm_dh_mean[k,], p_Xf_dh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Different household"
  
  p_PI = rbind(p_PI, p_PI_k)
}

p_PI$`3%` <- as.numeric(p_PI$`3%`)
p_PI$`98%` <- as.numeric(p_PI$`98%`)
# p_PI$category = paste0("(",categories$y[p_PI$category], ") ", categories$Behavior[p_PI$category]) 
# p_PI$category = ifelse(p_PI$category =="(19) Z", "(19) ownership assertion", p_PI$category)
p_PI$category = categories$Behavior[p_PI$category]
p_PI$label_both = paste0(p_PI$category, ", ", p_PI$samehouse)

tiff("../output/figure/predicted_initiator_sex_iF.tiff", units="in", width=12, height=15, res=300)
par(mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))
pd <- position_dodge(0.2)
ggplot(p_PI, aes(x=x, y=y, colour = V3)) + 
  geom_errorbar(aes(ymin=`3%`, ymax=`98%`), width=.1, position=pd) +
  geom_point(position=pd) +
  scale_color_discrete("Recipient's Sex", breaks=c("Xf", "Xm"),
                       labels=c("Female", "Male")) + 
  labs(x = "Initiator's Sex", y = "Predicted Probability",
       color = "Recipient's Sex") +
  facet_wrap(~factor(label_both, 
                     levels = paste0(rep(c(categories$Behavior), each = 2), ", ", 
                                     rep(c("Same household", "Different household"), 19))), 
             ncol = 4, scales = "free") + 
  theme(strip.text.x = element_text(size = 8),
        legend.position="bottom", legend.box = "vertical",
        legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size=9), 
        legend.text = element_text(size = 9), 
        axis.text = element_text(size = 7.5),
        axis.title.x = element_text(size = 9), axis.title.y= element_text(size = 9)) 
dev.off()


## Generating predictions by gender of recipient: mfit_iF ####
seq.length <- 2

pred_mX_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 0,  # male
  recp_g = c(0,1),
  same_house = 1
)

pred_fX_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 1,  # female
  recp_g = c(0,1),
  same_house = 1
)

pred_mX_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 0,  # male
  recp_g = c(0,1),
  same_house = 0
)

pred_fX_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 1,  # female
  recp_g = c(0,1),
  same_house = 0
)

p_mX_sh <- link.mn_iF (pred_mX_sh)
p_mX_sh_mean <- sapply( 1:length(p_mX_sh) , function(i) apply(p_mX_sh[[i]],2,mean) )

p_fX_sh <- link.mn_iF (pred_fX_sh)
p_fX_sh_mean <- sapply( 1:length(p_fX_sh) , function(i) apply(p_fX_sh[[i]],2,mean) )

p_mX_dh <- link.mn_iF (pred_mX_dh)
p_mX_dh_mean <- sapply( 1:length(p_mX_dh) , function(i) apply(p_mX_dh[[i]],2,mean) )

p_fX_dh <- link.mn_iF (pred_fX_dh)
p_fX_dh_mean <- sapply( 1:length(p_fX_dh) , function(i) apply(p_fX_dh[[i]],2,mean) )


p_PI = NULL
for(k in 1:length(categories$Behavior)){
  # same household
  p_mX_PI <- sapply( 1:length(p_mX_sh) , function(i) PI(p_mX_sh[[i]][,k], prob=0.95) ) 
  p_mX_PI <- as.data.frame( cbind(t(p_mX_PI), "mX") )
  p_fX_PI <- sapply( 1:length(p_fX_sh) , function(i) PI(p_fX_sh[[i]][,k], prob=0.95) ) 
  p_fX_PI <- as.data.frame( cbind(t(p_fX_PI), "fX") )
  
  p_PI_k <- rbind(p_mX_PI, p_fX_PI)
  p_PI_k$x <- rep(c("Male", "Female"), 2)
  p_PI_k$y <- c(p_mX_sh_mean[k,], p_fX_sh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Same household"
  
  p_PI = rbind(p_PI, p_PI_k)
  
  # different household
  p_mX_PI <- sapply( 1:length(p_mX_dh) , function(i) PI(p_mX_dh[[i]][,k], prob=0.95) ) 
  p_mX_PI <- as.data.frame( cbind(t(p_mX_PI), "mX") )
  p_fX_PI <- sapply( 1:length(p_fX_dh) , function(i) PI(p_fX_dh[[i]][,k], prob=0.95) ) 
  p_fX_PI <- as.data.frame( cbind(t(p_fX_PI), "fX") )
  
  p_PI_k <- rbind(p_mX_PI, p_fX_PI)
  p_PI_k$x <- rep(c("Male", "Female"), 2)
  p_PI_k$y <- c(p_mX_dh_mean[k,], p_fX_dh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Different household"
  
  p_PI = rbind(p_PI, p_PI_k)
}


p_PI$`3%` <- as.numeric(p_PI$`3%`)
p_PI$`98%` <- as.numeric(p_PI$`98%`)
# p_PI$category = paste0("(",categories$y[p_PI$category], ") ", categories$Behavior[p_PI$category]) 
# p_PI$category = ifelse(p_PI$category =="(19) Z", "(19) ownership assertion", p_PI$category)
p_PI$category = categories$Behavior[p_PI$category]
p_PI$label_both = paste0(p_PI$category, ", ", p_PI$samehouse)

tiff("../output/figure/predicted_recipient_sex_iF.tiff", units="in", width=12, height=15, res=300)
par(mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))
pd <- position_dodge(0.2)
ggplot(p_PI, aes(x=x, y=y, colour = V3)) + 
  geom_errorbar(aes(ymin=`3%`, ymax=`98%`), width=.1, position=pd) +
  geom_point(position=pd) +
  scale_color_discrete("Initiator's Sex", breaks=c("fX", "mX"),
                       labels=c("Female", "Male")) + 
  labs(x = "Recipient's Sex", y = "Predicted Probability",
       color = "Initiator's Sex") +
  facet_wrap(~factor(label_both, 
                     levels = paste0(rep(c(categories$Behavior), each = 2), ", ", 
                                     rep(c("Same household", "Different household"), 19))), 
             ncol = 4, scales = "free") + 
  theme(strip.text.x = element_text(size = 8),
        legend.position="bottom", legend.box = "vertical",
        legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size=9), 
        legend.text = element_text(size = 9), 
        axis.text = element_text(size = 7.5),
        axis.title.x = element_text(size = 9), axis.title.y= element_text(size = 9)) 
dev.off()


## Generating predictions by household status: mfit_iF ####
seq.length <- 2

pred_mm <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 0,  # male
  recp_g = 0,
  same_house = c(0,1)
)

pred_ff <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 1,  # female
  recp_g = 1,
  same_house = c(0,1)
)

pred_mf <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 0,  # male
  recp_g = 1,
  same_house = c(0,1)
)

pred_fm <- data.frame(
  init_id = 0,
  recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 1,  # female
  recp_g = 0,
  same_house = c(0,1) 
)

p_mm <- link.mn_iF (pred_mm)
p_mm_mean <- sapply( 1:length(p_mm) , function(i) apply(p_mm[[i]],2,mean) )

p_ff <- link.mn_iF (pred_ff)
p_ff_mean <- sapply( 1:length(p_ff) , function(i) apply(p_ff[[i]],2,mean) )

p_mf <- link.mn_iF (pred_mf)
p_mf_mean <- sapply( 1:length(p_mf) , function(i) apply(p_mf[[i]],2,mean) )

p_fm <- link.mn_iF (pred_fm)
p_fm_mean <- sapply( 1:length(p_fm) , function(i) apply(p_fm[[i]],2,mean) )


p_PI = NULL
for(k in 1:length(categories$Behavior)){
  p_mm_PI <- sapply( 1:length(p_mm) , function(i) PI(p_mm[[i]][,k], prob=0.95) ) 
  p_mm_PI <- as.data.frame( cbind(t(p_mm_PI), "mm") )
  p_ff_PI <- sapply( 1:length(p_ff) , function(i) PI(p_ff[[i]][,k], prob=0.95) ) 
  p_ff_PI <- as.data.frame( cbind(t(p_ff_PI), "ff") )
  p_mf_PI <- sapply( 1:length(p_mf) , function(i) PI(p_mf[[i]][,k], prob=0.95) ) 
  p_mf_PI <- as.data.frame( cbind(t(p_mf_PI), "mf") )
  p_fm_PI <- sapply( 1:length(p_fm) , function(i) PI(p_fm[[i]][,k], prob=0.95) ) 
  p_fm_PI <- as.data.frame( cbind(t(p_fm_PI), "fm") )
  
  p_PI_k <- bind_rows(p_mm_PI, p_ff_PI, p_mf_PI, p_fm_PI)
  p_PI_k$x <- rep(c("Different", "Same"), 4)
  p_PI_k$y <- c(p_mm_mean[k,], p_ff_mean[k,], p_mf_mean[k,], p_fm_mean[k,])
  
  p_PI_k$category <- k

  p_PI = rbind(p_PI, p_PI_k)
}

p_PI$`3%` <- as.numeric(p_PI$`3%`)
p_PI$`98%` <- as.numeric(p_PI$`98%`)
# p_PI$category = paste0("(",categories$y[p_PI$category], ") ", categories$Behavior[p_PI$category]) 
# p_PI$category = ifelse(p_PI$category =="(19) Z", "(19) ownership assertion", p_PI$category)
p_PI$category = categories$Behavior[p_PI$category]


tiff("../output/figure/predicted_household_iF.tiff", units="in", width=12, height=8, res=300)
par(mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))
pd <- position_dodge(0.2)
ggplot(p_PI, aes(x=x, y=y, colour = V3)) + 
  geom_errorbar(aes(ymin=`3%`, ymax=`98%`), width=.1, position=pd) +
  geom_point(position=pd) +
  scale_colour_discrete("Sex", breaks=c("ff", "fm", "mf", "mm"),
                        labels=c("Female Initiator, Female Recipient ", 
                                 "Female Initiator, Male Recipient ", 
                                 "Male Initiator, Female Recipient ", 
                                 "Male Initiator, Male Recipient ")) +
  labs(x = "Household Status", y = "Predicted Probability",
       color = "Sex") +
  facet_wrap(~factor(category, levels = categories$Behavior),
             ncol = 4, scales = "free") + 
  theme(strip.text.x = element_text(size = 8),
        legend.position="bottom", legend.box = "vertical",
        legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size=9), 
        legend.text = element_text(size = 9), 
        axis.text = element_text(size = 7.5),
        axis.title.x = element_text(size = 9), axis.title.y= element_text(size = 9)) 
dev.off()



## Generating predictions by age of initiator: mfit_ihF ####
seq.length <- 100
age_seq <- seq(from= min(df$init_age_z), to= max(df$init_age_z), length.out = seq.length)
preferred.ages <- c(0,2,4,6,8,10,12)
labels.at <- (preferred.ages - mean(df$Init_Age_At_Obs))/sd(df$Init_Age_At_Obs)

pred_dat_mm_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 0, # male
  recp_g = 0, # male
  same_house = 0 # in different household
)

pred_dat_mm_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 0, # male
  recp_g = 0, # male
  same_house = 1 # in same household
)

pred_dat_mf_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 0, # male
  recp_g = 1, # female
  same_house = 0 # in different household
)

pred_dat_mf_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 0, # male
  recp_g = 1, # female
  same_house = 1 # in same household
)

pred_dat_fm_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 1, # female
  recp_g = 0, # male
  same_house = 0 # in different household
)

pred_dat_fm_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 1, # female
  recp_g = 0, # male
  same_house = 1 # in same household
)

pred_dat_ff_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 1, # female
  recp_g = 1, # female
  same_house = 0 # in different household
)

pred_dat_ff_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = age_seq,
  recp_age_z = 0,
  init_g = 1, # female
  recp_g = 1, # female
  same_house = 1 # in same household
)

p_mm_dh <- link.mn_ihF ( pred_dat_mm_dh )
p_mm_dh_mean <- sapply( 1:length(p_mm_dh) , function(i) apply(p_mm_dh[[i]],2,mean) )

p_mm_sh <- link.mn_ihF ( pred_dat_mm_sh )
p_mm_sh_mean <- sapply( 1:length(p_mm_sh) , function(i) apply(p_mm_sh[[i]],2,mean) )

p_mf_dh <- link.mn_ihF ( pred_dat_mf_dh )
p_mf_dh_mean <- sapply( 1:length(p_mf_dh) , function(i) apply(p_mf_dh[[i]],2,mean) )

p_mf_sh <- link.mn_ihF ( pred_dat_mf_sh )
p_mf_sh_mean <- sapply( 1:length(p_mf_sh) , function(i) apply(p_mf_sh[[i]],2,mean) )

p_fm_dh <- link.mn_ihF ( pred_dat_fm_dh )
p_fm_dh_mean <- sapply( 1:length(p_fm_dh) , function(i) apply(p_fm_dh[[i]],2,mean) )

p_fm_sh <- link.mn_ihF ( pred_dat_fm_sh )
p_fm_sh_mean <- sapply( 1:length(p_fm_sh) , function(i) apply(p_fm_sh[[i]],2,mean) )

p_ff_dh <- link.mn_ihF ( pred_dat_ff_dh )
p_ff_dh_mean <- sapply( 1:length(p_ff_dh) , function(i) apply(p_ff_dh[[i]],2,mean) )

p_ff_sh <- link.mn_ihF ( pred_dat_ff_sh )
p_ff_sh_mean <- sapply( 1:length(p_ff_sh) , function(i) apply(p_ff_sh[[i]],2,mean) )

preferred.ages <- c(0,2,4,6,8,10,12)

p_PI = NULL
for(k in 1:length(categories$Behavior)){
  p_mm_PI <- sapply( 1:length(p_mm_sh) , function(i) PI(p_mm_sh[[i]][,k], prob=0.95) )
  p_mm_PI <- as.data.frame( cbind(t(p_mm_PI), "mm") )
  p_mf_PI <- sapply( 1:length(p_mf_sh) , function(i) PI(p_mf_sh[[i]][,k], prob=0.95) )
  p_mf_PI <- as.data.frame( cbind(t(p_mf_PI), "mf") )
  p_fm_PI <- sapply( 1:length(p_fm_sh) , function(i) PI(p_fm_sh[[i]][,k], prob=0.95) )
  p_fm_PI <- as.data.frame( cbind(t(p_fm_PI), "fm") )
  p_ff_PI <- sapply( 1:length(p_ff_sh) , function(i) PI(p_ff_sh[[i]][,k], prob=0.95) )
  p_ff_PI <- as.data.frame( cbind(t(p_ff_PI), "ff") )
  p_PI_k <- bind_rows(p_mm_PI, p_mf_PI, p_fm_PI, p_ff_PI)
  
  p_PI_k$x <- rep(age_seq, 4)
  p_PI_k$y <- c(p_mm_sh_mean[k,], p_mf_sh_mean[k,], p_fm_sh_mean[k,], p_ff_sh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Same household"
  
  p_PI = rbind(p_PI, p_PI_k)
  
  p_mm_PI <- sapply( 1:length(p_mm_dh) , function(i) PI(p_mm_dh[[i]][,k], prob=0.95) )
  p_mm_PI <- as.data.frame( cbind(t(p_mm_PI), "mm") )
  p_mf_PI <- sapply( 1:length(p_mf_dh) , function(i) PI(p_mf_dh[[i]][,k], prob=0.95) )
  p_mf_PI <- as.data.frame( cbind(t(p_mf_PI), "mf") )
  p_fm_PI <- sapply( 1:length(p_fm_dh) , function(i) PI(p_fm_dh[[i]][,k], prob=0.95) )
  p_fm_PI <- as.data.frame( cbind(t(p_fm_PI), "fm") )
  p_ff_PI <- sapply( 1:length(p_ff_dh) , function(i) PI(p_ff_dh[[i]][,k], prob=0.95) )
  p_ff_PI <- as.data.frame( cbind(t(p_ff_PI), "ff") )
  p_PI_k <- bind_rows(p_mm_PI, p_mf_PI, p_fm_PI, p_ff_PI)
  
  p_PI_k$x <- rep(age_seq, 4)
  p_PI_k$y <- c(p_mm_dh_mean[k,], p_mf_dh_mean[k,], p_fm_dh_mean[k,], p_ff_dh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Different household"
  
  p_PI = rbind(p_PI, p_PI_k)
}

p_PI$`3%` <- as.numeric(p_PI$`3%`)
p_PI$`98%` <- as.numeric(p_PI$`98%`)
# p_PI$category = paste0("(",categories$y[p_PI$category], ") ", categories$Behavior[p_PI$category]) 
# p_PI$category = ifelse(p_PI$category =="(19) Z", "(19) ownership assertion", p_PI$category)
p_PI$category = categories$Behavior[p_PI$category]
p_PI$label_both = paste0(p_PI$category, ", ", p_PI$samehouse)

tiff("../output/figure/predicted_initiator_age_ihF.tiff", units="in", width=12, height=15, res=300)
par(mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))

ggplot(p_PI, 
       aes(x = x, y = y, ymin = `3%`, ymax = `98%`, colour = V3, fill = V3)) +
  geom_line(linewidth=0.5) + 
  geom_ribbon(alpha=0.2, linetype = 0) +
  scale_colour_discrete("Sex", breaks=c("ff", "fm", "mf", "mm"),
                        labels=c("Female Initiator, Female Recipient ", 
                                 "Female Initiator, Male Recipient ", 
                                 "Male Initiator, Female Recipient ", 
                                 "Male Initiator, Male Recipient ")) +
  scale_fill_discrete("Sex", breaks=c("ff", "fm", "mf", "mm"),
                      labels=c("Female Initiator, Female Recipient ", 
                               "Female Initiator, Male Recipient ", 
                               "Male Initiator, Female Recipient ", 
                               "Male Initiator, Male Recipient ")) + 
  labs(x = "Initiator's Age (in years)", y = "Predicted Probability",
       color = "Sex", 
       fill = "Sex") +
  scale_x_continuous(breaks=labels.at, labels = preferred.ages) +
  facet_wrap(~factor(label_both, 
                     levels = paste0(rep(c(categories$Behavior), each = 2), ", ", 
                                     rep(c("Same household", "Different household"), 19))), 
             ncol = 4, scales = "free") + 

  theme(strip.text.x = element_text(size = 8),
        legend.position="bottom", legend.box = "vertical",
        legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size=9), 
        legend.text = element_text(size = 9), 
        axis.text = element_text(size = 7.5),
        axis.title.x = element_text(size = 9), axis.title.y= element_text(size = 9)) 


dev.off()

## Generating predictions by age of recipient: mfit_ihF ####
seq.length <- 100
age_seq <- seq (from= min(df$recp_age_z), to= max(df$recp_age_z), length.out = seq.length)
preferred.ages <- c(0,2,4,6,8,10,12)
labels.at <- (preferred.ages - mean(df$Recp_Age_At_Obs))/sd(df$Recp_Age_At_Obs)

pred_dat_mm_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 0, # male
  recp_g = 0, # male
  same_house = 0 # in different household
)

pred_dat_mm_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 0, # male
  recp_g = 0, # male
  same_house = 1 # in same household
)

pred_dat_mf_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 0, # male
  recp_g = 1, # female
  same_house = 0 # in different household
)

pred_dat_mf_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 0, # male
  recp_g = 1, # female
  same_house = 1 # in same household
)

pred_dat_fm_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 1, # female
  recp_g = 0, # male
  same_house = 0 # in different household
)

pred_dat_fm_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 1, # female
  recp_g = 0, # male
  same_house = 1 # in same household
)

pred_dat_ff_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 1, # female
  recp_g = 1, # female
  same_house = 0 # in different household
)

pred_dat_ff_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = age_seq,
  init_g = 1, # female
  recp_g = 1, # female
  same_house = 1 # in same household
)

p_mm_dh <- link.mn_ihF ( pred_dat_mm_dh )
p_mm_dh_mean <- sapply( 1:length(p_mm_dh) , function(i) apply(p_mm_dh[[i]],2,mean) )

p_mm_sh <- link.mn_ihF ( pred_dat_mm_sh )
p_mm_sh_mean <- sapply( 1:length(p_mm_sh) , function(i) apply(p_mm_sh[[i]],2,mean) )

p_mf_dh <- link.mn_ihF ( pred_dat_mf_dh )
p_mf_dh_mean <- sapply( 1:length(p_mf_dh) , function(i) apply(p_mf_dh[[i]],2,mean) )

p_mf_sh <- link.mn_ihF ( pred_dat_mf_sh )
p_mf_sh_mean <- sapply( 1:length(p_mf_sh) , function(i) apply(p_mf_sh[[i]],2,mean) )

p_fm_dh <- link.mn_ihF ( pred_dat_fm_dh )
p_fm_dh_mean <- sapply( 1:length(p_fm_dh) , function(i) apply(p_fm_dh[[i]],2,mean) )

p_fm_sh <- link.mn_ihF ( pred_dat_fm_sh )
p_fm_sh_mean <- sapply( 1:length(p_fm_sh) , function(i) apply(p_fm_sh[[i]],2,mean) )

p_ff_dh <- link.mn_ihF ( pred_dat_ff_dh )
p_ff_dh_mean <- sapply( 1:length(p_ff_dh) , function(i) apply(p_ff_dh[[i]],2,mean) )

p_ff_sh <- link.mn_ihF ( pred_dat_ff_sh )
p_ff_sh_mean <- sapply( 1:length(p_ff_sh) , function(i) apply(p_ff_sh[[i]],2,mean) )

preferred.ages <- c(0,2,4,6,8,10,12)

p_PI = NULL
for(k in 1:length(categories$Behavior)){
  p_mm_PI <- sapply( 1:length(p_mm_sh) , function(i) PI(p_mm_sh[[i]][,k], prob=0.95) )
  p_mm_PI <- as.data.frame( cbind(t(p_mm_PI), "mm") )
  p_mf_PI <- sapply( 1:length(p_mf_sh) , function(i) PI(p_mf_sh[[i]][,k], prob=0.95) )
  p_mf_PI <- as.data.frame( cbind(t(p_mf_PI), "mf") )
  p_fm_PI <- sapply( 1:length(p_fm_sh) , function(i) PI(p_fm_sh[[i]][,k], prob=0.95) )
  p_fm_PI <- as.data.frame( cbind(t(p_fm_PI), "fm") )
  p_ff_PI <- sapply( 1:length(p_ff_sh) , function(i) PI(p_ff_sh[[i]][,k], prob=0.95) )
  p_ff_PI <- as.data.frame( cbind(t(p_ff_PI), "ff") )
  p_PI_k <- bind_rows(p_mm_PI, p_mf_PI, p_fm_PI, p_ff_PI)
  
  p_PI_k$x <- rep(age_seq, 4)
  p_PI_k$y <- c(p_mm_sh_mean[k,], p_mf_sh_mean[k,], p_fm_sh_mean[k,], p_ff_sh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Same household"
  
  p_PI = rbind(p_PI, p_PI_k)
  
  p_mm_PI <- sapply( 1:length(p_mm_dh) , function(i) PI(p_mm_dh[[i]][,k], prob=0.95) )
  p_mm_PI <- as.data.frame( cbind(t(p_mm_PI), "mm") )
  p_mf_PI <- sapply( 1:length(p_mf_dh) , function(i) PI(p_mf_dh[[i]][,k], prob=0.95) )
  p_mf_PI <- as.data.frame( cbind(t(p_mf_PI), "mf") )
  p_fm_PI <- sapply( 1:length(p_fm_dh) , function(i) PI(p_fm_dh[[i]][,k], prob=0.95) )
  p_fm_PI <- as.data.frame( cbind(t(p_fm_PI), "fm") )
  p_ff_PI <- sapply( 1:length(p_ff_dh) , function(i) PI(p_ff_dh[[i]][,k], prob=0.95) )
  p_ff_PI <- as.data.frame( cbind(t(p_ff_PI), "ff") )
  p_PI_k <- bind_rows(p_mm_PI, p_mf_PI, p_fm_PI, p_ff_PI)
  
  p_PI_k$x <- rep(age_seq, 4)
  p_PI_k$y <- c(p_mm_dh_mean[k,], p_mf_dh_mean[k,], p_fm_dh_mean[k,], p_ff_dh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Different household"
  
  p_PI = rbind(p_PI, p_PI_k)
}

p_PI$`3%` <- as.numeric(p_PI$`3%`)
p_PI$`98%` <- as.numeric(p_PI$`98%`)
# p_PI$category = paste0("(",categories$y[p_PI$category], ") ", categories$Behavior[p_PI$category]) 
# p_PI$category = ifelse(p_PI$category =="(19) Z", "(19) ownership assertion", p_PI$category)
p_PI$category = categories$Behavior[p_PI$category]
p_PI$label_both = paste0(p_PI$category, ", ", p_PI$samehouse)

tiff("../output/figure/predicted_recipient_age_ihF.tiff", units="in", width=12, height=15, res=300)
par(mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))

ggplot(p_PI, 
       aes(x = x, y = y, ymin = `3%`, ymax = `98%`, colour = V3, fill = V3)) +
  geom_line(linewidth=0.5) + 
  geom_ribbon(alpha=0.2, linetype = 0) +
  scale_colour_discrete("Sex", breaks=c("ff", "fm", "mf", "mm"),
                        labels=c("Female Initiator, Female Recipient ", 
                                 "Female Initiator, Male Recipient ", 
                                 "Male Initiator, Female Recipient ", 
                                 "Male Initiator, Male Recipient ")) +
  scale_fill_discrete("Sex", breaks=c("ff", "fm", "mf", "mm"),
                      labels=c("Female Initiator, Female Recipient ", 
                               "Female Initiator, Male Recipient ", 
                               "Male Initiator, Female Recipient ", 
                               "Male Initiator, Male Recipient ")) + 
  labs(x = "Recipient's Age (in years)", y = "Predicted Probability",
       color = "Sex", 
       fill = "Sex") +
  scale_x_continuous(breaks=labels.at, labels = preferred.ages) +
  facet_wrap(~factor(label_both, 
                     levels = paste0(rep(c(categories$Behavior), each = 2), ", ", 
                                     rep(c("Same household", "Different household"), 19))), 
             ncol = 4, scales = "free") + 

  theme(strip.text.x = element_text(size = 8),
        legend.position="bottom", legend.box = "vertical",
        legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size=9), 
        legend.text = element_text(size = 9), 
        axis.text = element_text(size = 7.5),
        axis.title.x = element_text(size = 9), axis.title.y= element_text(size = 9)) 


dev.off()


## Generating predictions by gender of initiator: mfit_ihF ####
seq.length <- 2
pred_Xm_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = c(0,1),
  recp_g = 0, # male
  same_house = 1 # same household
)

pred_Xf_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = c(0,1),
  recp_g = 1, # female
  same_house = 1 # same household
)

pred_Xm_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = c(0,1),
  recp_g = 0, # male
  same_house = 0 # different household
)

pred_Xf_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = c(0,1),
  recp_g = 1, # female
  same_house = 0 # different household
)

p_Xm_sh <- link.mn_ihF (pred_Xm_sh)
p_Xm_sh_mean <- sapply( 1:length(p_Xm_sh) , function(i) apply(p_Xm_sh[[i]],2,mean) )

p_Xf_sh <- link.mn_ihF (pred_Xf_sh)
p_Xf_sh_mean <- sapply( 1:length(p_Xf_sh) , function(i) apply(p_Xf_sh[[i]],2,mean) )

p_Xm_dh <- link.mn_ihF (pred_Xm_dh)
p_Xm_dh_mean <- sapply( 1:length(p_Xm_dh) , function(i) apply(p_Xm_dh[[i]],2,mean) )

p_Xf_dh <- link.mn_ihF (pred_Xf_dh)
p_Xf_dh_mean <- sapply( 1:length(p_Xf_dh) , function(i) apply(p_Xf_dh[[i]],2,mean) )

p_PI = NULL
for(k in 1:length(categories$Behavior)){
  # same household
  p_Xm_PI <- sapply( 1:length(p_Xm_sh) , function(i) PI(p_Xm_sh[[i]][,k], prob=0.95) ) 
  p_Xm_PI <- as.data.frame( cbind(t(p_Xm_PI), "Xm") )
  p_Xf_PI <- sapply( 1:length(p_Xf_sh) , function(i) PI(p_Xf_sh[[i]][,k], prob=0.95) ) 
  p_Xf_PI <- as.data.frame( cbind(t(p_Xf_PI), "Xf") )
  
  p_PI_k <- rbind(p_Xm_PI, p_Xf_PI)
  p_PI_k$x <- rep(c("Male", "Female"), 2)
  p_PI_k$y <- c(p_Xm_sh_mean[k,], p_Xf_sh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Same household"
  
  p_PI = rbind(p_PI, p_PI_k)
  
  # different household
  p_Xm_PI <- sapply( 1:length(p_Xm_dh) , function(i) PI(p_Xm_dh[[i]][,k], prob=0.95) ) 
  p_Xm_PI <- as.data.frame( cbind(t(p_Xm_PI), "Xm") )
  p_Xf_PI <- sapply( 1:length(p_Xf_dh) , function(i) PI(p_Xf_dh[[i]][,k], prob=0.95) ) 
  p_Xf_PI <- as.data.frame( cbind(t(p_Xf_PI), "Xf") )
  
  p_PI_k <- rbind(p_Xm_PI, p_Xf_PI)
  p_PI_k$x <- rep(c("Male", "Female"), 2)
  p_PI_k$y <- c(p_Xm_dh_mean[k,], p_Xf_dh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Different household"
  
  p_PI = rbind(p_PI, p_PI_k)
}

p_PI$`3%` <- as.numeric(p_PI$`3%`)
p_PI$`98%` <- as.numeric(p_PI$`98%`)
# p_PI$category = paste0("(",categories$y[p_PI$category], ") ", categories$Behavior[p_PI$category]) 
# p_PI$category = ifelse(p_PI$category =="(19) Z", "(19) ownership assertion", p_PI$category)
p_PI$category = categories$Behavior[p_PI$category]
p_PI$label_both = paste0(p_PI$category, ", ", p_PI$samehouse)

tiff("../output/figure/predicted_initiator_sex_ihF.tiff", units="in", width=12, height=15, res=300)
par(mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))
pd <- position_dodge(0.2)
ggplot(p_PI,
       aes(x=x, y=y, colour = V3)) + 
  geom_errorbar(aes(ymin=`3%`, ymax=`98%`), width=.1, position=pd) +
  geom_point(position=pd) +
  scale_color_discrete("Recipient's Sex", breaks=c("Xf", "Xm"),
                       labels=c("Female", "Male")) + 
  labs(x = "Initiator's Sex", y = "Predicted Probability",
       color = "Recipient's Sex") +
  facet_wrap(~factor(label_both, 
                     levels = paste0(rep(c(categories$Behavior), each = 2), ", ", 
                                     rep(c("Same household", "Different household"), 19))), 
             ncol = 4, scales = "free") + 
  theme(strip.text.x = element_text(size = 8),
        legend.position="bottom", legend.box = "vertical",
        legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size=9), 
        legend.text = element_text(size = 9), 
        axis.text = element_text(size = 7.5),
        axis.title.x = element_text(size = 9), axis.title.y= element_text(size = 9)) 
dev.off()


## Generating predictions by gender of recipient: mfit_ihF ####
seq.length <- 2

pred_mX_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 0,  # male
  recp_g = c(0,1),
  same_house = 1
)

pred_fX_sh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 1,  # female
  recp_g = c(0,1),
  same_house = 1
)

pred_mX_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 0,  # male
  recp_g = c(0,1),
  same_house = 0
)

pred_fX_dh <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 1,  # female
  recp_g = c(0,1),
  same_house = 0
)

p_mX_sh <- link.mn_ihF (pred_mX_sh)
p_mX_sh_mean <- sapply( 1:length(p_mX_sh) , function(i) apply(p_mX_sh[[i]],2,mean) )

p_fX_sh <- link.mn_ihF (pred_fX_sh)
p_fX_sh_mean <- sapply( 1:length(p_fX_sh) , function(i) apply(p_fX_sh[[i]],2,mean) )

p_mX_dh <- link.mn_ihF (pred_mX_dh)
p_mX_dh_mean <- sapply( 1:length(p_mX_dh) , function(i) apply(p_mX_dh[[i]],2,mean) )

p_fX_dh <- link.mn_ihF (pred_fX_dh)
p_fX_dh_mean <- sapply( 1:length(p_fX_dh) , function(i) apply(p_fX_dh[[i]],2,mean) )


p_PI = NULL
for(k in 1:length(categories$Behavior)){
  # same household
  p_mX_PI <- sapply( 1:length(p_mX_sh) , function(i) PI(p_mX_sh[[i]][,k], prob=0.95) ) 
  p_mX_PI <- as.data.frame( cbind(t(p_mX_PI), "mX") )
  p_fX_PI <- sapply( 1:length(p_fX_sh) , function(i) PI(p_fX_sh[[i]][,k], prob=0.95) ) 
  p_fX_PI <- as.data.frame( cbind(t(p_fX_PI), "fX") )
  
  p_PI_k <- rbind(p_mX_PI, p_fX_PI)
  p_PI_k$x <- rep(c("Male", "Female"), 2)
  p_PI_k$y <- c(p_mX_sh_mean[k,], p_fX_sh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Same household"
  
  p_PI = rbind(p_PI, p_PI_k)
  
  # different household
  p_mX_PI <- sapply( 1:length(p_mX_dh) , function(i) PI(p_mX_dh[[i]][,k], prob=0.95) ) 
  p_mX_PI <- as.data.frame( cbind(t(p_mX_PI), "mX") )
  p_fX_PI <- sapply( 1:length(p_fX_dh) , function(i) PI(p_fX_dh[[i]][,k], prob=0.95) ) 
  p_fX_PI <- as.data.frame( cbind(t(p_fX_PI), "fX") )
  
  p_PI_k <- rbind(p_mX_PI, p_fX_PI)
  p_PI_k$x <- rep(c("Male", "Female"), 2)
  p_PI_k$y <- c(p_mX_dh_mean[k,], p_fX_dh_mean[k,])
  
  p_PI_k$category <- k
  p_PI_k$samehouse <- "Different household"
  
  p_PI = rbind(p_PI, p_PI_k)
}


p_PI$`3%` <- as.numeric(p_PI$`3%`)
p_PI$`98%` <- as.numeric(p_PI$`98%`)
# p_PI$category = paste0("(",categories$y[p_PI$category], ") ", categories$Behavior[p_PI$category]) 
# p_PI$category = ifelse(p_PI$category =="(19) Z", "(19) ownership assertion", p_PI$category)
p_PI$category = categories$Behavior[p_PI$category]
p_PI$label_both = paste0(p_PI$category, ", ", p_PI$samehouse)

tiff("../output/figure/predicted_recipient_sex_ihF.tiff", units="in", width=12, height=15, res=300)
par(mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))
pd <- position_dodge(0.2)
ggplot(p_PI,
       aes(x=x, y=y, colour = V3)) + 
  geom_errorbar(aes(ymin=`3%`, ymax=`98%`), width=.1, position=pd) +
  geom_point(position=pd) +
  scale_color_discrete("Initiator's Sex", breaks=c("fX", "mX"),
                       labels=c("Female", "Male")) + 
  labs(x = "Recipient's Sex", y = "Predicted Probability",
       color = "Initiator's Sex") +
  facet_wrap(~factor(label_both, 
                     levels = paste0(rep(c(categories$Behavior), each = 2), ", ", 
                                     rep(c("Same household", "Different household"), 19))), 
             ncol = 4, scales = "free") + 
  theme(strip.text.x = element_text(size = 8),
        legend.position="bottom", legend.box = "vertical",
        legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size=9), 
        legend.text = element_text(size = 9), 
        axis.text = element_text(size = 7.5),
        axis.title.x = element_text(size = 9), axis.title.y= element_text(size = 9)) 
dev.off()

## Generating predictions by household status: mfit_ihF ####
seq.length <- 2

pred_mm <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 0,  # male
  recp_g = 0,
  same_house = c(0,1)
)

pred_ff <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 1,  # female
  recp_g = 1,
  same_house = c(0,1)
)

pred_mf <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 0,  # male
  recp_g = 1,
  same_house = c(0,1)
)

pred_fm <- data.frame(
  init_id = 0,
  recp_id = 0,
  house_init_id = 0,
  house_recp_id = 0,
  init_age_z = 0,
  recp_age_z = 0,
  init_g = 1,  # female
  recp_g = 0,
  same_house = c(0,1) 
)

p_mm <- link.mn_ihF (pred_mm)
p_mm_mean <- sapply( 1:length(p_mm) , function(i) apply(p_mm[[i]],2,mean) )

p_ff <- link.mn_ihF (pred_ff)
p_ff_mean <- sapply( 1:length(p_ff) , function(i) apply(p_ff[[i]],2,mean) )

p_mf <- link.mn_ihF (pred_mf)
p_mf_mean <- sapply( 1:length(p_mf) , function(i) apply(p_mf[[i]],2,mean) )

p_fm <- link.mn_ihF (pred_fm)
p_fm_mean <- sapply( 1:length(p_fm) , function(i) apply(p_fm[[i]],2,mean) )


p_PI = NULL
for(k in 1:length(categories$Behavior)){
  p_mm_PI <- sapply( 1:length(p_mm) , function(i) PI(p_mm[[i]][,k], prob=0.95) ) 
  p_mm_PI <- as.data.frame( cbind(t(p_mm_PI), "mm") )
  p_ff_PI <- sapply( 1:length(p_ff) , function(i) PI(p_ff[[i]][,k], prob=0.95) ) 
  p_ff_PI <- as.data.frame( cbind(t(p_ff_PI), "ff") )
  p_mf_PI <- sapply( 1:length(p_mf) , function(i) PI(p_mf[[i]][,k], prob=0.95) ) 
  p_mf_PI <- as.data.frame( cbind(t(p_mf_PI), "mf") )
  p_fm_PI <- sapply( 1:length(p_fm) , function(i) PI(p_fm[[i]][,k], prob=0.95) ) 
  p_fm_PI <- as.data.frame( cbind(t(p_fm_PI), "fm") )
  
  p_PI_k <- bind_rows(p_mm_PI, p_ff_PI, p_mf_PI, p_fm_PI)
  p_PI_k$x <- rep(c("Different", "Same"), 4)
  p_PI_k$y <- c(p_mm_mean[k,], p_ff_mean[k,], p_mf_mean[k,], p_fm_mean[k,])
  
  p_PI_k$category <- k
  
  p_PI = rbind(p_PI, p_PI_k)
}

p_PI$`3%` <- as.numeric(p_PI$`3%`)
p_PI$`98%` <- as.numeric(p_PI$`98%`)
# p_PI$category = paste0("(",categories$y[p_PI$category], ") ", categories$Behavior[p_PI$category]) 
# p_PI$category = ifelse(p_PI$category =="(19) Z", "(19) ownership assertion", p_PI$category)
p_PI$category = categories$Behavior[p_PI$category]


tiff("../output/figure/predicted_household_ihF.tiff", units="in", width=12, height=8, res=300)
par(mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))
pd <- position_dodge(0.2)
ggplot(p_PI, aes(x=x, y=y, colour = V3)) + 
  geom_errorbar(aes(ymin=`3%`, ymax=`98%`), width=.1, position=pd) +
  geom_point(position=pd) +
  scale_colour_discrete("Sex", breaks=c("ff", "fm", "mf", "mm"),
                        labels=c("Female Initiator, Female Recipient ", 
                                 "Female Initiator, Male Recipient ", 
                                 "Male Initiator, Female Recipient ", 
                                 "Male Initiator, Male Recipient ")) +
  labs(x = "Household Status", y = "Predicted Probability",
       color = "Sex") +
  facet_wrap(~factor(category, levels = categories$Behavior), 
             ncol = 4, scales = "free") + 
  theme(strip.text.x = element_text(size = 8),
        legend.position="bottom", legend.box = "vertical",
        legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size=9), 
        legend.text = element_text(size = 9), 
        axis.text = element_text(size = 7.5),
        axis.title.x = element_text(size = 9), axis.title.y= element_text(size = 9)) 
dev.off()

