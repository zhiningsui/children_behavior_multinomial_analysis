library(tidyverse)
library(rethinking)
library(chron)
theme_set(theme_bw())

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
categories$Behavior_full <- c("(1) Comforting", "(2) Dominating", "(3) Helping", 
                         "(4) Leading", "(5) Dirty Looks", "(6) Physical Aggression", 
                         "(7) Requesting for Access", "(8) Requesting for Comfort",
                         "(9) Requesting for Help", "(10) Requesting for Sharing", 
                         "(11) Scolding", "(12) Sharing", "(13) Supporting Opinions",
                         "(14) Taking Away", "(15) Tattling", "(16) Aggressive Teasing",
                         "(17) Playful Teasing", "(18) Verbal Aggression", 
                         "(19) Ownership Assertion")
categories$Behavior_full <- factor(categories$Behavior_full, 
                              levels = c("(1) Comforting", "(2) Dominating", "(3) Helping", 
                                         "(4) Leading", "(5) Dirty Looks", "(6) Physical Aggression", 
                                         "(7) Requesting for Access", "(8) Requesting for Comfort",
                                         "(9) Requesting for Help", "(10) Requesting for Sharing", 
                                         "(11) Scolding", "(12) Sharing", "(13) Supporting Opinions",
                                         "(14) Taking Away", "(15) Tattling", "(16) Aggressive Teasing",
                                         "(17) Playful Teasing", "(18) Verbal Aggression", 
                                         "(19) Ownership Assertion"))


df$Behavior = categories$Behavior_full[df$y]

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

# number and frequency of each behavior
g <- df %>%
  group_by(y) %>%
  summarise(y = n()) %>%
  mutate(freq = round(y / sum(y), 3))

# Age of children and newborns
load('../data/demographic.RData') 
table(demographic$`Newborn during study period`, demographic$Sex)
demographic$`Starting age` <- as.numeric(demographic$`Age at start of study(Year)`) + as.numeric(demographic$`Age at start of study(Month)`)/12
###### TBC with Gladys

# Average age of initiator and recipient at each observation of each behavior
ave_age <- df %>%
  group_by(Behavior) %>%
  summarise(avg_init_age = mean(Init_Age_At_Obs),
            avg_recp_age = mean(Recp_Age_At_Obs)) %>%
  pivot_longer(cols = c(avg_init_age))

age <- df[, c("Behavior", "Init_Age_At_Obs", "Recp_Age_At_Obs")] %>%
  pivot_longer(cols = c(Init_Age_At_Obs, Recp_Age_At_Obs))

tiff("../output/figure/avg_age_at_obs.tiff", units="in", width=9, height=3.5, res=300)
ggplot(age, aes(x = factor(name), y=value)) + 
  geom_boxplot(width = 0.55, outlier.shape = NA) +
  geom_point(position = position_jitter(0.3), size = 0.1, alpha = 0.2, color = "blue") +
  facet_wrap(~Behavior, nrow = 3) +
  labs(x = "",
       y = "Age at Observation (in years)")+
  scale_x_discrete(breaks = c("Init_Age_At_Obs", "Recp_Age_At_Obs"),
                   labels = c("Initiator", "Recipient")) +
  stat_summary(fun=mean, 
               geom="point",
               shape=8, # 17 for solid triangle
               size=2, 
               color="red", 
               position = position_dodge2 (width = 0.76, preserve = "single")) +
  theme(axis.text = element_text(color = "black", size = 6),
        strip.text = element_text(size = 6),
        axis.title = element_text(size = 8))
dev.off()

# number of observations per individual
no_init <- as.data.frame(table(df$Initiator_ID))
colnames(no_init) <- c("id", "init_times")
no_recp <- as.data.frame(table(df$Recipient_ID))
colnames(no_recp) <- c("id", "recp_times")
no_obs_inid <- merge(no_init,no_recp, by = "id", all = TRUE)
no_obs_inid[is.na(no_obs_inid)] <- 0
no_obs_inid$total_times <- no_obs_inid$init_times + no_obs_inid$recp_times
no_obs_inid <- merge(no_obs_inid, demographic, by.x = "id", by.y = "Subject ID")

mean(no_obs_inid$total_times)
sd(no_obs_inid$total_times)

no_obs_inid_long <- no_obs_inid %>%
  pivot_longer(cols = c(init_times, recp_times, total_times))

tiff("../output/figure/number_of_times_being_init_recp.tiff", units="in", width=9, height=3.5, res=300)
ggplot(no_obs_inid_long, aes(x=factor(name), y=value, fill = factor(Sex))) + 
  geom_boxplot(width = 0.55, position=position_dodge(0.76), outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.5), size =0.9, alpha = 0.4) +
  labs(x = "",
       y = "Number of times")+
  scale_x_discrete(breaks = c("init_times", "recp_times", "total_times"),
                   labels = c("Initiator", "Recipient", "Initiator or Recipient"),
                   limits=rev) +
  stat_summary(fun=mean, 
               geom="point",
               shape=8, # 17 for solid triangle
               size=2, 
               color="yellow", 
               position = position_dodge2 (width = 0.76, preserve = "single")) +
  theme(axis.text = element_text(color = "black", size = 10),
        legend.position = c(0.93, 0.8),
        legend.title = element_text(size = 10)) +
  scale_fill_discrete(name = "Gender",
                      breaks = c("F", "M"),
                      labels = c("Female", "Male")) +
  coord_flip()

dev.off()

### Number of observations per household per household
# number of individuals per household
no_inid_per_house <- as.data.frame(table(demographic$`Subject ID`, demographic$House))[,-1]
no_inid_per_house <- no_inid_per_house %>%
  group_by(Var2) %>%
  summarize(number = sum(Freq))
sort(no_inid_per_house$number)
mean(no_inid_per_house$number)
sd(no_inid_per_house$number)
freq_inid_per_house <- as.data.frame(table(no_inid_per_house$number))

tiff("../output/figure/number_of_children_per_household.tiff", units="in", width=4.5, height=4.5, res=300)
ggplot(freq_inid_per_house, aes(y=Freq, x = Var1)) + 
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Freq), vjust=1.5, color="white", size=3.3) +
  theme(axis.text = element_text(colour="black")) +
  labs(y = "Number of households",
       x = "Number of children per household")
dev.off()

no_inid_per_house <- no_inid_per_house %>%
  arrange(number)
order <- no_inid_per_house$Var2

no_gender_per_house <- as.data.frame(table(demographic$House, demographic$Sex))
colnames(no_gender_per_house) <- c("house_id", "sex", "count")
no_gender_per_house$house_id <- factor(no_gender_per_house$house_id, levels = order)

tiff("../output/figure/gender_composition_in_each_household.tiff", units="in",width=7.5, height=4.5, res=300)
ggplot(no_gender_per_house, aes(fill=sex, y=count, x = house_id)) + 
  geom_bar(position="stack", stat="identity", width = 0.9) +
  theme(legend.position = c(0.9, 0.8),
        axis.text = element_text(colour="black"),
        axis.text.x = element_text(size = 6, angle=90)) +
  scale_fill_discrete(name = "Gender",
                      breaks = c("F", "M"),
                      labels = c("Female",
                                 "Male")) +
  scale_x_discrete(limits=rev) + 
  scale_y_continuous(breaks=c(1:9)) +
  # guides(fill=guide_legend(nrow=2, byrow=TRUE)) + 
  labs(x = "Household ID",
       y = "Number of children")
dev.off()

prop_gender_per_house <- no_gender_per_house %>%
  pivot_wider(names_from = sex, values_from = count, id_cols = house_id) %>%
  mutate(prop.F = F/(F+M)) %>%
  mutate(equal = ifelse(prop.F == 0.5, "equal", 
                        ifelse(prop.F > 0.5, "F", "M")))
table(prop_gender_per_house$equal)

# number of observations per household
no_init <- as.data.frame(table(df$Init_House))
colnames(no_init) <- c("id", "init_times")
no_recp <- as.data.frame(table(df$Recp_House))
colnames(no_recp) <- c("id", "recp_times")
no_obs_household <- merge(no_init,no_recp, by = "id", all = TRUE)
no_obs_household[is.na(no_obs_household)] <- 0
no_obs_household$total_times <- no_obs_household$init_times + no_obs_household$recp_times

no_obs_household_long <- no_obs_household %>%
  pivot_longer(cols = c(init_times, recp_times, total_times))
mean_house <- no_obs_household_long %>%
  group_by(name) %>%
  summarize(mean=mean(value), sd=sd(value))

### Behavior allocation by gender and household
descrip <- df %>%
  group_by(Initiator_Gender, Recipient_Gender, Behavior, same_household) %>%
  summarise(y = n()) %>%
  mutate(Gender = paste0(Initiator_Gender,"-", Recipient_Gender))

tiff("../output/figure/number_of_observations_by_covariate.tiff", units="in", width=12, height=4.5, res=300)
ggplot(descrip, aes(fill=Gender, y=y, x = Behavior)) + 
  geom_bar(position="stack", stat="identity") +
  coord_flip() + 
  facet_wrap(~same_household, nrow = 1,
             labeller = labeller(same_household = c("0" = "Same Household",
                                                    "1" = "Different Household"))) +
  theme(legend.position = "bottom",
        axis.text = element_text(colour="black")) +
  scale_fill_discrete(name = "Gender",
                      breaks = c("F-F", "F-M", "M-M", "M-F"),
                      labels = c("Female Initiator, Female Recipient", 
                                 "Female Initiator, Male Recipient",
                                 "Male Initiator, Female Recipient",
                                 "Male Initiator, Male Recipient")) +
  scale_x_discrete(limits=rev) + 
  # guides(fill=guide_legend(nrow=2, byrow=TRUE)) + 
  labs(y = "Number of Observations")
dev.off()

