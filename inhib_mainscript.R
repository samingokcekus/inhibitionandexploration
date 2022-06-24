library(readxl)
library(ggplot2)
library(lme4)
library(effects)
library(DHARMa)
library(performance)
library(MuMIn)
library(dplyr)
library(tidyverse)
library(purrr)
library(multcomp)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(effects)
library(nlme)
library(xtable)
library(ggstance)
library(jtools)


fullaug2019 <- readxl::read_excel("data/curiosity_leftright_19082019.xls")

#clean up 
as.data.frame(colnames(fullaug2019))
cols <- names(fullaug2019)[c(2,3,4,5,6,10,12,30)]
fullaug2019[cols] <- lapply(fullaug2019[cols], as.factor)
cols <- names(fullaug2019)[c(13:29,31:33)]
fullaug2019[cols] <- lapply(fullaug2019[cols], as.numeric)

#pull inhibition participants 
xdata <- fullaug2019
xdata <- xdata[which(xdata$inhib_percfirstt != "NA"),]
xdata <- xdata[which(xdata$species ==1 ),]

##add task experience 
task <- read.csv("data/taskinvolv_inds.csv")
xdata <- merge(xdata, task, by="name", all.x=TRUE)

#quick look ####
table(xdata$sex) #nice pretty even 
table(xdata$status) #also pretty even 
table(xdata$status, xdata$sex) 
hist(xdata$age_m, xlab = "Age in months")

#pull inhibition+curiosity participants 
xdata.c <- xdata[which(xdata$PC1pos_mean != "NA"),]



# results part 1 - visuals of just inhibition #### 

#sex
ggplot() +
  geom_boxplot(data=xdata.c, aes(x=sex, y=inhib_percfirstt, group = sex)) + 
  xlab("Sex") + 
  ylab("Percent trials first try") + 
  theme_classic()


#more variation among males 

#status
ggplot() +
  geom_boxplot(data=xdata.c, aes(x=status, y=inhib_percfirstt, group = status)) + 
  xlab("Status") + 
  ylab("Percent trials first try") + 
  theme_classic()
#helpers better, more variation among breeders 


#group size 
ggplot() +
  geom_boxplot(data=xdata.c, aes(x=groupsize, y=inhib_percfirstt, group = groupsize)) + 
  xlab("Group size") + 
  ylab("Percent trials first try") + 
  theme_classic()
#no pattern really - probably depends on age of group 

#age
ggplot() +
  geom_point(data=xdata.c, aes(x=age_m, y=inhib_percfirstt)) + 
  geom_smooth(data=xdata.c, aes(x=age_m, y=inhib_percfirstt), color='#787848', method=loess, size=.75) + 
  xlab("Age in months") + 
  ylab("Percent trials first try") + 
  theme_classic()
#first increase, then decrease with age 

#divide by breeder and helper 
ggplot() +
  geom_point(data=xdata.c[which(xdata.c$status == "b"),], aes(x=age_m, y=inhib_percfirstt), color="#C3B3BF") +
  geom_smooth(data=xdata.c[which(xdata.c$status == "b"),], aes(x=age_m, y=inhib_percfirstt), color='#C3B3BF', method=loess, size=1, se = FALSE) +
  ggpubr::stat_cor(data=xdata.c[which(xdata.c$status == "b"),], aes(x=age_m, y=inhib_percfirstt), label.y=10, label.x=140) + 
  annotate('text', x=160, hjust=0, y=15, label='Breeders') +
  geom_point(data=xdata.c[which(xdata.c$status == "h"),], aes(x=age_m, y=inhib_percfirstt), color="#697D75") +
  geom_smooth(data=xdata.c[which(xdata.c$status == "h"),], aes(x=age_m, y=inhib_percfirstt), color='#697D75', method=loess, size=1, se =FALSE) +
  ggpubr::stat_cor(data=xdata.c[which(xdata.c$status == "h"),], aes(x=age_m, y=inhib_percfirstt), label.y=40, label.x=2) + 
  annotate('text', x=2, hjust=0, y=45, label='Helpers') +
  xlab("Age in months") + 
  ylab("Percent trials first try") + 
  theme_classic()

summary(lm(inhib_percfirstt ~ age_2 + sex + status, data=xdata))
summary(lm(inhib_percfirstt ~ age_2 + sex + status + groupsize, data=xdata))

##looking at just task experience
ggplot() +
  geom_point(data=xdata.c, aes(x=experiencedtasks, y=inhib_percfirstt)) + 
  geom_smooth(data=xdata.c, aes(x=experiencedtasks, y=inhib_percfirstt), color='#787848', method=loess, size=.75) + 
  xlab("Number of tasks experiences") + 
  ylab("Percent trials first try") + 
  theme_classic()

###putting it all together
ggplot() +
  geom_point(data=xdata.c, aes(x=age_m, y=inhib_percfirstt, size=experiencedtasks)) + 
  geom_smooth(data=xdata.c, aes(x=age_m, y=inhib_percfirstt), color='#787848', method=lm, size=.75) + 
  facet_wrap(xdata.c$sexstatus) + 
  xlab("Age in months)") + 
  ylab("Percent trials first try") +
  theme_light()


#### with raw data #### 
raw_inhib <- readRDS("data/raw_inhib.Rds")

inhib <- raw_inhib[which(raw_inhib$species ==1),] #keep marmosets
inhib <- inhib[which(inhib$firsttry != "N/A"),]#temporarily two NAs from lars

inhib$firsttry <- ifelse(inhib$firsttry == "No", 0, 1) #recode first try 
inhib$anytry <- ifelse(inhib$firsttry == 1 | inhib$onemin == "Yes" | inhib$twomin == "Yes", 1, 0)

xdata.c$both <- "yes"

both <- xdata.c[,c(1,35)]

inhib.c <- merge(inhib, xdata.c)
unique(inhib.c$name)

#running the model 

#with group size 
pm1.g <- glmer(firsttry ~ scale(age_2) + scale(age_m) + status + sex + groupsize + (1|name) + (1|sessionnum), data = inhib.c, family = poisson)
summary(pm1.g)
plot(allEffects(pm1.g))

#no group size 
pm1 <- glmer(firsttry ~ scale(age_2) +  scale(age_m) + status   + sex + (1|name) + (1|sessionnum), data = inhib, family = binomial)
summary(pm1)
plot(allEffects(pm1))
simulationOutput <- simulateResiduals(fittedModel = pm1, plot = F)
plot(simulationOutput)
plotResiduals(simulationOutput, form = inhib.c$age_2)

AICc(pm1.g, pm1) #definitely better without group size 

#adding task experience
#no group size 
pm1.exp <- glmer(firsttry ~ scale(age_2) +  scale(age_m) + status + sex + experiencedtasks + (1|name) + (1|sessionnum), data = inhib.c, family = binomial)
summary(pm1.exp)
plot(allEffects(pm1))
simulationOutput <- simulateResiduals(fittedModel = pm1, plot = F)
plot(simulationOutput)
plotResiduals(simulationOutput, form = inhib.c$age_2)

AICc(pm1.g, pm1, pm1.exp) #best with experience

#could also do it by number of successes for each session 
temp <- as.data.frame(table(inhib.c$name, inhib.c$sessionnum, inhib.c$firsttry))
temp <- temp[which(temp$Var3 == 1),]
temp$Var3 <- NULL
colnames(temp) <- c("name","session", "firsttry")
inds <- xdata.c[,c(1:12)] #pull useful 
bysess <- merge(temp, inds, by="name", all.x=TRUE)

bysess <- merge(bysess, task, by="name", all.x=TRUE)

pm2 <- glmer(firsttry ~ scale(age_2) + scale(age_m) + status  + sex + experiencedtasks + (1|name) + (1|session), data = bysess, family = poisson)
summary(pm2)


#middle trial only 
mdata <- inhib.c[which(inhib.c$grapepos == "M"),]
pm1.m <- glmer(firsttry ~ scale(age_2) + scale(age_m) + status  + sex  + (1|name) + (1|sessionnum), data = mdata, family = poisson)
summary(pm1.m)


#what if we try to include learning here by looking at time 
pm3 <- glmer(firsttry ~ scale(age_2) + scale(age_m) + status  + sex  + scale(serialtrialnum) +  (1|name) + (1|sessionnum), data = inhib, family = poisson)
summary(pm3)

pm3.int <- glmer(firsttry ~ scale(age_2) + scale(age_m) + status + sex  + status*scale(serialtrialnum) + (1|name) + (1|sessionnum), data = inhib, family = poisson)
summary(pm3.int)
plot(allEffects(pm3.int))

ggplot(aes(x=serialtrialnum, y=firsttry), data =inhib.c) +
  geom_point() + 
  stat_smooth(method = "loess", se = F, span = .9, aes(group=status)) + 
  theme(panel.grid = ggplot2::element_blank()) +
  ylab("Correct first try") +
  xlab("Trial number") + 
  facet_wrap(inhib.c$status) + 
  theme_light()

ggplot(aes(x=serialtrialnum, y=anytry), data =inhib.c) +
  geom_point() + 
  stat_smooth(method = "loess", se = F, span = .9, aes(group=status)) + 
  theme(panel.grid = ggplot2::element_blank()) +
  ylab("Reward retrieved") +
  facet_wrap(inhib.c$status)


#middle vs right/left 
inhib.m <- inhib.c[which(inhib.c$grapepos == "M"),]
pm3.int.m <- glmer(firsttry ~ scale(age_2) + scale(age_m) + status + sex + status*scale(serialtrialnum) + (1|name) + (1|sessionnum), data = inhib.m, family = poisson)
summary(pm3.int.m) ###not sure why it doens't converge 

inhib.nm <- inhib.c[which(inhib.c$grapepos != "M"),]
pm3.int.nm <- glmer(firsttry ~ scale(age_2) + scale(age_m) + status + sex + groupsize + status*scale(serialtrialnum) + (1|name) + (1|sessionnum), data = inhib.nm, family = poisson)
summary(pm3.int.nm)


#learning over the sessions by session #### 
#visual
ggplot(aes(x=session, y=firsttry), data =bysess) +
  geom_point() + 
  stat_smooth(method = "loess", se = F, span = .9, aes(group=sexstatus)) + 
  theme(panel.grid = ggplot2::element_blank()) +
  ylab("Number correct first try") +
  xlab("Session") + 
  facet_wrap(bysess$sexstatus) + 
  theme_light()

ggplot(aes(x=session, y=firsttry), data =bysess) +
  geom_point() + 
  stat_smooth(method = "loess", se = F, span = .9, aes(group=status)) + 
  theme(panel.grid = ggplot2::element_blank()) +
  ylab("Number correct first try") +
  xlab("Session") + 
  facet_wrap(bysess$status) + 
  theme_light()

ggplot(aes(x=session, y=firsttry), data =bysess) +
  geom_point() + 
  stat_smooth(method = "loess", se = F, span = .9, aes(group=name)) + 
  theme(panel.grid = ggplot2::element_blank()) +
  ylab("Number correct first try") +
  facet_wrap(bysess$name)

#what if we just compare first and last session 
fs.bysess <- bysess[which(bysess$session == 1 | bysess$session == 5),]
ggplot(aes(x=session, y=firsttry), data =fs.bysess) +
  geom_point() + 
  stat_smooth(method = "loess", se = F, span = .9, aes(group=status)) + 
  theme(panel.grid = ggplot2::element_blank()) +
  ylab("Number correct first try") +
  facet_wrap(fs.bysess$status) + 
  theme_light()

#taking the difference 
fs.bysess <- tidyr::pivot_wider(fs.bysess, values_from="firsttry", names_from="session")
names(fs.bysess)[names(fs.bysess)=="1"] <- "one"
names(fs.bysess)[names(fs.bysess)=="5"] <- "five"

fs.bysess$change.fs <- fs.bysess$five - fs.bysess$one

ggplot() +
  geom_point(data=fs.bysess, aes(x=age_m, y=change.fs)) + 
  geom_smooth(data=fs.bysess, aes(x=age_m, y=change.fs), color='#787848', method=loess, size=.75) + 
  facet_wrap(fs.bysess$status) + 
  xlab("Age in months)") + 
  ylab("Change from first to last session") +
  theme_light()

summary(lm(change.fs ~ age_2 + age_m + sex + status, data=fs.bysess))
  
#relation to persistance?? 
as.data.frame(colnames(xdata.c))
temp <- xdata.c[,c(1,18)]
fs.bysess <- merge(fs.bysess, temp)

#doesn't seem to be related 
summary(lm(change.fs ~ persist_ptottime_cp5, data=fs.bysess))


###adding curiosity #### 
cc <- read.csv("data/curiosity_clean.csv")

#keeping the columns i want 
as.data.frame(colnames(cc))
ccs <- cc[,c(1:12,70:74,111:122)] 

#get means and sums for each individual 
as.data.frame(colnames(ccs))

library(dplyr)
#sum 
sum_m_d_all <- ccs %>% group_by(name) %>% summarise(sum_m_d_all = mean(sum_m_d, na.rm = T))
sum_as_d_all <- ccs %>% group_by(name) %>% summarise(sum_as_d_all = mean(sum_as_d, na.rm = T))
sum_asm_d_all <- ccs %>% group_by(name) %>% summarise(sum_asm_d_all = mean(sum_asm_d, na.rm = T))

temp <- merge(sum_m_d_all, sum_as_d_all)
ccs.sum <-  merge(temp, sum_asm_d_all)

#mean
as_allobjects_d <- ccs %>% group_by(name) %>% summarise(as_allobjects_d_all = mean(as_allobjects_d, na.rm = T))
m_allobjects_d <- ccs %>% group_by(name) %>% summarise(m_allobjects_d_all = mean(m_allobjects_d, na.rm = T))
as_allobjects_f <- ccs %>% group_by(name) %>% summarise(as_allobjects_f_all = mean(as_allobjects_f, na.rm = T))
m_allobjects_f <- ccs %>% group_by(name) %>% summarise(m_allobjects_f_all = mean(m_allobjects_f, na.rm = T))
as_numb_ob <- ccs %>% group_by(name) %>% summarise(as_numb_ob_all = mean(as_numb_ob, na.rm = T))
as_perc_ob <- ccs %>% group_by(name) %>% summarise(as_perc_ob_all = mean(as_perc_ob, na.rm = T))
m_numb_ob <- ccs %>% group_by(name) %>% summarise(m_numb_ob_all = mean(m_numb_ob, na.rm = T))
m_perc_ob <- ccs %>% group_by(name) %>% summarise(m_perc_ob_all = mean(m_perc_ob, na.rm = T))

temp <- merge(as_allobjects_d, m_allobjects_d)
temp <- merge(temp, as_allobjects_f)
temp <- merge(temp, m_allobjects_f)
temp <- merge(temp, as_numb_ob)
temp <- merge(temp, as_perc_ob)
temp <- merge(temp, m_numb_ob)
temp <- merge(temp, m_perc_ob)
ccs.mean <- temp

ccss <- merge(ccs.sum, ccs.mean)

#also just take the value from the first session 
ccf <- cc[,c(1, 3,70:74,111:118)] 
ccf <- ccf[which(ccf$session == "CP1"),]
ccf$session <- NULL
colnames(ccf) <- paste(colnames(ccf),"first",sep="_")
names(ccf)[1] <- "name"

CC <- merge(ccss, ccf)

#add inhib to this 
#clean up 
as.data.frame(colnames(fullaug2019))
cols <- names(fullaug2019)[c(2,3,4,5,6,10,12,30)]
fullaug2019[cols] <- lapply(fullaug2019[cols], as.factor)
cols <- names(fullaug2019)[c(13:29,31:33)]
fullaug2019[cols] <- lapply(fullaug2019[cols], as.numeric)

#pull inhibition participants 
xdata <- fullaug2019
xdata <- xdata[which(xdata$inhib_percfirstt != "NA"),]
xdata <- xdata[which(xdata$species ==1 ),]

CC.I <- merge(xdata, CC, by="name")

##add task experience 
CC.I <- merge(CC.I, task, by="name", all.x=TRUE)

table(CC.I$status, CC.I$sex)
hist(CC.I$age_m)


### results part 2 - visuals of just exploration#### 

#diversity - number of objects 
ggplot() +
  geom_boxplot(data=CC.I, aes(x=sex, y=as_numb_ob_first, group = sex)) + 
  xlab("Sex") + 
  ylab("Diversity score") + 
  theme_classic()

#status
ggplot() +
  geom_boxplot(data=CC.I, aes(x=status, y=as_numb_ob_first, group = status)) + 
  xlab("Status") + 
  ylab("Diversity score") + 
  theme_classic()


#group size 
ggplot() +
  geom_boxplot(data=CC.I, aes(x=groupsize, y=as_numb_ob_first, group = groupsize)) + 
  xlab("Group size") + 
  ylab("Diversity score") + 
  theme_classic()

#age
ggplot() +
  geom_point(data=CC.I, aes(x=age_m, y=as_numb_ob_first)) + 
  geom_smooth(data=CC.I, aes(x=age_m, y=as_numb_ob_first), color='#787848', method=lm, size=.75) + 
  xlab("Age in months") + 
  ylab("Diversity score") + 
  theme_classic()
#first increase, then decrease with age 

#divide by breeder and helper 
ggplot() +
  geom_point(data=CC.I[which(CC.I$status == "b"),], aes(x=age_m, y=as_numb_ob_first), color="#C3B3BF") +
  geom_smooth(data=CC.I[which(CC.I$status == "b"),], aes(x=age_m, y=as_numb_ob_first), color='#C3B3BF', method=loess, size=1, se = FALSE) +
  ggpubr::stat_cor(data=CC.I[which(CC.I$status == "b"),], aes(x=age_m, y=as_numb_ob_first), label.y=8, label.x=140) + 
  annotate('text', x=160, hjust=0, y=9, label='Breeders') +
  geom_point(data=CC.I[which(CC.I$status == "h"),], aes(x=age_m, y=as_numb_ob_first), color="#697D75") +
  geom_smooth(data=CC.I[which(CC.I$status == "h"),], aes(x=age_m, y=as_numb_ob_first), color='#697D75', method=loess, size=1, se =FALSE) +
  ggpubr::stat_cor(data=CC.I[which(CC.I$status == "h"),], aes(x=age_m, y=as_numb_ob_first), label.y=10, label.x=2) + 
  annotate('text', x=2, hjust=0, y=11, label='Helpers') +
  xlab("Age in months") + 
  ylab("Diversity score") + 
  theme_classic()

summary(lm(as_numb_ob_first ~ age_2 + age_m + sex + status, data=CC.I))
summary(lm(as_numb_ob_first ~ age_2 + age_m + sex + status + groupsize, data=CC.I))

###putting it all together
ggplot() +
  geom_point(data=CC.I, aes(x=age_m, y=as_numb_ob_first)) + 
  geom_smooth(data=CC.I, aes(x=age_m, y=as_numb_ob_first), color='#787848', method=lm, size=.75) + 
  facet_wrap(CC.I$sexstatus) + 
  xlab("Age (in months)") + 
  ylab("Diversity score") +
  theme_light()



# results part 3a - quick visual inhibition perc first +exploration in the first session ####

#number of objects 
ggplot() +
  geom_point(data=CC.I, aes(x=inhib_percfirstt, y=as_numb_ob_first)) + 
  geom_smooth(data=CC.I, aes(x=inhib_percfirstt, y=as_numb_ob_first), color='#787848', method=lm, size=.75) + 
  ggpubr::stat_cor(data=CC.I, aes(x=inhib_percfirstt, y=as_numb_ob_first)) + 
  xlab("Percent trials first try (inhibition)") + 
  ylab("Number of objects AS") + 
  theme_classic()

ggplot() +
  geom_point(data=CC.I, aes(x=inhib_percfirstt, y=as_numb_ob_first)) + 
  geom_smooth(data=CC.I, aes(x=inhib_percfirstt, y=as_numb_ob_first), color='#787848', method=lm, size=.75) + 
  ggpubr::stat_cor(data=CC.I, aes(x=inhib_percfirstt, y=as_numb_ob_first)) + 
  xlab("Percent trials first try (inhibition)") + 
  ylab("Number of objects AS") + 
  facet_wrap(CC.I$status) + 
  theme_classic()

summary(lm(inhib_percfirstt ~ age_2 + sex + status + groupsize + as_numb_ob_first, data=CC.I))

#more objects, lower inhibition 

ggplot() +
  geom_point(data=CC.I, aes(x=as_numb_ob_first, y=as.numeric(groupsize))) + 
  geom_smooth(data=CC.I, aes(x=as_numb_ob_first, y=as.numeric(groupsize)), color='#787848', method=lm, size=.75) + 
  ggpubr::stat_cor(data=CC.I, aes(x=as_numb_ob_first, y=as.numeric(groupsize))) + 
  xlab("Number of objects AS") + 
  ylab("Group size") + 
  theme_classic()

#those in larger groups touch more objects, so this makes sense 

#frequency 
ggplot() +
  geom_point(data=CC.I, aes(x=inhib_percfirstt, y=as_allobjects_f_first)) + 
  geom_smooth(data=CC.I, aes(x=inhib_percfirstt, y=as_allobjects_f_first), color='#787848', method=lm, size=.75) + 
  ggpubr::stat_cor(data=CC.I, aes(x=inhib_percfirstt, y=as_allobjects_f_first)) + 
  xlab("Percent trials first try (inhibition)") + 
  ylab("Frequency of AS") + 
  theme_classic()


summary(lm(inhib_percfirstt ~ age_2 + sex + status + groupsize + as_allobjects_f_first, data=CC.I))

summary(lm(inhib_percfirstt ~ age_2 + sex + status + groupsize + as_numb_ob_first + as_allobjects_f_first, data=CC.I))


#duration 
ggplot() +
  geom_point(data=CC.I, aes(x=inhib_percfirstt, y=as_allobjects_d_first)) + 
  geom_smooth(data=CC.I, aes(x=inhib_percfirstt, y=as_allobjects_d_first), color='#787848', method=lm, size=.75) + 
  ggpubr::stat_cor(data=CC.I, aes(x=inhib_percfirstt, y=as_allobjects_d_first)) + 
  xlab("Percent trials first try (inhibition)") + 
  ylab("Duration of AS") + 
  theme_classic()


summary(lm(inhib_percfirstt ~ age_2 + sex + status + groupsize + as_allobjects_d_first, data=CC.I))

ggplot() +
  geom_point(data=CC.I, aes(x=inhib_percfirstt, y=sum_asm_d_first)) + 
  geom_smooth(data=CC.I, aes(x=inhib_percfirstt, y=sum_asm_d_first), color='#787848', method=lm, size=.75) + 
  ggpubr::stat_cor(data=CC.I, aes(x=inhib_percfirstt, y=sum_asm_d_first)) + 
  xlab("Percent trials first try (inhibition)") + 
  ylab("Total time spent with panel") + 
  theme_classic()

###results part 3b - raw inhib + C models####

inhib.ccf <- merge(inhib, ccf, by="name")
inhib.ccf <- merge(inhib.ccf, task, by="name")
pmc1 <- glmer(firsttry ~ scale(age_2) + scale(age_m) + status + sex  + experiencedtasks + scale(as_numb_ob_first) + (1|name) + (1|sessionnum), data = inhib.ccf, family = poisson)
summary(pmc1)
plot(allEffects(pmc1))

pmc2 <- glmer(firsttry ~ scale(age_2) + scale(age_m) + status + sex  + experiencedtasks + scale(as_allobjects_f_first) + (1|name) + (1|sessionnum), data = inhib.ccf, family = poisson)
summary(pmc2)
plot(allEffects(pmc2))

pmc3 <- glmer(firsttry ~ scale(age_2) + scale(age_m) + status + sex  + experiencedtasks + scale(as_numb_ob_first)  + scale(as_allobjects_f_first) + (1|name) + (1|sessionnum), data = inhib.ccf, family = poisson)
summary(pmc3)
plot(allEffects(pmc3))


#final model 
pmc4 <- lme4::glmer(firsttry ~ scale(age_2) + scale(age_m) + status + sex  + experiencedtasks + scale(as_numb_ob_first)  + scale(as_allobjects_f_first) + scale(serialtrialnum) + (1|name) + (1|sessionnum), data = inhib.ccf, family = poisson)
summary(pmc4)
plot(allEffects(pmc4))


AICc(pmc1, pmc2, pmc3, pmc4)


###visuals for last model 

model_age <- data.frame(effect("scale(age_2)", pmc4, xlevels = 30))

model_age.plot <- ggplot(model_age, aes(x = age_2, y = fit)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha = 0.2) + 
  geom_jitter(data = inhib.ccf, aes(x = age_2, y = firsttry), height = 0.05, alpha = 0.2) + 
  ylab("Inhibition score") + 
  xlab("Age squared") + 
  theme_classic()

model_numob <- data.frame(effect("scale(as_numb_ob_first)", pmc4, xlevels = 30))

model_numob.plot <- ggplot(model_numob, aes(x = as_numb_ob_first, y = fit)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha = 0.2) + 
  geom_jitter(data = inhib.ccf, aes(x = as_numb_ob_first, y = firsttry), height = 0.05, alpha = 0.2) + 
  ylab("Inhibition score") + 
  xlab("Number of objects interacted with") + 
  theme_classic()

model_freq <- data.frame(effect("scale(as_allobjects_f_first)", pmc4, xlevels = 30))

model_freq.plot <- ggplot(model_freq, aes(x = as_allobjects_f_first, y = fit)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha = 0.2) + 
  geom_jitter(data = inhib.ccf, aes(x = as_allobjects_f_first, y = firsttry), height = 0.05, alpha = 0.2) + 
  ylab("Inhibition score") + 
  xlab("Frequency of interaction") + 
  theme_classic()

model <- as.data.frame(summary(pmc4)$coefficients)

jtools::effect_plot(pmc4, data=inhib.ccf, pred=age_2, interval = TRUE, plot.points = TRUE, jitter =.1)
jtools::plot_summs(pmc4, colors = "black", coefs = c("Age squared" = "scale(age_2)", "Breeding status (helper vs. breeder)" = "statush", "Sex (male vs. female)" = "sexm","Previous task experience" = "experiencedtasks",
                                   "Number of objects explored" = "scale(as_numb_ob_first)", "Frequency of exploration" = "scale(as_allobjects_f_first)",
                                  "Trial number" = "scale(serialtrialnum)"),
                   inner_ci_level =.9)
sjPlot::plot_model(pmc4)




