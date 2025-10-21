library(readxl)
library(dunn.test)
library(ggplot2)
library(dplyr)
library(lme4)
library(nlme)
library(emmeans)
library(tidyr)
library(ez)

equation =  "Angle~Timestamp*Age+(1|Subject)+(1|Subject:Repetition)"


xlabels <- c("2","10","20","30","40","50","60","70","80","90","100")
xticks = c(1,9,19,29,39,49,59,69,79,89,99)

stat_sig = 0.05
alpha_nosig = 0.5
alpha_sig = 1

color_young_sig = "#f8766d"
color_young_nosig = "#fef1f0"
color_eld_sig = "#00bfc4"
color_eld_nosig = "#e6f8f9"

# Load NS UC Roll from Excel
NSUCR <- read_excel("NSUCR_diff.xlsx",sheet = "Sheet1")

NSUCR <- NSUCR %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSUCR <- lmer(equation, data = NSUCR)
summary(lme_NSUCR)

emm <- emmeans(lme_NSUCR, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Upper Care Roll (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load NS UC Pitch from Excel
NSUCP <- read_excel("NSUCP_diff.xlsx",sheet = "Sheet1")

NSUCP <- NSUCP %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSUCP <- lmer(equation, data = NSUCP)
summary(lme_NSUCP)

emm <- emmeans(lme_NSUCP, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Upper Care Pitch (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load NS UC Yaw from Excel
NSUCY <- read_excel("NSUCY_diff.xlsx",sheet = "Sheet1")

NSUCY <- NSUCY %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSUCY <- lmer(equation, data = NSUCY)
summary(lme_NSUCY)

emm <- emmeans(lme_NSUCY, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Upper Care Yaw (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load NS MC Roll from Excel
NSMCR <- read_excel("NSMCR_diff.xlsx",sheet = "Sheet1")

NSMCR <- NSMCR %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSMCR <- lmer(equation, data = NSMCR)
summary(lme_NSMCR)

emm <- emmeans(lme_NSMCR, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Medium Care Roll (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load NS MC Pitch from Excel
NSMCP <- read_excel("NSMCP_diff.xlsx",sheet = "Sheet1")

NSMCP <- NSMCP %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSMCP <- lmer(equation, data = NSMCP)
summary(lme_NSMCP)

emm <- emmeans(lme_NSMCP, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Medium Care Pitch (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.40,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load NS MC Yaw from Excel
NSMCY <- read_excel("NSMCY_diff.xlsx",sheet = "Sheet1")

NSMCY <- NSMCY %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSMCY <- lmer(equation, data = NSMCY)
summary(lme_NSMCY)

emm <- emmeans(lme_NSMCY, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Medium Care Yaw (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 17))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load NS LC Roll from Excel
NSLCR <- read_excel("NSLCR_diff.xlsx",sheet = "Sheet1")

NSLCR <- NSLCR %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSLCR <- lmer(equation, data = NSLCR)
summary(lme_NSLCR)

emm <- emmeans(lme_NSLCR, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Lower Care Roll (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load NS LC Pitch from Excel
NSLCP <- read_excel("NSLCP_diff.xlsx",sheet = "Sheet1")

NSLCP <- NSLCP %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSLCP <- lmer(equation, data = NSLCP)
summary(lme_NSLCP)

emm <- emmeans(lme_NSLCP, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Lower Care Pitch (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.35,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load NS LC Yaw from Excel
NSLCY <- read_excel("NSLCY_diff.xlsx",sheet = "Sheet1")

NSLCY <- NSLCY %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSLCY <- lmer(equation, data = NSLCY)
summary(lme_NSLCY)

emm <- emmeans(lme_NSLCY, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Lower Care Yaw (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.45,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load NS FR Roll from Excel
NSFRR <- read_excel("NSFRR_diff.xlsx",sheet = "Sheet1")

NSFRR <- NSFRR %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSFRR <- lmer(equation, data = NSFRR)
summary(lme_NSFRR)

emm <- emmeans(lme_NSFRR, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Frontal Reaching Roll (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load NS FR Pitch from Excel
NSFRP <- read_excel("NSFRP_diff.xlsx",sheet = "Sheet1")

NSFRP <- NSFRP %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSFRP <- lmer(equation, data = NSFRP)
summary(lme_NSFRP)

emm <- emmeans(lme_NSFRP, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Frontal Reaching Pitch (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load NS FR Yaw from Excel
NSFRY <- read_excel("NSFRY_diff.xlsx",sheet = "Sheet1")

NSFRY <- NSFRY %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSFRY <- lmer(equation, data = NSFRY)
summary(lme_NSFRY)

emm <- emmeans(lme_NSFRY, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Frontal Reaching Yaw (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))

# Load NS UR Roll from Excel
NSURR <- read_excel("NSURR_diff.xlsx",sheet = "Sheet1")

NSURR <- NSURR %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSURR <- lmer(equation, data = NSURR)
summary(lme_NSURR)

emm <- emmeans(lme_NSURR, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Upper Reaching Roll (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 1))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load NS UR Pitch from Excel
NSURP <- read_excel("NSURP_diff.xlsx",sheet = "Sheet1")

NSURP <- NSURP %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSURP <- lmer(equation, data = NSURP)
summary(lme_NSURP)

emm <- emmeans(lme_NSURP, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Upper Reaching Pitch (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load NS UR Yaw from Excel
NSURY <- read_excel("NSURY_diff.xlsx",sheet = "Sheet1")

NSURY <- NSURY %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_NSURY <- lmer(equation, data = NSURY)
summary(lme_NSURY)

emm <- emmeans(lme_NSURY, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Upper Reaching Yaw (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST UC Roll from Excel
STUCR <- read_excel("STUCR_diff.xlsx",sheet = "Sheet1")

STUCR <- STUCR %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STUCR <- lmer(equation, data = STUCR)
summary(lme_STUCR)

emm <- emmeans(lme_STUCR, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Upper Care Roll (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST UC Pitch from Excel
STUCP <- read_excel("STUCP_diff.xlsx",sheet = "Sheet1")

STUCP <- STUCP %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STUCP <- lmer(equation, data = STUCP)
summary(lme_STUCP)

emm <- emmeans(lme_STUCP, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Upper Care Pitch (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST UC Yaw from Excel
STUCY <- read_excel("STUCY_diff.xlsx",sheet = "Sheet1")

STUCY <- STUCY %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STUCY <- lmer(equation, data = STUCY)
summary(lme_STUCY)

emm <- emmeans(lme_STUCY, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Upper Care Yaw (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.40,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST MC Roll from Excel
STMCR <- read_excel("STMCR_diff.xlsx",sheet = "Sheet1")

STMCR <- STMCR %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STMCR <- lmer(equation, data = STMCR)
summary(lme_STMCR)

emm <- emmeans(lme_STMCR, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Medium Care Roll (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.60,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST MC Pitch from Excel
STMCP <- read_excel("STMCP_diff.xlsx",sheet = "Sheet1")

STMCP <- STMCP %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STMCP <- lmer(equation, data = STMCP)
summary(lme_STMCP)

emm <- emmeans(lme_STMCP, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Medium Care Pitch (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.42,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST MC Yaw from Excel
STMCY <- read_excel("STMCY_diff.xlsx",sheet = "Sheet1")

STMCY <- STMCY %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STMCY <- lmer(equation, data = STMCY)
summary(lme_STMCY)

emm <- emmeans(lme_STMCY, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Medium Care Yaw (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST LC Roll from Excel
STLCR <- read_excel("STLCR_diff.xlsx",sheet = "Sheet1")

STLCR <- STLCR %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STLCR <- lmer(equation, data = STLCR)
summary(lme_STLCR)

emm <- emmeans(lme_STLCR, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Lower Care Roll (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST LC Pitch from Excel
STLCP <- read_excel("STLCP_diff.xlsx",sheet = "Sheet1")

STLCP <- STLCP %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STLCP <- lmer(equation, data = STLCP)
summary(lme_STLCP)

emm <- emmeans(lme_STLCP, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Lower Care Pitch (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.35,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST LC Yaw from Excel
STLCY <- read_excel("STLCY_diff.xlsx",sheet = "Sheet1")

STLCY <- STLCY %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STLCY <- lmer(equation, data = STLCY)
summary(lme_STLCY)

emm <- emmeans(lme_STLCY, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Lower Care Yaw (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 17))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST FR Roll from Excel
STFRR <- read_excel("STFRR_diff.xlsx",sheet = "Sheet1")

STFRR <- STFRR %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STFRR <- lmer(equation, data = STFRR)
summary(lme_STFRR)

emm <- emmeans(lme_STFRR, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Frontal Reaching Roll (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST FR Pitch from Excel
STFRP <- read_excel("STFRP_diff.xlsx",sheet = "Sheet1")

STFRP <- STFRP %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STFRP <- lmer(equation, data = STFRP)
summary(lme_STFRP)

emm <- emmeans(lme_STFRP, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))


ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Frontal Reaching Pitch (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST FR Yaw from Excel
STFRY <- read_excel("STFRY_diff.xlsx",sheet = "Sheet1")

STFRY <- STFRY %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STFRY <- lmer(equation, data = STFRY)
summary(lme_STFRY)

emm <- emmeans(lme_STFRY, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Frontal Reaching Yaw (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 6.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST UR Roll from Excel
STURR <- read_excel("STURR_diff.xlsx",sheet = "Sheet1")

STURR <- STURR %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STURR <- lmer(equation, data = STURR)
summary(lme_STURR)

emm <- emmeans(lme_STURR, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Upper Reaching Roll (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.45), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST UR Pitch from Excel
STURP <- read_excel("STURP_diff.xlsx",sheet = "Sheet1")

STURP <- STURP %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STURP <- lmer(equation, data = STURP)
summary(lme_STURP)

emm <- emmeans(lme_STURP, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Upper Reaching Pitch (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST UR Yaw from Excel
STURY <- read_excel("STURY_diff.xlsx",sheet = "Sheet1")

STURY <- STURY %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STURY <- lmer(equation, data = STURY)
summary(lme_STURY)

emm <- emmeans(lme_STURY, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Upper Reaching Yaw (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST DR Roll from Excel
STDRR <- read_excel("STDRR_diff.xlsx",sheet = "Sheet1")

STDRR <- STDRR %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STDRR <- lmer(equation, data = STDRR)
summary(lme_STDRR)

emm <- emmeans(lme_STDRR, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Driving Roll (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST DR Pitch from Excel
STDRP <- read_excel("STDRP_diff.xlsx",sheet = "Sheet1")

STDRP <- STDRP %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STDRP <- lmer(equation, data = STDRP)
summary(lme_STDRP)

emm <- emmeans(lme_STDRP, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Driving Pitch (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))


# Load ST DR Yaw from Excel
STDRY <- read_excel("STDRY_diff.xlsx",sheet = "Sheet1")

STDRY <- STDRY %>%
  mutate(
    Age = factor(Age, levels = c(0, 1), labels = c("Young", "Elderly")),
    Subject = factor(Subject),
    Repetition = factor(Repetition),
    Timestamp = factor(Timestamp, levels=unique(Timestamp))
  )


lme_STDRY <- lmer(equation, data = STDRY)
summary(lme_STDRY)

emm <- emmeans(lme_STDRY, ~ Timestamp*Age )
lsmeans <- summary(emm, infer = c(TRUE,TRUE), level = 0.95)  # 95% CIs
print(lsmeans)

Context <- paste(lsmeans$Timestamp)

cont <- contrast(emm,'pairwise',by='Timestamp',adjust = "bonferroni")
lscont <- summary(cont)
print(lscont)

for(i in 1:99){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+99] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+99] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+99] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+99] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Driving Yaw (diff)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21.5))) +
  scale_x_discrete(labels= xlabels, breaks=xticks, limits = as.character(c(0:100)))