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
xticks = c(1,10,20,30,40,50,60,70,80,90,100)

stat_sig = 0.05
alpha_nosig = 0.5
alpha_sig = 1

color_young_sig = "#f8766d"
color_young_nosig = "#fef1f0"
color_eld_sig = "#00bfc4"
color_eld_nosig = "#e6f8f9"



# Load NS UC Roll from Excel
NSUCR <- read_excel("NSUCR.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))


count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100


ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=45, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=45, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=75, y=45, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Upper Care Roll") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load NS UC Pitch from Excel
NSUCP <- read_excel("NSUCP.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))


count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100


ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=50, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=50, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=75, y=50, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Upper Care Pitch") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load NS UC Yaw from Excel
NSUCY <- read_excel("NSUCY.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100


ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=20, y=50, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=50, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=80, y=50, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Upper Care Yaw") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load NS MC Roll from Excel
NSMCR <- read_excel("NSMCR.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=45, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=30, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=75, y=45, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Medium Care Roll") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.85), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load NS MC Pitch from Excel
NSMCP <- read_excel("NSMCP.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=45, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=45, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=77, y=45, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Medium Care Pitch") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.90), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load NS MC Yaw from Excel
NSMCY <- read_excel("NSMCY.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=20, y=45, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=60, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=80, y=60, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Medium Care Yaw") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load NS LC Roll from Excel
NSLCR <- read_excel("NSLCR.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=90, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=90, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=75, y=90, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Lower Care Roll") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)

# Load NS LC Pitch from Excel
NSLCP <- read_excel("NSLCP.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=15, y=15, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=20, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=85, y=15, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Lower Care Pitch") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.48,.93), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load NS LC Yaw from Excel
NSLCY <- read_excel("NSLCY.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=20, y=20, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=20, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=85, y=0, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Lower Care Yaw") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load NS FR Roll from Excel
NSFRR <- read_excel("NSFRR.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=10, y=45, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=45, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=85, y=45, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Frontal Reaching Roll") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load NS FR Pitch from Excel
NSFRP <- read_excel("NSFRP.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=8, y=45, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=45, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=93, y=45, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Frontal Reaching Pitch") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load NS FR Yaw from Excel
NSFRY <- read_excel("NSFRY.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=50, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=50, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=75, y=50, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Frontal Reaching Yaw") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load NS UR Roll from Excel
NSURR <- read_excel("NSURR.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=50, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=50, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=73, y=50, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Upper Reaching Roll") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load NS UR Pitch from Excel
NSURP <- read_excel("NSURP.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=45, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=45, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=91, y=45, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Upper Reaching Pitch") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load NS UR Yaw from Excel
NSURY <- read_excel("NSURY.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=55, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=55, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=75, y=55, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Non Standardized Upper Reaching Yaw") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST UC Roll from Excel
STUCR <- read_excel("STUCR.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=45, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=45, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=80, y=45, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Upper Care Roll") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST UC Pitch from Excel
STUCP <- read_excel("STUCP.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=10, y=75, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=75, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=90, y=75, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Upper Care Pitch") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST UC Yaw from Excel
STUCY <- read_excel("STUCY.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=20, y=45, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=100, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=80, y=45, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Upper Care Yaw") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST MC Roll from Excel
STMCR <- read_excel("STMCR.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=65, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=25, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=80, y=65, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Medium Care Roll") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST MC Pitch from Excel
STMCP <- read_excel("STMCP.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=20, y=45, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=45, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=80, y=45, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Medium Care Pitch") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST MC Yaw from Excel
STMCY <- read_excel("STMCY.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=60, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=60, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=75, y=60, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Medium Care Yaw") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST LC Roll from Excel
STLCR <- read_excel("STLCR.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=100, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=100, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=75, y=100, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Lower Care Roll") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST LC Pitch from Excel
STLCP <- read_excel("STLCP.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=20, y=15, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=15, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=80, y=15, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Lower Care Pitch") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST LC Yaw from Excel
STLCY <- read_excel("STLCY.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=20, y=20, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=20, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=75, y=20, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Lower Care Yaw") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST FR Roll from Excel
STFRR <- read_excel("STFRR.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=15, y=15, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=45, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=85, y=15, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Frontal Reaching Roll") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST FR Pitch from Excel
STFRP <- read_excel("STFRP.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=15, y=45, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=45, y=45, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=80, y=45, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Frontal Reaching Pitch") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST FR Yaw from Excel
STFRY <- read_excel("STFRY.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=55, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=55, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=75, y=55, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Frontal Reaching Yaw") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST UR Roll from Excel
STURR <- read_excel("STURR.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=15, y=15, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=45, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=90, y=15, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Upper Reaching Roll") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.50,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=20),
        axis.title = element_text(size=20),
        axis.text.x = element_text(size=20), 
        axis.text.y = element_text(size=20),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST UR Pitch from Excel
STURP <- read_excel("STURP.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=15, y=45, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=45, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=85, y=45, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Upper Reaching Pitch") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST UR Yaw from Excel
STURY <- read_excel("STURY.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=25, y=60, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=60, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=75, y=60, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Standardized Upper Reaching Yaw") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST DR Roll from Excel
STDRR <- read_excel("STDRR.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=20, y=45, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=45, y=45, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=80, y=45, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Driving Roll") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.80), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST DR Pitch from Excel
STDRP <- read_excel("STDRP.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=20, y=45, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=55, y=45, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=80, y=45, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Driving Pitch") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)


# Load ST DR Yaw from Excel
STDRY <- read_excel("STDRY.xlsx",sheet = "Sheet1")

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

for(i in 1:100){
  if(lscont$p.value[i] < stat_sig){
    lsmeans$alpha[i] = alpha_sig
    lsmeans$alpha[i+100] = alpha_sig
    
    lsmeans$color[i] = color_young_sig
    lsmeans$color[i+100] = color_eld_sig
  }
  else{
    lsmeans$alpha[i] = alpha_nosig
    lsmeans$alpha[i+100] = alpha_nosig
    
    lsmeans$color[i] = color_young_nosig
    lsmeans$color[i+100] = color_eld_nosig
  }
}

lsmeans$alpha=factor(lsmeans$alpha, levels=c(alpha_nosig,alpha_sig), labels=c("p>0.05","p<0.05"))
lsmeans$color=factor(lsmeans$color, levels=c(color_young_nosig,color_young_sig,color_eld_nosig,color_eld_sig), labels=c("Young p>0.05","Young", "Elderly p>0.05","Elderly"))

count = 0

for(i in 1:33){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perFirst = (count/33)*100

count = 0

for(i in 34:66){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perSecond = (count/33)*100

count = 0

for(i in 67:100){
  if(lscont$p.value[i] < stat_sig){
    count = count + 1
  }
}

perThird = (count/33)*100

ggplot(data=lsmeans, aes(x = Timestamp, y = emmean, color = lsmeans$color)) +
  scale_color_manual(values=c("Young p>0.05"=color_young_nosig,"Young"=color_young_sig,"Elderly p>0.05"=color_eld_nosig,"Elderly"=color_eld_sig), breaks=c("Young","Elderly")) + 
  geom_point(size = 3) +
  geom_vline(xintercept=33) +
  geom_vline(xintercept=66) +
  geom_text(x=20, y=60, aes(label=paste(as.character(round(perFirst, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=50, y=60, aes(label=paste(as.character(round(perSecond, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  geom_text(x=80, y=60, aes(label=paste(as.character(round(perThird, digits=2)), "%", sep=" ")), color="black", show.legend = FALSE, size=8) + 
  guides(alpha="none") +  labs(x = "% Movement", y = "% ROM", color = "", alpha="" ) + 
  ggtitle("Driving Yaw") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=29), 
        legend.position = c(.49,.20), 
        legend.direction = "horizontal",
        legend.text = element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=25), 
        axis.text.y = element_text(size=25),
        plot.margin =  unit(c(0.5,0.7,0.5,0.5), "cm")) +
  scale_x_discrete(labels= xlabels, breaks=xticks)
