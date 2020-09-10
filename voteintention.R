library(ggplot2)
library(tikzDevice)

stemconsult_data_numeric <- read_csv("stemconsult_data_numeric.csv")
myvars <- c("ExpCondition", "voteIntention")
intention <- stemconsult_data_numeric[myvars]

intention$voteIntention[intention$voteIntention == -999] <- NA
intention$voteIntention[intention$voteIntention == -998] <- NA
intention$voteIntention[intention$voteIntention == -997] <- NA
intention$voteIntention[intention$voteIntention == -996] <- NA
intention$voteIntention[intention$voteIntention == -995] <- NA
intention$voteIntention[intention$voteIntention == -994] <- NA
intention <- na.omit(intention)

intention$ExpCondition <- as.factor(intention$ExpCondition)

model <- aov(voteIntention ~ ExpCondition, data=intention)

intention$voteIntention <- factor(intention$voteIntention, levels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"), labels=c("50Plus","CDA","CU","D66","DENK","GL","PvdA","PvdD","PVV","SGP","SP","VVD","VNL","FvD","Other"))

intention$voteIntention = factor(intention$voteIntention,levels(intention$voteIntention)[c(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)])

tikz(file = "voteintention.tex")
ggplot(data=intention, aes(x=voteIntention, fill=ExpCondition))+
  geom_bar(position = "dodge")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
  coord_flip()+
  scale_fill_grey(name="Version",  breaks=c("1", "2"),labels=c("A", "B"))+
  ylab("Count")+
  xlab("Party")+
  theme_classic()
dev.off()


