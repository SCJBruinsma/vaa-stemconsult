library(readr)
library(ggplot2)
library(tidyr)
library(tikzDevice)

parties_df_exp1 <- read_csv("parties.df.exp1.csv",col_names = FALSE)
users_df_exp1 <- read_csv("users.df.exp1.csv",col_names = FALSE)

parties_df_exp2 <- read_csv("parties.df.exp2.csv",col_names = FALSE)
users_df_exp2 <- read_csv("users.df.exp2.csv",col_names = FALSE)


################################################################# Matching #############################################################################################

ignoreValueAbove<-5
parties<-c("50Plus","CDA","CU","D66","GL","PvdA","PvdD","PVV","SGP","SP","VVD","DENK","VNL","FvD")

################ Hybrid #################

forEachUser_hybrid <- function(row, parties, ignoreValueAbove){
  #Find user's matchings with all parties
  match<-apply(parties, 1, forEachParty_hybrid, row, ignoreValueAbove);
  return(match)
}

forEachParty_hybrid <-function(row, userAns, ignoreValueAbove){
  hy <- hybrid(userAns, row, ignoreValueAbove);
  return(hy);
}

hybrid <- function(array1, array2, ignoreValueAbove){
  coeffMatrixHybrid <- matrix(c(1,0.5,0,-0.5,-1,0.5,0.625,0.25,-0.125,-0.5,0,0.25,0.5,0.25,0,-0.5,-0.125,0.25,0.625,0.5,-1,-0.5,0,0.5,1), 5, 5, byrow = T)
  if(length(array1)!=length(array2)){
    NA
  }else{
    coeff<-0;
    counter<-0;
    for(i in 1:length(array1)){
      if(array1[i]>ignoreValueAbove) next;
      if(array2[i]>ignoreValueAbove) { counter <- counter+1; next;}
      coeff <- coeff+(coeffMatrixHybrid[array1[i],array2[i]]);
      counter <- counter+1;
    }
    coeff/counter;
  }
};

stats_exp1_hybrid <-t(apply(users_df_exp1, 1, forEachUser_hybrid, parties_df_exp1, ignoreValueAbove));
stats_exp2_hybrid <-t(apply(users_df_exp2, 1, forEachUser_hybrid, parties_df_exp2, ignoreValueAbove));

colnames(stats_exp1_hybrid) <- parties
colnames(stats_exp2_hybrid) <- parties

write.csv(stats_exp1_hybrid,file = "match_hybrid_exp1", row.names = FALSE)
write.csv(stats_exp2_hybrid,file = "match_hybrid_exp2", row.names = FALSE)

################ City Block #################

forEachUser_city <- function(row, parties, ignoreValueAbove){
  #Find user's matchings with all parties
  match<-apply(parties, 1, forEachParty_city, row, ignoreValueAbove);
  return(match)
}

forEachParty_city <-function(row, userAns, ignoreValueAbove){
  cb <- city(userAns, row, ignoreValueAbove);
  return(cb);
}

city <- function(array1, array2, ignoreValueAbove){
coeffMatrixCityBlock<-matrix(c(1,0.5,0,-0.5,-1,0.5,1,0.5,0,-0.5,0,0.5,1,0.5,0,-0.5,0,0.5,1,0.5,-1,-0.5,0,0.5,1),5,5,byrow=T)
  if(length(array1)!=length(array2)){
    NA
  }else{
    coeff<-0;
    counter<-0;
    for(i in 1:length(array1)){
      if(array1[i]>ignoreValueAbove) next;
      if(array2[i]>ignoreValueAbove) { counter <- counter+1; next;}
      coeff <- coeff+(coeffMatrixCityBlock[array1[i],array2[i]]);
      counter <- counter+1;
    }
    coeff/counter;
  }
};

stats_exp1_city <-t(apply(users_df_exp1, 1, forEachUser_city, parties_df_exp1, ignoreValueAbove));
stats_exp2_city <-t(apply(users_df_exp2, 1, forEachUser_city, parties_df_exp2, ignoreValueAbove));

colnames(stats_exp1_city) <- parties
colnames(stats_exp2_city) <- parties

write.csv(stats_exp1_city,file = "match_city_exp1", row.names = FALSE)
write.csv(stats_exp2_city,file = "match_city_exp2", row.names = FALSE)

################ Euclidean #################

forEachUser_euclid <- function(row, parties, ignoreValueAbove){
  #Find user's matchings with all parties
  match<-apply(parties, 1, forEachParty_euclid, row, ignoreValueAbove);
  return(match)
}

forEachParty_euclid <-function(row, userAns, ignoreValueAbove){
  ec <- euclid(userAns, row, ignoreValueAbove);
  return(ec);
}

euclid <- function(array1, array2, ignoreValueAbove){
coeffMatrixEuclid<-matrix(c(1,0.875,0.5,-0.125,-1,0.875,1,0.875,0.5,-0.125,0.5,0.875,1,0.875,0.5,-0.125,0.5,0.875,1,0.875,-1,-0.125,0.5,0.875,1),5,5,byrow=T)
  if(length(array1)!=length(array2)){
    NA
  }else{
    coeff<-0;
    counter<-0;
    for(i in 1:length(array1)){
      if(array1[i]>ignoreValueAbove) next;
      if(array2[i]>ignoreValueAbove) { counter <- counter+1; next;}
      coeff <- coeff+(coeffMatrixEuclid[array1[i],array2[i]]);
      counter <- counter+1;
    }
    coeff/counter;
  }
};

stats_exp1_euclid <-t(apply(users_df_exp1, 1, forEachUser_euclid, parties_df_exp1, ignoreValueAbove));
stats_exp2_euclid <-t(apply(users_df_exp2, 1, forEachUser_euclid, parties_df_exp2, ignoreValueAbove));

colnames(stats_exp1_euclid) <- parties
colnames(stats_exp2_euclid) <- parties

write.csv(stats_exp1_euclid,file = "match_euclid_exp1", row.names = FALSE)
write.csv(stats_exp2_euclid,file = "match_euclid_exp2", row.names = FALSE)

################ Scalar #################

forEachUser_scalar <- function(row, parties, ignoreValueAbove){
  #Find user's matchings with all parties
  match<-apply(parties, 1, forEachParty_scalar, row, ignoreValueAbove);
  return(match)
}

forEachParty_scalar <-function(row, userAns, ignoreValueAbove){
  sc <- scalar(userAns, row, ignoreValueAbove);
  return(sc);
}

scalar <- function(array1, array2, ignoreValueAbove){
coeffMatrixScalar<-matrix(c(1,0.5,0,-0.5,-1,0.5,0.25,0,-0.25,-0.5,0,0,0,0,0,-0.5,-0.25,0,0.25,0.5,-1,-0.5,0,0.5,1),5,5,byrow=T)
  if(length(array1)!=length(array2)){
    NA
  }else{
    coeff<-0;
    counter<-0;
    for(i in 1:length(array1)){
      if(array1[i]>ignoreValueAbove) next;
      if(array2[i]>ignoreValueAbove) { counter <- counter+1; next;}
      coeff <- coeff+(coeffMatrixScalar[array1[i],array2[i]]);
      counter <- counter+1;
    }
    coeff/counter;
  }
};

stats_exp1_scalar <-t(apply(users_df_exp1, 1, forEachUser_scalar, parties_df_exp1, ignoreValueAbove));
stats_exp2_scalar <-t(apply(users_df_exp2, 1, forEachUser_scalar, parties_df_exp2, ignoreValueAbove));

colnames(stats_exp1_scalar) <- parties
colnames(stats_exp2_scalar) <- parties

write.csv(stats_exp1_scalar,file = "match_scalar_exp1", row.names = FALSE)
write.csv(stats_exp2_scalar,file = "match_scalar_exp2", row.names = FALSE)

###########################################################################################################################################################################


# Find highest match
match_exp1 <- apply(stats_exp1,1,function(x) which(x==max(x)))
match_exp2 <- apply(stats_exp2,1,function(x) which(x==max(x)))

# Count highest matches
matches_exp1 <- as.data.frame(table(unlist(lapply(match_exp1, unique))))
matches_exp2 <- as.data.frame(table(unlist(lapply(match_exp2, unique))))

matches_exp1$Freq <- matches_exp1$Freq / sum(matches_exp1$Freq)
matches_exp2$Freq <- matches_exp2$Freq / sum(matches_exp2$Freq)

matches_exp1$Freq <- matches_exp1$Freq *100
matches_exp2$Freq <- matches_exp2$Freq *100

names(matches_exp1)[names(matches_exp1)=="Freq"] <- "exp1"
names(matches_exp2)[names(matches_exp2)=="Freq"] <- "exp2"

matches <- cbind(parties,matches_exp1,matches_exp2)
matches <- subset(matches, select = c(parties,exp1,exp2))

rm(matches_exp1,matches_exp2)

# Barplots party match

data_barplot <- gather(matches, type, value, -parties)
data_barplot$type <- as.factor(data_barplot$type)
data_barplot$type <- factor(data_barplot$type, levels = c("exp2", "exp1"))
data_barplot$parties <- factor(data_barplot$parties, levels = c("VVD","VNL","SP","SGP","PVV","PvdD","PvdA","GL","FvD","DENK","D66","CU","CDA","50Plus"))

tikz(file = "plot_test.tex", width = 6, height = 6)
barplot <- ggplot(data_barplot, aes(parties, value)) + 
  geom_bar(aes(fill = type), stat = "identity", position=position_dodge(), width = .75)+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("exp1", "exp2"),labels=c("A", "B"))+
  xlab("Parties") + 
  ylab("Matches") +
  ggtitle("Number of Matches per Party for both Versions") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25)) +
  coord_flip() +
  geom_hline(aes(yintercept=5), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=10), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=15), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=20), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=25), color="grey", linetype="dashed")+
  theme_classic()
print(barplot)
dev.off()

# Clear Console, Environment, and Viewer

rm(list = ls())
dev.off(dev.list()["RStudioGD"])
cat("\014")

# Differences between questions - changed questions

library(ggplot2)
library(readr)

stemconsult <- read_csv("Netherlands2017_NumericExport.csv")
stemconsult$ExpCondition <- as.factor(stemconsult$ExpCondition)

q1 <- as.data.frame(table(stemconsult$q1, stemconsult$ExpCondition))
q1 <- reshape(q1, idvar = "Var1", timevar = "Var2", direction = "wide")
q1$Freq.1 <- (q1$Freq.1 / sum(q1$Freq.1)) *100
q1$Freq.2 <- (q1$Freq.2 / sum(q1$Freq.2)) *100
q1$Dif <- q1$Freq.2 - q1$Freq.1


plot_q1 <- ggplot(q1, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q1", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q2 <- as.data.frame(table(stemconsult$q2, stemconsult$ExpCondition))
q2 <- reshape(q2, idvar = "Var1", timevar = "Var2", direction = "wide")
q2$Freq.1 <- (q2$Freq.1 / sum(q2$Freq.1)) *100
q2$Freq.2 <- (q2$Freq.2 / sum(q2$Freq.2)) *100
q2$Dif <- q2$Freq.2 - q2$Freq.1


plot_q2 <- ggplot(q2, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q2", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q5 <- as.data.frame(table(stemconsult$q5, stemconsult$ExpCondition))
q5 <- reshape(q5, idvar = "Var1", timevar = "Var2", direction = "wide")
q5$Freq.1 <- (q5$Freq.1 / sum(q5$Freq.1)) *100
q5$Freq.2 <- (q5$Freq.2 / sum(q5$Freq.2)) *100
q5$Dif <- q5$Freq.2 - q5$Freq.1

plot_q5 <- ggplot(q5, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q5", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q10 <- as.data.frame(table(stemconsult$q10, stemconsult$ExpCondition))
q10 <- reshape(q10, idvar = "Var1", timevar = "Var2", direction = "wide")
q10$Freq.1 <- (q10$Freq.1 / sum(q10$Freq.1)) *100
q10$Freq.2 <- (q10$Freq.2 / sum(q10$Freq.2)) *100
q10$Dif <- q10$Freq.2 - q10$Freq.1

plot_q10 <- ggplot(q10, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q10", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q11 <- as.data.frame(table(stemconsult$q11, stemconsult$ExpCondition))
q11 <- reshape(q11, idvar = "Var1", timevar = "Var2", direction = "wide")
q11$Freq.1 <- (q11$Freq.1 / sum(q11$Freq.1)) *100
q11$Freq.2 <- (q11$Freq.2 / sum(q11$Freq.2)) *100
q11$Dif <- q11$Freq.2 - q11$Freq.1

plot_q11 <- ggplot(q11, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q11", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q15 <- as.data.frame(table(stemconsult$q15, stemconsult$ExpCondition))
q15 <- reshape(q15, idvar = "Var1", timevar = "Var2", direction = "wide")
q15$Freq.1 <- (q15$Freq.1 / sum(q15$Freq.1)) *100
q15$Freq.2 <- (q15$Freq.2 / sum(q15$Freq.2)) *100
q15$Dif <- q15$Freq.2 - q15$Freq.1

plot_q15 <- ggplot(q15, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q15", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())



q16 <- as.data.frame(table(stemconsult$q16, stemconsult$ExpCondition))
q16 <- reshape(q16, idvar = "Var1", timevar = "Var2", direction = "wide")
q16$Freq.1 <- (q16$Freq.1 / sum(q16$Freq.1)) *100
q16$Freq.2 <- (q16$Freq.2 / sum(q16$Freq.2)) *100
q16$Dif <- q16$Freq.2 - q16$Freq.1

plot_q16 <- ggplot(q16, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q16", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())



q17 <- as.data.frame(table(stemconsult$q17, stemconsult$ExpCondition))
q17 <- reshape(q17, idvar = "Var1", timevar = "Var2", direction = "wide")
q17$Freq.1 <- (q17$Freq.1 / sum(q17$Freq.1)) *100
q17$Freq.2 <- (q17$Freq.2 / sum(q17$Freq.2)) *100
q17$Dif <- q17$Freq.2 - q17$Freq.1

plot_q17 <- ggplot(q17, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q17", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q18 <- as.data.frame(table(stemconsult$q18, stemconsult$ExpCondition))
q18 <- reshape(q18, idvar = "Var1", timevar = "Var2", direction = "wide")
q18$Freq.1 <- (q18$Freq.1 / sum(q18$Freq.1)) *100
q18$Freq.2 <- (q18$Freq.2 / sum(q18$Freq.2)) *100
q18$Dif <- q18$Freq.2 - q18$Freq.1

plot_q18 <- ggplot(q18, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q18", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q21 <- as.data.frame(table(stemconsult$q21, stemconsult$ExpCondition))
q21 <- reshape(q21, idvar = "Var1", timevar = "Var2", direction = "wide")
q21$Freq.1 <- (q21$Freq.1 / sum(q21$Freq.1)) *100
q21$Freq.2 <- (q21$Freq.2 / sum(q21$Freq.2)) *100
q21$Dif <- q21$Freq.2 - q21$Freq.1

plot_q21 <- ggplot(q21, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q21", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())



q22 <- as.data.frame(table(stemconsult$q22, stemconsult$ExpCondition))
q22 <- reshape(q22, idvar = "Var1", timevar = "Var2", direction = "wide")
q22$Freq.1 <- (q22$Freq.1 / sum(q22$Freq.1)) *100
q22$Freq.2 <- (q22$Freq.2 / sum(q22$Freq.2)) *100
q22$Dif <- q22$Freq.2 - q22$Freq.1

plot_q22 <- ggplot(q22, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q22", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q25 <- as.data.frame(table(stemconsult$q25, stemconsult$ExpCondition))
q25 <- reshape(q25, idvar = "Var1", timevar = "Var2", direction = "wide")
q25$Freq.1 <- (q25$Freq.1 / sum(q25$Freq.1)) *100
q25$Freq.2 <- (q25$Freq.2 / sum(q25$Freq.2)) *100
q25$Dif <- q25$Freq.2 - q25$Freq.1

plot_q25 <- ggplot(q25, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q25", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
    axis.title.x=element_blank(),
    axis.title.y=element_blank())


# Make into a single plot

library(ggpubr)
library(tikzDevice)

tikz(file = "plot_test.tex", width = 6, height = 8)
question_plot <- ggarrange(plot_q1,plot_q2, plot_q5, plot_q10, plot_q11, plot_q15, plot_q16, plot_q17, plot_q18, plot_q21, plot_q22, plot_q25, ncol = 3, nrow = 4)
print(question_plot)
dev.off()


# Similar questions

library(ggplot2)
library(readr)

stemconsult <- read_csv("Netherlands2017_NumericExport.csv")
stemconsult$ExpCondition <- as.factor(stemconsult$ExpCondition)

q3 <- as.data.frame(table(stemconsult$q3, stemconsult$ExpCondition))
q3 <- reshape(q3, idvar = "Var1", timevar = "Var2", direction = "wide")
q3$Freq.1 <- (q3$Freq.1 / sum(q3$Freq.1)) *100
q3$Freq.2 <- (q3$Freq.2 / sum(q3$Freq.2)) *100
q3$Dif <- q3$Freq.2 - q3$Freq.1


plot_q3 <- ggplot(q3, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q3", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q4 <- as.data.frame(table(stemconsult$q4, stemconsult$ExpCondition))
q4 <- reshape(q4, idvar = "Var1", timevar = "Var2", direction = "wide")
q4$Freq.1 <- (q4$Freq.1 / sum(q4$Freq.1)) *100
q4$Freq.2 <- (q4$Freq.2 / sum(q4$Freq.2)) *100
q4$Dif <- q4$Freq.2 - q4$Freq.1


plot_q4 <- ggplot(q4, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q4", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q6 <- as.data.frame(table(stemconsult$q6, stemconsult$ExpCondition))
q6 <- reshape(q6, idvar = "Var1", timevar = "Var2", direction = "wide")
q6$Freq.1 <- (q6$Freq.1 / sum(q6$Freq.1)) *100
q6$Freq.2 <- (q6$Freq.2 / sum(q6$Freq.2)) *100
q6$Dif <- q6$Freq.2 - q6$Freq.1

plot_q6 <- ggplot(q6, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q6", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q7 <- as.data.frame(table(stemconsult$q7, stemconsult$ExpCondition))
q7 <- reshape(q7, idvar = "Var1", timevar = "Var2", direction = "wide")
q7$Freq.1 <- (q7$Freq.1 / sum(q7$Freq.1)) *100
q7$Freq.2 <- (q7$Freq.2 / sum(q7$Freq.2)) *100
q7$Dif <- q7$Freq.2 - q7$Freq.1

plot_q7 <- ggplot(q7, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q7", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q8 <- as.data.frame(table(stemconsult$q8, stemconsult$ExpCondition))
q8 <- reshape(q8, idvar = "Var1", timevar = "Var2", direction = "wide")
q8$Freq.1 <- (q8$Freq.1 / sum(q8$Freq.1)) *100
q8$Freq.2 <- (q8$Freq.2 / sum(q8$Freq.2)) *100
q8$Dif <- q8$Freq.2 - q8$Freq.1

plot_q8 <- ggplot(q8, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q8", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q9 <- as.data.frame(table(stemconsult$q9, stemconsult$ExpCondition))
q9 <- reshape(q9, idvar = "Var1", timevar = "Var2", direction = "wide")
q9$Freq.1 <- (q9$Freq.1 / sum(q9$Freq.1)) *100
q9$Freq.2 <- (q9$Freq.2 / sum(q9$Freq.2)) *100
q9$Dif <- q9$Freq.2 - q9$Freq.1

plot_q9 <- ggplot(q9, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q9", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())



q12 <- as.data.frame(table(stemconsult$q12, stemconsult$ExpCondition))
q12 <- reshape(q12, idvar = "Var1", timevar = "Var2", direction = "wide")
q12$Freq.1 <- (q12$Freq.1 / sum(q12$Freq.1)) *100
q12$Freq.2 <- (q12$Freq.2 / sum(q12$Freq.2)) *100
q12$Dif <- q12$Freq.2 - q12$Freq.1

plot_q12 <- ggplot(q12, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q12", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())



q13 <- as.data.frame(table(stemconsult$q13, stemconsult$ExpCondition))
q13 <- reshape(q13, idvar = "Var1", timevar = "Var2", direction = "wide")
q13$Freq.1 <- (q13$Freq.1 / sum(q13$Freq.1)) *100
q13$Freq.2 <- (q13$Freq.2 / sum(q13$Freq.2)) *100
q13$Dif <- q13$Freq.2 - q13$Freq.1

plot_q13 <- ggplot(q13, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q13", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q14 <- as.data.frame(table(stemconsult$q14, stemconsult$ExpCondition))
q14 <- reshape(q14, idvar = "Var1", timevar = "Var2", direction = "wide")
q14$Freq.1 <- (q14$Freq.1 / sum(q14$Freq.1)) *100
q14$Freq.2 <- (q14$Freq.2 / sum(q14$Freq.2)) *100
q14$Dif <- q14$Freq.2 - q14$Freq.1

plot_q14 <- ggplot(q14, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q14", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q19 <- as.data.frame(table(stemconsult$q19, stemconsult$ExpCondition))
q19 <- reshape(q19, idvar = "Var1", timevar = "Var2", direction = "wide")
q19$Freq.1 <- (q19$Freq.1 / sum(q19$Freq.1)) *100
q19$Freq.2 <- (q19$Freq.2 / sum(q19$Freq.2)) *100
q19$Dif <- q19$Freq.2 - q19$Freq.1

plot_q19 <- ggplot(q19, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q19", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())



q20 <- as.data.frame(table(stemconsult$q20, stemconsult$ExpCondition))
q20 <- reshape(q20, idvar = "Var1", timevar = "Var2", direction = "wide")
q20$Freq.1 <- (q20$Freq.1 / sum(q20$Freq.1)) *100
q20$Freq.2 <- (q20$Freq.2 / sum(q20$Freq.2)) *100
q20$Dif <- q20$Freq.2 - q20$Freq.1

plot_q20 <- ggplot(q20, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q20", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


q23 <- as.data.frame(table(stemconsult$q23, stemconsult$ExpCondition))
q23 <- reshape(q23, idvar = "Var1", timevar = "Var2", direction = "wide")
q23$Freq.1 <- (q23$Freq.1 / sum(q23$Freq.1)) *100
q23$Freq.2 <- (q23$Freq.2 / sum(q23$Freq.2)) *100
q23$Dif <- q23$Freq.2 - q23$Freq.1

plot_q23 <- ggplot(q23, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q23", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())



q24 <- as.data.frame(table(stemconsult$q24, stemconsult$ExpCondition))
q24 <- reshape(q24, idvar = "Var1", timevar = "Var2", direction = "wide")
q24$Freq.1 <- (q24$Freq.1 / sum(q24$Freq.1)) *100
q24$Freq.2 <- (q24$Freq.2 / sum(q24$Freq.2)) *100
q24$Dif <- q24$Freq.2 - q24$Freq.1

plot_q24 <- ggplot(q24, aes(Var1, Dif)) + 
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=-8), color="grey", linetype="dashed")+
  geom_bar(aes(fill = Var1), stat = "identity", position=position_dodge(), width = .75)+
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = c(-8, -4,  0,  4,  8)) +
  scale_x_discrete(breaks=c("-999", "1", "2", "3", "4", "5"), labels=c("NA", "CA", "A","N", "D", "CD"))+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("counts_1", "counts_2"),labels=c("A", "B"))+
  ggtitle("Q24", subtitle = NULL)+
  theme_classic()+
  theme(plot.title = element_text(size = 14, hjust = .5, vjust = 2.0),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

# Make into a single plot

library(ggpubr)
library(tikzDevice)

tikz(file = "plot_test_similar.tex", width = 6, height = 8)
question_plot <- ggarrange(plot_q3,plot_q4, plot_q6, plot_q7, plot_q8, plot_q9, plot_q12, plot_q13, plot_q14, plot_q19, plot_q20, plot_q23,plot_q24, ncol = 3, nrow = 5)
print(question_plot)
dev.off()




# VARIABLES

library(ggplot2)
library(readr)

stemconsult <- read_csv("Netherlands2017_NumericExport.csv")
stemconsult$ExpCondition <- as.factor(stemconsult$ExpCondition)

stemconsult$age[stemconsult$age == -999] <- NA
stemconsult$age[stemconsult$age == -998] <- NA
stemconsult$age[stemconsult$age == -997] <- NA
stemconsult$age[stemconsult$age == -996] <- NA
stemconsult$age[stemconsult$age == -995] <- NA

dataframes <- split(stemconsult, stemconsult$ExpCondition)
stemconsult_exp1 <- dataframes [[1]]
stemconsult_exp2 <- dataframes [[2]]
rm(dataframes)

# Sex

table(stemconsult$sex, stemconsult$ExpCondition)
chisq.test(table(stemconsult$sex, stemconsult$ExpCondition))

# Age
var.test(stemconsult_exp1$age,stemconsult_exp2$age)
t.test(stemconsult_exp1$age,stemconsult_exp2$age)

# Education
table(stemconsult$education, stemconsult$ExpCondition)
chisq.test(table(stemconsult$education, stemconsult$ExpCondition))

# Political Interest
table(stemconsult$interestInPolitics, stemconsult$ExpCondition)
chisq.test(table(stemconsult$interestInPolitics, stemconsult$ExpCondition))

# Vote Intention

table(stemconsult$voteIntention, stemconsult$ExpCondition)
chisq.test(table(stemconsult$voteIntention, stemconsult$ExpCondition))

# Vote Reason

table(stemconsult$voteReason, stemconsult$ExpCondition)
chisq.test(table(stemconsult$voteReason, stemconsult$ExpCondition))

# Previous Vote

table(stemconsult$prevVote, stemconsult$ExpCondition)
chisq.test(table(stemconsult$prevVote, stemconsult$ExpCondition))

# Matches

matches$parties <- NULL
matches
chisq.test(matches)


