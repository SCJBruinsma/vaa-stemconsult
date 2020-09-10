library(readr)
library(car)
library(lme4)
library(arm)
library(mlmRev)
library(gamm4)
library(reshape2)
library(lmerTest)
library(EMAtools)
library(multcomp)
library(GetR)
library(plyr)
library(stargazer)
library(ggplot2)


# Load data, select variables, and recode missing, remove questions that did not change

netherlands <- read_csv("stemconsult_data_numeric.csv")

variables <- c("id", "ExpCondition", "sex", "age", "education", "interestInPolitics", "q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10", "q11", "q12", "q13", "q14", "q15", "q16", "q17", "q18","q19","q20","q21","q22","q23","q24","q25","t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18","t19","t20","t21","t22","t23","t24","t25")
netherlands <- netherlands[variables]

netherlands[netherlands == -999] <- NA
netherlands[netherlands == -998] <- NA
netherlands[netherlands == -997] <- NA
netherlands[netherlands == -996] <- NA
netherlands[netherlands == -995] <- NA

netherlands <- na.omit(netherlands)

# Make political knowledge variable

netherlands$education_new <- netherlands$education
netherlands$education_new <- recode(netherlands$education_new, "1=1; 2=1; 3=2; 4=3; 5=4; 6=5; 7=5")
netherlands$interestInPolitics <- recode(netherlands$interestInPolitics, "1=5; 2=4; 4=2; 5=1")
netherlands$polknow <- netherlands$education_new + netherlands$interestInPolitics
netherlands$education_new <- NULL

netherlands$ExpCondition <- recode(netherlands$ExpCondition, "1='0'; 2='1'")
netherlands$sex <- recode(netherlands$sex, "1='0'; 2='1'")
netherlands$ExpCondition <- as.factor(netherlands$ExpCondition)

# Calculate matches

netherlands_exp0 <- as.data.frame(subset(netherlands,ExpCondition==0))
netherlands_exp1 <- as.data.frame(subset(netherlands,ExpCondition==1))
netherlands_exp0 <- subset(netherlands_exp0,select= c(q1,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13,q14,q15,q19,q20,q21,q23,q24))
netherlands_exp1 <- subset(netherlands_exp1,select= c(q1,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13,q14,q15,q19,q20,q21,q23,q24))

write_csv(netherlands_exp0, "netherlands_exp0_users_VIIII.csv", col_names = FALSE)
write_csv(netherlands_exp1, "netherlands_exp1_users_VIIII.csv", col_names = FALSE)

parties_exp0 <- read_csv("netherlands_exp0_parties_VIIII.csv",col_names = FALSE)
users_exp0 <- read_csv("netherlands_exp0_users_VIIII.csv",col_names = FALSE)

parties_exp1 <- read_csv("netherlands_exp1_parties_VIIII.csv",col_names = FALSE)
users_exp1 <- read_csv("netherlands_exp1_users_VIIII.csv",col_names = FALSE)

# Hybrid Algorithm

ignoreValueAbove<-5

forEachUser <- function(row, parties, ignoreValueAbove){
  #Find user's matchings with all parties
  match<-apply(parties, 1, forEachParty, row, ignoreValueAbove);
  return(match)
}

forEachParty <-function(row, userAns, ignoreValueAbove){
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

# This will produce the matrix of results
match_exp0<-t(apply(users_exp0, 1, forEachUser, parties_exp0, ignoreValueAbove));
match_exp1<-t(apply(users_exp1, 1, forEachUser, parties_exp1, ignoreValueAbove));

match_exp0<- match_exp0 * 100
match_exp1<- match_exp1 * 100

parties<-c("match_50Plus","match_CDA","match_CU","match_D66","match_GL","match_PvdA","match_PvdD","match_PVV","match_SGP","match_SP","match_VVD","match_DENK","match_VNL","match_FvD")
colnames(match_exp0) <- parties
colnames(match_exp1) <- parties

match_exp0 <- as.data.frame(match_exp0)
match_exp1 <- as.data.frame(match_exp1)

matches <- rbind(match_exp0, match_exp1) 
netherlands <- cbind(netherlands, matches)

rm(hybrid,forEachParty,forEachUser,variables,parties,ignoreValueAbove,users_exp0,users_exp1,parties_exp0,parties_exp1,netherlands_exp0,netherlands_exp1,matches,match_exp0,match_exp1)

# Gutmann errors

netherlands_exp1 <- subset(netherlands,ExpCondition==0)
netherlands_exp2 <- subset(netherlands,ExpCondition==1)

netherlands_exp1 <- subset(netherlands_exp1,select= c(q3,q6,q9,q11,q12,q14,q17,q25))
netherlands_exp2 <- subset(netherlands_exp2,select= c(q2,q3,q9,q11,q13,q14,q19,q25))

# Recode so the questions are oriented as they were found in the DSV

netherlands_exp1$q9 <- recode(netherlands_exp1$q9, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")
netherlands_exp2$q9 <- recode(netherlands_exp2$q9, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")
netherlands_exp2$q13 <- recode(netherlands_exp2$q13, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")
netherlands_exp2$q19 <- recode(netherlands_exp2$q19, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")

netherlands_exp1<- as.matrix(netherlands_exp1)
netherlands_exp2<- as.matrix(netherlands_exp2)

gutmann_exp1 <-guttmanErrors(netherlands_exp1)
gutmann_exp2 <-guttmanErrors(netherlands_exp2)

gutmann <- c(gutmann_exp1,gutmann_exp2)
netherlands <- cbind(netherlands, gutmann)

rm(netherlands_exp1,netherlands_exp2,gutmann,gutmann_exp1,gutmann_exp2)

#Recode to make 1=Completely disagree, 5=Completely Agree (original was other way around)

questions_list1 <- c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10", "q11", "q12", "q13", "q14", "q15", "q16", "q17", "q18","q19","q20","q21","q22","q23","q24","q25")
questions_list2 <- names(netherlands) %in% c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10", "q11", "q12", "q13", "q14", "q15", "q16", "q17", "q18","q19","q20","q21","q22","q23","q24","q25")

netherlands_questions <- netherlands[questions_list1]
netherlands <- netherlands[!questions_list2]

netherlands_questions[] <- lapply(netherlands_questions, recode, "1='5'; 2='4';3='3';4='2';5='1'")
netherlands <- cbind(netherlands,netherlands_questions)

rm(questions_list1,questions_list2,netherlands_questions)

#Seperate two Versions

netherlands_exp0 <- subset(netherlands,ExpCondition==0)
netherlands_exp1 <- subset(netherlands,ExpCondition==1)
rm(netherlands)

#Recode again the questions per version so a higher score is a more positive assessment of the object of the question

netherlands_exp0$q1 <- recode(netherlands_exp0$q1, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")
netherlands_exp1$q2 <- recode(netherlands_exp1$q2, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")
netherlands_exp1$q5 <- recode(netherlands_exp1$q5, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")
netherlands_exp0$q10 <- recode(netherlands_exp0$q10, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")
netherlands_exp1$q11 <- recode(netherlands_exp1$q11, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")
netherlands_exp0$q15 <- recode(netherlands_exp0$q15, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")
netherlands_exp1$q16 <- recode(netherlands_exp1$q16, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")
netherlands_exp1$q17 <- recode(netherlands_exp1$q17, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")
netherlands_exp0$q18 <- recode(netherlands_exp0$q18, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")
netherlands_exp0$q21 <- recode(netherlands_exp0$q21, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")
netherlands_exp0$q22 <- recode(netherlands_exp0$q22, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")
netherlands_exp1$q25 <- recode(netherlands_exp1$q25, "5='1'; 4='2'; 3='3'; 2='4'; 1='5'")

netherlands <- rbind(netherlands_exp0,netherlands_exp1)

# T Test Questions

t.test(netherlands$q1~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$q2~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$q5~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$q10~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$q11~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$q15~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$q16~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$q17~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$q18~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$q21~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$q22~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$q25~netherlands$ExpCondition, var.equal=FALSE)

# T Test Time

netherlands_exp0 <- subset(netherlands,ExpCondition==0)
netherlands_exp1 <- subset(netherlands,ExpCondition==1)
rm(netherlands)

time_list1 <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18","t19","t20","t21","t22","t23","t24","t25")
time_list2_0 <- names(netherlands_exp0) %in% c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18","t19","t20","t21","t22","t23","t24","t25")
time_list2_1 <- names(netherlands_exp1) %in% c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18","t19","t20","t21","t22","t23","t24","t25")

netherlands_exp0_time <- netherlands_exp0[time_list1]
netherlands_exp1_time <- netherlands_exp1[time_list1]
netherlands_exp0 <- netherlands_exp0[!time_list2_0]
netherlands_exp1 <- netherlands_exp1[!time_list2_1]

# Remove Outliers (Any observation outside 1.5 SD) - L. Collado Torres - Functions

findOutlier <- function(data, cutoff = 1.5) {
  sds <- apply(data, 2, sd, na.rm = TRUE)
  result <- mapply(function(d, s) {
    which(d > cutoff * s)
  }, data, sds)
  result
}

removeOutlier <- function(data, outliers) {
  result <- mapply(function(d, o) {
    res <- d
    res[o] <- NA
    return(res)
  }, data, outliers)
  return(as.data.frame(result))
}

outliers_exp0 <- findOutlier(netherlands_exp0_time)
outliers_exp1 <- findOutlier(netherlands_exp1_time)

netherlands_exp0_time <- removeOutlier(netherlands_exp0_time, outliers_exp0)
netherlands_exp1_time <- removeOutlier(netherlands_exp1_time, outliers_exp1)

netherlands_exp0 <- cbind(netherlands_exp0,netherlands_exp0_time)
netherlands_exp1 <- cbind(netherlands_exp1,netherlands_exp1_time)
netherlands <- rbind(netherlands_exp0,netherlands_exp1)

t.test(netherlands$t1~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$t2~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$t5~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$t10~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$t11~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$t15~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$t16~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$t17~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$t18~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$t21~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$t22~netherlands$ExpCondition, var.equal=FALSE)
t.test(netherlands$t25~netherlands$ExpCondition, var.equal=FALSE)


#Seperate data-sets for models

explicit <- subset(netherlands,select= c(id,ExpCondition,q1,q15,q21,q25,gutmann, polknow)) 
implicit <- subset(netherlands,select= c(id,ExpCondition,q2,q5,q10,q11,q16,q17,q18,q22,gutmann, polknow))
total<- subset(netherlands,select= c(id,ExpCondition,sex,age,education,interestInPolitics,gutmann, polknow,match_50Plus,match_CDA,match_CU,match_D66,match_GL,match_PvdA,match_PvdD,match_PVV,match_SGP,match_SP,match_VVD,match_DENK,match_VNL,match_FvD))

# Positive and Negative means

exp_pos_q1 <- explicit$q1[explicit$ExpCondition==1]
exp_pos_q15 <- explicit$q15[explicit$ExpCondition==1]
exp_pos_q21 <- explicit$q21[explicit$ExpCondition==1]
exp_pos_q25 <- explicit$q25[explicit$ExpCondition==0]
exp_neg_q1 <- explicit$q1[explicit$ExpCondition==0]
exp_neg_q15 <- explicit$q15[explicit$ExpCondition==0]
exp_neg_q21 <- explicit$q21[explicit$ExpCondition==0]
exp_neg_q25 <- explicit$q25[explicit$ExpCondition==1]

exp_pos <- c(exp_pos_q1,exp_pos_q15,exp_pos_q21,exp_pos_q25)
exp_neg <- c(exp_neg_q1,exp_neg_q15,exp_neg_q21,exp_neg_q25)
t.test(exp_pos, exp_neg)

imp_pos_q2 <- implicit$q2[implicit$ExpCondition==0]
imp_pos_q5 <- implicit$q5[implicit$ExpCondition==0]
imp_pos_q10 <- implicit$q10[implicit$ExpCondition==1]
imp_pos_q11 <- implicit$q11[implicit$ExpCondition==0]
imp_pos_q16 <- implicit$q16[implicit$ExpCondition==0]
imp_pos_q17 <- implicit$q17[implicit$ExpCondition==0]
imp_pos_q18 <- implicit$q18[implicit$ExpCondition==1]
imp_pos_q22 <- implicit$q22[implicit$ExpCondition==1]

imp_neg_q2 <- implicit$q2[implicit$ExpCondition==1]
imp_neg_q5 <- implicit$q5[implicit$ExpCondition==1]
imp_neg_q10 <- implicit$q10[implicit$ExpCondition==0]
imp_neg_q11 <- implicit$q11[implicit$ExpCondition==1]
imp_neg_q16 <- implicit$q16[implicit$ExpCondition==1]
imp_neg_q17 <- implicit$q17[implicit$ExpCondition==1]
imp_neg_q18 <- implicit$q18[implicit$ExpCondition==0]
imp_neg_q22 <- implicit$q22[implicit$ExpCondition==0]

imp_pos <- c(imp_pos_q2,imp_pos_q5,imp_pos_q10,imp_pos_q11,imp_pos_q16,imp_pos_q17,imp_pos_q18,imp_pos_q22)
imp_neg <- c(imp_neg_q2,imp_neg_q5,imp_neg_q10,imp_neg_q11,imp_neg_q16,imp_neg_q17,imp_neg_q18,imp_neg_q22)
t.test(imp_pos, imp_neg)


# Explicit Negatives

explicit$mean_q1 <- mean(explicit$q1[explicit$ExpCondition==1], na.rm = TRUE) # Make sure this is the positive mean
explicit$dif_q1 <- mean(explicit$q1[explicit$ExpCondition==1], na.rm = TRUE) - mean(explicit$q1[explicit$ExpCondition==0], na.rm = TRUE)
explicit$mean_q15 <- mean(explicit$q15[explicit$ExpCondition==1], na.rm = TRUE) # Make sure this is the positive mean
explicit$dif_q15 <- mean(explicit$q15[explicit$ExpCondition==1], na.rm = TRUE) - mean(explicit$q15[explicit$ExpCondition==0], na.rm = TRUE)
explicit$mean_q21 <- mean(explicit$q21[explicit$ExpCondition==1], na.rm = TRUE) # Make sure this is the positive mean
explicit$dif_q21 <- mean(explicit$q21[explicit$ExpCondition==1], na.rm = TRUE) - mean(explicit$q21[explicit$ExpCondition==0], na.rm = TRUE)
explicit$mean_q25 <- mean(explicit$q25[explicit$ExpCondition==0], na.rm = TRUE) # Make sure this is the positive mean
explicit$dif_q25 <- mean(explicit$q25[explicit$ExpCondition==0], na.rm = TRUE) - mean(explicit$q25[explicit$ExpCondition==1], na.rm = TRUE)

explicit$ExpCondition <- NULL
explicit$id <- as.factor(explicit$id)
explicit <- as.data.frame(explicit)

means_explicit <- c("id", "mean_q1", "mean_q15", "mean_q21", "mean_q25")
explicit_means <- explicit[means_explicit]
differences_explicit <- c("id", "dif_q1", "dif_q15", "dif_q21", "dif_q25")
explicit_differences <- explicit[differences_explicit]
explicit_vars <- c("id", "q1","q15", "q21","q25")
explicit_questions <- explicit[explicit_vars]

polknow_vars <- c("id","polknow")
explicit_polknow <- explicit[polknow_vars]
gutmann_vars <- c("id","gutmann")
explicit_gutmann <- explicit[gutmann_vars]

explicit_polknow <- melt(explicit_polknow, id="id")
explicit_questions <- melt(explicit_questions, id="id")
explicit_means <- melt(explicit_means, id="id") 
explicit_differences <- melt(explicit_differences, id="id")
explicit_gutmann <- melt(explicit_gutmann, id="id")

colnames(explicit_polknow)[2] <- "pol_know"
colnames(explicit_questions)[2] <- "question"
colnames(explicit_differences)[2] <- "difference"
colnames(explicit_means)[2] <- "mean"
colnames(explicit_gutmann)[2] <- "gutmann"

colnames(explicit_polknow)[3] <- "polknow_value"
colnames(explicit_questions)[3] <- "question_answer"
colnames(explicit_differences)[3] <- "difference_value"
colnames(explicit_means)[3] <- "mean_value"
colnames(explicit_gutmann)[3] <- "gutmann_value"

explicit_total <- merge(explicit_differences, explicit_means, by = "id")
explicit_total <- merge(explicit_total, explicit_questions, by = "id")
explicit_total <- merge(explicit_total, explicit_gutmann, by="id")
explicit_total <- merge(explicit_total, explicit_polknow, by="id")

rm(explicit_differences,explicit_means, explicit_age, explicit_education,explicit_polknow, explicit_gutmann, explicit_interest, explicit_questions, explicit_sex, age_vars, differences_explicit, education_vars, explicit_vars, gutmann_vars, interest_vars, means_explicit, sex_vars)

# Implicit Negatives

implicit$mean_q2 <- mean(implicit$q2[implicit$ExpCondition==0], na.rm = TRUE) # Make sure this is the positive mean
implicit$dif_q2 <- mean(implicit$q2[implicit$ExpCondition==0], na.rm = TRUE) - mean(implicit$q2[implicit$ExpCondition==1], na.rm = TRUE)
implicit$mean_q5 <- mean(implicit$q5[implicit$ExpCondition==0], na.rm = TRUE) # Make sure this is the positive mean
implicit$dif_q5 <- mean(implicit$q5[implicit$ExpCondition==0], na.rm = TRUE) - mean(implicit$q5[implicit$ExpCondition==1], na.rm = TRUE)
implicit$mean_q10 <- mean(implicit$q10[implicit$ExpCondition==1], na.rm = TRUE) # Make sure this is the positive mean
implicit$dif_q10 <- mean(implicit$q10[implicit$ExpCondition==1], na.rm = TRUE) - mean(implicit$q10[implicit$ExpCondition==0], na.rm = TRUE)
implicit$mean_q11 <- mean(implicit$q11[implicit$ExpCondition==0], na.rm = TRUE) # Make sure this is the positive mean
implicit$dif_q11 <- mean(implicit$q11[implicit$ExpCondition==0], na.rm = TRUE) - mean(implicit$q11[implicit$ExpCondition==1], na.rm = TRUE)
implicit$mean_q16 <- mean(implicit$q16[implicit$ExpCondition==0], na.rm = TRUE) # Make sure this is the positive mean
implicit$dif_q16 <- mean(implicit$q16[implicit$ExpCondition==0], na.rm = TRUE) - mean(implicit$q16[implicit$ExpCondition==1], na.rm = TRUE)
implicit$mean_q17 <- mean(implicit$q17[implicit$ExpCondition==0], na.rm = TRUE) # Make sure this is the positive mean
implicit$dif_q17 <- mean(implicit$q17[implicit$ExpCondition==0], na.rm = TRUE) - mean(implicit$q17[implicit$ExpCondition==1], na.rm = TRUE)
implicit$mean_q18 <- mean(implicit$q18[implicit$ExpCondition==1], na.rm = TRUE) # Make sure this is the positive mean
implicit$dif_q18 <- mean(implicit$q18[implicit$ExpCondition==1], na.rm = TRUE) - mean(implicit$q18[implicit$ExpCondition==0], na.rm = TRUE)
implicit$mean_q22 <- mean(implicit$q22[implicit$ExpCondition==1], na.rm = TRUE) # Make sure this is the positive mean
implicit$dif_q22 <- mean(implicit$q22[implicit$ExpCondition==1], na.rm = TRUE) - mean(implicit$q22[implicit$ExpCondition==0], na.rm = TRUE)

implicit$ExpCondition <- NULL
implicit$id <- as.factor(implicit$id)
implicit <- as.data.frame(implicit)

means_implicit <- c("id", "mean_q2", "mean_q5", "mean_q10", "mean_q11", "mean_q16", "mean_q17", "mean_q18", "mean_q22")
implicit_means <- implicit[means_implicit]
differences_implicit <- c("id", "dif_q2", "dif_q5", "dif_q10", "dif_q11", "dif_q16", "dif_q17", "dif_q18", "dif_q22")
implicit_differences <- implicit[differences_implicit]
implicit_vars <- c("id", "q2", "q5", "q10", "q11", "q16", "q17", "q18", "q22")
implicit_questions <- implicit[implicit_vars]

polknow_vars <- c("id","polknow")
implicit_polknow <- implicit[polknow_vars]
gutmann_vars <- c("id","gutmann")
implicit_gutmann <- implicit[gutmann_vars]


implicit_polknow <- melt(implicit_polknow, id="id")
implicit_questions <- melt(implicit_questions, id="id")
implicit_means <- melt(implicit_means, id="id") 
implicit_differences <- melt(implicit_differences, id="id")
implicit_gutmann <- melt(implicit_gutmann, id="id")

colnames(implicit_polknow)[2] <- "polknow"
colnames(implicit_questions)[2] <- "question"
colnames(implicit_differences)[2] <- "difference"
colnames(implicit_means)[2] <- "mean"
colnames(implicit_gutmann)[2] <- "gutmann"


colnames(implicit_polknow)[3] <- "polknow_value"
colnames(implicit_questions)[3] <- "question_answer"
colnames(implicit_differences)[3] <- "difference_value"
colnames(implicit_means)[3] <- "mean_value"
colnames(implicit_gutmann)[3] <- "gutmann_value"


implicit_total <- merge(implicit_differences, implicit_means, by = "id")
implicit_total <- merge(implicit_total, implicit_questions, by = "id")
implicit_total <- merge(implicit_total, implicit_gutmann, by="id")
implicit_total <- merge(implicit_total, implicit_polknow, by="id")

rm(polknow_vars,implicit_differences,implicit_means, implicit_age,implicit_polknow, implicit_education, implicit_gutmann, implicit_interest, implicit_questions, implicit_sex, age_vars, differences_implicit, education_vars, implicit_vars, gutmann_vars, interest_vars, means_implicit, sex_vars)


# Run models 


set.seed(42)

model_explicit <- lmer(question_answer ~ mean_value + difference_value + (1|question) + (1|id), data=explicit_total)
model_explicit
summary(model_explicit)
anova(model_explicit)
lme.dscore(model_explicit, data=explicit_total, type="lme4")
summary(glht(model_explicit))

model_implicit <- lmer(question_answer ~ mean_value + difference_value + (1|question) + (1|id), data=implicit_total, control=lmerControl(optimizer="bobyqa"))
model_implicit
summary(model_implicit)
anova(model_implicit)
lme.dscore(model_implicit, data=implicit_total, type="lme4")
summary(glht(model_implicit))

# Political Knowledge - Gutmann

model_explicit_polsop_gut <- lmer(question_answer ~ mean_value + difference_value + gutmann_value + gutmann_value*difference_value + (1|question) + (1|id), data=explicit_total)
model_explicit_polsop_gut
lme.dscore(model_explicit_polsop_gut, data=explicit_total, type="lme4")
summary(model_explicit_polsop_gut)

model_implicit_polsop_gut <- lmer(question_answer ~ mean_value + difference_value + gutmann_value + gutmann_value*difference_value  + (1|question) + (1|id), data=implicit_total)
model_implicit_polsop_gut
summary(model_implicit_polsop_gut)

# Political Knowledge - Additive

model_explicit_polsop_add <- lmer(question_answer ~ mean_value + difference_value + polknow_value + polknow_value*difference_value + (1|question) + (1|id), data=explicit_total)
model_explicit_polsop_add
summary(model_explicit_polsop_add)
summary(glht(model_explicit_polsop_add))

model_implicit_polsop_add <- lmer(question_answer ~ mean_value + difference_value + polknow_value + polknow_value*difference_value  + (1|question) + (1|id), data=implicit_total)
model_implicit_polsop_add
summary(model_implicit_polsop_add)
summary(glht(model_implicit_polsop_add))


#Graphs Sophistication-Mean Answer - Gutmann

explicit_positive <- ddply(explicit, .(gutmann), summarize,  q1=mean(q1[ExpCondition==1]), q15=mean(q15[ExpCondition==1]), q21=mean(q21[ExpCondition==1]), q25=mean(q25[ExpCondition==0]))
explicit_negative <- ddply(explicit, .(gutmann), summarize,  q1=mean(q1[ExpCondition==0]), q15=mean(q15[ExpCondition==0]), q21=mean(q21[ExpCondition==0]), q25=mean(q25[ExpCondition==1]))
Positive <- ((explicit_positive$q1+explicit_positive$q15+explicit_positive$q21+explicit_positive$q25)/4)
Negative <- ((explicit_negative$q1+explicit_negative$q15+explicit_negative$q21+explicit_negative$q25)/4)

graph_explicit_posneg <- as.data.frame(cbind(explicit_negative$gutmann, Negative, Positive))
graph_explicit_posneg <- melt(graph_explicit_posneg, id="V1")

explicit_posneg_gutman <- ggplot(graph_explicit_posneg, aes(V1, value, colour = variable)) + geom_smooth(method = "loess", size = 1.5, se=FALSE)+ geom_point() + xlab("Gutmann errors") + ylab("Mean Answer") + ggtitle("Explicit") + theme_classic() + scale_colour_grey() + theme(legend.title=element_blank(), legend.position="none") 


implicit_positive <- ddply(implicit, .(gutmann), summarize,  q2=mean(q2[ExpCondition==0]), q5=mean(q5[ExpCondition==0]), q10=mean(q10[ExpCondition==1]), q11=mean(q11[ExpCondition==0]), q16=mean(q16[ExpCondition==0]), q17=mean(q17[ExpCondition==0]), q18=mean(q18[ExpCondition==1]), q22=mean(q22[ExpCondition==1]))
implicit_negative <- ddply(implicit, .(gutmann), summarize,  q2=mean(q2[ExpCondition==1]), q5=mean(q5[ExpCondition==1]), q10=mean(q10[ExpCondition==0]), q11=mean(q11[ExpCondition==1]), q16=mean(q16[ExpCondition==1]), q17=mean(q17[ExpCondition==1]), q18=mean(q18[ExpCondition==0]), q22=mean(q22[ExpCondition==0]))
Positive <- ((implicit_positive$q2+implicit_positive$q5+implicit_positive$q10+implicit_positive$q11+implicit_positive$q16+implicit_positive$q17+implicit_positive$q18+implicit_positive$q22)/8)
Negative <- ((implicit_negative$q2+implicit_negative$q5+implicit_negative$q10+implicit_negative$q11+implicit_negative$q16+implicit_negative$q17+implicit_negative$q18+implicit_negative$q22)/8)

graph_implicit_posneg <- as.data.frame(cbind(implicit_negative$gutmann, Negative, Positive))
graph_implicit_posneg <- melt(graph_implicit_posneg, id="V1")

implicit_posneg_gutman <- ggplot(graph_implicit_posneg, aes(V1, value, colour = variable))  + geom_smooth(method = "loess", size = 1.5, se=FALSE) + geom_point() + xlab("Gutmann errors") + ylab("Mean Answer") + ggtitle("Implicit") + theme_classic() + scale_colour_grey() + theme(legend.title=element_blank(), legend.position="none") 


#Graphs Sophistication-Mean Answer - Additive

explicit_positive <- ddply(explicit, .(polknow), summarize,  q1=mean(q1[ExpCondition==1]), q15=mean(q15[ExpCondition==1]), q21=mean(q21[ExpCondition==1]), q25=mean(q25[ExpCondition==0]))
explicit_negative <- ddply(explicit, .(polknow), summarize,  q1=mean(q1[ExpCondition==0]), q15=mean(q15[ExpCondition==0]), q21=mean(q21[ExpCondition==0]), q25=mean(q25[ExpCondition==1]))
Positive <- ((explicit_positive$q1+explicit_positive$q15+explicit_positive$q21+explicit_positive$q25)/4)
Negative <- ((explicit_negative$q1+explicit_negative$q15+explicit_negative$q21+explicit_negative$q25)/4)

graph_explicit_posneg <- as.data.frame(cbind(explicit_negative$polknow, Negative, Positive))
graph_explicit_posneg <- melt(graph_explicit_posneg, id="V1")

explicit_posneg_polknow <- ggplot(graph_explicit_posneg, aes(V1, value, colour = variable)) + geom_point() + xlab("Political knowledge") + ylab("Mean Answer") + ggtitle("Explicit") + theme_classic() + scale_colour_grey() + theme(legend.title=element_blank(), legend.position="none") 


implicit_positive <- ddply(implicit, .(polknow), summarize,  q2=mean(q2[ExpCondition==0]), q5=mean(q5[ExpCondition==0]), q10=mean(q10[ExpCondition==1]), q11=mean(q11[ExpCondition==0]), q16=mean(q16[ExpCondition==0]), q17=mean(q17[ExpCondition==0]), q18=mean(q18[ExpCondition==1]), q22=mean(q22[ExpCondition==1]))
implicit_negative <- ddply(implicit, .(polknow), summarize,  q2=mean(q2[ExpCondition==1]), q5=mean(q5[ExpCondition==1]), q10=mean(q10[ExpCondition==0]), q11=mean(q11[ExpCondition==1]), q16=mean(q16[ExpCondition==1]), q17=mean(q17[ExpCondition==1]), q18=mean(q18[ExpCondition==0]), q22=mean(q22[ExpCondition==0]))
Positive <- ((implicit_positive$q2+implicit_positive$q5+implicit_positive$q10+implicit_positive$q11+implicit_positive$q16+implicit_positive$q17+implicit_positive$q18+implicit_positive$q22)/8)
Negative <- ((implicit_negative$q2+implicit_negative$q5+implicit_negative$q10+implicit_negative$q11+implicit_negative$q16+implicit_negative$q17+implicit_negative$q18+implicit_negative$q22)/8)

graph_implicit_posneg <- as.data.frame(cbind(implicit_negative$polknow, Negative, Positive))
graph_implicit_posneg <- melt(graph_implicit_posneg, id="V1")

implicit_posneg_polknow <- ggplot(graph_implicit_posneg, aes(V1, value, colour = variable)) + geom_point() + xlab("Political knowledge") + ylab("Mean Answer") + ggtitle("Implicit") + theme_classic() + scale_colour_grey() + theme(legend.title=element_blank(), legend.position="none") 


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

library(tikzDevice)

tikz('knowledge_means.tex')
multiplot(explicit_posneg_gutman,implicit_posneg_gutman, explicit_posneg_polknow, implicit_posneg_polknow, cols=2)
dev.off()


# Matching models 

total$ExpCondition <- as.numeric(total$ExpCondition)
total$sex <- as.numeric(total$sex)
total$age <- as.numeric(total$age)
total$education <- as.numeric(total$education)


################################################################################


match_50plus_a <- lm(match_50Plus ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_50plus_b <- lm(match_50Plus ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*age, data=total)
match_50plus_c <- lm(match_50Plus ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*sex, data=total)
match_50plus_d <- lm(match_50Plus ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_50plus_e <- lm(match_50Plus ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*interestInPolitics, data=total)

BIC(match_50plus_a)
BIC(match_50plus_b)
BIC(match_50plus_c)
BIC(match_50plus_d)
BIC(match_50plus_e)

match_CDA_a <- lm(match_CDA ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_CDA_b <- lm(match_CDA ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*age, data=total)
match_CDA_c <- lm(match_CDA ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*sex, data=total)
match_CDA_d <- lm(match_CDA ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_CDA_e <- lm(match_CDA ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*interestInPolitics, data=total)

BIC(match_CDA_a)
BIC(match_CDA_b)
BIC(match_CDA_c)
BIC(match_CDA_d)
BIC(match_CDA_e)

match_CU_a <- lm(match_CU ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_CU_b <- lm(match_CU ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*age, data=total)
match_CU_c <- lm(match_CU ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*sex, data=total)
match_CU_d <- lm(match_CU ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_CU_e <- lm(match_CU ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*interestInPolitics, data=total)

BIC(match_CU_a)
BIC(match_CU_b)
BIC(match_CU_c)
BIC(match_CU_d)
BIC(match_CU_e)

match_D66_a <- lm(match_D66 ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_D66_b <- lm(match_D66 ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*age, data=total)
match_D66_c <- lm(match_D66 ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*sex, data=total)
match_D66_d <- lm(match_D66 ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_D66_e <- lm(match_D66 ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*interestInPolitics, data=total)

BIC(match_D66_a)
BIC(match_D66_b)
BIC(match_D66_c)
BIC(match_D66_d)
BIC(match_D66_e)

match_GL_a <- lm(match_GL ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_GL_b <- lm(match_GL ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*age, data=total)
match_GL_c <- lm(match_GL ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*sex, data=total)
match_GL_d <- lm(match_GL ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_GL_e <- lm(match_GL ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*interestInPolitics, data=total)

BIC(match_GL_a)
BIC(match_GL_b)
BIC(match_GL_c)
BIC(match_GL_d)
BIC(match_GL_e)

match_PvdA_a <- lm(match_PvdA ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_PvdA_b <- lm(match_PvdA ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*age, data=total)
match_PvdA_c <- lm(match_PvdA ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*sex, data=total)
match_PvdA_d <- lm(match_PvdA ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_PvdA_e <- lm(match_PvdA ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*interestInPolitics, data=total)

BIC(match_PvdA_a)
BIC(match_PvdA_b)
BIC(match_PvdA_c)
BIC(match_PvdA_d)
BIC(match_PvdA_e)

match_PvdD_a <- lm(match_PvdD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_PvdD_b <- lm(match_PvdD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*age, data=total)
match_PvdD_c <- lm(match_PvdD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*sex, data=total)
match_PvdD_d <- lm(match_PvdD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_PvdD_e <- lm(match_PvdD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*interestInPolitics, data=total)

BIC(match_PvdD_a)
BIC(match_PvdD_b)
BIC(match_PvdD_c)
BIC(match_PvdD_d)
BIC(match_PvdD_e)

match_PVV_a <- lm(match_PVV ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_PVV_b <- lm(match_PVV ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*age, data=total)
match_PVV_c <- lm(match_PVV ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*sex, data=total)
match_PVV_d <- lm(match_PVV ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_PVV_e <- lm(match_PVV ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*interestInPolitics, data=total)

BIC(match_PVV_a)
BIC(match_PVV_b)
BIC(match_PVV_c)
BIC(match_PVV_d)
BIC(match_PVV_e)

match_SGP_a <- lm(match_SGP ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_SGP_b <- lm(match_SGP ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*age, data=total)
match_SGP_c <- lm(match_SGP ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*sex, data=total)
match_SGP_d <- lm(match_SGP ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_SGP_e <- lm(match_SGP ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*interestInPolitics, data=total)

BIC(match_SGP_a)
BIC(match_SGP_b)
BIC(match_SGP_c)
BIC(match_SGP_d)
BIC(match_SGP_e)

match_SP_a <- lm(match_SP ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_SP_b <- lm(match_SP ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*age, data=total)
match_SP_c <- lm(match_SP ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*sex, data=total)
match_SP_d <- lm(match_SP ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_SP_e <- lm(match_SP ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*interestInPolitics, data=total)

BIC(match_SP_a)
BIC(match_SP_b)
BIC(match_SP_c)
BIC(match_SP_d)
BIC(match_SP_e)

match_VVD_a <- lm(match_VVD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_VVD_b <- lm(match_VVD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*age, data=total)
match_VVD_c <- lm(match_VVD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*sex, data=total)
match_VVD_d <- lm(match_VVD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_VVD_e <- lm(match_VVD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*interestInPolitics, data=total)

BIC(match_VVD_a)
BIC(match_VVD_b)
BIC(match_VVD_c)
BIC(match_VVD_d)
BIC(match_VVD_e)

match_DENK_a <- lm(match_DENK ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_DENK_b <- lm(match_DENK ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*age, data=total)
match_DENK_c <- lm(match_DENK ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*sex, data=total)
match_DENK_d <- lm(match_DENK ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_DENK_e <- lm(match_DENK ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*interestInPolitics, data=total)

BIC(match_DENK_a)
BIC(match_DENK_b)
BIC(match_DENK_c)
BIC(match_DENK_d)
BIC(match_DENK_e)

match_VNL_a <- lm(match_VNL ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_VNL_b <- lm(match_VNL ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*age, data=total)
match_VNL_c <- lm(match_VNL ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*sex, data=total)
match_VNL_d <- lm(match_VNL ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_VNL_e <- lm(match_VNL ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*interestInPolitics, data=total)

BIC(match_VNL_a)
BIC(match_VNL_b)
BIC(match_VNL_c)
BIC(match_VNL_d)
BIC(match_VNL_e)

match_FvD_a <- lm(match_FvD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_FvD_b <- lm(match_FvD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*age, data=total)
match_FvD_c <- lm(match_FvD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*sex, data=total)
match_FvD_d <- lm(match_FvD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_FvD_e <- lm(match_FvD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*interestInPolitics, data=total)

BIC(match_FvD_a)
BIC(match_FvD_b)
BIC(match_FvD_c)
BIC(match_FvD_d)
BIC(match_FvD_e)


#Best Models (based on BIC)

match_50plus_d <- lm(match_50Plus ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_CDA_a <- lm(match_CDA ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_CU_d <- lm(match_CU ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_D66_d <- lm(match_D66 ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_GL_d <- lm(match_GL ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_PvdA_d <- lm(match_PvdA ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_PvdD_d <- lm(match_PvdD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_PVV_d <- lm(match_PVV ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_SGP_c <- lm(match_SGP ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_SP_d <- lm(match_SP ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_VVD_c <- lm(match_VVD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_DENK_a <- lm(match_DENK ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*education, data=total)
match_VNL_a <- lm(match_VNL ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)
match_FvD_a <- lm(match_FvD ~ ExpCondition + age + sex + education + interestInPolitics + gutmann + ExpCondition*gutmann, data=total)

total_table <- stargazer(match_50plus_d,match_CDA_a,match_CU_d,match_D66_d,match_GL_d,match_PvdA_d,match_PvdD_d,match_PVV_d,match_SGP_a,match_SP_d,match_VVD_a,match_DENK_d,match_VNL_a,match_FvD_a)
write(total_table, file = "total.tex")

# Multiplot (with distributions)

total$ExpCondition <- as.factor(total$ExpCondition)

Plus <- ggplot(total, aes(x=match_50Plus, color=ExpCondition)) +
  geom_histogram(fill="white", position="identity", binwidth=.5,  alpha=.5)+
  theme(legend.position="top") +
        scale_color_grey() +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) + 
        ylab("Count") +
        xlab("Match in %") +
        ggtitle("50Plus \n") +
        theme_classic() +
        theme(legend.position="none", plot.title = element_text(hjust = 0.5,vjust = 0.5),axis.title.x=element_blank(), axis.title.y = element_blank())



CDA <- ggplot(total, aes(x=match_CDA, color=ExpCondition)) +
  geom_histogram(fill="white", position="identity", binwidth=.5,  alpha=.5)+
  theme(legend.position="top") +
  scale_color_grey() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  ylab("Count") +
  xlab("Match in %") +
  ggtitle("CDA \n") +
  theme_classic() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,vjust = 0.5),axis.title.x=element_blank(), axis.title.y = element_blank())

CU <- ggplot(total, aes(x=match_CU, color=ExpCondition)) +
  geom_histogram(fill="white", position="identity", binwidth=.5,  alpha=.5)+
  theme(legend.position="top") +
  scale_color_grey() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  ylab("Count") +
  xlab("Match in %") +
  ggtitle("CU \n") +
  theme_classic() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,vjust = 0.5),axis.title.x=element_blank(), axis.title.y = element_blank())

D66 <- ggplot(total, aes(x=match_D66, color=ExpCondition)) +
  geom_histogram(fill="white", position="identity", binwidth=.5,  alpha=.5)+
  theme(legend.position="top") +
  scale_color_grey() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  ylab("Count") +
  xlab("Match in %") +
  ggtitle("D66 \n") +
  theme_classic() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,vjust = 0.5),axis.title.x=element_blank(), axis.title.y = element_blank())

GL<- ggplot(total, aes(x=match_GL, color=ExpCondition)) +
  geom_histogram(fill="white", position="identity", binwidth=.5,  alpha=.5)+
  theme(legend.position="top") +
  scale_color_grey() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  ylab("Count") +
  xlab("Match in %") +
  ggtitle("GL \n") +
  theme_classic() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,vjust = 0.5),axis.title.x=element_blank(), axis.title.y = element_blank())

PvdA <- ggplot(total, aes(x=match_PvdA, color=ExpCondition)) +
  geom_histogram(fill="white", position="identity", binwidth=.5,  alpha=.5)+
  theme(legend.position="top") +
  scale_color_grey() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  ylab("Count") +
  xlab("Match in %") +
  ggtitle("PvdA \n") +
  theme_classic() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,vjust = 0.5),axis.title.x=element_blank(), axis.title.y = element_blank())

PvdD <- ggplot(total, aes(x=match_PvdD, color=ExpCondition)) +
  geom_histogram(fill="white", position="identity", binwidth=.5,  alpha=.5)+
  theme(legend.position="top") +
  scale_color_grey() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  ylab("Count") +
  xlab("Match in %") +
  ggtitle("PvdD \n") +
  theme_classic() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,vjust = 0.5),axis.title.x=element_blank(), axis.title.y = element_blank())

PVV <- ggplot(total, aes(x=match_PVV, color=ExpCondition)) +
  geom_histogram(fill="white", position="identity", binwidth=.5,  alpha=.5)+
  theme(legend.position="top") +
  scale_color_grey() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  ylab("Count") +
  xlab("Match in %") +
  ggtitle("PVV \n") +
  theme_classic() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,vjust = 0.5),axis.title.x=element_blank(), axis.title.y = element_blank())

SGP <- ggplot(total, aes(x=match_SGP, color=ExpCondition)) +
  geom_histogram(fill="white", position="identity", binwidth=.5,  alpha=.5)+
  theme(legend.position="top") +
  scale_color_grey() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  ylab("Count") +
  xlab("Match in %") +
  ggtitle("SGP \n") +
  theme_classic() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,vjust = 0.5),axis.title.x=element_blank(), axis.title.y = element_blank())

SP <- ggplot(total, aes(x=match_SP, color=ExpCondition)) +
  geom_histogram(fill="white", position="identity", binwidth=.5,  alpha=.5)+
  theme(legend.position="top") +
  scale_color_grey() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  ylab("Count") +
  xlab("Match in %") +
  ggtitle("SP \n") +
  theme_classic() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,vjust = 0.5),axis.title.x=element_blank(), axis.title.y = element_blank())

VVD <- ggplot(total, aes(x=match_VVD, color=ExpCondition)) +
  geom_histogram(fill="white", position="identity", binwidth=.5,  alpha=.5)+
  theme(legend.position="top") +
  scale_color_grey() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  ylab("Count") +
  xlab("Match in %") +
  ggtitle("VVD \n") +
  theme_classic() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,vjust = 0.5),axis.title.x=element_blank(), axis.title.y = element_blank())

DENK <- ggplot(total, aes(x=match_DENK, color=ExpCondition)) +
  geom_histogram(fill="white", position="identity", binwidth=.5,  alpha=.5)+
  theme(legend.position="top") +
  scale_color_grey() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  ylab("Count") +
  xlab("Match in %") +
  ggtitle("DENK \n") +
  theme_classic() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,vjust = 0.5),axis.title.x=element_blank(), axis.title.y = element_blank())

VNL <- ggplot(total, aes(x=match_VNL, color=ExpCondition)) +
  geom_histogram(fill="white", position="identity", binwidth=.5,  alpha=.5)+
  theme(legend.position="top") +
  scale_color_grey() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  ylab("Count") +
  xlab("Match in %") +
  ggtitle("VNL \n") +
  theme_classic() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,vjust = 0.5),axis.title.x=element_blank(), axis.title.y = element_blank())

FvD <- ggplot(total, aes(x=match_FvD, color=ExpCondition)) +
  geom_histogram(fill="white", position="identity", binwidth=.5,  alpha=.5)+
  theme(legend.position="top") +
  scale_color_grey() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  ylab("Count") +
  xlab("Match in %") +
  ggtitle("FvD \n") +
  theme_classic() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5,vjust = 0.5),axis.title.x=element_blank(), axis.title.y = element_blank())

tikz('multiplot.tex')
multiplot_graph <- multiplot(Plus, CDA, CU, D66, DENK, FvD, GL, PvdA, PvdD, PVV, SGP, SP, VVD, VNL, cols=3)
dev.off()



# PLOTS WITH THE HIGHEST MATCHES - RUN FOR BOTH GROUPS AGAIN

parties_df_exp1 <- read_csv("~/Dropbox/Thesis/Questionnaire/Reliability/Matching/parties.df.exp1.csv",col_names = FALSE)
users_df_exp1 <- read_csv("~/Dropbox/Thesis/Questionnaire/Reliability/Matching/users.df.exp1.csv",col_names = FALSE)

parties_df_exp2 <- read_csv("~/Dropbox/Thesis/Questionnaire/Reliability/Matching/parties.df.exp2.csv",col_names = FALSE)
users_df_exp2 <- read_csv("~/Dropbox/Thesis/Questionnaire/Reliability/Matching/users.df.exp2.csv",col_names = FALSE)

# Run these functions after loading to csvs
ignoreValueAbove<-5

forEachUser <- function(row, parties, ignoreValueAbove){
  #Find user's matchings with all parties
  match<-apply(parties, 1, forEachParty, row, ignoreValueAbove);
  return(match)
}

forEachParty <-function(row, userAns, ignoreValueAbove){
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


# This will produce the matrix of results
stats_exp1<-t(apply(users_df_exp1, 1, forEachUser, parties_df_exp1, ignoreValueAbove));
stats_exp2<-t(apply(users_df_exp2, 1, forEachUser, parties_df_exp2, ignoreValueAbove));

# Add column names
parties<-c("50Plus","CDA","CU","D66","GL","PvdA","PvdD","PVV","SGP","SP","VVD","DENK","VNL","FvD")
colnames(stats_exp1) <- parties
colnames(stats_exp2) <- parties

# Write to csv
write.csv(stats_exp1,file = "match_exp1", row.names = FALSE)
write.csv(stats_exp2,file = "match_exp2", row.names = FALSE)

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

