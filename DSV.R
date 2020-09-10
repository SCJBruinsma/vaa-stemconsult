library(readr)
library(mokken)
library(stargazer)
library(poLCA)
library(psych)
library(GPArotation)
library(dplyr)
library(car)

##################################################### Prepare data #####################################################

netherlands <- read_csv("stemconsult_data_numeric.csv")

#####################################################  Cleaning #####################################################

# Remove first 50 users
netherlands <- subset(netherlands, id > 50)

# VAA after March 15

netherlands <- subset(netherlands, date < "2017-03-16")

# VAA between 10-12 March

netherlands <- subset(netherlands, date < "2017-03-10" | date > "2017-03-12")

# Returning users

netherlands <- subset(netherlands, attempts == 1)

# Total time less than 100 seconds

netherlands <- subset(netherlands, totalTime > 75)

# At least one answer less than 2 seconds

netherlands <- subset(netherlands, t1 > 1.9)
netherlands <- subset(netherlands, t2 > 1.9)
netherlands <- subset(netherlands, t3 > 1.9)
netherlands <- subset(netherlands, t4 > 1.9)
netherlands <- subset(netherlands, t5 > 1.9)
netherlands <- subset(netherlands, t6 > 1.9)
netherlands <- subset(netherlands, t7 > 1.9)
netherlands <- subset(netherlands, t8 > 1.9)
netherlands <- subset(netherlands, t9 > 1.9)
netherlands <- subset(netherlands, t10 > 1.9)
netherlands <- subset(netherlands, t11 > 1.9)
netherlands <- subset(netherlands, t12 > 1.9)
netherlands <- subset(netherlands, t13 > 1.9)
netherlands <- subset(netherlands, t14 > 1.9)
netherlands <- subset(netherlands, t15 > 1.9)
netherlands <- subset(netherlands, t16 > 1.9)
netherlands <- subset(netherlands, t17 > 1.9)
netherlands <- subset(netherlands, t18 > 1.9)
netherlands <- subset(netherlands, t19 > 1.9)
netherlands <- subset(netherlands, t20 > 1.9)
netherlands <- subset(netherlands, t21 > 1.9)
netherlands <- subset(netherlands, t22 > 1.9)
netherlands <- subset(netherlands, t23 > 1.9)
netherlands <- subset(netherlands, t24 > 1.9)
netherlands <- subset(netherlands, t25 > 1.9)

# 12 statements in the same way

netherlands <- netherlands[!(netherlands$q1==netherlands$q2 & netherlands$q2==netherlands$q3 & netherlands$q3==netherlands$q4 & netherlands$q4==netherlands$q5 & netherlands$q5==netherlands$q6 & netherlands$q6==netherlands$q7 & netherlands$q7==netherlands$q8 & netherlands$q8==netherlands$q9 & netherlands$q9==netherlands$q10 & netherlands$q10==netherlands$q11 & netherlands$q11==netherlands$q12),]
netherlands <- netherlands[!(netherlands$q2==netherlands$q3 & netherlands$q3==netherlands$q4 & netherlands$q4==netherlands$q5 & netherlands$q5==netherlands$q6 & netherlands$q6==netherlands$q7 & netherlands$q7==netherlands$q8 & netherlands$q8==netherlands$q9 & netherlands$q9==netherlands$q10 & netherlands$q10==netherlands$q11 & netherlands$q11==netherlands$q12 & netherlands$q12==netherlands$q13),]
netherlands <- netherlands[!(netherlands$q3==netherlands$q4 & netherlands$q4==netherlands$q5 & netherlands$q5==netherlands$q6 & netherlands$q6==netherlands$q7 & netherlands$q7==netherlands$q8 & netherlands$q8==netherlands$q9 & netherlands$q9==netherlands$q10 & netherlands$q10==netherlands$q11 & netherlands$q11==netherlands$q12 & netherlands$q12==netherlands$q13 & netherlands$q13==netherlands$q14),]
netherlands <- netherlands[!(netherlands$q4==netherlands$q5 & netherlands$q5==netherlands$q6 & netherlands$q6==netherlands$q7 & netherlands$q7==netherlands$q8 & netherlands$q8==netherlands$q9 & netherlands$q9==netherlands$q10 & netherlands$q10==netherlands$q11 & netherlands$q11==netherlands$q12 & netherlands$q12==netherlands$q13 & netherlands$q13==netherlands$q14 & netherlands$q14==netherlands$q15),]
netherlands <- netherlands[!(netherlands$q5==netherlands$q6 & netherlands$q6==netherlands$q7 & netherlands$q7==netherlands$q8 & netherlands$q8==netherlands$q9 & netherlands$q9==netherlands$q10 & netherlands$q10==netherlands$q11 & netherlands$q11==netherlands$q12 & netherlands$q12==netherlands$q13 & netherlands$q13==netherlands$q14 & netherlands$q14==netherlands$q15 & netherlands$q15==netherlands$q16),]
netherlands <- netherlands[!(netherlands$q6==netherlands$q7 & netherlands$q7==netherlands$q8 & netherlands$q8==netherlands$q9 & netherlands$q9==netherlands$q10 & netherlands$q10==netherlands$q11 & netherlands$q11==netherlands$q12 & netherlands$q12==netherlands$q13 & netherlands$q13==netherlands$q14 & netherlands$q14==netherlands$q15 & netherlands$q15==netherlands$q16 & netherlands$q16==netherlands$q17),]
netherlands <- netherlands[!(netherlands$q7==netherlands$q8 & netherlands$q8==netherlands$q9 & netherlands$q9==netherlands$q10 & netherlands$q10==netherlands$q11 & netherlands$q11==netherlands$q12 & netherlands$q12==netherlands$q13 & netherlands$q13==netherlands$q14 & netherlands$q14==netherlands$q15 & netherlands$q15==netherlands$q16 & netherlands$q16==netherlands$q17 & netherlands$q17==netherlands$q18),]
netherlands <- netherlands[!(netherlands$q8==netherlands$q9 & netherlands$q9==netherlands$q10 & netherlands$q10==netherlands$q11 & netherlands$q11==netherlands$q12 & netherlands$q12==netherlands$q13 & netherlands$q13==netherlands$q14 & netherlands$q14==netherlands$q15 & netherlands$q15==netherlands$q16 & netherlands$q16==netherlands$q17 & netherlands$q17==netherlands$q18 & netherlands$q18==netherlands$q19),]
netherlands <- netherlands[!(netherlands$q9==netherlands$q10 & netherlands$q10==netherlands$q11 & netherlands$q11==netherlands$q12 & netherlands$q12==netherlands$q13 & netherlands$q13==netherlands$q14 & netherlands$q14==netherlands$q15 & netherlands$q15==netherlands$q16 & netherlands$q16==netherlands$q17 & netherlands$q17==netherlands$q18 & netherlands$q18==netherlands$q19 & netherlands$q19==netherlands$q20),]
netherlands <- netherlands[!(netherlands$q10==netherlands$q11 & netherlands$q11==netherlands$q12 & netherlands$q12==netherlands$q13 & netherlands$q13==netherlands$q14 & netherlands$q14==netherlands$q15 & netherlands$q15==netherlands$q16 & netherlands$q16==netherlands$q17 & netherlands$q17==netherlands$q18 & netherlands$q18==netherlands$q19 & netherlands$q19==netherlands$q20 & netherlands$q20==netherlands$q21),]
netherlands <- netherlands[!(netherlands$q11==netherlands$q12 & netherlands$q12==netherlands$q13 & netherlands$q13==netherlands$q14 & netherlands$q14==netherlands$q15 & netherlands$q15==netherlands$q16 & netherlands$q16==netherlands$q17 & netherlands$q17==netherlands$q18 & netherlands$q18==netherlands$q19 & netherlands$q19==netherlands$q20 & netherlands$q20==netherlands$q21 & netherlands$q21==netherlands$q22),]
netherlands <- netherlands[!(netherlands$q12==netherlands$q13 & netherlands$q13==netherlands$q14 & netherlands$q14==netherlands$q15 & netherlands$q15==netherlands$q16 & netherlands$q16==netherlands$q17 & netherlands$q17==netherlands$q18 & netherlands$q18==netherlands$q19 & netherlands$q19==netherlands$q20 & netherlands$q20==netherlands$q21 & netherlands$q21==netherlands$q22 & netherlands$q22==netherlands$q23),]
netherlands <- netherlands[!(netherlands$q13==netherlands$q14 & netherlands$q14==netherlands$q15 & netherlands$q15==netherlands$q16 & netherlands$q16==netherlands$q17 & netherlands$q17==netherlands$q18 & netherlands$q18==netherlands$q19 & netherlands$q19==netherlands$q20 & netherlands$q20==netherlands$q21 & netherlands$q21==netherlands$q22 & netherlands$q22==netherlands$q23 & netherlands$q23==netherlands$q24),]
netherlands <- netherlands[!(netherlands$q14==netherlands$q15 & netherlands$q15==netherlands$q16 & netherlands$q16==netherlands$q17 & netherlands$q17==netherlands$q18 & netherlands$q18==netherlands$q19 & netherlands$q19==netherlands$q20 & netherlands$q20==netherlands$q21 & netherlands$q21==netherlands$q22 & netherlands$q22==netherlands$q23 & netherlands$q23==netherlands$q24 & netherlands$q24==netherlands$q25),]


# Remove unnecessary variables

variables <- c("ExpCondition", "sex", "age", "education", "interestInPolitics", "q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10", "q11", "q12", "q13", "q14", "q15", "q16", "q17", "q18","q19","q20","q21","q22","q23","q24","q25")
netherlands <- netherlands[variables]
netherlands$sex[netherlands$sex == 3] <- NA
netherlands$sex[netherlands$sex == 1] <- 0
netherlands$sex[netherlands$sex == 2] <- 1
netherlands[netherlands == -999|netherlands == -998|netherlands == -997|netherlands == -996|netherlands == -995] <- NA
netherlands <- na.omit(netherlands)

variables2 <- c("ExpCondition", "q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10", "q11", "q12", "q13", "q14", "q15", "q16", "q17", "q18","q19","q20","q21","q22","q23","q24","q25")
netherlands <- netherlands[variables2]

stemconsult_exp1 <- as.data.frame(subset(netherlands,ExpCondition==1))
stemconsult_exp2 <- as.data.frame(subset(netherlands,ExpCondition==2))
stemconsult_exp1 <- subset(stemconsult_exp1,select= c(q1:q25))
stemconsult_exp2 <- subset(stemconsult_exp2,select= c(q1:q25))

rm(netherlands,variables,variables2)


##################################################### Start DSV #####################################################

economic <- c("q1","q2","q4","q5","q7","q15","q16","q18","q21","q22","q23","q24")
social <- c("q3","q6","q8","q9","q10","q11","q12","q13","q14","q17","q19","q20","q25")

stemconsult_exp1_ec <- stemconsult_exp1[economic]
stemconsult_exp1_so <- stemconsult_exp1[social]
stemconsult_exp2_ec <- stemconsult_exp2[economic]
stemconsult_exp2_so <- stemconsult_exp2[social]

stemconsult_exp1_ec_rev = 6 - stemconsult_exp1_ec
stemconsult_exp1_so_rev = 6 - stemconsult_exp1_so
stemconsult_exp2_ec_rev = 6 - stemconsult_exp2_ec
stemconsult_exp2_so_rev = 6 - stemconsult_exp2_so

colnames(stemconsult_exp1_ec_rev) <- paste(colnames(stemconsult_exp1_ec_rev), "rev", sep = "_")
colnames(stemconsult_exp1_so_rev) <- paste(colnames(stemconsult_exp1_so_rev), "rev", sep = "_")
colnames(stemconsult_exp2_ec_rev) <- paste(colnames(stemconsult_exp2_ec_rev), "rev", sep = "_")
colnames(stemconsult_exp2_so_rev) <- paste(colnames(stemconsult_exp2_so_rev), "rev", sep = "_")

stemconsult_exp1_ec <- cbind(stemconsult_exp1_ec,stemconsult_exp1_ec_rev)
stemconsult_exp1_so <- cbind(stemconsult_exp1_so,stemconsult_exp1_so_rev)
stemconsult_exp2_ec <- cbind(stemconsult_exp2_ec,stemconsult_exp2_ec_rev)
stemconsult_exp2_so <- cbind(stemconsult_exp2_so,stemconsult_exp2_so_rev)

rm(stemconsult_exp1_ec_rev,stemconsult_exp1_so_rev, stemconsult_exp2_ec_rev, stemconsult_exp2_so_rev)

economic_exp1 <- c("q1_rev","q2_rev","q4_rev","q5","q7_rev","q15","q16","q18_rev","q21_rev","q22_rev","q23","q24_rev")
social_exp1 <- c("q3","q6","q8","q9_rev","q10_rev","q11","q12","q13_rev","q14","q17","q19","q20_rev","q25")
economic_exp2 <- c("q1","q2","q4_rev","q5_rev","q7_rev","q15_rev","q16_rev","q18","q21","q22","q23","q24_rev")
social_exp2 <- c("q3","q6","q8","q9_rev","q10","q11_rev","q12","q13_rev","q14","q17_rev","q19","q20_rev","q25_rev")

stemconsult_exp1_ec <- stemconsult_exp1_ec[economic_exp1]
stemconsult_exp1_so <- stemconsult_exp1_so[social_exp1]
stemconsult_exp2_ec <- stemconsult_exp2_ec[economic_exp2]
stemconsult_exp2_so <- stemconsult_exp2_so[social_exp2]

# Coerce into matrix so we can run Mokken

stemconsult_exp1_ec_mat <- as.matrix(stemconsult_exp1_ec)
stemconsult_exp1_so_mat <- as.matrix(stemconsult_exp1_so)
stemconsult_exp2_ec_mat <- as.matrix(stemconsult_exp2_ec)
stemconsult_exp2_so_mat <- as.matrix(stemconsult_exp2_so)

# Run Mokken Scaling

coefH_exp1_ec <- coefH(stemconsult_exp1_ec_mat, se = TRUE, nice.output = TRUE, group.var = NULL)
coefH_exp1_so <- coefH(stemconsult_exp1_so_mat, se = TRUE, nice.output = TRUE, group.var = NULL)
coefH_exp2_ec <- coefH(stemconsult_exp2_ec_mat, se = TRUE, nice.output = TRUE, group.var = NULL)
coefH_exp2_so <- coefH(stemconsult_exp2_so_mat, se = TRUE, nice.output = TRUE, group.var = NULL)

coefH_exp1_ec
coefH_exp1_so
coefH_exp2_ec
coefH_exp2_so

#Monotonicity - For crit values

monoton_exp1_ec <- check.monotonicity(stemconsult_exp1_ec_mat, minsize=100)
monoton_exp1_so <- check.monotonicity(stemconsult_exp1_so_mat, minsize=100)
monoton_exp2_ec <- check.monotonicity(stemconsult_exp2_ec_mat, minsize=100)
monoton_exp2_so <- check.monotonicity(stemconsult_exp2_so_mat, minsize=100)

View(summary(monoton_exp1_ec))
View(summary(monoton_exp1_so))
View(summary(monoton_exp2_ec))
View(summary(monoton_exp2_so))

# Check LCRC and cronbach a

f_ec_exp1 <- cbind(q1_rev, q2_rev, q4_rev, q5, q7_rev, q15, q16, q18_rev, q21_rev, q22_rev, q23, q24_rev) ~ 1
f_so_exp1 <- cbind(q3,q6,q8,q9_rev,q10_rev,q11,q12,q13_rev,q14,q17,q19,q20_rev,q25) ~ 1
f_ec_exp2 <- cbind(q1,q2,q4_rev,q5_rev,q7_rev,q15_rev,q16_rev,q18,q21,q22,q23,q24_rev) ~ 1
f_so_exp2 <- cbind(q3,q6,q8,q9_rev,q10,q11_rev,q12,q13_rev,q14,q17_rev,q19,q20_rev,q25_rev) ~ 1

set.seed(42)
poLCA(f_ec_exp1, stemconsult_exp1_ec, nclass = 4, verbose=TRUE)
set.seed(42)
poLCA(f_so_exp1, stemconsult_exp1_so, nclass = 4, verbose=TRUE)
set.seed(42)
poLCA(f_ec_exp2, stemconsult_exp2_ec, nclass = 4, verbose=TRUE)
set.seed(42)
poLCA(f_so_exp2, stemconsult_exp2_so, nclass = 5, verbose=TRUE)

check.reliability(stemconsult_exp1_ec_mat, LCRC = TRUE, nclass = 4)
check.reliability(stemconsult_exp1_so_mat, LCRC = TRUE, nclass = 5)
check.reliability(stemconsult_exp2_ec_mat, LCRC = TRUE, nclass = 4)
check.reliability(stemconsult_exp2_so_mat, LCRC = TRUE, nclass = 5)

# Ordinal Cronbach a and ordinal omega

exp1_ec_poly <- polychoric(stemconsult_exp1_ec)
alpha(exp1_ec_poly$rho)
omega(exp1_ec_poly$rho)

exp1_so_poly <- polychoric(stemconsult_exp1_so)
alpha(exp1_so_poly$rho)
omega(exp1_so_poly$rho)

exp2_ec_poly <- polychoric(stemconsult_exp2_ec)
alpha(exp2_ec_poly$rho)
omega(exp2_ec_poly$rho)

exp2_so_poly <- polychoric(stemconsult_exp2_so)
alpha(exp2_so_poly$rho)
omega(exp2_so_poly$rho)



##################################################### Calculate New Scales ##################################################### 

analysis <- c("q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q16","q17","q18","q19","q20","q21","q22","q23","q24","q25")

stemconsult_exp1 <- stemconsult_exp1[analysis]
stemconsult_exp2 <- stemconsult_exp2[analysis]

stemconsult_exp1[1:23] <- lapply(stemconsult_exp1[1:23], as.numeric)
stemconsult_exp2[1:23] <- lapply(stemconsult_exp2[1:23], as.numeric)

stemconsult_exp1_rev = 6 - stemconsult_exp1
stemconsult_exp2_rev = 6 - stemconsult_exp2
colnames(stemconsult_exp1_rev) <- paste(colnames(stemconsult_exp1_rev), "rev", sep = "_")
colnames(stemconsult_exp2_rev) <- paste(colnames(stemconsult_exp2_rev), "rev", sep = "_")
stemconsult_exp1 <- cbind(stemconsult_exp1,stemconsult_exp1_rev)
stemconsult_exp2 <- cbind(stemconsult_exp2,stemconsult_exp2_rev)
stemconsult_exp1_mat <- as.matrix(stemconsult_exp1)
stemconsult_exp2_mat <- as.matrix(stemconsult_exp2)
rm(stemconsult_exp1_rev, stemconsult_exp2_rev)

# Mokken

scales_exp1_aisp <- aisp(stemconsult_exp1_mat, search = "normal", lowerbound=.3) #AISP procedure
scales_exp1_ga <- aisp(stemconsult_exp1_mat, search = "ga", lowerbound=.3) # GA procedure
scales_exp2_aisp <- aisp(stemconsult_exp2_mat, search = "normal",  lowerbound=.3) #AISP procedure
scales_exp2_ga <- aisp(stemconsult_exp2_mat, search = "ga", lowerbound=.3) # GA procedure

write.csv(scales_exp1_ga, "exp1.csv")
write.csv(scales_exp2_ga, "exp2.csv")

scale_exp1_1_list <- c("q3","q4_rev","q9_rev","q11","q12","q13_rev","q14","q19_rev","q25")
scale_exp2_1_list <- c("q2","q3","q9_rev","q11","q12","q13_rev","q14","q19_rev","q25")

scale_exp1_1 <- stemconsult_exp1[scale_exp1_1_list]
scale_exp2_1 <- stemconsult_exp2[scale_exp2_1_list]
scale_exp1_1_mat <- as.matrix(scale_exp1_1)
scale_exp2_1_mat <- as.matrix(scale_exp2_1)

coefH_exp1_1 <- coefH(scale_exp1_1_mat, se = TRUE, nice.output = TRUE, group.var = NULL)
coefH_exp2_1 <- coefH(scale_exp2_1_mat, se = TRUE, nice.output = TRUE, group.var = NULL)

coefH_exp1_1
coefH_exp2_1

monoton_exp1_1 <- check.monotonicity(scale_exp1_1_mat, minsize=100)
monoton_exp2_1 <- check.monotonicity(scale_exp2_1_mat, minsize=100)

View(summary(monoton_exp1_1))
View(summary(monoton_exp2_1))


# Check LCRC and cronbach a

f_ec_exp1_new <- cbind(q3,q4_rev,q9_rev,q11,q12,q13_rev,q14,q19_rev,q25) ~ 1
f_ec_exp2_new <- cbind(q2,q3,q9_rev,q11,q12,q13_rev,q14,q19_rev,q25) ~ 1


set.seed(42)
poLCA(f_ec_exp1_new, scale_exp1_1, nclass = 5, verbose=TRUE)
set.seed(42)
poLCA(f_ec_exp2_new, scale_exp2_1, nclass = 3, verbose=TRUE)

check.reliability(scale_exp1_1, LCRC = TRUE, nclass = 5)
check.reliability(scale_exp2_1, LCRC = TRUE, nclass = 3)


# Ordinal Cronbach a and ordinal omega

scale_exp1_1_poly <- polychoric(scale_exp1_1)
psych::alpha(scale_exp1_1_poly$rho)
omega(scale_exp1_1_poly$rho)

scale_exp2_1_poly <- polychoric(scale_exp2_1)
psych::alpha(scale_exp2_1_poly$rho)
omega(scale_exp2_1_poly$rho)

rm(list=ls(all=TRUE))
