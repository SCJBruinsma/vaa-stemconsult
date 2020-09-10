library(ggplot2)
library(reshape2)
library(tikzDevice)
library(readr)

Netherlands2017_NumericExport <- read_csv("stemconsult_data_numeric.csv")

variables <- c("ExpCondition", "t1", "t2", "t3","t4", "t5", "t6","t7", "t8", "t9","t10", "t11", "t12","t13", "t14", "t15","t16", "t17", "t18","t19", "t20", "t21", "t22","t23", "t24", "t25")
stemconsult <- stemconsult_data_numeric[variables]
rm(Netherlands2017_NumericExport, variables)

stemconsult_exp1 <- subset(stemconsult, ExpCondition== 1)
stemconsult_exp2 <- subset(stemconsult, ExpCondition== 2)

stemconsult_exp1$ExpCondition <- NULL 
stemconsult_exp2$ExpCondition <- NULL 


# Remove Outliers (Any observation outside 1.5 SD) - L. Collado Torres

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

outliers_exp1 <- findOutlier(stemconsult_exp1)
outliers_exp2 <- findOutlier(stemconsult_exp2)


stemconsult_exp1 <- removeOutlier(stemconsult_exp1, outliers_exp1)
stemconsult_exp2 <- removeOutlier(stemconsult_exp2, outliers_exp2)


# Make the plots

mean_exp1 <- as.data.frame(colMeans(stemconsult_exp1, na.rm = TRUE))
mean_exp2 <- as.data.frame(colMeans(stemconsult_exp2, na.rm = TRUE))

means <- cbind(mean_exp1,mean_exp2)
means  <- cbind(rownames(means), data.frame(means, row.names=NULL))

colnames(means)[1] <- "item"
colnames(means)[2] <- "exp1"
colnames(means)[3] <- "exp2"

means_melt <- melt(means)
means_melt$item <- factor(means_melt$item,levels(means_melt$item)[c(1,12,19:25,2:11,13:18)])
levels(means_melt$item) <- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q23","Q24","Q25")
means_melt$item <- factor(means_melt$item, levels=rev(levels(means_melt$item)))


means$difference <- means$exp1 - means$exp2
means$exp1 <- NULL
means$exp2 <- NULL
means$item <- factor(means$item,levels(means$item)[c(1,12,19:25,2:11,13:18)])
levels(means$item) <- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q23","Q24","Q25")
means$item <- factor(means$item, levels=rev(levels(means$item)))

rm(mean_exp1,mean_exp2, stemconsult)


tikz(file = "plot_mean_response.tex", width = 6, height = 6)
ggplot(means_melt, aes(x = item, y= value, fill = variable)) +
  geom_col(width=.8, position = "dodge")+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("exp1", "exp2"),labels=c("A", "B"))+
  xlab("Item") + 
  ylab("Mean Response Time") +
  ggtitle("Mean Response Time per Question") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10.2), breaks = c(0,2,4,6,8,10)) +
  coord_flip() +
  geom_hline(aes(yintercept=2), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=6), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=10), color="grey", linetype="dashed")+
  theme_classic()
print(barplot)
dev.off()

tikz(file = "plot_difference_response.tex", width = 6, height = 6)
ggplot(means, aes(x = item, y= difference)) +
  geom_col(width=.5, position = "dodge")+
  scale_fill_grey(name="Version",  breaks=c("exp1", "exp2"),labels=c("A", "B"))+
  xlab("Item") + 
  ylab("Mean Response Time") +
  ggtitle("Difference in Mean Response time per Question") +
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 3)) +
  coord_flip() +
  geom_hline(aes(yintercept=-1), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=1), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=2), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=3), color="grey", linetype="dashed")+
  theme_classic()
print(barplot)
dev.off()


# With total time clean


Netherlands2017_NumericExport <- read_csv("Netherlands2017_NumericExport.csv")

variables <- c("ExpCondition", "t1", "t2", "t3","t4", "t5", "t6","t7", "t8", "t9","t10", "t11", "t12","t13", "t14", "t15","t16", "t17", "t18","t19", "t20", "t21", "t22","t23", "t24", "t25", "totalTime")
stemconsult <- stemconsult_data_numeric[variables]
rm(Netherlands2017_NumericExport, variables)

stemconsult_exp1 <- subset(stemconsult, ExpCondition== 1)
stemconsult_exp2 <- subset(stemconsult, ExpCondition== 2)

stemconsult_exp1$ExpCondition <- NULL 
stemconsult_exp2$ExpCondition <- NULL 

totalTime_exp1 <- stemconsult_exp1$totalTime
totalTime_exp1 <- as.data.frame(totalTime_exp1)
totalTime_exp2 <- stemconsult_exp2$totalTime
totalTime_exp2 <- as.data.frame(totalTime_exp2)

# Remove Outliers (Any observation outside 1.5 SD) - L. Collado Torres

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

outliers_exp1 <- findOutlier(totalTime_exp1)
outliers_exp2 <- findOutlier(totalTime_exp2)

stemconsult_exp1 <- stemconsult_exp1[-outliers_exp1,]
stemconsult_exp2 <- stemconsult_exp2[-outliers_exp2,]

stemconsult_exp1_full <- stemconsult_exp1
stemconsult_exp2_full <- stemconsult_exp2

stemconsult_exp1$totalTime <- NULL 
stemconsult_exp2$totalTime <- NULL 

# Make the plots

mean_exp1 <- as.data.frame(colMeans(stemconsult_exp1, na.rm = TRUE))
mean_exp2 <- as.data.frame(colMeans(stemconsult_exp2, na.rm = TRUE))

means <- cbind(mean_exp1,mean_exp2)
means  <- cbind(rownames(means), data.frame(means, row.names=NULL))

colnames(means)[1] <- "item"
colnames(means)[2] <- "exp1"
colnames(means)[3] <- "exp2"

means_melt <- melt(means)
means_melt$item <- factor(means_melt$item,levels(means_melt$item)[c(1,12,19:25,2:11,13:18)])
levels(means_melt$item) <- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q23","Q24","Q25")
means_melt$item <- factor(means_melt$item, levels=rev(levels(means_melt$item)))


means$difference <- means$exp1 - means$exp2
means$exp1 <- NULL
means$exp2 <- NULL
means$item <- factor(means$item,levels(means$item)[c(1,12,19:25,2:11,13:18)])
levels(means$item) <- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q23","Q24","Q25")
means$item <- factor(means$item, levels=rev(levels(means$item)))

rm(mean_exp1,mean_exp2, stemconsult)

tikz(file = "plot_mean_response.tex", width = 6, height = 6)
ggplot(means_melt, aes(x = item, y= value, fill = variable)) +
  geom_col(width=.8, position = "dodge")+
  scale_fill_grey(start = 0, end = .9,name="Version",  breaks=c("exp1", "exp2"),labels=c("A", "B"))+
  xlab("Item") + 
  ylab("Mean Response Time") +
  ggtitle("Mean Response Time per Question") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12), breaks = c(0,2,4,6,8,10,12)) +
  coord_flip() +
  geom_hline(aes(yintercept=2), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=4), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=6), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=8), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=10), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=12), color="grey", linetype="dashed")+
  theme_classic()
print(barplot)
dev.off()

tikz(file = "plot_difference_response.tex", width = 6, height = 6)
ggplot(means, aes(x = item, y= difference)) +
  geom_col(width=.5, position = "dodge")+
  scale_fill_grey(name="Version",  breaks=c("exp1", "exp2"),labels=c("A", "B"))+
  xlab("Item") + 
  ylab("Mean Response Time") +
  ggtitle("Difference in Mean Response time per Question") +
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 3)) +
  coord_flip() +
  geom_hline(aes(yintercept=-1), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=0), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=1), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=2), color="grey", linetype="dashed")+
  geom_hline(aes(yintercept=3), color="grey", linetype="dashed")+
  theme_classic()
print(barplot)
dev.off()


# grouped boxplot
ggplot(means_melt, aes(x=item, y=value, fill=variable)) + 
  geom_boxplot()

# One box per treatment
ggplot(means_melt, aes(x=item, y=value, fill=variable)) + 
  geom_boxplot() +
  facet_wrap(~variable)

# one box per variety
ggplot(means_melt, aes(x=item, y=value, fill=variable)) + 
  geom_boxplot() +
  facet_wrap(~item, scale="free")

# Means for totalTime

var.test(stemconsult_exp1_full$totalTime,stemconsult_exp2_full$totalTime)
t.test(stemconsult_exp1_full$totalTime,stemconsult_exp2_full$totalTime, var.equal=FALSE)

plot(stemconsult_exp1_full$totalTime, type = "o") 
hist(stemconsult_exp1_full$totalTime)
plot(density(stemconsult_exp1_full$totalTime))
boxplot(stemconsult_exp1_full$totalTime)

plot(stemconsult_exp2_full$totalTime, type = "o") 
hist(stemconsult_exp2_full$totalTime)
plot(density(stemconsult_exp2_full$totalTime))
boxplot(stemconsult_exp2_full$totalTime)



# Time differences 

stemconsult_data_numeric <- read_csv("stemconsult_data_numeric.csv")
variables <- c("ExpCondition", "t1", "t2", "t3","t4", "t5", "t6","t7", "t8", "t9","t10", "t11", "t12","t13", "t14", "t15","t16", "t17", "t18","t19", "t20", "t21", "t22","t23", "t24", "t25")
stemconsult <- stemconsult_data_numeric[variables]
rm(stemconsult_data_numeric, variables)

stemconsult_exp1 <- subset(stemconsult, ExpCondition== 1)
stemconsult_exp2 <- subset(stemconsult, ExpCondition== 2)

stemconsult_exp1$ExpCondition <- NULL 
stemconsult_exp2$ExpCondition <- NULL 

# Remove Outliers (Any observation outside 1.5 SD) - L. Collado Torres

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

outliers_exp1 <- findOutlier(stemconsult_exp1)
outliers_exp2 <- findOutlier(stemconsult_exp2)


stemconsult_exp1 <- removeOutlier(stemconsult_exp1, outliers_exp1)
stemconsult_exp2 <- removeOutlier(stemconsult_exp2, outliers_exp2)
stemconsult_exp1$ExpCondition <- 1 
stemconsult_exp2$ExpCondition <- 2 
stemconsult <- rbind(stemconsult_exp1,stemconsult_exp2)
stemconsult$ExpCondition <- as.factor(stemconsult$ExpCondition)

t.test(stemconsult$t1~stemconsult$ExpCondition, var.equal=FALSE)
t.test(stemconsult$t15~stemconsult$ExpCondition, var.equal=FALSE)
t.test(stemconsult$t21~stemconsult$ExpCondition, var.equal=FALSE)
t.test(stemconsult$t25~stemconsult$ExpCondition, var.equal=FALSE)

t.test(stemconsult$t2~stemconsult$ExpCondition, var.equal=FALSE)
t.test(stemconsult$t5~stemconsult$ExpCondition, var.equal=FALSE)
t.test(stemconsult$t10~stemconsult$ExpCondition, var.equal=FALSE)
t.test(stemconsult$t11~stemconsult$ExpCondition, var.equal=FALSE)
t.test(stemconsult$t16~stemconsult$ExpCondition, var.equal=FALSE)
t.test(stemconsult$t17~stemconsult$ExpCondition, var.equal=FALSE)
t.test(stemconsult$t18~stemconsult$ExpCondition, var.equal=FALSE)
t.test(stemconsult$t22~stemconsult$ExpCondition, var.equal=FALSE)


