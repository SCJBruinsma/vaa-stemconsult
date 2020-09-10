library(ca)
library(readr)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)

# DATA INPUT

set.seed(42)

stemconsult_data_numeric <- read_csv("stemconsult_data_numeric.csv")

stemconsult_exp1 <- as.data.frame(subset(stemconsult_data_numeric,ExpCondition==1))
stemconsult_exp2 <- as.data.frame(subset(stemconsult_data_numeric,ExpCondition==2))

stemconsult_exp1[stemconsult_exp1==-999] <- 9
stemconsult_exp2[stemconsult_exp2==-999] <- 9

economic <- c("q1","q2","q4","q7","q18","q21","q22","q5","q15","q16","q23","q24")
social <- c("q9","q10","q13","q20","q3","q6","q8","q11","q12","q14","q17","q19","q25")

stemconsult_exp1_ec <- stemconsult_exp1[economic]
stemconsult_exp1_so <- stemconsult_exp1[social]
stemconsult_exp2_ec <- stemconsult_exp2[economic]
stemconsult_exp2_so <- stemconsult_exp2[social]


# MCA - VERSION A

stemconsult_exp1_ec.b <- mjca(stemconsult_exp1_ec,lambda="Burt")$Burt
stemconsult_exp1_so.b <- mjca(stemconsult_exp1_so,lambda="Burt")$Burt

stemconsult_exp1_ec.b.NA_N <- c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66)
stemconsult_exp1_so.b.NA_N <- c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66)
stemconsult_exp1_ec.b.sub <- c(1,2,3,4,5,7,8,9,10,11,13,14,15,16,17,19,20,21,22,23,25,26,27,28,29,31,32,33,34,35,37,38,39,40,41,43,44,45,46,47,49,50,51,52,53,55,56,57,58,59,61,62,63,64,65)
stemconsult_exp1_so.b.sub <- c(1,2,3,4,5,7,8,9,10,11,13,14,15,16,17,19,20,21,22,23,25,26,27,28,29,31,32,33,34,35,37,38,39,40,41,43,44,45,46,47,49,50,51,52,53,55,56,57,58,59,61,62,63,64,65)

#ECONOMIC

labels <- rownames(plot_stemconsult_exp1_ec.b_df)
labels <- c("q1:1",  "",  "q1:3",  "",  "q1:5",  "q1:9" , "q2:1",  "",  "q2:3",  "" , "q2:5",  "q2:9" , "" , "" , "q4:3" , "", "q4:5",  "q4:9",  "q7:1",  "","q7:3"  ,"q7:4" , "q7:5" , "q7:9",  "q18:1", "", "q18:3", "", "q18:5", "q18:9", "q21:1", "", "q21:3", "", "q21:5", "q21:9", "q22:1", "", "q22:3", "", "q22:5", "q22:9", "q5:1" , ""  ,"q5:3" , "q5:4"  ,""  ,"q5:9",  "q15:1" ,"", "q15:3", "", "", "q15:9" ,"q16:1", "" ,"q16:3" ,"", "q16:5", "q16:9", "q23:1", "", "q23:3", "", "q23:5", "q23:9")


#FULL - 1st and 2nd Dimension

plot_stemconsult_exp1_ec.b <- plot(ca(stemconsult_exp1_ec.b),dim=c(1,2), main = "Stemconsult (A) Economic Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp1_ec.b_df <- as.data.frame(plot_stemconsult_exp1_ec.b$cols)
ggplot(plot_stemconsult_exp1_ec.b_df) +
  geom_text_repel(size = 3.5,
                  aes(Dim1, Dim2, label = labels),min.segment.length = unit(1.5, "lines")) +
  geom_point(aes(Dim1, Dim2), color = 'black', size=2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (A) Economic Scale \n") +
  scale_x_continuous(name="Dimension 1 (13.8%)",limits=c(-1.5, 0.8))+
  scale_y_continuous(name="Dimension 2 (12.8%)",limits=c(-0.3, 2.5))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))

# 1st and 2nd substantial

plot_stemconsult_exp1_ec.b.sub <- plot(ca(stemconsult_exp1_ec.b,subsetcol=stemconsult_exp1_ec.b.sub,subsetrow=stemconsult_exp1_ec.b.sub),dim=c(1,3), main = "Stemconsult (A) Economic Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp1_ec.b.sub_df <- as.data.frame(plot_stemconsult_exp1_ec.b.sub$cols)
ggplot(plot_stemconsult_exp1_ec.b.sub_df) +
  geom_point(aes(Dim1, Dim2), color = 'black', size=2) +
  geom_text_repel(
    aes(Dim1, Dim2, label = rownames(plot_stemconsult_exp1_ec.b.sub_df)),min.segment.length = unit(1, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (A) Economic Scale \n Neutral & No Opinion \n") +
  scale_x_continuous(name="Dimension 1 (17.6%)",limits=c(-.3, 0.7))+
  scale_y_continuous(name="Dimension 2 (10.7%)",limits=c(-.1, 0.1))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))

#FULL - 1st and 3nd Dimension

plot_stemconsult_exp1_ec_13.b <- plot(ca(stemconsult_exp1_ec.b),dim=c(1,3), main = "Stemconsult (A) Economic Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp1_ec_13.b_df <- as.data.frame(plot_stemconsult_exp1_ec_13.b$cols)
ggplot(plot_stemconsult_exp1_ec_13.b_df) +
  geom_point(aes(Dim1, Dim3), color = 'black', size=2) +
  geom_text_repel(size = 3.5,
                  aes(Dim1, Dim3, label = labels),min.segment.length = unit(1.5, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (A) Economic Scale \n") +
  scale_x_continuous(name="Dimension 1 (13.8%)",limits=c(-1.5, 0.5))+
  scale_y_continuous(name="Dimension 3 (8.5%)",limits=c(-1.2, 0.7))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))

#FULL - 2st and 3nd Dimension

plot_stemconsult_exp1_ec_23.b <- plot(ca(stemconsult_exp1_ec.b),dim=c(2,3), main = "Stemconsult (A) Economic Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp1_ec_23.b_df <- as.data.frame(plot_stemconsult_exp1_ec_23.b$cols)
ggplot(plot_stemconsult_exp1_ec_23.b_df) +
  geom_point(aes(Dim2, Dim3), color = 'black', size=2) +
  geom_text_repel(size = 3.5,
                  aes(Dim2, Dim3, label = rownames(plot_stemconsult_exp1_ec_23.b_df)),min.segment.length = unit(1.5, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (A) Economic Scale \n") +
  scale_x_continuous(name="Dimension 2 (12.8%)",limits=c(-.2, 2.5))+
  scale_y_continuous(name="Dimension 3 (8.5%)",limits=c(-1.2, 0.6))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))

#NO OPINION & NEUTRAL
plot_stemconsult_exp1_ec.b.NA_N <- plot(ca(stemconsult_exp1_ec.b,subsetcol=stemconsult_exp1_ec.b.NA_N,subsetrow=stemconsult_exp1_ec.b.NA_N),dim=c(1,2), main = "Stemconsult (A) Economic Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp1_ec.b.NA_N_df <- as.data.frame(plot_stemconsult_exp1_ec.b.NA_N$cols)
ggplot(plot_stemconsult_exp1_ec.b.NA_N_df) +
  geom_point(aes(Dim1, Dim2), color = 'black', size=2) +
  geom_text_repel(
    aes(Dim1, Dim2, label = rownames(plot_stemconsult_exp1_ec.b.NA_N_df)),min.segment.length = unit(1, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (A) Economic Scale \n Neutral & No Opinion \n") +
  scale_x_continuous(name="Dimension 1 (37.5%)",limits=c(-.2, 2.5))+
  scale_y_continuous(name="Dimension 2 (8.6%)",limits=c(-.1, 0.5))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))





#SOCIAL

labels <- rownames(plot_stemconsult_exp1_so.b_df)
labels <- c("q9:1",  "q9:2",  "" , "",  "q9:5" , "q9:9" , "q10:1", "q10:2" ,"" ,"" ,"q10:5" ,"q10:9", "q13:1","q13:2", "", "" ,"q13:5", "q13:9", "q20:1", "", "", "q20:4" ,"q20:5" ,"q20:9", "q3:1" , "q3:2" , ""  ,""  ,"q3:5" , "q3:9"  ,"q6:1" , "q6:2" , "" , "",  "q6:5"  ,"q6:9" , "q8:1" , "q8:2" , "" , "" , "q8:5" , "q8:9" , "q11:1","q11:2", "q11:3" ,"q11:4", "q11:5", "q11:9" ,"q12:1" ,"q12:2", "", "" ,"q12:5", "q12:9", "q14:1", "q14:2", "q14:3", "q14:4", "q14:5", "q14:9", "q17:1", "q17:2" ,"" ,"", "q17:5", "q17:9", "q19:1" ,"q19:2" ,"" ,"", "q19:5", "q19:9", "q25:1" ,"q25:2" ,"" ,"" ,"q25:5", "q25:9")

plot_stemconsult_exp1_so.b <- plot(ca(stemconsult_exp1_so.b),dim=c(1,2), main = "Stemconsult (A) Social Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp1_so.b_df <- as.data.frame(plot_stemconsult_exp1_so.b$cols)
ggplot(plot_stemconsult_exp1_so.b_df) +
  geom_point(aes(Dim1, Dim2), color = 'black', size=2) +
  geom_text_repel(size = 3.5,
                  aes(Dim1, Dim2, label = rownames(plot_stemconsult_exp1_so.b_df)),min.segment.length = unit(1.5, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (A) Social Scale \n") +
  scale_x_continuous(name="Dimension 1 (23.7%)",limits=c(-1.3, 1.0))+
  scale_y_continuous(name="Dimension 2 (16.3%)",limits=c(-0.3, 4.3))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))

#FULL - 1st and 3nd Dimension

plot_stemconsult_exp1_so_13.b <- plot(ca(stemconsult_exp1_so.b),dim=c(1,3), main = "Stemconsult (A) Social Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp1_so_13.b_df <- as.data.frame(plot_stemconsult_exp1_so_13.b$cols)
ggplot(plot_stemconsult_exp1_so_13.b_df) +
  geom_point(aes(Dim1, Dim3), color = 'black', size=2) +
  geom_text_repel(size = 3.5,
                  aes(Dim1, Dim3, label = labels),min.segment.length = unit(1.5, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (A) Social Scale \n") +
  scale_x_continuous(name="Dimension 1 (23.7%)",limits=c(-1.5, 0.8))+
  scale_y_continuous(name="Dimension 3 (12.2%)",limits=c(-1.5, 0.4))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))

#FULL - 2st and 3nd Dimension

plot_stemconsult_exp1_so_23.b <- plot(ca(stemconsult_exp1_so.b),dim=c(2,3), main = "Stemconsult (A) Social Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp1_so_23.b_df <- as.data.frame(plot_stemconsult_exp1_so_23.b$cols)
ggplot(plot_stemconsult_exp1_so_23.b_df) +
  geom_point(aes(Dim2, Dim3), color = 'black', size=2) +
  geom_text_repel(size = 3.5,
                  aes(Dim2, Dim3, label = rownames(plot_stemconsult_exp1_so_23.b_df)),min.segment.length = unit(1.5, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (A) Social Scale \n") +
  scale_x_continuous(name="Dimension 2 (16.3%)",limits=c(-.2, 4.3))+
  scale_y_continuous(name="Dimension 3 (12.2%)",limits=c(-1.5, 0.6))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))

#NO OPINION & NEUTRAL
plot_stemconsult_exp1_so.b.NA_N <- plot(ca(stemconsult_exp1_so.b,subsetcol=stemconsult_exp1_so.b.NA_N,subsetrow=stemconsult_exp1_so.b.NA_N),dim=c(1,2), main = "Stemconsult (A) Social Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp1_so.b.NA_N_df <- as.data.frame(plot_stemconsult_exp1_so.b.NA_N$cols)
ggplot(plot_stemconsult_exp1_so.b.NA_N_df) +
  geom_point(aes(Dim1, Dim2), color = 'black', size=2) +
  geom_text_repel(
    aes(Dim1, Dim2, label = rownames(plot_stemconsult_exp1_so.b.NA_N_df)),min.segment.length = unit(1, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (A) Social Scale \n Neutral & No Opinion \n") +
  scale_x_continuous(name="Dimension 1 (49.1%)",limits=c(-.2, 4.2))+
  scale_y_continuous(name="Dimension 2 (7.4%)",limits=c(-.05, 0.5))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))






# MCA - VERSION B

stemconsult_exp2_ec.b <- mjca(stemconsult_exp2_ec,lambda="Burt")$Burt
stemconsult_exp2_so.b <- mjca(stemconsult_exp2_so,lambda="Burt")$Burt

stemconsult_exp2_ec.b.NA_N <- c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66)
stemconsult_exp2_so.b.NA_N <- c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66)

#ECONOMIC

#FULL - 1st and 2nd Dimension

plot_stemconsult_exp2_ec.b <- plot(ca(stemconsult_exp2_ec.b),dim=c(1,2), main = "Stemconsult (B) Economic Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp2_ec.b_df <- as.data.frame(plot_stemconsult_exp2_ec.b$cols)
ggplot(plot_stemconsult_exp2_ec.b_df) +
  geom_point(aes(Dim1, Dim2), color = 'black', size=2) +
  geom_text_repel(size = 3.5,
                  aes(Dim1, Dim2, label = rownames(plot_stemconsult_exp2_ec.b_df)),min.segment.length = unit(1.5, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (B) Economic Scale \n") +
  scale_x_continuous(name="Dimension 1 (13.1%)",limits=c(-.5, 1.7))+
  scale_y_continuous(name="Dimension 2 (10.7%)",limits=c(-1.6, 0.5))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))

#FULL - 1st and 3nd Dimension

plot_stemconsult_exp2_ec_13.b <- plot(ca(stemconsult_exp2_ec.b),dim=c(1,3), main = "Stemconsult (B) Economic Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp2_ec_13.b_df <- as.data.frame(plot_stemconsult_exp2_ec_13.b$cols)
ggplot(plot_stemconsult_exp2_ec_13.b_df) +
  geom_point(aes(Dim1, Dim3), color = 'black', size=2) +
  geom_text_repel(size = 3.5,
                  aes(Dim1, Dim3, label = rownames(plot_stemconsult_exp2_ec_13.b_df)),min.segment.length = unit(1.5, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (B) Economic Scale \n") +
  scale_x_continuous(name="Dimension 1 (13.1%)",limits=c(-0.4, 1.5))+
  scale_y_continuous(name="Dimension 3 (7.4%)",limits=c(-0.8, 0.6))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))

#FULL - 2st and 3nd Dimension

plot_stemconsult_exp2_ec_23.b <- plot(ca(stemconsult_exp2_ec.b),dim=c(2,3), main = "Stemconsult (B) Economic Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp2_ec_23.b_df <- as.data.frame(plot_stemconsult_exp2_ec_23.b$cols)
ggplot(plot_stemconsult_exp2_ec_23.b_df) +
  geom_point(aes(Dim2, Dim3), color = 'black', size=2) +
  geom_text_repel(size = 3.5,
                  aes(Dim2, Dim3, label = rownames(plot_stemconsult_exp2_ec_23.b_df)),min.segment.length = unit(1.5, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (B) Economic Scale \n") +
  scale_x_continuous(name="Dimension 2 (10.7%)",limits=c(-1.6, 0.5))+
  scale_y_continuous(name="Dimension 3 (7.4%)",limits=c(-0.8, 0.6))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))

#NO OPINION & NEUTRAL
plot_stemconsult_exp2_ec.b.NA_N <- plot(ca(stemconsult_exp2_ec.b,subsetcol=stemconsult_exp2_ec.b.NA_N,subsetrow=stemconsult_exp2_ec.b.NA_N),dim=c(1,2), main = "Stemconsult (B) Economic Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp2_ec.b.NA_N_df <- as.data.frame(plot_stemconsult_exp2_ec.b.NA_N$cols)
ggplot(plot_stemconsult_exp2_ec.b.NA_N_df) +
  geom_point(aes(Dim1, Dim2), color = 'black', size=2) +
  geom_text_repel(
    aes(Dim1, Dim2, label = rownames(plot_stemconsult_exp2_ec.b.NA_N_df)),min.segment.length = unit(1, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (B) Economic Scale \n Neutral & No Opinion \n") +
  scale_x_continuous(name="Dimension 1 (29.1%)",limits=c(-.2, 2.2))+
  scale_y_continuous(name="Dimension 2 (10.1%)",limits=c(-.2, 0.8))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))


#SOCIAL

plot_stemconsult_exp2_so.b <- plot(ca(stemconsult_exp2_so.b),dim=c(1,2), main = "Stemconsult (B) Social Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp2_so.b_df <- as.data.frame(plot_stemconsult_exp2_so.b$cols)
ggplot(plot_stemconsult_exp2_so.b_df) +
  geom_point(aes(Dim1, Dim2), color = 'black', size=2) +
  geom_text_repel(size = 3.5,
                  aes(Dim1, Dim2, label = rownames(plot_stemconsult_exp2_so.b_df)),min.segment.length = unit(1.5, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (B) Social Scale \n") +
  scale_x_continuous(name="Dimension 1 (21.6%)",limits=c(-1.1, 0.7))+
  scale_y_continuous(name="Dimension 2 (13.3%)",limits=c(-0.4, 1.2))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))

#FULL - 1st and 3nd Dimension

plot_stemconsult_exp2_so_13.b <- plot(ca(stemconsult_exp2_so.b),dim=c(1,3), main = "Stemconsult (B) Social Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp2_so_13.b_df <- as.data.frame(plot_stemconsult_exp2_so_13.b$cols)
ggplot(plot_stemconsult_exp2_so_13.b_df) +
  geom_point(aes(Dim1, Dim3), color = 'black', size=2) +
  geom_text_repel(size = 3.5,
                  aes(Dim1, Dim3, label = rownames(plot_stemconsult_exp2_so_13.b_df)),min.segment.length = unit(1.5, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (B) Social Scale \n") +
  scale_x_continuous(name="Dimension 1 (21.6%)",limits=c(-1.1, 0.9))+
  scale_y_continuous(name="Dimension 3 (10.2%)",limits=c(-.2, 3.6))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))

#FULL - 2st and 3nd Dimension

plot_stemconsult_exp2_so_23.b <- plot(ca(stemconsult_exp2_so.b),dim=c(2,3), main = "Stemconsult (B) Social Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp2_so_23.b_df <- as.data.frame(plot_stemconsult_exp2_so_23.b$cols)
ggplot(plot_stemconsult_exp2_so_23.b_df) +
  geom_point(aes(Dim2, Dim3), color = 'black', size=2) +
  geom_text_repel(size = 3.5,
                  aes(Dim2, Dim3, label = rownames(plot_stemconsult_exp2_so_23.b_df)),min.segment.length = unit(1.5, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (B) Social Scale \n") +
  scale_x_continuous(name="Dimension 2 (13.3%)",limits=c(-.4, 1.4))+
  scale_y_continuous(name="Dimension 3 (10.2%)",limits=c(-.2, 3.6))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))

#NO OPINION & NEUTRAL
plot_stemconsult_exp2_so.b.NA_N <- plot(ca(stemconsult_exp2_so.b,subsetcol=stemconsult_exp2_so.b.NA_N,subsetrow=stemconsult_exp2_so.b.NA_N),dim=c(1,2), main = "Stemconsult (B) Social Scale", what=c("none","all"),col=c("#000000","#000000"),col.lab=c("#000000","#000000"),pch=c(16,2,20,2))
plot_stemconsult_exp2_so.b.NA_N_df <- as.data.frame(plot_stemconsult_exp2_so.b.NA_N$cols)
ggplot(plot_stemconsult_exp2_so.b.NA_N_df) +
  geom_point(aes(Dim1, Dim2), color = 'black', size=2) +
  geom_text_repel(
    aes(Dim1, Dim2, label = rownames(plot_stemconsult_exp2_so.b.NA_N_df)),min.segment.length = unit(1, "lines")) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Stemconsult (B) Social Scale \n Neutral & No Opinion \n") +
  scale_x_continuous(name="Dimension 1 (34.8%)",limits=c(-.1, 3.6))+
  scale_y_continuous(name="Dimension 2 (9.5%)",limits=c(-.05, 0.5))+
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))







