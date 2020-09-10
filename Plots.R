library(ggplot2)
library(grid)
library(plyr)

#################################################################### Gutman errors ############################################################

netherlands <- read.csv("netherlands.csv", row.names=1)

netherlands$ExpCondition <- as.factor(netherlands$ExpCondition)
mu <- ddply(netherlands, "ExpCondition", summarise, grp.mean=mean(gutmann))

x <- c(95,95)
y <- c(0.035, 0.033)
z <- c("Version A: bar(x)=20.90 ; sd=16.99","Version B: bar(x)=22.66 ; sd=18.42")
text <- as.data.frame(cbind(x,y))
text <- cbind(text,z)
text$z <- as.character(text$z)

tikz('appendix_density.tex', width=6, height=6, sanitize = TRUE)

ggplot() +
  geom_density(data=netherlands, aes(x=gutmann, color=ExpCondition),show.legend=FALSE)+
  stat_density(data=netherlands, aes(x=gutmann, color=ExpCondition),geom="line",position="identity")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=ExpCondition), linetype="dashed",show.legend=FALSE)+
  scale_x_continuous(limits = c(0,166), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,0.04), expand = c(0, 0)) +
  scale_color_grey(name="Experimental Condition",
                   breaks=c("0", "1"),
                   labels=c("Version A", "Version B")) +
  xlab("# of Gutmann Errors")+
  ylab("Density")+
  ggtitle("# of Gutmann Errors per Experimental Condition")+
  theme_classic() +
  theme(legend.position="bottom")+
  geom_text(data=text, x = 92, y = 0.0351, label = "Version A:")+
  geom_text(data=text, x = 92, y = 0.0331, label = "Version B:")+
  geom_text(data=text, x = 100, y = 0.0351, label = "bar(x)", parse=T)+
  geom_text(data=text, x = 100, y = 0.0331, label = "bar(x)", parse=T)+
  geom_text(data=text, x = 114, y = 0.0351, label = "= 20.90 ; sd = 16.99")+
  geom_text(data=text, x = 114, y = 0.0331, label = "= 22.66 ; sd = 18.42")

dev.off()

rm(list=ls())

#################################################################### Marginal Effects ####################################################################

hybrid_margins <- read.csv("hybrid_margins.csv", row.names=1)
hybrid_margins$factor <- as.character(hybrid_margins$factor)
hybrid_margins$question <- as.character(hybrid_margins$question)
hybrid_margins$party <- as.character(hybrid_margins$party)
hybrid_margins <- hybrid_margins[ which(hybrid_margins$question!='Positive'),]

hybrid_margins$question<- as.factor(hybrid_margins$question)
hybrid_margins$party <- as.factor(hybrid_margins$party)
hybrid_margins$party <- factor(hybrid_margins$party,levels(hybrid_margins$party)[c(7,1,2,3,4,5,6,8,9,10,11,12,13,14)])
hybrid_margins$party <- factor(hybrid_margins$party, levels=rev(levels(hybrid_margins$party)))
hybrid_margins$question <- factor(hybrid_margins$question,levels(hybrid_margins$question)[c(1,8,12,2,3,4,5,6,7,9,10,11)])

levels(hybrid_margins$party)[levels(hybrid_margins$party)=="Plus"] <- "50Plus"

ggplot(hybrid_margins, aes(x=AME, y=party)) +
  geom_errorbarh(height=.2, size=0.3, colour="black", aes(y=party, xmin=lower, xmax=upper)) +
  geom_point(shape=20, size=1)+
  geom_vline(xintercept = 0, linetype="dotted")+
  scale_x_continuous(limits = c(-6.83,6.86),breaks = c(-7, 0, 7))+
  facet_wrap(~question, nrow = 1, scales = "free_x")+
  ggtitle("Marginal Effect of the positive-negative condition (Hybrid Algorithm)")+
  xlab("Marginal Effect")+
  ylab("Party")+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())

rm(list=ls())

#################################################################### Conditional Effects - Gutmann ####################################################################

interplot_gutmann_hybrid.csv <- read.csv("interplot_gutmann_hybrid.csv", stringsAsFactors=FALSE)
interplot_gutmann_hybrid.csv$X <- NULL
interplot_gutmann_hybrid.csv <- interplot_gutmann_hybrid.csv[ which(interplot_gutmann_hybrid.csv$value!='Positive'),]
interplot_gutmann_hybrid.csv$value <- as.factor(interplot_gutmann_hybrid.csv$value)
interplot_gutmann_hybrid.csv$value <- factor(interplot_gutmann_hybrid.csv$value,levels(interplot_gutmann_hybrid.csv$value)[c(1,8,12,2,3,4,5,6,7,9,10,11)])
interplot_gutmann_hybrid.csv$party <- as.factor(interplot_gutmann_hybrid.csv$party)

hybrid_list <- split(interplot_gutmann_hybrid.csv, interplot_gutmann_hybrid.csv$party)
hybrid_cda <- hybrid_list$CDA
hybrid_cu <- hybrid_list$CU
hybrid_d66 <- hybrid_list$D66
hybrid_denk <- hybrid_list$DENK
hybrid_fvd <- hybrid_list$FvD
hybrid_gl <- hybrid_list$GL
hybrid_plus <- hybrid_list$Plus
hybrid_pvda <- hybrid_list$PvdA
hybrid_pvdd <- hybrid_list$PvdD
hybrid_pvv <- hybrid_list$PVV
hybrid_sgp <- hybrid_list$SGP
hybrid_sp <- hybrid_list$SP
hybrid_vnl <- hybrid_list$VNL
hybrid_vvd <- hybrid_list$VVD

levels(hybrid_plus$party)[levels(hybrid_plus$party)=="Plus"] <- "50Plus"

hybrid_plus <- ggplot() +
  geom_line(data = hybrid_plus, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_plus, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-35, 0, 35), lim=c(-34,32.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("50Plus")+
  ggtitle("Conditional Effects of the # of Gutmann Errors (Hybrid Algorithm)")+
  theme_classic()+
  theme(plot.title = element_text(size=14), strip.background=element_blank(), axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

hybrid_cda <- ggplot() +
  geom_line(data = hybrid_cda, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_cda, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-35, 0, 35), lim=c(-34,32.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("CDA")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_cu <- ggplot() +
  geom_line(data = hybrid_cu, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_cu, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-35, 0, 35), lim=c(-34,32.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("CU")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_d66 <- ggplot() +
  geom_line(data = hybrid_d66, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_d66, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-35, 0, 35), lim=c(-34,32.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("D66")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_denk <- ggplot() +
  geom_line(data = hybrid_denk, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_denk, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-35, 0, 35), lim=c(-34,32.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("DENK")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_fvd <- ggplot() +
  geom_line(data = hybrid_fvd, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_fvd, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-35, 0, 35), lim=c(-34,32.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("FvD")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_gl <- ggplot() +
  geom_line(data = hybrid_gl, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_gl, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-35, 0, 35), lim=c(-34,32.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("GL")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvda <- ggplot() +
  geom_line(data = hybrid_pvda, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_pvda, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-35, 0, 35), lim=c(-34,32.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PvdA")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvdd <- ggplot() +
  geom_line(data = hybrid_pvdd, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_pvdd, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-35, 0, 35), lim=c(-34,32.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PvdD")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvv <- ggplot() +
  geom_line(data = hybrid_pvv, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_pvv, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-35, 0, 35), lim=c(-34,32.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PVV")+
  theme_classic()+
  theme(axis.line.x= element_blank(), axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_sgp <- ggplot() +
  geom_line(data = hybrid_sgp, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_sgp, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-35, 0, 35), lim=c(-34,32.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("SGP")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_sp <- ggplot() +
  geom_line(data = hybrid_sp, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_sp, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-35, 0, 35), lim=c(-34,32.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("SP")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_vnl <- ggplot() +
  geom_line(data = hybrid_vnl, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_vnl, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-35, 0, 35), lim=c(-34,32.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("VNL")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_vvd <- ggplot() +
  geom_line(data = hybrid_vvd, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_vvd, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-35, 0, 35), lim=c(-34,32.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("VVD")+
  xlab("# of Gutmann Errors")+
  theme_classic()+
  theme(axis.title.x = element_text(size=10), axis.title.y = element_text(size=8, angle=0, vjust=0.5), strip.text=element_blank())

grid.newpage()
grid.draw(rbind(ggplotGrob(hybrid_plus), ggplotGrob(hybrid_cda), ggplotGrob(hybrid_cu), ggplotGrob(hybrid_d66), ggplotGrob(hybrid_denk), ggplotGrob(hybrid_fvd), ggplotGrob(hybrid_gl), ggplotGrob(hybrid_pvda), ggplotGrob(hybrid_pvdd), ggplotGrob(hybrid_pvv), ggplotGrob(hybrid_sgp), ggplotGrob(hybrid_sp), ggplotGrob(hybrid_vnl), ggplotGrob(hybrid_vvd), size = "last"))

rm(list=ls())

#################################################################### Conditional Effects - Political Interest ####################################################################

interplot_hybrid_interestInPolitics <- read.csv("interplot_interestInPolitics_hybrid.csv", stringsAsFactors=FALSE)
interplot_hybrid_interestInPolitics$X <- NULL
interplot_hybrid_interestInPolitics <- interplot_hybrid_interestInPolitics[ which(interplot_hybrid_interestInPolitics$value!='Positive'),]
interplot_hybrid_interestInPolitics$value <- as.factor(interplot_hybrid_interestInPolitics$value)
interplot_hybrid_interestInPolitics$value <- factor(interplot_hybrid_interestInPolitics$value,levels(interplot_hybrid_interestInPolitics$value)[c(1,8,12,2,3,4,5,6,7,9,10,11)])
interplot_hybrid_interestInPolitics$party <- as.factor(interplot_hybrid_interestInPolitics$party)

hybrid_list <- split(interplot_hybrid_interestInPolitics, interplot_hybrid_interestInPolitics$party)
hybrid_cda <- hybrid_list$CDA
hybrid_cu <- hybrid_list$CU
hybrid_d66 <- hybrid_list$D66
hybrid_denk <- hybrid_list$DENK
hybrid_fvd <- hybrid_list$FvD
hybrid_gl <- hybrid_list$GL
hybrid_plus <- hybrid_list$Plus
hybrid_pvda <- hybrid_list$PvdA
hybrid_pvdd <- hybrid_list$PvdD
hybrid_pvv <- hybrid_list$PVV
hybrid_sgp <- hybrid_list$SGP
hybrid_sp <- hybrid_list$SP
hybrid_vnl <- hybrid_list$VNL
hybrid_vvd <- hybrid_list$VVD

levels(hybrid_plus$party)[levels(hybrid_plus$party)=="Plus"] <- "50Plus"

hybrid_plus <- ggplot() +
  geom_point(data = hybrid_plus, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_plus, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.4)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-21.2,26))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("50Plus")+
  ggtitle("Conditional Effects of Political Interest (Hybrid Algorithm)")+
  theme_classic()+
  theme(plot.title = element_text(size=14), strip.background=element_blank(), axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

hybrid_cda <- ggplot() +
  geom_point(data = hybrid_cda, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_cda, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.4)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-21.2,26))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("CDA")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_cu <- ggplot() +
  geom_point(data = hybrid_cu, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_cu, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.4)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-21.2,26))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("CU")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_d66 <- ggplot() +
  geom_point(data = hybrid_d66, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_d66, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.4)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-21.2,26))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("D66")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_denk <- ggplot() +
  geom_point(data = hybrid_denk, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_denk, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.4)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-21.2,26))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("DENK")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_fvd <- ggplot() +
  geom_point(data = hybrid_fvd, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_fvd, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.4)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-21.2,26))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("FvD")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_gl <- ggplot() +
  geom_point(data = hybrid_gl, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_gl, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.4)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-21.2,26))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("GL")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvda <- ggplot() +
  geom_point(data = hybrid_pvda, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_pvda, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.4)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-21.2,26))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PvdA")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvdd <- ggplot() +
  geom_point(data = hybrid_pvdd, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_pvdd, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.4)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-21.2,26))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PvdD")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvv <- ggplot() +
  geom_point(data = hybrid_pvv, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_pvv, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.4)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-21.2,26))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PVV")+
  theme_classic()+
  theme(axis.line.x= element_blank(), axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_sgp <- ggplot() +
  geom_point(data = hybrid_sgp, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_sgp, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.4)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-21.2,26))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("SGP")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_sp <- ggplot() +
  geom_point(data = hybrid_sp, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_sp, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.4)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-21.2,26))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("SP")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_vnl <- ggplot() +
  geom_point(data = hybrid_vnl, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_vnl, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.4)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-21.2,26))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("VNL")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_vvd <- ggplot() +
  geom_point(data = hybrid_vvd, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_vvd, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.4)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-21.2,26))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("VVD")+
  xlab("Political Interest")+
  theme_classic()+
  theme(axis.title.x = element_text(size=10), axis.title.y = element_text(size=8, angle=0, vjust=0.5), strip.text=element_blank())

grid.newpage()
grid.draw(rbind(ggplotGrob(hybrid_plus), ggplotGrob(hybrid_cda), ggplotGrob(hybrid_cu), ggplotGrob(hybrid_d66), ggplotGrob(hybrid_denk), ggplotGrob(hybrid_fvd), ggplotGrob(hybrid_gl), ggplotGrob(hybrid_pvda), ggplotGrob(hybrid_pvdd), ggplotGrob(hybrid_pvv), ggplotGrob(hybrid_sgp), ggplotGrob(hybrid_sp), ggplotGrob(hybrid_vnl), ggplotGrob(hybrid_vvd), size = "last"))


#################################################################### Conditional Effect - Age ####################################################################

interplot_hybrid_age <- read.csv("interplot_age_hybrid.csv", stringsAsFactors=FALSE)
interplot_hybrid_age$X <- NULL
interplot_hybrid_age <- interplot_hybrid_age[ which(interplot_hybrid_age$value!='Positive'),]
interplot_hybrid_age$value <- as.factor(interplot_hybrid_age$value)
interplot_hybrid_age$value <- factor(interplot_hybrid_age$value,levels(interplot_hybrid_age$value)[c(1,8,12,2,3,4,5,6,7,9,10,11)])
interplot_hybrid_age$party <- as.factor(interplot_hybrid_age$party)

hybrid_list <- split(interplot_hybrid_age, interplot_hybrid_age$party)
hybrid_cda <- hybrid_list$CDA
hybrid_cu <- hybrid_list$CU
hybrid_d66 <- hybrid_list$D66
hybrid_denk <- hybrid_list$DENK
hybrid_fvd <- hybrid_list$FvD
hybrid_gl <- hybrid_list$GL
hybrid_plus <- hybrid_list$Plus
hybrid_pvda <- hybrid_list$PvdA
hybrid_pvdd <- hybrid_list$PvdD
hybrid_pvv <- hybrid_list$PVV
hybrid_sgp <- hybrid_list$SGP
hybrid_sp <- hybrid_list$SP
hybrid_vnl <- hybrid_list$VNL
hybrid_vvd <- hybrid_list$VVD

levels(hybrid_plus$party)[levels(hybrid_plus$party)=="Plus"] <- "50Plus"

hybrid_plus <- ggplot() +
  geom_line(data = hybrid_plus, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_plus, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-24.1,29.1))+
  scale_x_continuous(breaks=c(20, 50, 80), lim=c(16,79))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("50Plus")+
  ggtitle("Conditional Effects of Age (Hybrid Algorithm)")+
  theme_classic()+
  theme(plot.title = element_text(size=14), strip.background=element_blank(), axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

hybrid_cda <- ggplot() +
  geom_line(data = hybrid_cda, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_cda, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-24.1,29.1))+
  scale_x_continuous(breaks=c(20, 50, 80), lim=c(16,79))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("CDA")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_cu <- ggplot() +
  geom_line(data = hybrid_cu, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_cu, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-24.1,29.1))+
  scale_x_continuous(breaks=c(20, 50, 80), lim=c(16,79))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("CU")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_d66 <- ggplot() +
  geom_line(data = hybrid_d66, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_d66, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-24.1,29.1))+
  scale_x_continuous(breaks=c(20, 50, 80), lim=c(16,79))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("D66")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_denk <- ggplot() +
  geom_line(data = hybrid_denk, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_denk, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-24.1,29.1))+
  scale_x_continuous(breaks=c(20, 50, 80), lim=c(16,79))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("DENK")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_fvd <- ggplot() +
  geom_line(data = hybrid_fvd, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_fvd, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-24.1,29.1))+
  scale_x_continuous(breaks=c(20, 50, 80), lim=c(16,79))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("FvD")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_gl <- ggplot() +
  geom_line(data = hybrid_gl, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_gl, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-24.1,29.1))+
  scale_x_continuous(breaks=c(20, 50, 80), lim=c(16,79))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("GL")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvda <- ggplot() +
  geom_line(data = hybrid_pvda, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_pvda, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-24.1,29.1))+
  scale_x_continuous(breaks=c(20, 50, 80), lim=c(16,79))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PvdA")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvdd <- ggplot() +
  geom_line(data = hybrid_pvdd, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_pvdd, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-24.1,29.1))+
  scale_x_continuous(breaks=c(20, 50, 80), lim=c(16,79))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PvdD")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvv <- ggplot() +
  geom_line(data = hybrid_pvv, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_pvv, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-24.1,29.1))+
  scale_x_continuous(breaks=c(20, 50, 80), lim=c(16,79))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PVV")+
  theme_classic()+
  theme(axis.line.x= element_blank(), axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_sgp <- ggplot() +
  geom_line(data = hybrid_sgp, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_sgp, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-24.1,29.1))+
  scale_x_continuous(breaks=c(20, 50, 80), lim=c(16,79))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("SGP")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_sp <- ggplot() +
  geom_line(data = hybrid_sp, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_sp, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-24.1,29.1))+
  scale_x_continuous(breaks=c(20, 50, 80), lim=c(16,79))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("SP")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_vnl <- ggplot() +
  geom_line(data = hybrid_vnl, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_vnl, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-24.1,29.1))+
  scale_x_continuous(breaks=c(20, 50, 80), lim=c(16,79))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("VNL")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_vvd <- ggplot() +
  geom_line(data = hybrid_vvd, aes(x=fake, y=coef1, group=party)) +
  geom_ribbon(data = hybrid_vvd, aes(x=fake,ymin=ub, ymax=lb), alpha=0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-24.1,29.1))+
  scale_x_continuous(breaks=c(20, 50, 80), lim=c(16,79))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("VVD")+
  xlab("Age")+
  theme_classic()+
  theme(axis.title.x = element_text(size=10), axis.title.y = element_text(size=8, angle=0, vjust=0.5), strip.text=element_blank())

grid.newpage()
grid.draw(rbind(ggplotGrob(hybrid_plus), ggplotGrob(hybrid_cda), ggplotGrob(hybrid_cu), ggplotGrob(hybrid_d66), ggplotGrob(hybrid_denk), ggplotGrob(hybrid_fvd), ggplotGrob(hybrid_gl), ggplotGrob(hybrid_pvda), ggplotGrob(hybrid_pvdd), ggplotGrob(hybrid_pvv), ggplotGrob(hybrid_sgp), ggplotGrob(hybrid_sp), ggplotGrob(hybrid_vnl), ggplotGrob(hybrid_vvd), size = "last"))

rm(list=ls())

#################################################################### Conditional Effects - Sex ####################################################################

interplot_hybrid_sex <- read.csv("interplot_sex_hybrid.csv", stringsAsFactors=FALSE)
interplot_hybrid_sex$X <- NULL
interplot_hybrid_sex <- interplot_hybrid_sex[ which(interplot_hybrid_sex$value!='Positive'),]
interplot_hybrid_sex$value <- as.factor(interplot_hybrid_sex$value)
interplot_hybrid_sex$value <- factor(interplot_hybrid_sex$value,levels(interplot_hybrid_sex$value)[c(1,8,12,2,3,4,5,6,7,9,10,11)])
interplot_hybrid_sex$party <- as.factor(interplot_hybrid_sex$party)

hybrid_list <- split(interplot_hybrid_sex, interplot_hybrid_sex$party)
hybrid_cda <- hybrid_list$CDA
hybrid_cu <- hybrid_list$CU
hybrid_d66 <- hybrid_list$D66
hybrid_denk <- hybrid_list$DENK
hybrid_fvd <- hybrid_list$FvD
hybrid_gl <- hybrid_list$GL
hybrid_plus <- hybrid_list$Plus
hybrid_pvda <- hybrid_list$PvdA
hybrid_pvdd <- hybrid_list$PvdD
hybrid_pvv <- hybrid_list$PVV
hybrid_sgp <- hybrid_list$SGP
hybrid_sp <- hybrid_list$SP
hybrid_vnl <- hybrid_list$VNL
hybrid_vvd <- hybrid_list$VVD

levels(hybrid_plus$party)[levels(hybrid_plus$party)=="Plus"] <- "50Plus"

hybrid_plus <- ggplot() +
  geom_point(data = hybrid_plus, aes(x=fake, y=coef1, group=party)) +
  geom_errorbar(data = hybrid_plus, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-26.2,28.5))+
  scale_x_discrete(breaks=c(0,1), lim=c(0,1),labels=c("0" = "F", "1" = "M"))+
   geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("50Plus")+
  ggtitle("Conditional Effects of Sex (Hybrid Algorithm)")+
  theme_classic()+
  theme(plot.title = element_text(size=14), strip.background=element_blank(), axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

hybrid_cda <- ggplot() +
  geom_point(data = hybrid_cda, aes(x=fake, y=coef1, group=party)) +
  geom_errorbar(data = hybrid_cda, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-26.2,28.5))+
  scale_x_discrete(breaks=c(0,1), lim=c(0,1),labels=c("0" = "F", "1" = "M"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("CDA")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_cu <- ggplot() +
  geom_point(data = hybrid_cu, aes(x=fake, y=coef1, group=party)) +
  geom_errorbar(data = hybrid_cu, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-26.2,28.5))+
  scale_x_discrete(breaks=c(0,1), lim=c(0,1),labels=c("0" = "F", "1" = "M"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("CU")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_d66 <- ggplot() +
  geom_point(data = hybrid_d66, aes(x=fake, y=coef1, group=party)) +
  geom_errorbar(data = hybrid_d66, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-26.2,28.5))+
  scale_x_discrete(breaks=c(0,1), lim=c(0,1),labels=c("0" = "F", "1" = "M"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("D66")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_denk <- ggplot() +
  geom_point(data = hybrid_denk, aes(x=fake, y=coef1, group=party)) +
  geom_errorbar(data = hybrid_denk, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-26.2,28.5))+
  scale_x_discrete(breaks=c(0,1), lim=c(0,1),labels=c("0" = "F", "1" = "M"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("DENK")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_fvd <- ggplot() +
  geom_point(data = hybrid_fvd, aes(x=fake, y=coef1, group=party)) +
  geom_errorbar(data = hybrid_fvd, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-26.2,28.5))+
  scale_x_discrete(breaks=c(0,1), lim=c(0,1),labels=c("0" = "F", "1" = "M"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("FvD")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_gl <- ggplot() +
  geom_point(data = hybrid_gl, aes(x=fake, y=coef1, group=party)) +
  geom_errorbar(data = hybrid_gl, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-26.2,28.5))+
  scale_x_discrete(breaks=c(0,1), lim=c(0,1),labels=c("0" = "F", "1" = "M"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("GL")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvda <- ggplot() +
  geom_point(data = hybrid_pvda, aes(x=fake, y=coef1, group=party)) +
  geom_errorbar(data = hybrid_pvda, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-26.2,28.5))+
  scale_x_discrete(breaks=c(0,1), lim=c(0,1),labels=c("0" = "F", "1" = "M"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PvdA")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvdd <- ggplot() +
  geom_point(data = hybrid_pvdd, aes(x=fake, y=coef1, group=party)) +
  geom_errorbar(data = hybrid_pvdd, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-26.2,28.5))+
  scale_x_discrete(breaks=c(0,1), lim=c(0,1),labels=c("0" = "F", "1" = "M"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PvdD")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvv <- ggplot() +
  geom_point(data = hybrid_pvv, aes(x=fake, y=coef1, group=party)) +
  geom_errorbar(data = hybrid_pvv, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-26.2,28.5))+
  scale_x_discrete(breaks=c(0,1), lim=c(0,1),labels=c("0" = "F", "1" = "M"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PVV")+
  theme_classic()+
  theme(axis.line.x= element_blank(), axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_sgp <- ggplot() +
  geom_point(data = hybrid_sgp, aes(x=fake, y=coef1, group=party)) +
  geom_errorbar(data = hybrid_sgp, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-26.2,28.5))+
  scale_x_discrete(breaks=c(0,1), lim=c(0,1),labels=c("0" = "F", "1" = "M"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("SGP")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_sp <- ggplot() +
  geom_point(data = hybrid_sp, aes(x=fake, y=coef1, group=party)) +
  geom_errorbar(data = hybrid_sp, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-26.2,28.5))+
  scale_x_discrete(breaks=c(0,1), lim=c(0,1),labels=c("0" = "F", "1" = "M"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("SP")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_vnl <- ggplot() +
  geom_point(data = hybrid_vnl, aes(x=fake, y=coef1, group=party)) +
  geom_errorbar(data = hybrid_vnl, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-26.2,28.5))+
  scale_x_discrete(breaks=c(0,1), lim=c(0,1),labels=c("0" = "F", "1" = "M"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("VNL")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_vvd <- ggplot() +
  geom_point(data = hybrid_vvd, aes(x=fake, y=coef1, group=party)) +
  geom_errorbar(data = hybrid_vvd, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.2)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-26.2,28.5))+
  scale_x_discrete(breaks=c(0,1), lim=c(0,1),labels=c("0" = "F", "1" = "M"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("VVD")+
  xlab("Sex")+
  theme_classic()+
  theme(axis.title.x = element_text(size=10), axis.title.y = element_text(size=8, angle=0, vjust=0.5), strip.text=element_blank())

grid.newpage()
grid.draw(rbind(ggplotGrob(hybrid_plus), ggplotGrob(hybrid_cda), ggplotGrob(hybrid_cu), ggplotGrob(hybrid_d66), ggplotGrob(hybrid_denk), ggplotGrob(hybrid_fvd), ggplotGrob(hybrid_gl), ggplotGrob(hybrid_pvda), ggplotGrob(hybrid_pvdd), ggplotGrob(hybrid_pvv), ggplotGrob(hybrid_sgp), ggplotGrob(hybrid_sp), ggplotGrob(hybrid_vnl), ggplotGrob(hybrid_vvd), size = "last"))

rm(list=ls())

#################################################################### Conditional Effects - Education ####################################################################

interplot_hybrid_education <- read.csv("interplot_education_hybrid.csv", stringsAsFactors=FALSE)
interplot_hybrid_education$X <- NULL
interplot_hybrid_education <- interplot_hybrid_education[ which(interplot_hybrid_education$value!='Positive'),]
interplot_hybrid_education$value <- as.factor(interplot_hybrid_education$value)
interplot_hybrid_education$value <- factor(interplot_hybrid_education$value,levels(interplot_hybrid_education$value)[c(1,8,12,2,3,4,5,6,7,9,10,11)])
interplot_hybrid_education$party <- as.factor(interplot_hybrid_education$party)

hybrid_list <- split(interplot_hybrid_education, interplot_hybrid_education$party)
hybrid_cda <- hybrid_list$CDA
hybrid_cu <- hybrid_list$CU
hybrid_d66 <- hybrid_list$D66
hybrid_denk <- hybrid_list$DENK
hybrid_fvd <- hybrid_list$FvD
hybrid_gl <- hybrid_list$GL
hybrid_plus <- hybrid_list$Plus
hybrid_pvda <- hybrid_list$PvdA
hybrid_pvdd <- hybrid_list$PvdD
hybrid_pvv <- hybrid_list$PVV
hybrid_sgp <- hybrid_list$SGP
hybrid_sp <- hybrid_list$SP
hybrid_vnl <- hybrid_list$VNL
hybrid_vvd <- hybrid_list$VVD

levels(hybrid_plus$party)[levels(hybrid_plus$party)=="Plus"] <- "50Plus"

hybrid_plus <- ggplot() +
  geom_point(data = hybrid_plus, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_plus, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.5)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-20.9,24.7))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), lim=c(1,7))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("50Plus")+
  ggtitle("Conditional Effects of Education (Hybrid Algorithm)")+
  theme_classic()+
  theme(plot.title = element_text(size=14), strip.background=element_blank(), axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

hybrid_cda <- ggplot() +
  geom_point(data = hybrid_cda, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_cda, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.5)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-20.9,24.7))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), lim=c(1,7))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("CDA")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_cu <- ggplot() +
  geom_point(data = hybrid_cu, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_cu, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.5)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-20.9,24.7))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), lim=c(1,7))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("CU")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_d66 <- ggplot() +
  geom_point(data = hybrid_d66, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_d66, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.5)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-20.9,24.7))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), lim=c(1,7))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("D66")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_denk <- ggplot() +
  geom_point(data = hybrid_denk, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_denk, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.5)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-20.9,24.7))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), lim=c(1,7))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("DENK")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_fvd <- ggplot() +
  geom_point(data = hybrid_fvd, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_fvd, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.5)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-20.9,24.7))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), lim=c(1,7))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("FvD")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_gl <- ggplot() +
  geom_point(data = hybrid_gl, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_gl, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.5)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-20.9,24.7))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), lim=c(1,7))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("GL")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvda <- ggplot() +
  geom_point(data = hybrid_pvda, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_pvda, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.5)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-20.9,24.7))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), lim=c(1,7))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PvdA")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvdd <- ggplot() +
  geom_point(data = hybrid_pvdd, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_pvdd, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.5)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-20.9,24.7))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), lim=c(1,7))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PvdD")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_pvv <- ggplot() +
  geom_point(data = hybrid_pvv, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_pvv, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.5)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-20.9,24.7))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), lim=c(1,7))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("PVV")+
  theme_classic()+
  theme(axis.line.x= element_blank(), axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_sgp <- ggplot() +
  geom_point(data = hybrid_sgp, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_sgp, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.5)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-20.9,24.7))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), lim=c(1,7))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("SGP")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_sp <- ggplot() +
  geom_point(data = hybrid_sp, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_sp, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.5)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-20.9,24.7))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), lim=c(1,7))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("SP")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_vnl <- ggplot() +
  geom_point(data = hybrid_vnl, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_vnl, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.5)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-20.9,24.7))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), lim=c(1,7))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("VNL")+
  theme_classic()+
  theme(axis.line.x= element_blank(),axis.title.y = element_text(size=8,angle=0, vjust=0.5),axis.title.x= element_blank(), axis.text.x=element_blank(), strip.text=element_blank(), axis.ticks.x=element_blank())

hybrid_vvd <- ggplot() +
  geom_point(data = hybrid_vvd, aes(x=fake, y=coef1, group=party), size = 0.9) +
  geom_errorbar(data = hybrid_vvd, aes(x=fake,ymin=ub, ymax=lb), alpha=1, width = 0.5)+
  facet_wrap(~value, nrow=1)+
  scale_y_continuous(breaks=c(-20, 0, 20), lim=c(-20.9,24.7))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), lim=c(1,7))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("VVD")+
  xlab("Education")+
  theme_classic()+
  theme(axis.title.x = element_text(size=10), axis.title.y = element_text(size=8, angle=0, vjust=0.5), strip.text=element_blank())

grid.newpage()
grid.draw(rbind(ggplotGrob(hybrid_plus), ggplotGrob(hybrid_cda), ggplotGrob(hybrid_cu), ggplotGrob(hybrid_d66), ggplotGrob(hybrid_denk), ggplotGrob(hybrid_fvd), ggplotGrob(hybrid_gl), ggplotGrob(hybrid_pvda), ggplotGrob(hybrid_pvdd), ggplotGrob(hybrid_pvv), ggplotGrob(hybrid_sgp), ggplotGrob(hybrid_sp), ggplotGrob(hybrid_vnl), ggplotGrob(hybrid_vvd), size = "last"))


