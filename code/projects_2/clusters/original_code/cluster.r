# setwd("C:\\Users\\Miquel\\Desktop\\Papers Spine\\Spinal\\Data")
# 
# data <- read.csv("DATA_FILLED.csv",sep=",", na.strings=c("","NA"))
# data_long <- read.csv("data_long.csv",sep=",", na.strings=c("","NA"))



load('clusters/rr.Rdata.RData')

require(etm)
library(psych)
library(ggplot2)
library(MatchIt)
library(party)
require('survival'); require('GGally')
library(randomForestSRC)
library(survminer)
library(ggRandomForests)
library(risksetROC)
library(glmnet)
library(rms)
library(doParallel)
library(sjPlot)
library(Amelia)
library(caret)
library(ranger)
library(Hmisc)
library(DMwR)    
library(dendextend)
library(colorspace)
library(tidyr)
library("factoextra")
library("StatMatch")
library("ggsci")
library("NbClust")
library(compareGroups)
library(plotly)
# Sys.setenv("plotly_username"="miquelserra")
# Sys.setenv("plotly_api_key"="cnnMH5i0PAtxBDDLf1cs")
packageVersion('plotly')

options(scipen = 999)

### Setting unimportant to 0

data_comp <- data

data$COMP <- NULL
data$Site <- NULL
data$Surgeon <- NULL

##

nums <- sapply(data,is.numeric)
data_num <- as.data.frame(data[,nums])


## Baseline Groups & Optimal Clustering

data_cluster <- as.data.frame(data_num[,c(28:31,36,38,40,42,44:51)])

##

data_cluster <- scale(data_cluster[,3:14])
#data_cluster <- scale(data_cluster)

# setwd("C:\\Users\\Miquel\\Desktop\\Papers Spine\\2. Unsupervised Learning (Spine)\\Submission\\R&R\\Figures")

tiff("clusters/fig2.tiff", width = 15, height = 10, units = 'cm', res = 300, compression = 'lzw')

fviz_nbclust(data_cluster, kmeans, method = "gap_stat")

dev.off()

##

dd <- dist(data_cluster)

hc <- hclust(dd, method = "ward.D2")

par(mfrow=c(1,1))
plot(hc)




fviz_dend(hc, cex = 0.5)

tiff("clusters/fig1.tiff", width = 15, height = 15, units = 'cm', res = 300, compression = 'lzw')
fviz_dend(hc, cex = 0.5, horiz = TRUE, k = 3, k_colors = "jco",
          main = "",
          xlab = "", ylab = "", sub = "",type="phylogenic",
          repel = TRUE,   phylo_layout = "layout.gem")
dev.off()


data_num <- data.frame(data_num)
data_num$class <- as.factor(cutree(hc,k=3))

data_cluster <- data_num[,c(28:31,36,38,40,42,44:51)]

data_cluster$class <- as.factor(cutree(hc,k=3))

levels(data_cluster$class) <- c("YC","ORev","OPrim")

## Surgery Dendogram

data_cluster_surgery <- data_num[,c(28,69:73)]

data_cluster_surgery <- scale(data_cluster_surgery)

# setwd("C:\\Users\\Miquel\\Desktop\\Papers Spine\\2. Unsupervised Learning (Spine)\\Submission\\R&R\\Figures")

tiff("clusters/fig4.tiff", width = 15, height = 10, units = 'cm', res = 300, compression = 'lzw')
fviz_nbclust(data_cluster_surgery, kmeans, method = "gap_stat")
dev.off()


nb <- NbClust(data_cluster_surgery, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

par(mfrow=c(1,3))
fviz_nbclust(data_cluster_surgery, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Gap method - Types of surgery") + ylab
fviz_nbclust(data_cluster_surgery, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
set.seed(123)
fviz_nbclust(data_cluster_surgery, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

dd2 <- dist(data_cluster_surgery)

hc2 <- hclust(dd2, method = "ward.D")

par(mfrow=c(1,1))
plot(hc2)

fviz_dend(hc2, cex = 0.5)
# setwd("C:\\Users\\Miquel\\Desktop\\Papers Spine\\2. Unsupervised Learning (Spine)\\Submission\\R&R\\Figures")

tiff("fig3.tiff", width = 15, height = 10, units = 'cm', res = 300, compression = 'lzw')
fviz_dend(hc2, cex = 0.5, horiz = F, k = 4, k_colors = "jco",
          main = "",
          xlab = "", ylab = "", sub = "",type="phylogenic",
          repel = TRUE,   phylo_layout = "layout.gem")
dev.off()

data_num$class_s <- as.factor(cutree(hc2,k=4))

data_cluster_surgery <- data_num[,c(27:28,69:79)]

data_cluster_surgery$class_s <- as.factor(cutree(hc2,k=4))

## Rename Clusters
data_cluster$class
data_cluster_surgery$class_s 

## Create HeatMaps


## Groups

data_num$change_ODI <-(data_num$ODI_score_2years-data_num$ODI_score_Baseline)/data_num$ODI_score_Baseline
data_num$change_SRS_subtotal <-(data_num$SRS22_subtotal_score_2years-data_num$SRS22_subtotal_score_Baseline)/data_num$SRS22_subtotal_score_Baseline
data_num$change_SRS_function <-(data_num$SRS22_function_score_2years-data_num$SRS22_function_score_Baseline)/data_num$SRS22_function_score_Baseline
data_num$change_SRS_MH <-(data_num$SRS22_MH_score_2years-data_num$SRS22_MH_score_Baseline)/data_num$SRS22_MH_score_Baseline
data_num$change_SRS_pain <-(data_num$SRS22_pain_score_2years-data_num$SRS22_pain_score_Baseline)/data_num$SRS22_pain_score_Baseline
data_num$change_SRS_SI <-(data_num$SRS22_SI_score_2years-data_num$SRS22_SI_score_Baseline)/data_num$SRS22_SI_score_Baseline
data_num$change_MCS <-(data_num$SF36v2_MCS_score_2years-data_num$SF36v2_MCS_score_Baseline)/data_num$SF36v2_MCS_score_Baseline
data_num$change_PCS <-(data_num$SF36v2_PCS_score_2years-data_num$SF36v2_PCS_score_Baseline)/data_num$SF36v2_PCS_score_Baseline

tt_both <- compareGroups(class ~ class_s , data_num)
ttable_tt_both <- createTable(tt_both)

ttable_tt_both

tt_both_1 <- compareGroups(class ~ . , data_num)
tt_both_1 <- compareGroups(class ~ . , data_cluster)

summary(tt_both_1)#$SF36v2_PCS_score_Baseline


ttable_tt_class <- createTable(tt_both_1)
summary(ttable_tt_class)
ttable_tt_class$descr

export2html(ttable_tt_class, file="tab.html")
export2html(ttable_tt_class_s, file="tab2.html")

export2csv(ttable_tt_both, file="tab_1.csv")
export2csv(ttable_tt_class, file="tab_2.csv")
export2csv(ttable_tt_class_s, file="tab_3.csv")


tt_both_2 <- compareGroups(class_s ~ . , data_num)
tt_both_2 <- compareGroups(class_s ~ . , data_cluster_surgery)
data_cluster_surgery <- data.frame(data_cluster_surgery)
ttable_tt_class_s <- createTable(tt_both_2)
ttable_tt_class_s

summary(tt_both_2)

tt_both <- compareGroups(class_s ~ . , data_num)
ttable_tt_both <- createTable(tt_both)

tt_both <- compareGroups(class ~ . , data_num)
ttable_tt_both <- createTable(tt_both)


ttable_tt_both



export2csv(ttable, file="Cluster.csv")

### Lvel plots
## Example data
x <- data_num$class
y <- data_num$class_s
datam <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)

# Levelplot with ggplot2
library(ggplot2)
ggplot(data, aes(X, Y, z= Z)) + geom_tile(aes(fill = Z)) + theme_bw()

# To change the color of the gradation :
ggplot(data, aes(X, Y, z= Z)) + geom_tile(aes(fill = Z)) + 
  theme_bw() + 
  scale_fill_gradient(low="white", high="blue") 


### Final Table


table_good <- table(data_num$class,data_num$class_s)
prop.table(table_good,1)


### Plots
data_long$Cluster <- rep(data_num$class,3)
data_long$Cluster_s <- rep(data_num$class_s,3)

library(gridExtra)
plot1 <- ggplot(data_long, aes(x=Time, y=ODI_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster,color=Cluster)) + scale_color_jco()
plot2 <- ggplot(data_long, aes(x=Time, y=SRS22_function_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster,color=Cluster)) + scale_color_jco()
plot3 <- ggplot(data_long, aes(x=Time, y=SRS22_MH_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster,color=Cluster)) + scale_color_jco()
plot4 <- ggplot(data_long, aes(x=Time, y=SRS22_pain_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster,color=Cluster)) + scale_color_jco()
plot5 <- ggplot(data_long, aes(x=Time, y=SRS22_SI_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster,color=Cluster)) + scale_color_jco()
plot6 <- ggplot(data_long, aes(x=Time, y=SRS22_subtotal_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster,color=Cluster)) + scale_color_jco()
plot7 <- ggplot(data_long, aes(x=Time, y=SF36v2_PCS_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster,color=Cluster)) + scale_color_jco()
plot8 <- ggplot(data_long, aes(x=Time, y=SF36v2_MCS_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster,color=Cluster)) + scale_color_jco()

grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,ncol=4)

plot1 <- ggplot(data_long, aes(x=Time, y=ODI_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster_s,color=Cluster_s)) + scale_color_jco()
plot2 <- ggplot(data_long, aes(x=Time, y=SRS22_function_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster_s,color=Cluster_s)) + scale_color_jco()
plot3 <- ggplot(data_long, aes(x=Time, y=SRS22_MH_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster_s,color=Cluster_s)) + scale_color_jco()
plot4 <- ggplot(data_long, aes(x=Time, y=SRS22_pain_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster_s,color=Cluster_s)) + scale_color_jco()
plot5 <- ggplot(data_long, aes(x=Time, y=SRS22_SI_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster_s,color=Cluster_s)) + scale_color_jco()
plot6 <- ggplot(data_long, aes(x=Time, y=SRS22_subtotal_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster_s,color=Cluster_s)) + scale_color_jco()
plot7 <- ggplot(data_long, aes(x=Time, y=SF36v2_PCS_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster_s,color=Cluster_s)) + scale_color_jco()
plot8 <- ggplot(data_long, aes(x=Time, y=SF36v2_MCS_score)) + theme_classic2() + geom_point(aes(group=X),cex=.5) + geom_smooth(method="loess",aes(group=Cluster_s,color=Cluster_s)) + scale_color_jco()

grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,ncol=4)

plot <- ggplot(data_num , aes(x=change_ODI))
plot + geom_density() + facet_grid(class ~  class_s)


library(plyr)
library(dplyr) 

data_num$COMP <- data_comp$COMP

cdata <- ddply(data_num,class ~ class_s, summarize, odi = mean(change_ODI))
cdata$PCS <- ddply(data_num,class ~ class_s, summarize, mean_mean = mean(change_PCS))[,3]
cdata$MCS <- ddply(data_num,class ~ class_s, summarize, mean_mean = mean(change_MCS))[,3]
cdata$change_SRS_subtotal <- ddply(data_num,class ~ class_s, summarize, mean_mean = mean(change_SRS_subtotal))[,3]
cdata$change_SRS_SI <- ddply(data_num,class ~ class_s, summarize, mean_mean = mean(change_SRS_SI))[,3]
cdata$change_SRS_MH <- ddply(data_num,class ~ class_s, summarize, mean_mean = mean(change_SRS_MH))[,3]
cdata$change_SRS_function <- ddply(data_num,class ~ class_s, summarize, mean_mean = mean(change_SRS_function))[,3]
cdata$change_SRS_pain <- ddply(data_num,class ~ class_s, summarize, mean_mean = mean(change_SRS_pain))[,3]
cdata$comp <- ddply(data_num,class ~ class_s, summarize, comp = mean(COMP))
cdata$n <- ddply(data_num,class ~ class_s, summarize, n = n())
cdata$n <- cdata$n$n
cdata$comp <- cdata$comp$comp
setwd("C:\\Users\\Miquel\\Desktop\\Papers Spine\\2. Unsupervised Learning (Spine)")

#### Multipanel plot
levels(cdata$class) <- c("1) Young Coronal", "2) Old Revision", "3) Old Primary")
levels(cdata$class_s) <- c("1) 3CO","2) No 3CO/IBF" ,"3) IBF" ,"4) SPO")

p1 <- ggplot(cdata, aes(class, class_s, z= n)) + geom_tile(aes(fill = n)) + theme_classic() + xlab("") + ylab("") + scale_colour_gradient2(aesthetics = "fill") + geom_text(aes(label = round(n, 1)))
p2 <- ggplot(cdata, aes(class, class_s, z= PCS)) + geom_tile(aes(fill = PCS)) + theme_classic() + xlab("") + ylab("") + scale_colour_gradient2(aesthetics = "fill") + geom_text(aes(label = round(PCS, 2)))
p3 <- ggplot(cdata, aes(class, class_s, z= MCS)) + geom_tile(aes(fill = MCS)) + theme_classic() + xlab("") + ylab("") + scale_colour_gradient2(aesthetics = "fill") + geom_text(aes(label = round(MCS, 2)))
p4 <- ggplot(cdata, aes(class, class_s, z= odi)) + geom_tile(aes(fill = odi)) + theme_classic() + xlab("") + ylab("") + scale_colour_gradient2(aesthetics = "fill") + geom_text(aes(label = round(comp,2)))
p5 <- ggplot(cdata, aes(class, class_s, z= change_SRS_subtotal)) + geom_tile(aes(fill = change_SRS_subtotal)) + theme_classic() + xlab("") + ylab("") + scale_colour_gradient2(aesthetics = "fill") + geom_text(aes(label = round(n, 1)))
p6 <- ggplot(cdata, aes(class, class_s, z= change_SRS_SI)) + geom_tile(aes(fill = change_SRS_SI)) + theme_classic() + xlab("") + ylab("") + scale_colour_gradient2(aesthetics = "fill") + geom_text(aes(label = round(PCS, 2)))
p7 <- ggplot(cdata, aes(class, class_s, z= change_SRS_MH)) + geom_tile(aes(fill = change_SRS_MH)) + theme_classic() + xlab("") + ylab("") + scale_colour_gradient2(aesthetics = "fill") + geom_text(aes(label = round(MCS, 2)))
p8 <- ggplot(cdata, aes(class, class_s, z= change_SRS_function)) + geom_tile(aes(fill = change_SRS_function)) + theme_classic() + xlab("") + ylab("") + scale_colour_gradient2(aesthetics = "fill") + geom_text(aes(label = round(comp,2)))
p9 <- ggplot(cdata, aes(class, class_s, z= change_SRS_pain)) + geom_tile(aes(fill = change_SRS_pain)) + theme_classic() + xlab("") + ylab("") + scale_colour_gradient2(aesthetics = "fill") + geom_text(aes(label = round(comp,2)))
p10 <- ggplot(cdata, aes(class, class_s, z= comp)) + geom_tile(aes(fill = comp)) + theme_classic() + xlab("") + ylab("") + scale_colour_gradient2(aesthetics = "fill") + geom_text(aes(label = round(comp,2)))


library(gridExtra)

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, nrow=5)


####


p_odi <- ggplot(data_num,aes(x=change_ODI)) + 
  xlab("% Change in ODI score at 2 Years") + geom_density(alpha=.8,adjust = 1.5) + xlim(c(-0.5,2)) +
  theme_bw() + facet_grid(class ~ class_s) +
  geom_vline(data=cdata_odi, aes(xintercept=mean_mean),
             size=0.5, colour="black")
ggplotly(p_odi)

p_srs <- ggplot(data_num,aes(x=change_SRS_subtotal)) + 
  xlab("% Change in ODI score at 2 Years") + geom_histogram(alpha=.8,adjust = 1.5) + xlim(c(-0.5,2)) +
  theme_classic() + facet_grid(class ~ class_s) +
  geom_vline(data=cdata_odi, aes(xintercept=mean_mean),
             size=0.5, colour="black")
ggplotly(p_srs)

p_srs <- ggplot(data_num,aes(x=change_SRS_SI)) + 
  xlab("% Change in ODI score at 2 Years") + geom_histogram(alpha=.8,adjust = 1.5) + xlim(c(-0.5,2)) +
  theme_classic() + facet_grid(class ~ class_s) +
  geom_vline(data=cdata_odi, aes(xintercept=mean_mean),
             size=0.5, colour="black")
ggplotly(p_srs)

## Normalize Scores



data_long$class <- rep(data_num$class, 3)
data_long$class_s <- rep(data_num$class_s, 3)

data_long[,64:71] <- scale(data_long[,64:71])



p_srs <- ggplot(data_long,aes(y=SRS22_subtotal_score,x=Time)) +  
  xlab("% Change in SRS Self-Image Score at 2 Years") + geom_point() + geom_smooth()+
  theme_classic() + facet_grid(class ~ class_s) 
ggplotly(p_srs)

## B

data <- read.csv("eff.csv",sep=",", na.strings=c("","NA"))

library(ggplot2)
library(ggsci)
library(gridExtra)
library(ggpubr)

data$Surgery.Class.<- as.factor(data$Surgery.Class.)

plot <- ggplot(data, aes(x=Major.Complication.Rate.)) + theme_classic()
p1 <- plot + geom_point(aes(y=ODI., size=n, color=Patient.Class.)) + geom_text(aes(y=ODI.,label=Surgery.Class.),nudge_y = -0.02,nudge_x = 0.02,angle = 45) + scale_fill_jama() + scale_color_jama() + scale_size(range = c(0, 10)) + xlim(c(0,.7)) + ylim(c(-.1,1.15)) + geom_abline(intercept = 0, slope = 1) + ylab("% ODI Improvement") + xlab("% Major Complication Rate")
p2 <- plot + geom_point(aes(y=SF36v2.PCS., size=n, color=Patient.Class.)) + geom_text(aes(y=SF36v2.PCS.,label=Surgery.Class.),nudge_y = -0.02,nudge_x = 0.02,angle = 45) + scale_fill_jama() + scale_color_jama() + scale_size(range = c(0, 10)) + xlim(c(0,.7)) + ylim(c(-.1,1.15)) + geom_abline(intercept = 0, slope = 1) + ylab("% SF36v2 PCS Improvement") + xlab("")
p3 <- plot + geom_point(aes(y=SF36v2.MCS., size=n, color=Patient.Class.)) + geom_text(aes(y=SF36v2.MCS.,label=Surgery.Class.),nudge_y = -0.02,nudge_x = 0.02,angle = 45) + scale_fill_jama() + scale_color_jama() + scale_size(range = c(0, 10)) + xlim(c(0,.7)) + ylim(c(-.1,1.15)) + geom_abline(intercept = 0, slope = 1) + ylab("% SF36v2 MCS Improvement") + xlab("")
p4 <- plot + geom_point(aes(y=SRS.subtotal., size=n, color=Patient.Class.)) + geom_text(aes(y=SRS.subtotal.,label=Surgery.Class.),nudge_y = -0.02,nudge_x = 0.02,angle = 45) + scale_fill_jama() + scale_color_jama() + scale_size(range = c(0, 10)) + xlim(c(0,.7)) + ylim(c(-.1,1.15)) + geom_abline(intercept = 0, slope = 1) + ylab("% SRS22 subtotal Improvement") + xlab("")
p5 <- plot + geom_point(aes(y=SRS.SI., size=n, color=Patient.Class.)) + geom_text(aes(y=SRS.SI.,label=Surgery.Class.),nudge_y = -0.02,nudge_x = 0.02,angle = 45) + scale_fill_jama() + scale_color_jama() + scale_size(range = c(0, 10)) + xlim(c(0,.7)) + ylim(c(-.1,1.15)) + geom_abline(intercept = 0, slope = 1) + ylab("% SRS22 self-image Improvement") + xlab("")
p6 <- plot + geom_point(aes(y=SRS.MH., size=n, color=Patient.Class.)) + geom_text(aes(y=SRS.MH.,label=Surgery.Class.),nudge_y = -0.02,nudge_x = 0.02,angle = 45) + scale_fill_jama() + scale_color_jama() + scale_size(range = c(0, 10)) + xlim(c(0,.7)) + ylim(c(-.1,1.15)) + geom_abline(intercept = 0, slope = 1) + ylab("% SRS22 mental-health Improvement") + xlab("")
p7 <- plot + geom_point(aes(y=SRS.function., size=n, color=Patient.Class.)) + geom_text(aes(y=SRS.function.,label=Surgery.Class.),nudge_y = -0.02,nudge_x = 0.02,angle = 45) + scale_fill_jama() + scale_color_jama() + scale_size(range = c(0, 10)) + xlim(c(0,.7)) + ylim(c(-.1,1.15)) + geom_abline(intercept = 0, slope = 1) + ylab("% SRS22 function Improvement") + xlab("")
p8 <- plot + geom_point(aes(y=SRS.pain., size=n, color=Patient.Class.)) + geom_text(aes(y=SRS.pain.,label=Surgery.Class.),nudge_y = -0.02,nudge_x = 0.02,angle = 45) + scale_fill_jama() + scale_color_jama() + scale_size(range = c(0, 10)) + xlim(c(0,.7)) + ylim(c(-.1,1.15)) + geom_abline(intercept = 0, slope = 1) + ylab("% SRS22 pain Improvement") + xlab("")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p1)

pp <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               p3 + theme(legend.position="none"),
                               p4 + theme(legend.position="none"),
                               p5 + theme(legend.position="none"),
                               p6 + theme(legend.position="none"),
                               p7 + theme(legend.position="none"),
                               p8 + theme(legend.position="none"),
                               nrow=2),
                   mylegend, nrow=2,heights=c(10, 1))
pp
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow=2, ncol=4, common.legend = TRUE, legend="bottom")



### Linear models kool

library(arm)

data_num$comb <- with(data_num, interaction(class, class_s))

fit1 <- lm(COMP ~ ., data_num)
library(leaps)
best.subset <- regsubsets(COMP~., data_num, nvmax=10)
summary(fit1)
coefplot(fit1)

### 
ddply(data, c("COMP", "Prior_Spine_Surgery"), summarise,
      mean = mean(value), sd = sd(value),
      sem = sd(value)/sqrt(length(value)))




