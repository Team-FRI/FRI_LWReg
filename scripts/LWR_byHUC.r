Fish_meta <- read.csv("C:/GitHub/FRI_LWReg/data/FinalFish.csv")

#by HUC
Fish_meta$HUC <- substr(Fish_meta$HUC12Watershed, start = 1, stop = 6) #currently HUC6


####Blacknose####
fish <- subset(Fish_meta, Species == "Blacknose Dace" )

TAB= table(fish$HUC)
fish <-fish[ifelse(TAB[fish$HUC]<10, FALSE, TRUE),]

species.order <- sort(unique(fish$HUC))

ab_row <- NULL
ab_row_temp <- NULL


for (i in seq_along(species.order)){
  spp <- species.order[[i]]
  fishbin <- subset(fish,  HUC == spp)
  
  count <- nrow(fishbin)
  
  mean_length <- mean(fishbin$Length_mm)
  min_length <- min(fishbin$Length_mm)
  max_length <- max(fishbin$Length_mm)
  
  mean_weight <- mean(fishbin$Wt_g)
  min_weight <- min(fishbin$Wt_g)
  max_weight <- max(fishbin$Wt_g)
  
  lm1 <- lm(logW~logL, data = fishbin, na.action = na.omit)
  tempcoef <- cbind(lm1$coefficients, confint(lm1))
  ab_row_temp <- c(spp,count,mean_length,min_length,max_length,mean_weight,min_weight,max_weight, 
                   10^tempcoef[1,],tempcoef[2,], summary(lm1)$adj.r.squared, summary(lm1)$coefficients[1,4] , summary(lm1)$coefficients[2,4] )
  ab_row_temp <- t(as.data.frame(ab_row_temp))
  colnames(ab_row_temp) <- c("site","count","mean_length","min_length","max_length","mean_weight","min_weight",
                             "max_weight","a", "a_2.5", "a_97.5", "b", "b_2.5", "b_97.5", "Adj R2", "p-value_a", "p-value_b")
  ab_row <- rbind(ab_row,ab_row_temp)
  
}

ab_row2 <- as.data.frame(ab_row)
ab_row2 <- data.frame(apply(ab_row2, 2, function(x) as.numeric(as.character(x))))
ab_row <- cbind(ab_row[,1], ab_row2[,2:17])
names(ab_row)[names(ab_row) == 'ab_row[, 1]'] <- 'site'

ab_row <- ab_row[order(ab_row$b_97.5),]
ab_row <- transform(ab_row, site=reorder(site, -b) ) 


#Plot
library(ggplot2)
blacknose_HUC <- ggplot(ab_row) +
  geom_point( aes(x=as.factor(site), y=b), stat="identity", alpha=0.7, size = 10) +
  geom_errorbar( aes(x=as.factor(site), ymin=b_2.5, ymax=b_97.5), width=0.4, colour="orange", alpha=0.9, size=1.3)+
  scale_y_continuous(limits = c(2.63, 3.53)) +
  theme_minimal()+
  xlab("Year") +
  ylab("b ± 95% Confidence Interval")+
  annotate("text",x=1,y=2.95,label="n = 215")+
  annotate("text",x=2,y=2.95,label="n = 1485")+
  theme(axis.title.y = element_text(angle=90, hjust = 0.5, vjust=1.5, size=18))+
  theme(axis.title.x = element_text(angle=0, hjust = 0.5, vjust=-1.2, size=18))+
  theme(axis.text=element_text(size=12))

####Creek Chub####
fish <- subset(Fish_meta, Species == "Creek Chub" )

TAB= table(fish$HUC)
fish <-fish[ifelse(TAB[fish$HUC]<10, FALSE, TRUE),]

species.order <- sort(unique(fish$HUC))

ab_row <- NULL
ab_row_temp <- NULL


for (i in seq_along(species.order)){
  spp <- species.order[[i]]
  fishbin <- subset(fish,  HUC == spp)
  
  count <- nrow(fishbin)
  
  mean_length <- mean(fishbin$Length_mm)
  min_length <- min(fishbin$Length_mm)
  max_length <- max(fishbin$Length_mm)
  
  mean_weight <- mean(fishbin$Wt_g)
  min_weight <- min(fishbin$Wt_g)
  max_weight <- max(fishbin$Wt_g)
  
  lm1 <- lm(logW~logL, data = fishbin, na.action = na.omit)
  tempcoef <- cbind(lm1$coefficients, confint(lm1))
  ab_row_temp <- c(spp,count,mean_length,min_length,max_length,mean_weight,min_weight,max_weight, 
                   10^tempcoef[1,],tempcoef[2,], summary(lm1)$adj.r.squared, summary(lm1)$coefficients[1,4] , summary(lm1)$coefficients[2,4] )
  ab_row_temp <- t(as.data.frame(ab_row_temp))
  colnames(ab_row_temp) <- c("site","count","mean_length","min_length","max_length","mean_weight","min_weight",
                             "max_weight","a", "a_2.5", "a_97.5", "b", "b_2.5", "b_97.5", "Adj R2", "p-value_a", "p-value_b")
  ab_row <- rbind(ab_row,ab_row_temp)
  
}

ab_row2 <- as.data.frame(ab_row)
ab_row2 <- data.frame(apply(ab_row2, 2, function(x) as.numeric(as.character(x))))
ab_row <- cbind(ab_row[,1], ab_row2[,2:17])
names(ab_row)[names(ab_row) == 'ab_row[, 1]'] <- 'site'

ab_row <- ab_row[order(ab_row$b_97.5),]
ab_row <- transform(ab_row, site=reorder(site, -b) ) 


#Plot
library(ggplot2)
chub_HUC <- ggplot(ab_row) +
  geom_point( aes(x=as.factor(site), y=b), stat="identity", alpha=0.7, size = 10) +
  geom_errorbar( aes(x=as.factor(site), ymin=b_2.5, ymax=b_97.5), width=0.4, colour="orange", alpha=0.9, size=1.3)+
  scale_y_continuous(limits = c(2.63, 3.53)) +
  theme_minimal()+
  xlab("Year") +
  ylab("b ± 95% Confidence Interval")+
  annotate("text",x=1,y=2.85,label="n = 158")+
  annotate("text",x=2,y=2.85,label="n = 884")+
  theme(axis.title.y = element_text(angle=90, hjust = 0.5, vjust=1.5, size=18))+
  theme(axis.title.x = element_text(angle=0, hjust = 0.5, vjust=-1.2, size=18))+
  theme(axis.text=element_text(size=12))

####Longnose#####
fish <- subset(Fish_meta, Species == "Longnose Dace" )

TAB= table(fish$HUC)
fish <-fish[ifelse(TAB[fish$HUC]<10, FALSE, TRUE),]

species.order <- sort(unique(fish$HUC))

ab_row <- NULL
ab_row_temp <- NULL


for (i in seq_along(species.order)){
  spp <- species.order[[i]]
  fishbin <- subset(fish,  HUC == spp)
  
  count <- nrow(fishbin)
  
  mean_length <- mean(fishbin$Length_mm)
  min_length <- min(fishbin$Length_mm)
  max_length <- max(fishbin$Length_mm)
  
  mean_weight <- mean(fishbin$Wt_g)
  min_weight <- min(fishbin$Wt_g)
  max_weight <- max(fishbin$Wt_g)
  
  lm1 <- lm(logW~logL, data = fishbin, na.action = na.omit)
  tempcoef <- cbind(lm1$coefficients, confint(lm1))
  ab_row_temp <- c(spp,count,mean_length,min_length,max_length,mean_weight,min_weight,max_weight, 
                   10^tempcoef[1,],tempcoef[2,], summary(lm1)$adj.r.squared, summary(lm1)$coefficients[1,4] , summary(lm1)$coefficients[2,4] )
  ab_row_temp <- t(as.data.frame(ab_row_temp))
  colnames(ab_row_temp) <- c("site","count","mean_length","min_length","max_length","mean_weight","min_weight",
                             "max_weight","a", "a_2.5", "a_97.5", "b", "b_2.5", "b_97.5", "Adj R2", "p-value_a", "p-value_b")
  ab_row <- rbind(ab_row,ab_row_temp)
  
}

ab_row2 <- as.data.frame(ab_row)
ab_row2 <- data.frame(apply(ab_row2, 2, function(x) as.numeric(as.character(x))))
ab_row <- cbind(ab_row[,1], ab_row2[,2:17])
names(ab_row)[names(ab_row) == 'ab_row[, 1]'] <- 'site'

ab_row <- ab_row[order(ab_row$b_97.5),]
ab_row <- transform(ab_row, site=reorder(site, -b) ) 


#Plot
library(ggplot2)
longnose_HUC <- ggplot(ab_row) +
  geom_point( aes(x=as.factor(site), y=b), stat="identity", alpha=0.7, size = 10) +
  geom_errorbar( aes(x=as.factor(site), ymin=b_2.5, ymax=b_97.5), width=0.4, colour="orange", alpha=0.9, size=1.3)+
  scale_y_continuous(limits = c(2.63, 3.53)) +
  theme_minimal()+
  xlab("Year") +
  ylab("b ± 95% Confidence Interval")+
  annotate("text",x=1,y=3.05,label="n = 108")+
  annotate("text",x=2,y=3.05,label="n = 751")+
  theme(axis.title.y = element_text(angle=90, hjust = 0.5, vjust=1.5, size=18))+
  theme(axis.title.x = element_text(angle=0, hjust = 0.5, vjust=-1.2, size=18))+
  theme(axis.text=element_text(size=12))

####Tessellated####
fish <- subset(Fish_meta, Species == "Tessellated Darter" )


TAB= table(fish$HUC)
fish <-fish[ifelse(TAB[fish$HUC]<10, FALSE, TRUE),]

species.order <- sort(unique(fish$HUC))

ab_row <- NULL
ab_row_temp <- NULL


for (i in seq_along(species.order)){
  spp <- species.order[[i]]
  fishbin <- subset(fish,  HUC == spp)
  
  count <- nrow(fishbin)
  
  mean_length <- mean(fishbin$Length_mm)
  min_length <- min(fishbin$Length_mm)
  max_length <- max(fishbin$Length_mm)
  
  mean_weight <- mean(fishbin$Wt_g)
  min_weight <- min(fishbin$Wt_g)
  max_weight <- max(fishbin$Wt_g)
  
  lm1 <- lm(logW~logL, data = fishbin, na.action = na.omit)
  tempcoef <- cbind(lm1$coefficients, confint(lm1))
  ab_row_temp <- c(spp,count,mean_length,min_length,max_length,mean_weight,min_weight,max_weight, 
                   10^tempcoef[1,],tempcoef[2,], summary(lm1)$adj.r.squared, summary(lm1)$coefficients[1,4] , summary(lm1)$coefficients[2,4] )
  ab_row_temp <- t(as.data.frame(ab_row_temp))
  colnames(ab_row_temp) <- c("site","count","mean_length","min_length","max_length","mean_weight","min_weight",
                             "max_weight","a", "a_2.5", "a_97.5", "b", "b_2.5", "b_97.5", "Adj R2", "p-value_a", "p-value_b")
  ab_row <- rbind(ab_row,ab_row_temp)
  
}

ab_row2 <- as.data.frame(ab_row)
ab_row2 <- data.frame(apply(ab_row2, 2, function(x) as.numeric(as.character(x))))
ab_row <- cbind(ab_row[,1], ab_row2[,2:17])
names(ab_row)[names(ab_row) == 'ab_row[, 1]'] <- 'site'

ab_row <- ab_row[order(ab_row$b_97.5),]
ab_row <- transform(ab_row, site=reorder(site, -b) ) 


#Plot
library(ggplot2)
tess_HUC <- ggplot(ab_row) +
  geom_point( aes(x=as.factor(site), y=b), stat="identity", alpha=0.7, size = 10) +
  geom_errorbar( aes(x=as.factor(site), ymin=b_2.5, ymax=b_97.5), width=0.4, colour="orange", alpha=0.9, size=1.3)+
  scale_y_continuous(limits = c(2.63, 3.53)) +
  theme_minimal()+
  xlab("Year") +
  ylab("b ± 95% Confidence Interval")+
  annotate("text",x=1,y=3.15,label="n = 143")+
  annotate("text",x=2,y=3.15,label="n = 255")+
  theme(axis.title.y = element_text(angle=90, hjust = 0.5, vjust=1.5, size=18))+
  theme(axis.title.x = element_text(angle=0, hjust = 0.5, vjust=-1.2, size=18))+
  theme(axis.text=element_text(size=12))

####Multipanel####
blacknose_HUC
tess_HUC
longnose_HUC
chub_HUC


library(ggpubr)
library(grid)


setwd("C:/GitHub/FRI_LWReg/outputs")

#Dimensions set for x and y to be same size. Need to make final fig take up a smidge more space so y isn't compressed.
jpeg(filename = "LWRbyHUC.jpg", width = 1000, height = 1000, units = "px", pointsize = 12,
     quality = 400)

allplots <- ggarrange(blacknose_HUC + rremove("xlab"), 
                      chub_HUC + rremove("xlab")+ rremove("ylab"), 
                      longnose_HUC, 
                      tess_HUC +rremove("ylab"), 
                      ncol = 2, nrow = 2)

annotate_figure(allplots, left = textGrob("A", vjust = -12, hjust = -2.5,gp = gpar(cex = 3.5)),
                right = textGrob("B", vjust = -12, hjust = 16.25,gp = gpar(cex = 3.5)),
                top = textGrob("C", vjust = 18.25, hjust = 14,gp = gpar(cex = 3.5)),
                bottom = textGrob("D", vjust = -12.75, hjust = -1,gp = gpar(cex = 3.5)))

dev.off()









