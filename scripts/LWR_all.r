fish <- read.csv("C:/GitHub/FRI_LWReg/data/FinalFish.csv")
species.order <- sort(unique(fish$Species))


ab_row <- NULL
ab_row_temp <- NULL

for (i in seq_along(species.order)){
  spp <- species.order[[i]]
  fishbin <- subset(fish,  Species == spp)
  
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
  colnames(ab_row_temp) <- c("species","count","mean_length","min_length","max_length","mean_weight","min_weight",
                             "max_weight","a", "a_2.5", "a_97.5", "b", "b_2.5", "b_97.5", "Adj R2", "p-value_a", "p-value_b")
  ab_row <- rbind(ab_row,ab_row_temp)
  
}


setwd("C:/Github/FRI_LWReg/outputs/")
write.csv(ab_row, file = "LWR_full.csv")
