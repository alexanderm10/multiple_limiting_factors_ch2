library(car)
library(dplR)
se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}

# Loading in climate data from the PRISM extracts
t.mean <- read.csv("climate_data/prism_met_sites_wide_tmean.csv", header=T)
precip <- read.csv("climate_data/prism_met_sites_wide_ppt.csv", header=T)
summary(t.mean)

# We have Harvard P1 and Harvard P2 as separate sites

harvard <- c("Harvard Forest (Tower Plot 1)", "Harvard Forest (Tower Plot 2)")

t.mean$Site.Name <- as.factor(ifelse(t.mean$Site.Name %in% harvard, "Harvard", paste(t.mean$Site.Name)))
summary(t.mean)

t.mean2 <- aggregate(t.mean[,3:ncol(t.mean)], by=t.mean[,c("Site.Name", "Year")], FUN=mean)
summary(t.mean2)

t.mean2$grow.seas <- rowMeans(t.mean2[,c("May", "Jun", "Jul", "Aug", "Sep")])
summary(t.mean2)


# Precip
precip$Site.Name <- as.factor(ifelse(precip$Site.Name %in% harvard, "Harvard", paste(precip$Site.Name)))
summary(precip)

precip2 <- aggregate(precip[,3:ncol(precip)], by=precip[,c("Site.Name", "Year")], FUN=mean)
summary(precip2)

precip2$grow.seas <- rowSums(precip2[,c("May", "Jun", "Jul", "Aug", "Sep")])

climate.use <- data.frame(Site = t.mean2$Site.Name,
						  tmean = t.mean2$grow.seas,
						  precip = precip2$grow.seas,
						  Year = t.mean2$Year)
summary(climate.use)

write.csv(climate.use, file="processed_data/climate_growing_season.csv", row.names=F)						  


####################################################
# Combining my data.use from script #1 to this set of climate data
####################################################

data.use <- read.csv("processed_data/tree_data_use.csv", header=T)
summary(data.use)
dim(data.use)
summary(climate.use)
dim(climate.use)

unique(data.use$Site)
unique(climate.use$Site)

data.use2 <- merge(data.use, climate.use, all.x=T, all.y=T)
dim(data.use2)
summary(data.use2)

# taking onlt the trees that dated so that we don't have any gapfilled data in the analysis
data.use2 <- data.use2[data.use2$Dated=="Y" & !is.na(data.use2$Dated),]
summary(data.use2)

write.csv(data.use2, file="processed_data/AllSites_tree_plus_climate.csv", row.names=F)