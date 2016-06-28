library(dplR)
library(ggplot2)
library(car)

#--------------------------------------------------------
# Want to graph the mean rw and 95% Ci for each site
#--------------------------------------------------------

data.use <- read.csv("processed_data/AllSites_tree_plus_climate.csv", header=T)
summary(data.use)

# Subsetting Live trees
data.use <- data.use[data.use$Live.Dead=="LIVE" & !data.use$Canopy.Class=="F",]

data.use$group <- data.use$Species
data.use$group <- recode(data.use$group, "'CAOV' = 'CARYA'; 'CACO' = 'CARYA'; 
  							'CATE' = 'CARYA'; 'ACSAC' = 'ACSA'; 'BEAL' = 'BETULA'; 'BELE' = 'BETULA'; 'QUMU' = 'QUAL'")

data.use$Canopy.Class <- recode(data.use$Canopy.Class, "'C' = 'D'")
summary(data.use)

group.use <- c("ACRU", "ACSA", "BETULA", "CARYA", "FAGR", "FRAX", "PIST", "QUAL", "QURU", "QUVE", "SAAL", "TSCA", "ULRU")

data.use <- data.use[data.use$group %in% group.use,]

# 
# Aggregating by site by year to get the mean RW for each year for each canopy Class
cc.mean.rw <- data.frame(mean = aggregate(data.use$RW, by=data.use[,c("Site", "Canopy.Class", "Year")], FUN=mean, na.rm=T))
names(cc.mean.rw)<- c("Site", "Canopy.Class", "Year", "rw.mean")                  
summary(cc.mean.rw)


# Generating the 95% CI for RW at each site for each year                   
# I was having trouble graphing if I generated both quantiles at the same time, so I am doing them separately and merging the upper and lower into 1 DF
cc.lwr.rw <- aggregate(data.use$RW, by=list(data.use$Site,data.use$Canopy.Class, data.use$Year), FUN=quantile, probs=0.025, na.rm=T)
cc.upr.rw <- aggregate(data.use$RW, by=list(data.use$Site,data.use$Canopy.Class, data.use$Year), FUN=quantile, probs=0.975, na.rm=T)                   

names(cc.lwr.rw) <- c("Site", "Canopy.Class", "Year", "lwr")
names(cc.upr.rw) <- c("Site", "Canopy.Class", "Year", "upr")

#Merging CI's
cc.ci.rw <- merge(cc.lwr.rw, cc.upr.rw, all.x=T, all.y=F)
summary(cc.ci.rw)

# Merging CI with Mean

cc.mean.rw <- merge(cc.mean.rw, cc.ci.rw, all.x=T, all.y=F)
summary(cc.mean.rw)
sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

ggplot(data=cc.mean.rw[cc.mean.rw$Site %in% sites.use,]) + facet_wrap(Canopy.Class~Site, scales="free_x") +
  geom_ribbon(aes(x=Year, ymin=lwr, ymax=upr), alpha=0.5) +
  geom_line(aes(x=Year, y=rw.mean))+
  poster.theme2


#-----------------------------------------------
# Now looking at just the mean RW for the total site for each year
  
# Aggregating by site by year to get the mean RW for each year for each group by canopy class
# mean.rw <- data.frame(mean = aggregate(data.use$RW, by=list(data/use$Speciesdata.use$Site, data.use$Year), FUN=mean, na.rm=T))



mean.rw <- aggregate(data.use$RW, by=data.use[, c("Site", "group", "Canopy.Class", "Year")], FUN=mean, na.rm=T)

names(mean.rw)[names(mean.rw)=="x"]<- c("rw.mean")                  
summary(mean.rw)


# Generating the 95% CI for RW at each site for each year                   
# I was having trouble graphing if I generated both quantiles at the same time, so I am doing them separately and merging the upper and lower into 1 DF
lwr.rw <- aggregate(data.use$RW, by=data.use[,c("Site", "group", "Canopy.Class", "Year")], FUN=quantile, probs=0.025, na.rm=T)
upr.rw <- aggregate(data.use$RW, by=data.use[, c("Site", "group", "Canopy.Class", "Year")], FUN=quantile, probs=0.975, na.rm=T)                   

names(lwr.rw)[names(lwr.rw)=="x"]<- c("rw.lwr")      
names(upr.rw)[names(upr.rw)=="x"]<- c("rw.upr")

#Merging CI's
ci.rw <- merge(lwr.rw, upr.rw, all.x=T, all.y=F)
summary(ci.rw)

# Merging CI with Mean

mean.rw <- merge(mean.rw, ci.rw, all.x=T, all.y=F)
summary(mean.rw)

sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

#species.use <- c("TSCA", "QURU", "ACRU", "BEAL", "ACSA", "LITU", "QUAL", "CAOV", "CACO", "CATE", "JUVI", "QUVE", "PCRU", "THOC", "PIST")


mean.rw <- mean.rw[mean.rw$Site %in% sites.use,]
#mean.rw <- mean.rw[mean.rw$Species %in% species.use,]

group.colors <- read.csv("spp.Colors.csv", header=T)	
summary(group.colors)	

group.fig <- unique(data.use$group)
group.fig <- group.fig[order(group.fig)]
colors.use <- as.vector(c(paste(group.colors[group.colors$Species %in% group.fig, "color"])))
		


ggplot(data=mean.rw) + facet_grid(Canopy.Class~Site, scales="free_x") +
  geom_ribbon(aes(x=Year, ymin=rw.lwr, ymax=rw.upr, fill=group), alpha=0.5) +
  geom_line(aes(x=Year, y=rw.mean, color=group))  +
			 scale_color_manual(values=colors.use) +
			 scale_fill_manual(values=colors.use)


#-----------------------------------------------
# 
  
# Aggregating by site by year to get the mean RW for each year for each group
# mean.rw <- data.frame(mean = aggregate(data.use$RW, by=list(data/use$Speciesdata.use$Site, data.use$Year), FUN=mean, na.rm=T))



mean.rw <- aggregate(data.use$RW, by=data.use[, c("Site", "group", "Year")], FUN=mean, na.rm=T)

names(mean.rw)[names(mean.rw)=="x"]<- c("rw.mean")                  
summary(mean.rw)


# Generating the 95% CI for RW at each site for each year                   
# I was having trouble graphing if I generated both quantiles at the same time, so I am doing them separately and merging the upper and lower into 1 DF
lwr.rw <- aggregate(data.use$RW, by=data.use[,c("Site", "group", "Year")], FUN=quantile, probs=0.025, na.rm=T)
upr.rw <- aggregate(data.use$RW, by=data.use[, c("Site", "group", "Year")], FUN=quantile, probs=0.975, na.rm=T)                   

names(lwr.rw)[names(lwr.rw)=="x"]<- c("rw.lwr")      
names(upr.rw)[names(upr.rw)=="x"]<- c("rw.upr")

#Merging CI's
ci.rw <- merge(lwr.rw, upr.rw, all.x=T, all.y=F)
summary(ci.rw)

# Merging CI with Mean

mean.rw <- merge(mean.rw, ci.rw, all.x=T, all.y=F)
summary(mean.rw)

sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

#species.use <- c("TSCA", "QURU", "ACRU", "BEAL", "ACSA", "LITU", "QUAL", "CAOV", "CACO", "CATE", "JUVI", "QUVE", "PCRU", "THOC", "PIST")


mean.rw <- mean.rw[mean.rw$Site %in% sites.use,]
#mean.rw <- mean.rw[mean.rw$Species %in% species.use,]

group.colors <- read.csv("spp.Colors.csv", header=T)	
summary(group.colors)	

group.fig <- unique(data.use$group)
group.fig <- group.fig[order(group.fig)]
colors.use <- as.vector(c(paste(group.colors[group.colors$Species %in% group.fig, "color"])))
		


ggplot(data=mean.rw) + facet_grid(group~Site, scales="free_x") +
  geom_ribbon(aes(x=Year, ymin=rw.lwr, ymax=rw.upr, fill=group), alpha=0.5) +
  geom_line(aes(x=Year, y=rw.mean, color=group))  +
			 scale_color_manual(values=colors.use) +
			 scale_fill_manual(values=colors.use)
#-----------------------------------------------
# Now looking at just the mean RW for the total site for each year
  
# Aggregating by site by year to get the mean RW for each year for each site
# mean.rw <- data.frame(mean = aggregate(data.use$RW, by=list(data/use$Speciesdata.use$Site, data.use$Year), FUN=mean, na.rm=T))



mean.rw <- aggregate(data.use$RW, by=data.use[, c("Site", "Year")], FUN=mean, na.rm=T)

names(mean.rw)[names(mean.rw)=="x"]<- c("rw.mean")                  
summary(mean.rw)


# Generating the 95% CI for RW at each site for each year                   
# I was having trouble graphing if I generated both quantiles at the same time, so I am doing them separately and merging the upper and lower into 1 DF
lwr.rw <- aggregate(data.use$RW, by=data.use[,c("Site", "Year")], FUN=quantile, probs=0.025, na.rm=T)
upr.rw <- aggregate(data.use$RW, by=data.use[, c("Site", "Year")], FUN=quantile, probs=0.975, na.rm=T)                   

names(lwr.rw)[names(lwr.rw)=="x"]<- c("rw.lwr")      
names(upr.rw)[names(upr.rw)=="x"]<- c("rw.upr")

#Merging CI's
ci.rw <- merge(lwr.rw, upr.rw, all.x=T, all.y=F)
summary(ci.rw)

# Merging CI with Mean

mean.rw <- merge(mean.rw, ci.rw, all.x=T, all.y=F)
summary(mean.rw)

sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

#species.use <- c("TSCA", "QURU", "ACRU", "BEAL", "ACSA", "LITU", "QUAL", "CAOV", "CACO", "CATE", "JUVI", "QUVE", "PCRU", "THOC", "PIST")


mean.rw <- mean.rw[mean.rw$Site %in% sites.use,]
#mean.rw <- mean.rw[mean.rw$Species %in% species.use,]

group.colors <- read.csv("spp.Colors.csv", header=T)	
summary(group.colors)	

group.fig <- unique(data.use$group)
group.fig <- group.fig[order(group.fig)]
colors.use <- as.vector(c(paste(group.colors[group.colors$Species %in% group.fig, "color"])))
		


ggplot(data=mean.rw) + facet_grid(Site~., scales="free_x") +
  geom_ribbon(aes(x=Year, ymin=rw.lwr, ymax=rw.upr), alpha=0.5) +
  geom_line(aes(x=Year, y=rw.mean))  +
			 scale_color_manual(values=colors.use) +
			 scale_fill_manual(values=colors.use)