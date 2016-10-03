library(ggplot2)
require(mgcv)
require(lsmeans)
require(car)
require(moments)
# Running ANOVA between Extreme years and normal years BAI for Canopy.Class
load("processed_data/gam2_weights_processed.Rdata")
summary(gam2.weights)

# Running ANOVA's to determine differences between ambient years and extreme years
# Using a lme here to take into account the differences between sites as random effeects
summary(gam2.weights)
# Truncating to 1950 becasue that's the extent of the climate analysis
gam2.weights <- gam2.weights[gam2.weights$Year >=1950,]


# cr.test <- lme(log(BA.inc) ~ Precip.Mark*Canopy.Class, random=list(group=~1, Site=~1, PlotID=~1, TreeID=~1), data=gam2.weights)
# lsmeans(cr.test, pairwise~Precip.Mark*Canopy.Class, adjust="tukey")	
# cr.test2 <- lme(log(BA.inc) ~ Temp.Mark*Canopy.Class, random=list(TreeID=~1), data=gam2.weights)
# lsmeans(cr.test2, pairwise~Temp.Mark*Canopy.Class, adjust="tukey")	

# Same fixed effects structure as the gams; add an interaction between temp & precip 
# because we fit those jointly in the gam
cr.testD <- aov(log(BA.inc) ~ Precip.Mark*Temp.Mark + Site + PlotID + TreeID + group, data=gam2.weights[gam2.weights$Canopy.Class=="D",])
cr.testI <- aov(log(BA.inc) ~ Precip.Mark*Temp.Mark + Site + PlotID + TreeID + group, data=gam2.weights[gam2.weights$Canopy.Class=="I",])
cr.testS <- aov(log(BA.inc) ~ Precip.Mark*Temp.Mark + Site + PlotID + TreeID + group, data=gam2.weights[gam2.weights$Canopy.Class=="S",])
plot(resid(cr.testD) ~ predict(cr.testD))
plot(resid(cr.testI) ~ predict(cr.testI))
plot(resid(cr.testS) ~ predict(cr.testS))

summary(cr.testD)
tukey.d <- TukeyHSD(cr.testD)
# tukey.d["Precip.Mark:Temp.Mark"] # not significant, so we won't look at it
tukey.d["Precip.Mark"]
tukey.d["Temp.Mark"]
# Saving table to publication folder
#write.table(summary(cr.testD), file="~/PhD/publications/2016/Ch2_multiple_limiting_factors/D_anova.txt")

summary(cr.testI)
tukey.i <- TukeyHSD(cr.testI)
# tukey.i["Precip.Mark:Temp.Mark"] # not significant, so we won't look at it
tukey.i["Precip.Mark"]
tukey.i["Temp.Mark"]

summary(cr.testS)
tukey.s <- TukeyHSD(cr.testS)
tukey.s["Precip.Mark:Temp.Mark"] # Significant (for better or worse)
tukey.s["Precip.Mark"]
tukey.s["Temp.Mark"]


# lsmeans(cr.testD, pairwise~Precip.Mark, adjust="tukey")	
# lsmeans(cr.testD, pairwise~Temp.Mark, adjust="tukey")	
# lsmeans(cr.testI, pairwise~Precip.Mark, adjust="tukey")	
# lsmeans(cr.testS, pairwise~Precip.Mark, adjust="tukey")	

###########################################################
# probabity density functions for extreme years
###########################################################
load("processed_data/climate_markeryears.Rdata")
summary(gam2.weights)

# Making a combined temp/precip marker year
gam2.weights$mix.mark <- as.factor(paste(gam2.weights$Temp.Mark, gam2.weights$Precip.Mark, sep="-"))
summary(gam2.weights)

# calculating median BAI for temp and precip
median.temp.s <- data.frame(type= unique(gam2.weights$Temp.Mark))
for(i in unique(gam2.weights$Temp.Mark)){
  median.temp.s[median.temp.s$type==i,"median"] <- median(gam2.weights[gam2.weights$Temp.Mark==i & gam2.weights$Canopy.Class=="S","BA.inc"])
}
median.temp.s$Canopy.Class <- as.factor("S")
median.temp.i <- data.frame(type= unique(gam2.weights$Temp.Mark))
for(i in unique(gam2.weights$Temp.Mark)){
  median.temp.i[median.temp.s$type==i,"median"] <- median(gam2.weights[gam2.weights$Temp.Mark==i & gam2.weights$Canopy.Class=="I","BA.inc"])
}
median.temp.i$Canopy.Class <- as.factor("I")

median.temp.d <- data.frame(type= unique(gam2.weights$Temp.Mark))
for(i in unique(gam2.weights$Temp.Mark)){
  median.temp.d[median.temp.s$type==i,"median"] <- median(gam2.weights[gam2.weights$Temp.Mark==i & gam2.weights$Canopy.Class=="D","BA.inc"])
}
median.temp.d$Canopy.Class <- as.factor("D")

median.temp <- rbind(median.temp.d, median.temp.i, median.temp.s)

#############
# Precip
median.precip.s <- data.frame(type= unique(gam2.weights$Precip.Mark))
for(i in unique(gam2.weights$Precip.Mark)){
  median.precip.s[median.precip.s$type==i,"median"] <- median(gam2.weights[gam2.weights$Precip.Mark==i & gam2.weights$Canopy.Class=="S","BA.inc"])
}
median.precip.s$Canopy.Class <- as.factor("S")
median.precip.i <- data.frame(type= unique(gam2.weights$Precip.Mark))
for(i in unique(gam2.weights$Precip.Mark)){
  median.precip.i[median.precip.s$type==i,"median"] <- median(gam2.weights[gam2.weights$Precip.Mark==i & gam2.weights$Canopy.Class=="I","BA.inc"])
}
median.precip.i$Canopy.Class <- as.factor("I")

median.precip.d <- data.frame(type= unique(gam2.weights$Precip.Mark))
for(i in unique(gam2.weights$Precip.Mark)){
  median.precip.d[median.precip.s$type==i,"median"] <- median(gam2.weights[gam2.weights$Precip.Mark==i & gam2.weights$Canopy.Class=="D","BA.inc"])
}
median.precip.d$Canopy.Class <- as.factor("D")

median.precip <- rbind(median.precip.d, median.precip.i, median.precip.s)





ggplot(gam2.weights) + facet_grid(Canopy.Class~.) +
	geom_density(aes(x=BA.inc,color=Temp.Mark, fill=Temp.Mark), alpha=0.1) +
  geom_vline(data=median.temp, aes(xintercept=median, color=type)) +
  scale_color_manual(values=c("grey50", "blue", "red")) +
	scale_fill_manual(values=c("grey50", "blue", "red")) +
	xlim(0,25) +
	labs(x= "BAI", y="Density") +
	theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=rel(1.0)),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(nrow=1)) +
	theme(axis.title.y= element_text(size=rel(1.1), face="bold"))+
	theme(axis.title.x= element_text(size=rel(1.1), face="bold"))
	
ggplot(gam2.weights) + facet_grid(Canopy.Class~.) +
	geom_density(aes(x=BA.inc.Clim,color=Precip.Mark, fill=Precip.Mark), alpha=0.1) +
  geom_vline(data=median.precip, aes(xintercept=median, color=type)) +
  scale_color_manual(values=c("grey50", "brown", "darkgreen")) +
	scale_fill_manual(values=c("grey50", "brown", "darkgreen")) +
	xlim(0,25) +
	labs(x= "BAI", y="Density") +
	theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=rel(1.0)),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(nrow=1)) +
	theme(axis.title.y= element_text(size=rel(1.1), face="bold"))+
	theme(axis.title.x= element_text(size=rel(1.1), face="bold"))

# Calculating median growth for each extreme year combination
marker.median.s <- data.frame(type= unique(gam2.weights$mix.mark))

for(i in unique(gam2.weights$mix.mark)){
	marker.median.s[marker.median.s$type==i,"median"] <- median(gam2.weights[gam2.weights$mix.mark==i & gam2.weights$Canopy.Class=="S","BA.inc"])
}
marker.median.s$Canopy.Class <- as.factor("S")

marker.median.d <- data.frame(type= unique(gam2.weights$mix.mark))

for(i in unique(gam2.weights$mix.mark)){
	marker.median.d[marker.median.s$type==i,"median"] <- median(gam2.weights[gam2.weights$mix.mark==i & gam2.weights$Canopy.Class=="D","BA.inc"])
}
marker.median.d$Canopy.Class <- as.factor("D")

marker.median.i <- data.frame(type= unique(gam2.weights$mix.mark))

for(i in unique(gam2.weights$mix.mark)){
	marker.median.i[marker.median.s$type==i,"median"] <- median(gam2.weights[gam2.weights$mix.mark==i & gam2.weights$Canopy.Class=="I","BA.inc"])
}
marker.median.i$Canopy.Class <- as.factor("I")

marker.median <- rbind(marker.median.i, marker.median.d, marker.median.s)

ggplot(gam2.weights[gam2.weights$mix.mark %in% c("A-A", "hot-wet", "hot-dry", "hot-A") & gam2.weights$Site=="Oak Openings Toledo",]) + facet_grid(Canopy.Class~Site) +
	geom_density(aes(x=BA.inc,color=mix.mark,fill=mix.mark), alpha=0.1) +
	#geom_vline(data=marker.median[marker.median$type %in% c("A-A", "hot-wet", "hot-dry", "hot-A"),], aes(xintercept=median, color=type))+
	xlim(0,25)+ 
	labs(x= "BAI", y="Density") +
	theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=rel(1.0)),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(nrow=1)) +
	theme(axis.title.y= element_text(size=rel(1.1), face="bold"))+
	theme(axis.title.x= element_text(size=rel(1.1), face="bold"))



###########################################################
# Attemping an SEA analysis
###########################################################

summary(gam2.weights)
gam2.weights$Year <- as.factor(gam2.weights$Year)

sea.df <- recast(gam2.weights[,c("Year", "TreeID", "BA.inc")], Year~TreeID)
summary(sea.df)
row.names(sea.df) <- sea.df$Year


D.names <- gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="MO", "TreeID"]
I.names <- gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="IN", "TreeID"]
S.names <- gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="IN", "TreeID"]

hot.years <- unique(gam2.weights$Year[gam2.weights$Temp.Mark == "hot" & gam2.weights$State=="MO"])
cool.years<- unique(gam2.weights$Year[gam2.weights$Temp.Mark == "cold"])
wet.years <- unique(gam2.weights$Year[gam2.weights$Precip.Mark == "wet"])
dry.years <- unique(gam2.weights$Year[gam2.weights$Precip.Mark == "dry"])

wet.hot.years <- as.numeric(intersect(hot.years, wet.years))
wet.cool.years <- as.numeric(intersect(cool.years, wet.years))
dry.hot.years <- as.numeric(intersect(hot.years, dry.years))
dry.cool.years <- as.numeric(intersect(cool.years, dry.years))

dom.bai <- sea.df[,names(sea.df) %in% D.names]
head(dom.bai)

dom.bai.chron <- chron(dom.bai)
summary(dom.bai.chron)

int.bai <- sea.df[,names(sea.df) %in% I.names]
int.bai.chron <- chron(int.bai)
sup.bai <- sea.df[,names(sea.df) %in% S.names]

test <- as.data.frame(dom.bai["TP2086"])
meow <- c(1955, 1959)

sea.test <- sea(dom.bai.chron, hot.years , lag=2, resample=1000)
summary(sea.test)

foo <- sea.test$se.unscaled
names(foo) <- sea.test$lag

barplot(foo, col=ifelse(sea.test$p < 0.05, "grey30", "grey75"), ylab = "BAI", xlab="Superposed Epoch")
	