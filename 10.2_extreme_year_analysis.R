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
summary(gam2.weights)

ggplot(gam2.weights) + facet_grid(Canopy.Class~.) +
	geom_density(aes(x=BA.inc,color=Temp.Mark, fill=Temp.Mark), alpha=0.1) +
	xlim(0,25)

ggplot(gam2.weights) + facet_grid(Canopy.Class~.) +
	geom_density(aes(x=BA.inc,color=Precip.Mark,fill=Precip.Mark), alpha=0.1)+
	xlim(0,25)

ggplot(gam2.weights) + facet_grid(Canopy.Class~.) +
	geom_density(aes(x=BA.inc,fill=Precip.Mark))


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
	