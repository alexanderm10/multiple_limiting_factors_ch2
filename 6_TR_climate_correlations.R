library(mgcv)
library(ggplot2)
library(car)
# Christy's GAMM code

# gam1 <- gamm(Y ~  s(Biomass, bs="cr", k=3, by=PFT) + s(tair, k=k, by=PFT) + s(precipf, k=k, by=PFT) + s(CO2, k=k, by=PFT), random=list(PlotID=~1), data=data) 

# Loading in my data

data.use <- read.csv("processed_data/AllSites_tree_plus_climate_and_BA.csv", header=T)

data.use$group <- data.use$Species
data.use$group <- recode(data.use$group, "'CAOV' = 'CARYA'; 'CACO' = 'CARYA'; 
  							'CATE' = 'CARYA'; 'ACSAC' = 'ACSA'; 'BEAL' = 'BETULA'; 'BELE' = 'BETULA'; 'QUMU' = 'QUAL'")

data.use$group.plot <- as.factor(paste(data.use$group, data.use$PlotID, sep="."))

data.use$Canopy.Class <- recode(data.use$Canopy.Class, "'C' = 'D'")
summary(data.use)


# reducing the amount of data for the test runs
sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

test <- data.use[data.use$Site %in% sites.use & !is.na(data.use$RW) & !is.na(data.use$BA.inc),]
summary(test)

test$group.cc <- as.factor(paste(test$group, test$Canopy.Class, sep="."))
summary(test)


# Get a list of what predictors & responses I'm using
predictors.all <- c("tmean", "precip", "Species", "dbh.recon", "Canopy.Class", "spp.plot", "group", "group.plot", "group.cc")

# Getting rid of observations that have NAs in the important variables
test <- test[complete.cases(test[,predictors.all]),]
test <- test[test$Live.Dead=="LIVE" & !test$Canopy.Class=="F",]

# Subsetting to a set of species that we have enough data to feel good about
#species.use <- c("TSCA", "QURU", "ACRU", "BEAL", "ACSA", "LITU", "QUAL", "CAOV", "CACO", "CATE", "JUVI", "QUVE", "PCRU", "THOC", "PIST")
group.use <- c("ACRU", "ACSA", "BETULA", "CARYA", "FAGR", "FRAX", "PIST", "QUAL", "QURU", "QUVE", "SAAL", "TSCA", "ULRU")

test <- test[test$group %in% group.use,]

summary(test)

par(new=F)
plot(test[test$TreeID=="MMA003", "BA.inc"]~ test[test$TreeID=="MMA003","Year"], type="l")


summary(test)


# Truncatign at 1950
test <- test[test$Year >= 1950 & test$Year <= 2012,]

tree.rw <- test
summary(tree.rw)

summary(test)



save(test, file="processed_data/test_tree_data_1950_2012.Rdata")

sites.rw <- list()

tree.rw$Year <- as.factor(tree.rw$Year)

for(s in unique(tree.rw$Site)){
	sites.rw[[s]] <- recast(tree.rw[tree.rw$Site==s,c("Year", "TreeID", "RW")], Year ~ TreeID)	
}
summary(sites.rw)

save(sites.rw, file="processed_data/sites_rw.Rdata")


summary(sites.rw[[4]])
sites.rw
# Making Indices of all the different sites using a 30 yr cubic smoothing spline
sites.i <- list()

for(s in names(sites.rw)){
	sites.i[[s]] <- detrend(sites.rw[[s]][,2:ncol(sites.rw[[s]])], method="Mean")
}
summary(sites.i[[1]])

# making chronologies for each site

site.chron <- list()

for(s in names(sites.i)){
	
	site.chron[[s]] <- chron(sites.i[[s]][,2:ncol(sites.i[[s]])], prefix = substr(s, 1, 3), prewhiten = T) 
	site.chron[[s]][,"Year"]<- c(1950:2012) 
}

summary(site.chron)


# Making each site it's own dataframe
harvard.crn <- as.data.frame(site.chron[[1]])

howland.crn <- as.data.frame(site.chron[[2]])

misso.crn <- as.data.frame(site.chron[[3]])

mmf.crn <- as.data.frame(site.chron[[4]])

oakop.crn <- as.data.frame(site.chron[[5]])



# Merging the residual chronologies back together
sites.crn <- data.frame(Harvard = harvard.crn[, "Harres"], 
						Howland = howland.crn[,"Howres"], 
						Missouri = misso.crn[,"Misres"],
						Morgan_Monroe = mmf.crn[,"Morres"], 
						Oak_openings = oakop.crn[,"Oakres"])

summary(sites.crn)








# Load in climate data

climate.use <- read.csv("processed_data/climate_growing_season.csv")

summary(climate.use)

climate.use <- climate.use[climate.use$Site %in% sites.use,]
climate.use <- climate.use[climate.use$Year>=1950 & climate.use$Year <=2012,]

climate.site <- list()

for(s in unique(climate.use$Site)){
	climate.site[[s]] <- climate.use[climate.use$Site==s,]
}

summary(climate.site)
save(climate.site, file="processed_data/sites_use_climate.Rdata")


class(climate.site[[1]][,"Year"])

head(climate.site[[1]])

summary(climate.site)

harv.clim <- climate.site[["Harvard"]][,c("tmean", "precip")]

how.clim <- climate.site[["Howland"]][,c("tmean", "precip")]

miss.clim <- climate.site[["Missouri Ozark"]][,c("tmean", "precip")]

mmf.clim <- climate.site[["Morgan Monroe State Park"]][,c("tmean", "precip")]

oak.clim <- climate.site[["Oak Openings Toledo"]][,c("tmean", "precip")]


dim(harv.clim)
dim(harvard.crn)

# harvard
harv.cor<- cor(harvard.crn, harv.clim[,c("tmean", "precip")], method="pearson")
harvard <- data.frame(cor = c(harv.cor[1:2,"tmean"],  harv.cor[1:2,"precip"]),
					  var = c("tmean", "tmean", "precip", "precip"))
					  
harvard$type <- c("std", "res")
harvard$site <- "Harvard" 


# Howland
how.cor <- cor(howland.crn, how.clim[,c("tmean", "precip")], method="pearson")
how.cor

howland <- data.frame(cor = c(how.cor[1:2,"tmean"],  how.cor[1:2,"precip"]),
					  var = c("tmean", "tmean", "precip", "precip"))

					  
howland$type <- c("std", "res")
howland$site <- "Howland" 



# Missouri
mo.cor <- cor(misso.crn, miss.clim[,c("tmean", "precip")], method="pearson")
mo.cor

missouri <- data.frame(cor = c(mo.cor[1:2,"tmean"],  mo.cor[1:2,"precip"]),
					  var = c("tmean", "tmean", "precip", "precip"))

					  
missouri$type <- c("std", "res")
missouri$site <- "Missouri" 

# Morgan Monroe
mmf.cor <- cor(mmf.crn, mmf.clim[,c("tmean", "precip")], method="pearson")
mmf.cor

morgan <- data.frame(cor = c(mmf.cor[1:2,"tmean"],  mmf.cor[1:2,"precip"]),
					  var = c("tmean", "tmean", "precip", "precip"))

					  
morgan$type <- c("std", "res")
morgan$site <- "Morgan-Monroe" 

# Oak openings
oakop.cor <- cor(oakop.crn, oak.clim[,c("tmean", "precip")], method="pearson")
oakop.cor

oakop <- data.frame(cor = c(oakop.cor[1:2,"tmean"],  oakop.cor[1:2,"precip"]),
					  var = c("tmean", "tmean", "precip", "precip"))

					  
oakop$type <- c("std", "res")
oakop$site <- "Oak-Openings" 

# Merging standard chronologies into one file

clim.cor <- merge(harvard, howland, all.x=T, all.y=T)
clim.cor2 <- merge(clim.cor, missouri,all.x=T, all.y=T)
clim.cor3 <- merge(clim.cor2, morgan,all.x=T, all.y=T)
clim.cor4 <- merge(clim.cor3, oakop,all.x=T, all.y=T)

summary(clim.cor4)
clim.cor4$site <- as.factor(clim.cor4$site)
clim.cor4$type <- as.factor(clim.cor4$type)
clim.cor4$cor <- as.numeric(clim.cor4$cor)

# Sig value for 61 df = 0.209
clim.cor4$sig <- ifelse(clim.cor4$cor < -0.209 | clim.cor4$cor > 0.209, "Y", "N")
clim.cor4$sig <- factor(clim.cor4$sig, levels = c("Y", "N"))

clim.cor4$site <- factor(clim.cor4$site, levels = c("Missouri", "Morgan-Monroe", "Oak-Openings", "Harvard", "Howland"))

clim.cor4$Site <- clim.cor4$site
clim.cor4$Site <- recode(clim.cor4$Site, "'Missouri' = 'MO'; 'Morgan-Monroe' = 'IN'; 'Oak-Openings' = 'OH'; 'Harvard' = 'MA'; 'Howland' = 'ME'")
clim.cor4$Site <- factor(clim.cor4$Site, levels = c("MO", "IN", "OH", "MA", "ME"))


pdf("figures/site_correlations.pdf", width= 13, height = 8.5)
ggplot(data=clim.cor4[clim.cor4$type=="std",]) + facet_grid(var~. , scales="free_x") +
	geom_bar(aes(x=Site, y=cor, fill=sig), stat="identity", position="dodge", colour="black") +
	geom_hline(yintercept=0.209, linetype="dashed") + 
	geom_hline(yintercept=-0.209, linetype="dashed") + 	
	geom_hline(yintercept=0, linetype="solid") +
	scale_fill_manual(values= c("green", "grey50")) + 
	poster.theme2

	 
dev.off()



