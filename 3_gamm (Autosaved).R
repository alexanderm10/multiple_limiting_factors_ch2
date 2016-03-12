library(mgcv)
library(ggplot2)
library(car)
# Christy's GAMM code

# gam1 <- gamm(Y ~  s(Biomass, bs="cr", k=3, by=PFT) + s(tair, k=k, by=PFT) + s(precipf, k=k, by=PFT) + s(CO2, k=k, by=PFT), random=list(PlotID=~1), data=data) 

# Loading in my data

data.use <- read.csv("processed_data/AllSites_tree_plus_climate.csv", header=T)
summary(data.use)

data.use$group <- data.use$Species
data.use$group <- recode(data.use$group, "'CAOV' = 'CARYA'; 'CACO' = 'CARYA'; 
  							'CATE' = 'CARYA'; 'ACSAC' = 'ACSA'; 'BEAL' = 'BETULA'; 'BELE' = 'BETULA'; 'QUMU' = 'QUAL'")

data.use$group.plot <- as.factor(paste(data.use$group, data.use$PlotID, sep="."))


# reducing the amount of data for the test runs
sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

test <- data.use[data.use$Site %in% sites.use,]
summary(test)

# Get a list of what predictors & responses I'm using
predictors.all <- c("RW", "tmean", "precip", "Species", "dbh.recon", "Canopy.Class", "spp.plot", "Site", "group", "group.plot")

# Getting rid of observations that have NAs in the important variables
test <- test[complete.cases(test[,predictors.all]),]
test <- test[test$Live.Dead=="LIVE" & !test$Canopy.Class=="F",]

# Subsetting to a set of species that we have enough data to feel good about
#species.use <- c("TSCA", "QURU", "ACRU", "BEAL", "ACSA", "LITU", "QUAL", "CAOV", "CACO", "CATE", "JUVI", "QUVE", "PCRU", "THOC", "PIST")
group.use <- c("ACRU", "ACSA", "BETULA", "CARYA", "FAGR", "FRAX", "PIST", "QUAL", "QURU", "QUVE", "SAAL", "TSCA", "ULRU")

test <- test[test$group %in% group.use,]







summary(test)
summary(test$Live.Dead)
summary(test$Canopy.Class)
# RW <- test$RW
# temp <- test$tmean
# precip <- test$precip
# canopy <- test$Canopy.Class
# size <- test$DBH..cm.
# species <- unique(test$Species)
# library(ggplot2)
# ggplot(data=test) +
   # facet_wrap(~PlotID) +
   # geom_histogram(aes(x=dbh.recon))
# ggplot(data=test) +
   # facet_wrap(~Species) +
   # geom_histogram(aes(x=dbh.recon))


# hist(test$dbh.recon)

################################################### 
# HERE'S THE GAMM!!!
################################################### 
# RW ~ CLIMATE(Species) + Size
gam1 <- gamm(RW ~ s(tmean, k=3, by=group) + # tmean*Species 
                  s(precip, k=3, by=group) +
                  s(dbh.recon, k=3, by=group.plot) +
                  Canopy.Class, 
                  random=list(Site=~1, PlotID=~1),
                  data=test)

save(gam1, file="processed_data/gam1_climate_by_species.Rdata")

# s(tmean, by=Spp.Can) + s(tmean, by=Canopy)
# s(tmean, by=Spp) + s(tmean, by=Canopy) + s(tmean, by=Spp.Can)

gam2 <- gamm(RW ~ s(tmean, k=3, by=Canopy.Class) +
                  s(precip, k=3, by=Canopy.Class) +
                  s(dbh.recon, k=3, by=Canopy.Class)+
                  Species,
                  random=list(Site=~1, PlotID=~1),
                  data=test)
 

 par(mfrow=c(4,2)); plot(gam1$gam, ylim=c(-0.025, 0.025))
 par(mfrow=c(4,2)); plot(gam2$gam, ylim=c(-0.025, 0.025))
 
# MAKE gam2 by= canopy class
# make sure to when adding multiple site to list Site in teh random list.
# Random effects are hierarchical 
 
 
################################################### 
# Copied over from 0_process_gamm.R
# This will give us the sensitivities of RW in a pretty format.

# Set up a dummy dataset for the script to run correctly
# number of simulations to run

load("processed_data/gam1_climate_by_species.Rdata")

n <- 100
source("0_Calculate_GAMM_Posteriors.R")
# Fitting our model to the data to see if we're doing a pasable job of capturing the variance
# If things don't match, we shoudl take our sensitiivty curves with a grain of salt
model.pred <- post.distns(model.gam=gam1, model.name="species_response", newdata=test, vars=predictors.all, n=n, terms=F)
summary(model.pred$ci)

# Need help dealing with the list that is set up here.  Need help with the aggregation
model.pred2 <- model.pred$ci
summary(model.pred2)
dim(model.pred2)


# Aggregating to the group level in teh same way we do below with the ring widths
# We can then compare our modeled RW to our measured RW and see how things look
# Sanity Check #1
mean.model <- aggregate(model.pred2$RW, by = model.pred2[, c("group", "Site", "Year")], FUN=mean, na.rm=T)
names(mean.model)[names(mean.model)=="x"] <- c("rw.mean") 
mean.model[,"rw.lwr"] <- aggregate(model.pred2$RW, by=model.pred2[,c("group", "Site", "Year")], FUN=quantile, probs=0.025, na.rm=T)[,"x"]
mean.model[,"rw.upr"] <- aggregate(model.pred2$RW, by=model.pred2[,c("group", "Site", "Year")], FUN=quantile, probs=0.975, na.rm=T)[,"x"]
head(mean.model)



# aggregating the raw data for graphing
mean.rw <- aggregate(data.use$RW, by=data.use[, c("group", "Site", "Year")], FUN=mean, na.rm=T)
names(mean.rw)[names(mean.rw)=="x"]<- c("rw.mean")                  
mean.rw[,"rw.lwr"] <- aggregate(data.use$RW, by=data.use[,c("group", "Site", "Year")], FUN=quantile, probs=0.025, na.rm=T)[,"x"]
mean.rw[,"rw.upr"] <- aggregate(data.use$RW, by=data.use[,c("group", "Site", "Year")], FUN=quantile, probs=0.975, na.rm=T)[,"x"]
head(mean.rw)

mean.rw <- mean.rw[mean.rw$Site %in% sites.use,]

#mean.rw$Species <- mean.rw$group





summary(model.pred$ci)

# Sanity Check #1 graph
pdf("figures/gam1_sanitycheck1.pdf", width= 13, height = 8.5)
ggplot(data=mean.rw) + facet_wrap(group ~ Site, scales="fixed") + theme_bw() +
	# plot the data
	geom_ribbon(aes(x=Year, ymin=rw.lwr, ymax=rw.upr), alpha=0.5) +
	geom_line(aes(x=Year, y=rw.mean), size=1) +
	# Plot our model
	geom_ribbon(data=mean.model, aes(x=Year, ymin=rw.lwr, ymax=rw.upr), fill="red3", alpha=0.3) +
	geom_line(data=mean.model, aes(x=Year, y=rw.mean), color="red3", alpha=0.8, size=1) +
	labs(title="Gamm Model vs. Data", x="Year", y="RW")
dev.off()


# Sanity Check #2
# Pulling random trees from both the data.use and the model.pred2 to see how they compare

n <- 100
data.use2 <- data.use[data.use$Site %in% sites.use,]
data.use2 <- data.use2[data.use2$group %in% group.use,]

sanity2.trees <- sample(data.use2$TreeID, size=n, replace=F) 
summary(sanity2.trees)

summary(mean.rw)
summary(data.use)


# Sanity Check #2 graph
pdf("figures/gam1_sanitycheck2.pdf", width= 13, height = 8.5)
ggplot(data=data.use[data.use$TreeID %in% sanity2.trees,]) + facet_wrap(TreeID~ Site, scales="fixed") + theme_bw() +
	# plot the data
	#geom_ribbon(aes(x=Year, ymin=rw.lwr, ymax=rw.upr), alpha=0.5) +
	geom_line(aes(x=Year, y=RW, size=1)) +
	# Plot our model
	#geom_ribbon(data=model.pred2[model.pred2$TreeID %in% sanity2.trees,], aes(x=Year, ymin=rw.lwr, ymax=rw.upr), fill="red3", alpha=0.3) +
	geom_line(data=model.pred2[model.pred2$TreeID %in% sanity2.trees,], aes(x=Year, y=RW), color="red3", alpha=0.8, size=1) +
	labs(title="Gamm Model vs. Data", x="Year", y="RW")
dev.off()



# running scripts to get the weights
source("0_Calculate_GAMM_Weights.R")

gam1.weights <- factor.weights(model.gam = gam1, model.name = "species_response", newdata = test, extent = "", vars = predictors.all)

summary(gam1.weights)



#----------------------------------------------
# Gam1 graphs
n <- 100
data <- test
 

		n.out = n

		new.dat <- data.frame(Model="species_response",
							  Extent=as.factor(paste(min(data$Year), max(data$Year), sep="-")))
		
		# Figure out which vars are numeric vs. factor
		vars.num <- vector()
		for(v in predictors.all){
			if(class(data[,v]) %in% c("numeric", "integer")) vars.num <- c(vars.num, v)
		}
		
		# Getting the unique values of our factor variables and adding them to the data frame
		for(v in predictors.all[!predictors.all %in% vars.num & !predictors.all=="Species"]){
			# if v is a factor, merge all unique values into the dataframe
			var.temp <- data.frame(x=unique(data[,v])) 
			names(var.temp) <- v
			new.dat <- merge(new.dat, var.temp, all.x=T, all.y=T)
		}
		# getting species from species.plot
		new.dat$Species <- as.factor(substr(new.dat$spp.plot, 1, 4))
		
		# Putting the numerical variables into an array and adding it in 
		var.temp <- data.frame(array(dim=c(n.out, length(vars.num))))
		names(var.temp) <- vars.num
		for(v in vars.num){
			var.temp[,v] <- seq(min(data[,v], na.rm=T), max(data[,v], na.rm=T), length.out=n.out)
		}								
		new.dat <- merge(new.dat, var.temp, all.x=T, all.y=T)
		summary(new.dat)
								
		# SOurce & run the function
		source("0_Calculate_GAMM_Posteriors.R")
		ci.terms.pred <- post.distns(model.gam=gam1, model.name="species_response", n=n, newdata=new.dat, vars=predictors.all, terms=T)
		
		ci.out <- ci.terms.pred$ci # separting out the confidence interval 
		ci.out[,predictors.all[!predictors.all %in% vars.num]] <- new.dat[,predictors.all[!predictors.all %in% vars.num]] # copying over our factor labels
		ci.out$x <- as.numeric(ci.out$x) # making x numeric; will make factors NA
		summary(ci.out)
		
spp.colors <- read.csv("spp.Colors.csv", header=T)	
summary(spp.colors)	

spp.fig <- unique(ci.out$Species)
spp.fig <- spp.fig[order(spp.fig)]
colors.use <- as.vector(c(paste(spp.colors[spp.colors$Species %in% spp.fig, "color"])))
		
		
		ggplot(data=ci.out[ci.out$Effect %in% c("tmean", "precip"), ]) + 
			facet_wrap(Species~Effect, scales="free_x") +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=Species), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=Species)) +
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)
		
		ci.out$PlotID <- as.factor(substr(ci.out$spp.plot, 6, nchar(paste(ci.out$spp.plot)))) # adding a plotID factor
		summary(ci.out)
		
		ggplot(data=ci.out[ci.out$Effect == "dbh.recon", ]) + 
			facet_wrap(~PlotID) +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=Species), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=Species)) +
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)

#----------------------------------------------
# Gam2 graphs
n <- 100 
data <- test
 

		n.out = n

		new.dat <- data.frame(Model="harv_mmf_cc.test",
							  Extent=as.factor(paste(min(data$Year), max(data$Year), sep="-")))
		
		# Figure out which vars are numeric vs. factor
		vars.num <- vector()
		for(v in predictors.all){
			if(class(data[,v]) %in% c("numeric", "integer")) vars.num <- c(vars.num, v)
		}
		
		# Getting the unique values of our factor variables and adding them to the data frame
		for(v in predictors.all[!predictors.all %in% vars.num & !predictors.all=="Species"]){
			# if v is a factor, merge all unique values into the dataframe
			var.temp <- data.frame(x=unique(data[,v])) 
			names(var.temp) <- v
			new.dat <- merge(new.dat, var.temp, all.x=T, all.y=T)
		}
		# getting species from species.plot
		new.dat$Species <- as.factor(substr(new.dat$spp.plot, 1, 4))
		
		# Putting the numerical variables into an array and adding it in 
		var.temp <- data.frame(array(dim=c(n.out, length(vars.num))))
		names(var.temp) <- vars.num
		for(v in vars.num){
			var.temp[,v] <- seq(min(data[,v], na.rm=T), max(data[,v], na.rm=T), length.out=n.out)
		}								
		new.dat <- merge(new.dat, var.temp, all.x=T, all.y=T)
		summary(new.dat)
								
		# SOurce & run the function
		source("0_Calculate_GAMM_Posteriors.R")
		ci.terms.pred2 <- post.distns(model.gam=gam2, model.name="harv_mmf_cc.test", n=n, newdata=new.dat, vars=predictors.all, terms=T)
		
		ci.out2 <- ci.terms.pred2$ci # separting out the confidence interval 
		ci.out2[,predictors.all[!predictors.all %in% vars.num]] <- new.dat[,predictors.all[!predictors.all %in% vars.num]] # copying over our factor labels
		ci.out2$x <- as.numeric(ci.out$x) # making x numeric; will make factors NA
		summary(ci.out2)
		
		ggplot(data=ci.out2[ci.out2$Effect %in% c("tmean", "precip"), ]) + 
			facet_wrap(~Effect, scales="free_x") +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=Canopy.Class), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=Canopy.Class))
		
		ci.out2$PlotID <- as.factor(substr(ci.out$spp.plot, 6, nchar(paste(ci.out$spp.plot)))) # adding a plotID factor
		summary(ci.out2)
		
		ggplot(data=ci.out2[ci.out2$Effect == "dbh.recon", ]) + 
			#facet_wrap(~PlotID) +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=Canopy.Class), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=Canopy.Class))


 
 