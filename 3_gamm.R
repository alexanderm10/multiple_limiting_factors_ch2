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


test$log.dbh <- log(test$dbh.recon)
summary(test)

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


# test2 <- test[test$group %in% c("QURU", "ACRU") & test$Year>=1980,]
# test2 <- test[test$group %in% c("QURU", "ACRU"),]
test2 <- test[test$Site %in% "Morgan Monroe State Park",]
# test2$log.dbh <- log(test2$dbh.recon)
# summary(test2)


summary(test)
test[test$BA.inc==0, "BA.inc"] <- 1e-6
# test.gam3 <- test
# test.gam3$Canopy.Class <- recode(test.gam3$Canopy.Class, "'C' = 'D'")
# summary(test.gam3)
################################################### 
# HERE'S THE GAMM!!!
################################################### 
# RW ~ CLIMATE(Species) + Size
                  
gam1 <- gamm(log(BA.inc)~ s(tmean, k=3, by=group) +
                  s(precip, k=3, by=group) +
                  s(dbh.recon, k=3, by=group) +
                   Canopy.Class,
                  random=list(Site=~1, PlotID=~1),
                  data=test, control=list(niterEM=0, sing.tol=1e-20, opt="optim"))


                  
# # gam1.test <- gamm(BA.inc ~ s(tmean, k=3, by=group) + # tmean*Species 
                  # s(precip, k=3, by=group) +
                  # s(log.dbh, k=3, by=group.plot) +
                  # Canopy.Class, 
                  # random=list(Site =~1, PlotID=~1),
                  # data=test2)
             

save(gam1, file="processed_data/gam_results/gam1_climate_by_species.Rdata")

# s(tmean, by=Spp.Can) + s(tmean, by=Canopy)
# s(tmean, by=Spp) + s(tmean, by=Canopy) + s(tmean, by=Spp.Can)

                  
gam2 <- gamm(log(BA.inc)~ s(tmean, k=3, by=Canopy.Class) +
                  s(precip, k=3, by=Canopy.Class) +
                  s(dbh.recon, k=3, by=group) +
                  group,
                  random=list(Site=~1, PlotID=~1),
                  data=test)                  
                  
# # gam2.test <- gamm(BA.inc~ s(tmean, k=3, by=Canopy.Class) +
                  # s(precip, k=3, by=Canopy.Class) +
                  # s(log.dbh, k=3, by=group) +
                  # group,
                  # random=list(Site=~1, PlotID=~1),
                  # data=test2)  
 
gam3 <- gamm(log(BA.inc)~ s(tmean, k=3, by=group.cc) +
                  s(precip, k=3, by=group.cc) +
                  s(dbh.recon, k=3, by=group.cc) +
                  Canopy.Class + group,
                  random=list(Site=~1, PlotID=~1),
                  data=test, control=list(niterEM=0, sing.tol=1e-20, opt="optim"))

 save(gam2, file="processed_data/gam_results/gam2_climate_by_canopyclass.Rdata") 
 save(gam3, file="processed_data/gam_results/gam3_climate_by_canopyclass_interactions.Rdata")
 

 par(mfrow=c(4,2)); plot(gam1$gam, ylim=c(-0.025, 0.025))
 par(mfrow=c(4,2)); plot(gam2$gam, ylim=c(-0.025, 0.025))
 
# MAKE gam2 by= canopy class
# make sure to when adding multiple site to list Site in teh random list.
# Random effects are hierarchical 
 
 

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
		# need to skip group & group.cc so we aren't trying to match Carya & Quercus etc
		predictors.all <- predictors.all[!predictors.all %in% c("Species", "spp.plot")]
		for(v in predictors.all[!predictors.all %in% vars.num & !(predictors.all %in% c("group", "group.cc"))]){
			# if v is a factor, merge all unique values into the dataframe
			var.temp <- data.frame(x=unique(data[,v])) 
			names(var.temp) <- v
			new.dat <- merge(new.dat, var.temp, all.x=T, all.y=T)
		}
		# getting species from species.plot
		new.dat$group <- as.factor(substr(new.dat$group.plot, 1, 4))
		
		# Adding in the group.canopy class then getting rid of combinations we don't actually have in our data
		new.dat$group.cc <- as.factor(paste(new.dat$group, new.dat$Canopy.Class, sep="."))
		new.dat <- new.dat[new.dat$group.cc %in% unique(test$group.cc),]
		summary(new.dat)
		
		# Putting the numerical variables into an array and adding it in 
		var.temp <- data.frame(array(dim=c(n.out, length(vars.num))))
		names(var.temp) <- vars.num
		for(v in vars.num){
			var.temp[,v] <- seq(min(data[,v], na.rm=T), max(data[,v], na.rm=T), length.out=n.out)
		}								
		new.dat <- merge(new.dat, var.temp, all.x=T, all.y=T)
		summary(new.dat)
								
write.csv(new.dat, file="processed_data/sensitivity_extaction_dataframe.csv", row.names=F)								
# Change which gamm you look at here!
n <- 100						
		# SOurce & run the function
		source("0_Calculate_GAMM_Posteriors.R")
		g1.ci.terms.pred <- post.distns(model.gam=gam1, model.name="species_response", n=n, newdata=new.dat, vars=predictors.all, terms=T)
		
		g1.ci.out <- g1.ci.terms.pred$ci # separting out the confidence interval 
		g1.ci.out[,predictors.all[!predictors.all %in% vars.num]] <- new.dat[,predictors.all[!predictors.all %in% vars.num]] # copying over our factor labels
		g1.ci.out$x <- as.numeric(g1.ci.out$x) # making x numeric; will make factors NA
		summary(g1.ci.out)
		
spp.colors <- read.csv("spp.Colors.csv", header=T)	
summary(spp.colors)	

group.fig <- unique(g1.ci.out$group)
group.fig <- group.fig[order(group.fig)]
colors.use <- as.vector(c(paste(spp.colors[spp.colors$Species %in% group.fig, "color"])))

ci.terms.graph <- g1.ci.out
ci.terms.graph[ci.terms.graph$mean<(-15),"mean"] <- NA 
ci.terms.graph[ci.terms.graph$lwr<(-15),"lwr"] <- -15
ci.terms.graph[ci.terms.graph$upr<(-15),"upr"] <- -15 
ci.terms.graph[which(ci.terms.graph$mean>10),"mean"] <- NA 
ci.terms.graph[ci.terms.graph$lwr>(10),"lwr"] <- 10 
ci.terms.graph[ci.terms.graph$upr>(10),"upr"] <- 10 
		
pdf("figures/gam1_sensitivities.pdf", width= 13, height = 8.5)		
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean", "precip", "dbh.recon"), ]) + 
			facet_grid(~Effect, scales="free_x") +
			geom_line(aes(x=x, y=0), linetype="dashed")+
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group)) +
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)+
			poster.theme2+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2", 							"y"^"-1",")"))))
dev.off()

pdf("figures/gam1_sensitivities_Size.pdf", width= 13, height = 8.5)		
		ggplot(data=ci.terms.graph[ (ci.terms.graph$Effect=="dbh.recon" & ci.terms.graph$x<=50),]) + 
			facet_wrap(~group, scales="free_x") +
			geom_line(aes(x=x, y=0), linetype="dashed")+
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group)) +
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)+
			poster.theme2+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2", 							"y"^"-1",")"))))
dev.off()



pdf("figures/gam1_sensitivities_ACRU.pdf", width= 13, height = 8.5)					
	ggplot(data=ci.terms.graph[ci.terms.graph$group %in% "ACRU" & (ci.terms.graph$Effect %in% c("tmean", "precip") | (ci.terms.graph$Effect=="dbh.recon" & ci.terms.graph$x<=50)),]) + 
			facet_grid(~Effect, scales="free_x") +
			geom_line(aes(x=x, y=0), linetype="dashed")+
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=Effect), alpha=0.4) +
			geom_line(aes(x=x, y=mean, color=Effect)) +
			scale_color_manual(values= c("red", "blue", "green")) +
			scale_fill_manual(values=c("red", "blue", "green"))+
			poster.theme2+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2", 							"y"^"-1",")"))))
dev.off()





ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "tmean", ]) + 
			facet_grid(~Effect, scales="free_x") +
			geom_line(aes(x=x, y=0), linetype="dashed")+
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group)) +
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)
			
			ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "precip", ]) + 
			facet_grid(Canopy.Class~Effect, scales="free_x") +
			geom_line(aes(x=x, y=0), linetype="dashed")+
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group)) +
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)

			
		
				
		
		
		g1.ci.out$PlotID <- as.factor(substr(g1.ci.out$group.plot, 6, nchar(paste(g1.ci.out$group.plot)))) # adding a plotID factor
		summary(g1.ci.out)
		
		ggplot(data=g1.ci.out[g1.ci.out$Effect == "dbh.recon", ]) + 
			facet_wrap(~PlotID) +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group)) +
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)

#----------------------------------------------
# GAM 2								
		# SOurce & run the function
		source("0_Calculate_GAMM_Posteriors.R")
		g2.ci.terms.pred2 <- post.distns(model.gam=gam2, model.name="harv_mmf_cc.test", n=n, newdata=new.dat, vars=predictors.all, terms=T)
		
		g2.ci.out2 <- g2.ci.terms.pred2$ci # separting out the confidence interval 
		g2.ci.out2[,predictors.all[!predictors.all %in% vars.num]] <- new.dat[,predictors.all[!predictors.all %in% vars.num]] # copying over our factor labels
		g2.ci.out2$x <- as.numeric(g2.ci.out2$x) # making x numeric; will make factors NA
		summary(g2.ci.out2)

pdf("figures/gam2_sensitivities.pdf", width= 13, height = 8.5)		
		ggplot(data=g2.ci.out2[g2.ci.out2$Effect %in% c("tmean", "precip"), ]) + 
			facet_wrap(~Effect, scales="free_x") +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=Canopy.Class), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=Canopy.Class))+
			scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00")) +
			scale_fill_manual(values=c("#0072B2", "#009E73",  "#E69F00")) +
			poster.theme2+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2", 							"y"^"-1",")"))))
dev.off()		
		g2.ci.out2$PlotID <- as.factor(substr(g2.ci.out2$group.plot, 6, nchar(paste(g2.ci.out2$group.plot)))) # adding a plotID factor
		summary(g2.ci.out2)
pdf("figures/gam2_sensitivities_size.pdf", width = 13, height= 8.5)		
		ggplot(data=g2.ci.out2[g2.ci.out2$Effect == "dbh.recon" & g2.ci.out2$x<=50, ]) + 
			facet_wrap(~group) +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group)) +
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)+
			poster.theme2
dev.off()

 #----------------------------------------------
# GAM 3								
		# SOurce & run the function
"processed_data/gam_results/gam3_climate_by_canopyclass_interactions.Rdata"
load("processed_data/gam_results/gam3_climate_by_canopyclass_interactions.Rdata")		
		source("0_Calculate_GAMM_Posteriors.R")
		g3.ci.terms.pred <- post.distns(model.gam=gam3, model.name="harv_mmf_cc.test", n=n, newdata=new.dat, vars=predictors.all, terms=T)
		
		g3.ci.out <- g3.ci.terms.pred$ci # separting out the confidence interval 
		g3.ci.out[,predictors.all[!predictors.all %in% vars.num]] <- new.dat[,predictors.all[!predictors.all %in% vars.num]] # copying over our factor labels
		g3.ci.out$x <- as.numeric(g3.ci.out$x) # making x numeric; will make factors NA
		summary(g3.ci.out)
		
ci.terms.graph <- g3.ci.out
ci.terms.graph[ci.terms.graph$mean<(-2.5),"mean"] <- NA 
ci.terms.graph[ci.terms.graph$lwr<(-2.5),"lwr"] <- -2.5
ci.terms.graph[ci.terms.graph$upr<(-2.5),"upr"] <- -2.5 
ci.terms.graph[which(ci.terms.graph$mean>2.5),"mean"] <- NA 
ci.terms.graph[ci.terms.graph$lwr>(2.5),"lwr"] <- 2.5 
ci.terms.graph[ci.terms.graph$upr>(2.5),"upr"] <- 2.5 

pdf("figures/gam3_sensitivities.pdf", width= 13, height = 8.5)
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean", "precip"), ]) + 
			facet_grid(Canopy.Class~Effect, scales="free_x") +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group))+
			poster.theme2+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2", 							"y"^"-1",")"))))
dev.off()

pdf("figures/gam3_sensitivities_size.pdf", width= 13, height = 8.5)
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "dbh.recon" & ci.terms.graph$x<=50, ]) + 
			facet_wrap(~group.cc, scales="free_x") +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group))+
			#poster.theme2+
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2", 							"y"^"-1",")"))))
dev.off()



ci.terms.graph <- g3.ci.out
ci.terms.graph[ci.terms.graph$mean<(-13),"mean"] <- NA 
ci.terms.graph[ci.terms.graph$lwr<(-13),"lwr"] <- -13
ci.terms.graph[ci.terms.graph$upr<(-13),"upr"] <- -13 
ci.terms.graph[which(ci.terms.graph$mean>8),"mean"] <- NA 
ci.terms.graph[ci.terms.graph$lwr>(8),"lwr"] <- 8 
ci.terms.graph[ci.terms.graph$upr>(8),"upr"] <- 8 

			
# Oaks only
			
pdf("figures/oak_climate_effects.pdf", width= 13, height = 8.5)		
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean", "precip") & substr(ci.terms.graph$group.cc, 1,2)=="QU", ]) + 
			facet_grid(Canopy.Class~Effect, scales="free_x") +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group))+
			scale_color_manual(values=c("#E69F00", "#0072B2", "#009E73")) +
			scale_fill_manual(values=c("#E69F00", "#0072B2", "#009E73")) +
			poster.theme2+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2", "y"^"-1",")"))))
dev.off()			
			
		
		g3.ci.out$PlotID <- as.factor(substr(g3.ci.out$group.plot, 6, nchar(paste(g3.ci.out$group.plot)))) # adding a plotID factor
		summary(g3.ci.out)
		
		ggplot(data=g3.ci.out[g3.ci.out$Effect == "dbh.recon", ]) + 
			facet_wrap(~PlotID) +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group))

pdf("figures/oak_size_effects.pdf", width= 13, height = 8.5)		
	ggplot(data=g3.ci.out[g3.ci.out$Effect == "dbh.recon" & substr(g3.ci.out$group.cc, 1,2)=="QU", ]) + 
			facet_grid(~Canopy.Class) +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group)) +
			scale_color_manual(values=c("#E69F00", "#0072B2", "#009E73")) +
			scale_fill_manual(values=c("#E69F00", "#0072B2", "#009E73")) +
			poster.theme2+
			labs(x = "DBH (cm)", y = expression(bold(paste("Effect on BAI (mm"^"2", "y"^"-1",")"))))
 dev.off()