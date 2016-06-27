library(ggplot2)

load(file="processed_data/gamm_weights/gam2_weights.Rdata")

summary(gam2.weights)
factors.fits <- c("fit.tmean", "fit.precip", "fit.dbh.recon", "fit.full", "BA.inc")
factors.weights <- c("weight.tmean", "weight.dbh.recon", "weight.precip")

# Transforming things back to BA.inc rather than log
gam2.weights[,which(substr(names(gam2.weights),1,3)=="fit")] <- exp(gam2.weights[,which(substr(names(gam2.weights),1,3)=="fit")] )



othervars <- c("Year", "Site", "Canopy.Class", "Model")

data.graph1 <- aggregate(gam2.weights[,factors.fits], by = gam2.weights[,othervars], FUN= mean, na.rm=T)

data.graph1[,paste(factors.fits, "upr", sep=".")] <- aggregate(gam2.weights[,factors.fits], by = gam2.weights[,othervars], FUN= quantile, prob= 0.975, na.rm=T)[,factors.fits]

data.graph1[,paste(factors.fits, "lwr", sep=".")] <- aggregate(gam2.weights[,factors.fits], by = gam2.weights[,othervars], FUN= quantile, prob= 0.025, na.rm=T)[,factors.fits]

summary(data.graph1)

data.graph2 <- aggregate(abs(gam2.weights[,factors.weights]), by = gam2.weights[,othervars], FUN= mean, na.rm=T)

data.graph2[,paste(factors.weights, "upr", sep=".")] <- aggregate(abs(gam2.weights[,factors.weights]), by = gam2.weights[,othervars], FUN= quantile, prob= 0.975, na.rm=T)[,factors.weights]

data.graph2[,paste(factors.weights, "lwr", sep=".")] <- aggregate(abs(gam2.weights[,factors.weights]), by = gam2.weights[,othervars], FUN= quantile, prob= 0.025, na.rm=T)[,factors.weights]

summary(data.graph2)

data.graph <- merge(data.graph1, data.graph2, all.x=T, all.y=T)

# data.graph <- gam2.weights[gam2.weights$TreeID== "MMA014",]
summary(data.graph)
gam2.weights$wts.check <- rowSums(abs(gam2.weights[,c("weight.tmean", "weight.precip", "weight.dbh.recon")]))
data.graph$wts.check <- rowSums(abs(data.graph[,c("weight.tmean", "weight.precip", "weight.dbh.recon")]))

summary(gam2.weights)
summary(data.graph)

# Ordering the data for graphing

data.graph<- data.graph[order(data.graph$Year, data.graph$Canopy.Class, data.graph$Site, decreasing=F),]


plot.rgb <- function(STATE, CC, SIZE){	geom_line(data=data.graph[data.graph$State==STATE & data.graph$Canopy.Class==CC,],aes(x=Year, y=fit.full), size=SIZE,
  		        color=rgb(abs(data.graph[data.graph$State==STATE & data.graph$Canopy.Class==CC,"weight.tmean"     ]), # red
                        abs(data.graph[data.graph$State==STATE & data.graph$Canopy.Class==CC,"weight.dbh.recon"     ]), # green
                        abs(data.graph[data.graph$State==STATE & data.graph$Canopy.Class==CC,"weight.precip"   ]))) }   # blue


# Plotting the Obs and modeled with influence coloring
data.graph$State <- recode(data.graph$Site, "'Howland' = 'ME';'Harvard' = 'MA';'Morgan Monroe State Park' = 'IN';'Missouri Ozark' = 'MO';'Oak Openings Toledo' = 'OH'")
data.graph$State <- factor(data.graph$State, levels=c("MO", "IN", "OH", "MA", "ME"))

# Plotting the Obs and modeled with influence coloring
pdf("figures/gam2_canopyclass_BAI_limiting_factors.pdf", width= 13, height = 8.5)
ggplot(data.graph) + facet_grid(Canopy.Class~State) +
	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	plot.rgb("MA", "C", 3) +
	plot.rgb("MA", "D", 3) +
	plot.rgb("MA", "I", 3) +
	plot.rgb("MA", "S", 3) +
		
	plot.rgb("ME", "C", 3) +
	plot.rgb("ME", "D", 3) +
	plot.rgb("ME", "I", 3) +
	plot.rgb("ME", "S", 3) +
		
	plot.rgb("MO", "C", 3) +
	plot.rgb("MO", "D", 3) +
	plot.rgb("MO", "I", 3) +
	plot.rgb("MO", "S", 3) +
	
	plot.rgb("IN", "C", 3) +
	plot.rgb("IN", "D", 3) +
	plot.rgb("IN", "I", 3) +
	plot.rgb("IN", "S", 3) +
	

	plot.rgb("OH", "C", 3) +
	plot.rgb("OH", "D", 3) +
	plot.rgb("OH", "I", 3) +
	plot.rgb("OH", "S", 3)+
	labs(title= "Canopy Class", x="Year", y = expression(bold(paste("BAI (mm"^"2", "y"^"-1",")")))) +
	poster.theme2
dev.off()		
# Just plotting the BAI fits
summary(data.graph)

ggplot(data.graph) + facet_wrap(group~Site) +
  scale_x_continuous(expand=c(0,0), name="Year") +
  scale_y_continuous(expand=c(0,0), name="BAI") +
  # facet_wrap(~TreeID, scales="free_y", space="free") +
  # geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  geom_line(aes(x=Year, y=fit.full), size=2, alpha=0.5) +
  geom_ribbon(aes(x=Year, ymin=fit.full.lwr, ymax=fit.full.upr), alpha=0.3)

# Plotting the Effects

pdf("figures/gam2_influence_in_time.pdf", width= 13, height = 8.5)
ggplot(data.graph) + facet_grid(Canopy.Class~Site) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="Effect on RW (in mm)") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=fit.tmean), size=2, color="red") +
	geom_line(aes(x=Year, y=fit.precip), size=2, color="blue") +
	geom_line(aes(x=Year, y=fit.dbh.recon), size=2, color="green")
	
dev.off()	



#---------------------------------------------------
# Plotting just MMF and Harvard for Ameridendro

sites.use2 <- c("Harvard", "Morgan Monroe State Park")

# Plotting the Obs and modeled with influence coloring
pdf("figures/gam2_canopyclass_BAI_limiting_factors_ameridendro.pdf", width= 13, height = 8.5)
ggplot(data.graph[data.graph$Site %in% sites.use2,]) + facet_grid(Canopy.Class~Site) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="BAI") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	plot.rgb("Harvard", "C", 3) +
	plot.rgb("Harvard", "D", 3) +
	plot.rgb("Harvard", "I", 3) +
	plot.rgb("Harvard", "S", 3) +

	
	plot.rgb("Morgan Monroe State Park", "C", 3) +
	plot.rgb("Morgan Monroe State Park", "D", 3) +
	plot.rgb("Morgan Monroe State Park", "I", 3) +
	plot.rgb("Morgan Monroe State Park", "S", 3) +
	poster.theme2
	

dev.off()		
# Just plotting the BAI fits
summary(data.graph)

ggplot(data.graph) + facet_wrap(group~Site) +
  scale_x_continuous(expand=c(0,0), name="Year") +
  scale_y_continuous(expand=c(0,0), name="BAI") +
  # facet_wrap(~TreeID, scales="free_y", space="free") +
  # geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  geom_line(aes(x=Year, y=fit.full), size=2, alpha=0.5) +
  geom_ribbon(aes(x=Year, ymin=fit.full.lwr, ymax=fit.full.upr), alpha=0.3)

# Plotting the Effects

pdf("figures/gam2_influence_in_time_ameridendro.pdf", width= 13, height = 8.5)
ggplot(data.graph[data.graph$Site %in% sites.use2,]) + facet_grid(Canopy.Class~Site) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="Effect on RW (in mm)") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=fit.tmean), size=2, color="red") +
	geom_line(aes(x=Year, y=fit.precip), size=2, color="blue") +
	geom_line(aes(x=Year, y=fit.dbh.recon), size=2, color="green") +
	poster.theme2
	
dev.off()	

