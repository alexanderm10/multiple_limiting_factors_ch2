library(ggplot2)
library(car)
load(file="processed_data/gamm_weights/gam4_weights.Rdata")

summary(gam4.weights)
factors.fits <- c("fit.tmean", "fit.precip", "fit.dbh.recon", "fit.full", "BA.inc")
factors.weights <- c("weight.tmean", "weight.dbh.recon", "weight.precip")

# Transforming things back to BA.inc rather than log
gam4.weights[,which(substr(names(gam4.weights),1,3)=="fit")] <- exp(gam4.weights[,which(substr(names(gam4.weights),1,3)=="fit")] )

othervars <- c("Year", "Site","Model")

data.graph1 <- aggregate(gam4.weights[,factors.fits], by = gam4.weights[,othervars], FUN= mean, na.rm=T)

data.graph1[,paste(factors.fits, "upr", sep=".")] <- aggregate(gam4.weights[,factors.fits], by = gam4.weights[,othervars], FUN= quantile, prob= 0.975, na.rm=T)[,factors.fits]

data.graph1[,paste(factors.fits, "lwr", sep=".")] <- aggregate(gam4.weights[,factors.fits], by = gam4.weights[,othervars], FUN= quantile, prob= 0.025, na.rm=T)[,factors.fits]

summary(data.graph1)

data.graph2 <- aggregate(abs(gam4.weights[,factors.weights]), by = gam4.weights[,othervars], FUN= mean, na.rm=T)

data.graph2[,paste(factors.weights, "upr", sep=".")] <- aggregate(abs(gam4.weights[,factors.weights]), by = gam4.weights[,othervars], FUN= quantile, prob= 0.975, na.rm=T)[,factors.weights]

data.graph2[,paste(factors.weights, "lwr", sep=".")] <- aggregate(abs(gam4.weights[,factors.weights]), by = gam4.weights[,othervars], FUN= quantile, prob= 0.025, na.rm=T)[,factors.weights]

summary(data.graph2)

data.graph <- merge(data.graph1, data.graph2, all.x=T, all.y=T)

# data.graph <- gam4.weights[gam4.weights$TreeID== "MMA014",]
summary(data.graph)
gam4.weights$wts.check <- rowSums(abs(gam4.weights[,c("weight.tmean", "weight.precip", "weight.dbh.recon")]))
data.graph$wts.check <- rowSums(abs(data.graph[,c("weight.tmean", "weight.precip", "weight.dbh.recon")]))

summary(gam4.weights)
summary(data.graph)

# Ordering the data for graphing

data.graph<- data.graph[order(data.graph$Year, data.graph$group, data.graph$Site, decreasing=F),]
summary(data.graph)

plot.rgb <- function(STATE,SIZE){	geom_line(data=data.graph[data.graph$State==STATE,],aes(x=Year, y=fit.full), size=SIZE,
  		        color=rgb(abs(data.graph[data.graph$State==STATE,"weight.tmean"     ]), # red
                        abs(data.graph[data.graph$State==STATE,"weight.dbh.recon"     ]), # green
                        abs(data.graph[data.graph$State==STATE,"weight.precip"   ]))) }   # blue

# Plotting the Obs and modeled with influence coloring
data.graph$State <- recode(data.graph$Site, "'Howland' = 'ME';'Harvard' = 'MA';'Morgan Monroe State Park' = 'IN';'Missouri Ozark' = 'MO';'Oak Openings Toledo' = 'OH'")
data.graph$State <- factor(data.graph$State, levels=c("MO", "IN", "OH", "MA", "ME"))

# summary(data.graph[!data.graph$group %in% c("BETULA", "CARYA", "FAGR", "FRAX", "SAAL"),])

pdf("figures/gam4_SITE_limiting_factor.pdf", width= 13, height = 8.5)
ggplot(data.graph) + facet_grid(State~.) +
	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam4.weights[gam4.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	plot.rgb("MA", 3) +
	plot.rgb("ME", 3) +
	plot.rgb("OH", 3) +
	plot.rgb("IN", 3) +
	plot.rgb("MO", 3) +
	
	labs(title= "Site", x="Year", y = expression(bold(paste("BAI (mm"^"2", "y"^"-1",")")))) +
	poster.theme2
dev.off()
	
# Just plotting the BAI fits
summary(data.graph)
ggplot(data.graph) + facet_wrap(group~Site) +
  scale_x_continuous(expand=c(0,0), name="Year") +
  scale_y_continuous(expand=c(0,0), name="BAI") +
  # facet_wrap(~TreeID, scales="free_y", space="free") +
  # geom_ribbon(data=gam4.weights[gam4.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  geom_line(aes(x=Year, y=fit.full), size=2, alpha=0.5) +
  geom_ribbon(aes(x=Year, ymin=fit.full.lwr, ymax=fit.full.upr), alpha=0.3)


summary(data.graph)
data.graph$State <- factor(data.graph$State, levels=c("MO", "IN", "OH", "MA", "ME"))

# Plotting the Effects
pdf("figures/gam4_influence_in_time.pdf", width= 13, height = 8.5)

ggplot(data.graph) + facet_grid(State~.) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="Effect on RW (in mm)") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam4.weights[gam4.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=fit.tmean), size=1, color="red") +
	geom_line(aes(x=Year, y=fit.precip), size=1, color="blue") +
	geom_line(aes(x=Year, y=fit.dbh.recon), size=1, color="green")+
	ylim(c(0,5))+
	labs(title= "Site", x="Year", y = "Model Fits") +
	poster.theme2
dev.off()

#------------------------------------
# # Just MMF and Harvard


# summary(data.graph)
# sites.use2 <- c("Harvard", "Morgan Monroe State Park")

# pdf("figures/gam4_Species_BAI_limiting_factor_ameridendro.pdf", width= 13, height = 8.5)
# ggplot(data.graph[data.graph$Site %in% sites.use2,]) + facet_grid(group~Site) +
	# scale_x_continuous(expand=c(0,0), name="Year") +
	# scale_y_continuous(expand=c(0,0), name="BAI") +
	# # facet_wrap(~TreeID, scales="free_y", space="free") +
	# # geom_ribbon(data=gam4.weights[gam4.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	# geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	# geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	# geom_line(aes(x=Year, y = 0), linetype="dashed") +
	# plot.rgb("Harvard", "ACRU", 3) +
	# plot.rgb("Harvard", "PIST", 3) +
	# plot.rgb("Harvard", "TSCA", 3) +
	# plot.rgb("Harvard", "QURU", 3) +
	# plot.rgb("Harvard", "QUVE", 3) +
	# plot.rgb("Harvard", "FRAX", 3) +
	# plot.rgb("Harvard", "BETULA", 3) +
	# plot.rgb("Harvard", "ACSA", 3) +
	# plot.rgb("Harvard", "QUAL", 3) +
	# plot.rgb("Harvard", "FAGR", 3) +
	# plot.rgb("Harvard", "ULRU", 3) +
	# plot.rgb("Harvard", "CARYA", 3) +
	# plot.rgb("Harvard", "SAAL", 3) +
	
	
	# plot.rgb("Morgan Monroe State Park", "ACRU", 3) +
	# plot.rgb("Morgan Monroe State Park", "PIST", 3) +
	# plot.rgb("Morgan Monroe State Park", "TSCA", 3) +
	# plot.rgb("Morgan Monroe State Park", "QURU", 3) +
	# plot.rgb("Morgan Monroe State Park", "QUVE", 3) +
	# plot.rgb("Morgan Monroe State Park", "FRAX", 3) +
	# plot.rgb("Morgan Monroe State Park", "BETULA", 3) +
	# plot.rgb("Morgan Monroe State Park", "ACSA", 3) +
	# plot.rgb("Morgan Monroe State Park", "QUAL", 3) +
	# plot.rgb("Morgan Monroe State Park", "FAGR", 3) +
	# plot.rgb("Morgan Monroe State Park", "ULRU", 3) +
	# plot.rgb("Morgan Monroe State Park", "CARYA", 3) +
	# plot.rgb("Morgan Monroe State Park", "SAAL", 3) +
	# poster.theme2


# dev.off()
	
# # Just plotting the BAI fits
# summary(data.graph)
# ggplot(data.graph[data.graph$Site %in% sites.use2,]) + facet_wrap(group~Site) +
  # scale_x_continuous(expand=c(0,0), name="Year") +
  # scale_y_continuous(expand=c(0,0), name="BAI") +
  # # facet_wrap(~TreeID, scales="free_y", space="free") +
  # # geom_ribbon(data=gam4.weights[gam4.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  # geom_line(aes(x=Year, y=fit.full), size=2, alpha=0.5) +
  # geom_ribbon(aes(x=Year, ymin=fit.full.lwr, ymax=fit.full.upr), alpha=0.3)

# summary(data.graph)
# # Plotting the Effects
# pdf("figures/gam4_influence_in_time_ameridendro.pdf", width= 13, height = 8.5)

# ggplot(data.graph[data.graph$Site %in% sites.use2,]) + facet_grid(group~Site) +
	# scale_x_continuous(expand=c(0,0), name="Year") +
	# scale_y_continuous(expand=c(0,0), name="Effect on RW (in mm)") +
	# # facet_wrap(~TreeID, scales="free_y", space="free") +
	# # geom_ribbon(data=gam4.weights[gam4.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	# geom_line(aes(x=Year, y=fit.tmean), size=1, color="red") +
	# geom_line(aes(x=Year, y=fit.precip), size=1, color="blue") +
	# geom_line(aes(x=Year, y=fit.dbh.recon), size=1, color="green")+ 
	# poster.theme2
# dev.off()

gam4.data.graph <- data.graph
save(gam4.data.graph, file="processed_data/gam4_graph_data.Rdata")
