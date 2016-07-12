library(ggplot2)
library(car)
require(plyr)
require(ggplot2)
require(RColorBrewer)
require(reshape)

cbbPalette <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00")

cols <- brewer.pal(8,"Set1")
cols <- c(cols[2], cols[1], cols[3:8]) # inverts red/blue order

source("poster_theme.R")
# Doing some data exploration with the cliamte data.

data.use <- read.csv("processed_data/AllSites_tree_plus_climate_and_BA.csv", header=T)
summary(data.use)

sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

data.use <- data.use[data.use$Site %in% sites.use,]

summary(data.use[data.use$Site=="Howland","tmean"])

data.use$Site <- factor(data.use$Site, levels = c("Missouri Ozark", "Morgan Monroe State Park", "Oak Openings Toledo", "Harvard", "Howland"))


summary(data.use)
data.use <- data.use[data.use$Year >=1935,]

# data.use <- na.omit(data.use)
# data.use <- droplevels(data.use)

# Script that allows polygons to be drawn around the scatter cloud to show site conditions better.
df <- data.use
find_hull <- function(data.use) data.use[chull(data.use$tmean, data.use$precip),]
hulls <- ddply(df, "Site", find_hull)
summary(hulls)

	
	
fig2 <- ggplot(data=data.use, aes(tmean, precip, colour=Site, fill=Site)) + 
  geom_point() + 
  geom_hline(yintercept=0, colour="darkgrey") + 
  geom_vline(xintercept=0, colour="darkgrey") +
  scale_colour_manual("", values = cbbPalette) +
  scale_fill_manual("", values = cbbPalette) +
  #labs(x = "Temperature", y = "Precip")+
  labs(title = "Site Climate 1935-2012", x = "Mean GS Temp (˚C)", y = expression(bold(paste("Cumulative GS Precip (mm yr"^"-1",")"))))+
		 theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines"))+
		 theme(legend.position=c(0.2,0.85))+
		 poster.theme2+
		 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
        scale_x_continuous(limits=c(10, 30))+
        scale_y_continuous(limits=c(100, 1200))
  
  
fig2

# Fig. 2A
# Density lines
fig2a <- fig2 + geom_density2d(alpha=.5); fig2a

# Fig. 2B
# Convex hulls
# fig2b <- 

pdf("figures/Prelim_Figures/climate_space2.pdf", height = 8, width = 13)
fig2 + geom_polygon(data=hulls, alpha=.2)+
	labs(title = "Site Climate 1935-2012", x = "Mean GS Temp (˚C)", y = expression(bold(paste("Cumulative GS Precip (mm yr"^"-1",")"))))+
		 theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines"))+
		 theme(legend.position=c(0.2,0.85))+
		 poster.theme2+
		 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
        scale_x_continuous(limits=c(10, 30))+
        scale_y_continuous(limits=c(100, 1200))
dev.off()



pdf("figures/Prelim_Figures/climate_space.pdf", height = 8, width = 13)
ggplot(data=data.use[data.use$Year >=1935,]) +
	geom_point(aes(x=tmean, y=precip, color=Site, shape=Site), size=4)+
	scale_colour_manual("", values = cbbPalette) +
  scale_fill_manual("", values = cbbPalette) +
		
	labs(title = "Site Climate 1935-2012", x = "Mean GS Temp (˚C)", y = expression(bold(paste("Cumulative GS Precip (mm yr"^"-1",")"))))+
		 theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines"))+
		 theme(legend.position=c(0.85,0.85))+
		 poster.theme2+
		 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))
dev.off()

