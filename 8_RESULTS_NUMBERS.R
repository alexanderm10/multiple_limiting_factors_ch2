# Different Numbers for results section of Ch. 2 Manuscript
library(car)
#all input data used in GAMMS
load("ch2_combined_data_use.Rdata")
summary(test)

test$state <- recode(test$Site, c("'Missouri Ozark'='MO';'Morgan Monroe State Park'='IN';'Oak Openings Toledo'='OH';'Harvard'='MA';'Howland'='ME'"))
summary(test)


plot.data <-read.csv("raw_input_files/DOE_plus_valles.csv", header=T)
# summary(plot.data)

sites <- c("MO", "IN", "OH", "MA", "ME")
plot.data$Site..Tower. <- recode(plot.data$Site..Tower., "'Missouri Ozark'='MO';'Morgan Monroe State Park'='IN';'Oak Openings Toledo'='OH';'Harvard Forest (Tower Plot 1)'='MA';'Harvard Forest (Tower Plot 2)'='MA'; 'Howland'='ME'")
plot.data <- plot.data[plot.data$Site..Tower. %in% sites,]
plot.data$Site..Tower. <- droplevels(plot.data$Site..Tower.)

summary(plot.data)
density.data<- data.frame(Site = sites,
						  dens.mean = NA,
						  dens.sd = NA)
dim(density.data)
for(s in sites){
	density.data[density.data$Site==s, "dens.mean"] <- mean(plot.data[plot.data$Site..Tower.==s, "Density.Live..stems.ha."], na.rm=T)
	
	density.data[density.data$Site==s, "dens.sd"] <- sd(plot.data[plot.data$Site..Tower.==s, "Density.Live..stems.ha."], na.rm=T)
}
density.data




#############################################
# Site Demographic Characteristics
#############################################
#---------------------------------
# Dominant Trees

# MO
summary(unique(test[test$Canopy.Class=="D" & test$state=="MO", c("TreeID","group")]))
# IN
summary(unique(test[test$Canopy.Class=="D" & test$state=="IN", c("TreeID","group")]))
# OH
summary(unique(test[test$Canopy.Class=="D" & test$state=="OH", c("TreeID","group")]))
# MA
summary(unique(test[test$Canopy.Class=="D" & test$state=="MA", c("TreeID","group")]))
# ME
summary(unique(test[test$Canopy.Class=="D" & test$state=="ME", c("TreeID","group")]))

#---------------------------------
# Intermediate Trees
# MO
summary(unique(test[test$Canopy.Class=="I" & test$state=="MO", c("TreeID","group")]))
# IN
summary(unique(test[test$Canopy.Class=="I" & test$state=="IN", c("TreeID","group")]))
# 
summary(unique(test[test$Canopy.Class=="I" & test$state=="OH", c("TreeID","group")]))
# MA
summary(unique(test[test$Canopy.Class=="I" & test$state=="MA", c("TreeID","group")]))
# ME
summary(unique(test[test$Canopy.Class=="I" & test$state=="ME", c("TreeID","group")]))



#---------------------------------
# Suppressed Trees
# MO
summary(unique(test[test$Canopy.Class=="S" & test$state=="MO", c("TreeID","group")]))
# 
summary(unique(test[test$Canopy.Class=="S" & test$state=="IN", c("TreeID","group")]))
# OH
summary(unique(test[test$Canopy.Class=="S" & test$state=="OH", c("TreeID","group")]))
# MA
summary(unique(test[test$Canopy.Class=="S" & test$state=="MA", c("TreeID","group")]))
# ME
summary(unique(test[test$Canopy.Class=="S" & test$state=="ME", c("TreeID","group")]))


#############################################
# Site Climate Characteristics
#############################################

# Missouri 

summary(test[test$state=="MO",])


# Indiana
summary(test[test$state=="IN",])


# Ohio
summary(test[test$state=="OH",])


# Harvard
summary(test[test$state=="MA",])

# Howland
summary(test[test$state=="ME",])
