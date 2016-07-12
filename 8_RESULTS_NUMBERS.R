# Different Numbers for results section of Ch. 2 Manuscript
library(car)
#all input data used in GAMMS
load("ch2_combined_data_use.Rdata")
summary(test)

test$state <- recode(test$Site, c("'Missouri Ozark'='MO';'Morgan Monroe State Park'='IN';'Oak Openings Toledo'='OH';'Harvard'='MA';'Howland'='ME'"))
summary(test)

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
