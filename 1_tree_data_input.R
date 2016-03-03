library(car)
library(dplR)
se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}


#################################################################################################
# Loading up .csv file that has meta data and RWL files for ring widths
# Also doing some housekeeping (unit conversions, name formats) up front to make the workflow smoother
#################################################################################################

#load in core details data sheet.  Has living/dead, pith info, measurement info.
#loading the dplR to use the basal area reconstruction functions.
core.data <- read.csv("raw_input_files/DOE_AllSites_core_data.csv", na.strings=c("", "NA", "#VALUE!", "*", " "), header=T)
summary(core.data)



write.csv(core.data, file="processed_data/core_data.csv", row.names=F)

#importing the diameter files of all trees sampled: includes tree id, spp, plot assignment, and DBH 
tree.data <- read.csv("raw_input_files/DOE_AllSitesTreeData.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)

summary(tree.data)


##################################################################
# Importing ring widths
##################################################################
# Load in Ross DOE ring widths from the east
# importing ring widths of dated samples 

tree.rw <- read.csv("raw_input_files/DOE_AllSites_Gapfilled.csv", header=T)

summary(tree.rw)

##################################################################
# Getting dbh reconstruction
##################################################################

dbh.recon <- read.csv("processed_data/DOE_Allsites_GapFilling_DBHrecon_ALL.csv", header=T, row.names=1)

summary(dbh.recon)
row.names(dbh.recon)


dbh.recon.stack <- stack(dbh.recon)
summary(dbh.recon.stack)
names(dbh.recon.stack) <- c("dbh.recon", "TreeID")
dbh.recon.stack$Year <- as.numeric(paste(row.names(dbh.recon)))
summary(tree.rw)

data.use <- merge(tree.rw, dbh.recon.stack, all.x=T, all.y=F) 
summary(data.use)

write.csv(data.use, "processed_data/tree_data_use.csv", row.names=F)

