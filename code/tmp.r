# save a copy
save.image(paste(wd,"mun-reelection.RData",sep=""))

# load image
rm(list = ls())
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data/"
setwd(dd)

load(paste(wd,"mun-reelection.RData",sep=""))
# drop incumbents before 1997
sel <- which(inc$yr<1997)
inc <- inc[-sel,]

options(width = 67)
