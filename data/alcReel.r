wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data/"
setwd(wd)

d <- read.csv("alcaldesReelectos2018.csv", stringsAsFactors = FALSE)

sel <- which(d$reelecto=="no")
d$dreel <- 1
d$dreel[sel] <- 0

nrow(d) 

t <- table(d$reelecto)
t
round(t/nrow(d),2)

sel <- which(d$mun=="Monterrey")
d[sel,]

table(d$edo, d$dreel)

table(d$part2018)

# recode party
d$part <- d$part2015
# keep 2018 party where known
sel <- which(d$part2018!="")
d$part[sel] <- d$part2018[sel]

sel <- grep("indep", d$part)
d$part[sel] <- "indep"
sel <- grep("pan",d$part) 
d$part[sel] <- "pan/coal"
sel <- grep("pri",d$part) 
d$part[sel] <- "pri/coal"
sel <- grep("morena",d$part) 
d$part[sel] <- "morena/coal"
sel <- grep("prd mc",d$part) 
d$part[sel] <- "prd"
sel <- grep("pmc",d$part) 
d$part[sel] <- "mc"
sel <- grep("pvem-na",d$part) 
d$part[sel] <- "pvem"
sel <- grep("psd|pudc",d$part) 
d$part[sel] <- "local"

table(d$part, useNA = "ifany")
table(d$part, d$dreel)

# género
table(d$mujer)
sel <- grep("[Nn]o", d$mujer)
d$mujer[sel] <- "hombre"
sel <- grep("[Ss][ií]", d$mujer)
d$mujer[sel] <- "mujer"

table(d$mujer, d$dreel)


d[,c("edo","mun","alcalde","part","reelecto","mujer")]

