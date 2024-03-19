###################################################################################
## Luminosity                                                                    ##
## Read seccion-level 2020 populations and generate pop-weighted municipal stats ##
###################################################################################
cese <- read.csv("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/data/censo2020-se.csv")
cese <- cese[, c("edon","seccion","inegi","ife","POBTOT")] # trim columns
colnames(cese)[5] <- "ptot"
cese$seccion <- cese$edon*10000 + cese$seccion
head(cese)
##
## Gets state conversion function
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
source( paste(pth, "edo2edon.r", sep = "/") )
rm(pth)
##
## reads seccion-level 2018 luminosity data, adds 2020 population
lumu <- data.frame()
for (i in 1:32){
    ##i <- 2
    pth <- paste0("/home/eric/Downloads/Desktop/data/mapas/luminosity/data/secciones/", edon2edo(i), "/lum2018.csv")
    luse <- read.csv(pth)  ## read state's se-level luminosity
    luse$seccion <- luse$edon*10000 + luse$seccion
    luse <- merge(x = luse, y = cese[, c("seccion", "ife", "ptot")], by = "seccion", all.x = TRUE, all.y = FALSE) ## add ptot inegi
    luse$ife <- luse$edon*1000 + luse$ife
    luse$PTOT <- ave(luse$ptot, as.factor(luse$ife), FUN=function(x) sum(x, na.rm=TRUE)) ## sum municipal pops
    luse$meanwpop <- luse$mean * luse$ptot / luse$PTOT  ## step to generate meanwpop
    luse$meanwpop <- round(ave(luse$meanwpop, as.factor(luse$ife), FUN=function(x) sum(x, na.rm=TRUE)), 3) ## generate meanwpop
    tmp <- luse[duplicated(luse$ife)==FALSE, c("ife", "meanwpop")]
    lumu <- rbind(lumu, tmp)
    ##luse[1,]
}
summary(lumu)
##
ids <- merge(x = ids, y = lumu, by = "ife", all.x = TRUE, all.y = FALSE)
ids$lumwpop20 <- ids$meanwpop; ids$meanwpop <- NULL
ids[1,]


