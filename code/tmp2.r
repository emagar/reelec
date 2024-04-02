## reads seccion-level 2018 luminosity data, adds 2020 population
lumu <- data.frame()
for (i in 1:32){
    for (j in 1997:2018){
    ##i <- 2
    ##j <- 2018
    pth <- paste0("/home/eric/Downloads/Desktop/data/mapas/luminosity/data/secciones/", edon2edo(i), "/lum", j, ".csv")
    luse <- read.csv(pth)  ## read state's se-level luminosity
    luse$yr <- j
    luse$seccion <- luse$edon*10000 + luse$seccion
    luse <- merge(x = luse, y = cese[, c("seccion", "ife", "ptot")], by = "seccion", all.x = TRUE, all.y = FALSE) ## add ptot inegi
    luse$ife <- luse$edon*1000 + luse$ife
    luse$PTOT <- ave(luse$ptot, as.factor(luse$ife), FUN=function(x) sum(x, na.rm=TRUE)) ## sum municipal pops
    luse$meanwpop <- luse$mean * luse$ptot / luse$PTOT  ## step to generate meanwpop
    luse$meanwpop <- round(ave(luse$meanwpop, as.factor(luse$ife), FUN=function(x) sum(x, na.rm=TRUE)), 3) ## generate meanwpop
    tmp <- luse[duplicated(luse$ife)==FALSE, c("ife", "yr", "meanwpop")]
    lumu <- rbind(lumu, tmp)
    ##luse[1,]
}
summary(lumu)
