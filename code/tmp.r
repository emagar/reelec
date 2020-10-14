######################################################
## term-limited / reelected or not through yrs plot ##
######################################################
vot <- vot.dup # duplicate for debud
#vot.dup <- vot # restore
vot$status <- NA
vot$status[vot$dtermlim==1] <- "1 term limited"
vot$status[vot$dtermlim==0] <- "3 can reelect next race"
vot$status[vot$race.current=="Reelected"] <- "2 reelected last race (term limited)"
#table(vot$yr,vot$status, useNA = "ifany")
# will receive state/yr sums
tmp1 <- data.frame(y2014=rep(NA,32),
                  y2015=rep(NA,32),
                  y2016=rep(NA,32),
                  y2017=rep(NA,32),
                  y2018=rep(NA,32),
                  y2019=rep(NA,32),
                  y2020=rep(NA,32))
tmp2 <- tmp3 <- tmp1
#
for (e in 1:32){
    for (y in 2014:2020){
        #e <- 1; y <- 2016 # debug
        #table(vot$edon,vot$yr) # debud
        tmp.ey <- vot[which(vot$edon==e & vot$yr==y),] # subset
        if (nrow(tmp.ey)>0){
            tmp1[e,(y-2013)] <- length(grep("1", tmp.ey$status)) # how many 1s
            tmp2[e,(y-2013)] <- length(grep("2", tmp.ey$status)) # how many 2s
            tmp3[e,(y-2013)] <- length(grep("3", tmp.ey$status)) # how many 3s
        }
    }
}
# extraordinarias to NA
tmp1[tmp1>0 & tmp1<5] <- NA
tmp2[tmp2>0 & tmp2<5] <- NA
tmp3[tmp3>0 & tmp3<5] <- NA
tmp3[21,6] <- NA
#
# df round 1 had no el
tmp1[9,1] <- tmp2[9,1] <- tmp3[9,1] <- 16
# all term limited round 1
tmp2[,1] <- tmp3[,1] <- 0
# cps has a mistake???
tmp1[7,2] <- 0
# gue has a mistake???
tmp1[12,2] <- 0
#
# columns should add to 2030 - 16 municipios round 1 (minus df)
tmp4 <- tmp1[,1] + tmp2[,1] + tmp3[,1]
tmp4[is.na(tmp4)] <- 0 # nmun
tmp5 <- table(vot$edon[vot$round==10])
tmp5 <- as.vector(tmp5) # frequencies
tmp5 <- c(tmp5[1:8],16,tmp5[9:31]) # add df
tmp4[tmp4!=tmp5] <- tmp5[tmp4!=tmp5]
tmp1[,1] <- tmp4 # fill tot mun
#
# fill NAs with left col
for (y in 2:7){
    #y <- 2 # debug
    tmp1[which(is.na(tmp1[,y])), y] <- tmp1[which(is.na(tmp1[,y])), (y-1)]
    tmp2[which(is.na(tmp2[,y])), y] <- tmp2[which(is.na(tmp2[,y])), (y-1)]
    tmp3[which(is.na(tmp3[,y])), y] <- tmp3[which(is.na(tmp3[,y])), (y-1)]
}
# columns should add to ~2030 municipios
tmp4 <- tmp1 + tmp2 + tmp3
colSums(tmp4)
#
# add usos object
tmp11 <- tmp1
tmp11[] <- 0
tmp11[20,] <- 427
tmp11[7,7] <- 1
tmp11[12,7] <- 1
tmp11[16,] <- 1
# add hgo ver object
tmp12 <- tmp1
tmp12[] <- 0
tmp12[c(13,30),] <- tmp1[c(13,30),]
tmp1[c(13,30),] <- 0
#
# pre reform make ver hgo std term lim
tmp1[c(13,30),1]  <- tmp12[c(13,30),1]
tmp12[c(13,30),1] <- 0
#
# yr aggs
#tmp4 <- tmp1 + tmp11 + tmp12 + tmp2 + tmp3 # with usos
tmp4 <- tmp1 + tmp12 + tmp2 + tmp3 # without usos
tmp1  <- colSums(tmp1)  / colSums(tmp4)
tmp2  <- colSums(tmp2)  / colSums(tmp4)
tmp3  <- colSums(tmp3)  / colSums(tmp4)
#tmp11 <- colSums(tmp11) / colSums(tmp4)
tmp12 <- colSums(tmp12) / colSums(tmp4)
#
# plot without usos
setwd(wd)
#pdf(file = "../graph/horizon-yrs.pdf", width = 7, height = 4)
library(RColorBrewer)
colors <- c(rev(brewer.pal(4, "Greens")),"gray")
par(mar = c(2.5,4,1,2)+.1) # bottom, left, top, right 
plot(c(.75,11.5), c(0,1), type = "n", axes = FALSE,
     xlab = "", ylab ="% municipalities")
axis(1, at=1:8, labels=c(2014,"'15","'16","'17","'18","'19","'20","'21*"))
axis(2, at=seq(0,1,.1), labels = FALSE)
axis(2, at=seq(0,1,.2), labels = seq(0,100,20))
abline(h=seq(0,1,.1), lty=1, col = "gray90")
abline(v=1.5, lty=2, col = "black")
text(x=1.65, y=.95, labels="Reform", col = "black", cex = .8, srt = 90)
# white box to cover hlines in right side
polygon(c(8.5,12,12,8.5),c(-3,-3,3,3), border = "white", col = "white")
#
y <- 1
    x <- c(y-.15,y+.15,y+.15,y-.15)
    y1 <- rep(0,2)
    y2 <- rep(1,2)
    polygon(x, c(y1, y2), col = "gray")
#
for (y in 2:7){
    #y <- 1
    x <- c(y-.15,y+.15,y+.15,y-.15)
    y1 <- rep(0,2)
    y2 <- rep(tmp2[y],2)
    polygon(x, c(y1, y2), col = colors[1])
    y1 <- y2
    y2 <- y2 + tmp3[y]
    polygon(x, c(y1, y2), col = colors[2])
    y1 <- y2
    y2 <- y2 + tmp1[y]
    polygon(x, c(y1, y2), col = colors[3])
    y1 <- y2
    y2 <- rep(1,2)
    polygon(x, c(y1, y2), col = "gray") # without usos
}
#
# add 2021 (exception won't be needed after the els take place)
tmptmp1 <- 0 # other than those who reelected, no incumbent term lim in 2021 and thereafter
tmptmp12 <- tmp12["y2020"] # non-reformers
tmptmp2 <- 7/colSums(tmp4)[7] # durango terms end 2022, 7 reelected 2019
tmptmp21 <- tmp3[7] # # indetermined category
tmptmp3 <- tmp2[7] - tmptmp2 + tmp1[7] # can run next round
y <- 8
x <- c(y-.15,y+.15,y+.15,y-.15)
y1 <- rep(0,2)
y2 <- rep(tmptmp2,2)
polygon(x, c(y1, y2), col = colors[1])
y1 <- y2
y2 <- y2 + tmptmp3
polygon(x, c(y1, y2), col = colors[2])
## y1 <- y2
## y2 <- y2 + tmptmp1
## polygon(x, c(y1, y2), col = colors[3])
y1 <- y2
y2 <- y2 + tmptmp21
polygon(x, c(y1, y2), col = colors[4])
y1 <- y2
y2 <- rep(1,2)
polygon(x, c(y1, y2), col = "gray") # without usos
#
# footnote
mtext("* forthcoming", side = 1, line = 1.5, outer = FALSE, adj = 1, cex = .7)
#
legend(x=8.5, y=.6,
       legend =  rev(c("reelected","can run again","term limited","to be determined","non-reformers")), # without usos
       fill =    rev(c(colors[1],colors[2],colors[3],colors[4],"gray")),                     # without usos
       bty = "n")
#dev.off()
#
## # plot including with usos (needs tmp11 colsums above)
## setwd(wd)
## #pdf(file = "../graph/horizon-yrs-wusos.pdf", width = 7, height = 4)
## par(mar = c(2.5,4,2,2)+.1) # bottom, left, top, right 
## plot(seq(.75,9.75,1), c(0,0,0,0,0,0,0,0,0,1), type = "n", axes = FALSE,
##      xlab = "", ylab ="% municipalities")
## axis(1, at=1:8, labels=2014:2021)
## axis(2, at=seq(0,1,.1), labels = FALSE)
## axis(2, at=seq(0,1,.2), labels = seq(0,100,20))
## for (y in 1:7){
##     #y <- 1
##     x <- c(y-.15,y+.15,y+.15,y-.15)
##     y1 <- rep(0,2)
##     y2 <- rep(tmp2[y],2)
##     polygon(x, c(y1, y2), col = "black")
##     y1 <- y2
##     y2 <- rep(tmp2[y]+tmp3[y],2)
##     polygon(x, c(y1, y2), col = "gray")
##     y1 <- y2
##     y2 <- rep(tmp2[y]+tmp3[y]+tmp1[y],2)
##     polygon(x, c(y1, y2), col = "white")
##     y1 <- y2
##     y2 <- rep(tmp2[y]+tmp3[y]+tmp1[y]+tmp12[y],2)
##     polygon(x, c(y1, y2), col = "black", density = 10, angle = -45) # with usos
##     y1 <- y2
##     y2 <- rep(1,2)
##     polygon(x, c(y1, y2), col = "black", density = 10, angle = 45) # usos
## }
## legend(x=7.5, y=.6,
##        legend =  rev(c("reelected","can run again","term limited","non-reformers","unelected")), # with usos
##        fill =    rev(c("black","gray","white","black","black")),                                 # with usos
##        angle =   rev(c(NA,NA,NA,-45,45)),                                                        # with usos
##        density = rev(c(NA,NA,NA,10,10)),                                                         # with usos
##        bty = "n")
## #dev.off()
#
################################################################
## term-limited / reelected or not through yrs plot ENDS HERE ##
################################################################
