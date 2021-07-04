rm(list=ls())

source("~/Dropbox/data/useful-functions/myxtab.r")


setwd("~/Desktop/MXelsCalendGovt/elecReturns/data")

d <- read.csv(file = "dfdf1997-present-incumbents.csv", stringsAsFactors = FALSE)

sel <- grep("Reran-out-p-lost", d$race.after)
d$race.after[sel] <- "Out-p-lost"

# keep dead incumbent info
sel <- grep("Dead", d$race.after, ignore.case = TRUE)
d$ddead <- 0
d$ddead[sel] <- 1
# recode race.after
d$race.after[sel] <- gsub("Dead-term-limited", "Term-limited", d$race.after[sel])
d$race.after[sel] <- gsub("Dead-reran-"      , "Out-"        , d$race.after[sel])
d$race.after[sel] <- gsub("Dead-"            , "Out-"        , d$race.after[sel])
#table(d$race.after[sel])
#table(d$race.after)
rm(sel)

# manipulate cases where incumbent/reelected switched parties so coded as party won/lost accordingly
table(d$yr, d$race.after)
sel1 <- grep("Reelected-dif-p", d$race.after)
#d$emm[sel1] # debug
# subtract one round from emm codes
tmp <- d$emm[sel1]
tmp1 <- sub(           "^([a-z]+[-])([0-9]{2})([.][0-9]{2}.)$", "\\1", tmp, perl = TRUE)
tmp2 <- as.numeric(sub("^([a-z]+[-])([0-9]{2})([.][0-9]{2}.)$", "\\2", tmp, perl = TRUE))
tmp3 <- sub(           "^([a-z]+[-])([0-9]{2})([.][0-9]{2}.)$", "\\3", tmp, perl = TRUE)
# select next round obs
tmp4 <- paste(tmp1, tmp2+1, tmp3, sep = "")
sel2 <- which(d$emm %in% tmp4)
# paste switched-to party as incumbent's original
d$part[sel1] <- d$part[sel2]
# change race.after
d$race.after[sel1] <- "Reelected"
# select previous round obs
tmp4 <- paste(tmp1, tmp2-1, tmp3, sep = "")
sel2 <- which(d$emm %in% tmp4)
## # This block will be needed from *2024 on*, examples from mu.incumbents
## # change previous race.after when needed to make time-series consistent with above recode BY HAND
## data.frame(emm.pre=d$emm[sel2], win.pre=d$part[sel2], race.after.pre=d$race.after[sel2], win=d$part[sel1])
## sel <- which(d$emm=="cps-15.100"); d$race.after[sel] <- "Term-limited-p-lost"
## sel <- which(d$emm=="cua-15.004"); d$race.after[sel] <- "Term-limited-p-won"
## sel <- which(d$emm=="cua-15.004"); d$race.after[sel] <- "Term-limited-p-won"
#
# manipulate cases where incumbent/beaten switched parties so coded as party won/lost accordingly
sel1 <- grep("Reran-beaten-dif-p", d$race.after)
if (length(sel1)>0) {
    # fish party from note
    d$emm[sel1]
    d$note[sel1]
    tmp <- sub("only ", "", d$note[sel1]) # remove "only"
    tmp1 <- sub("^reran under (.+) and lost$", "\\1", tmp)
    # paste new party as the original
    d$win[sel1] <- tmp1
    # change race.after
    d$race.after[sel1] <- "Reran-beaten"
}
# clean
rm(tmp,tmp1,tmp2,tmp3,tmp4,sel1,sel2)

table(d$yr, d$race.after)

# recode labels so they appear win/lose in table
library(plyr)
d$race.after <- mapvalues(d$race.after, from = c("Out-p-won","Out-p-lost","Reelected","Reran-beaten"), to = c("3Out-p-won","4Out-p-lost","1Reelected","2Reran-beaten"))


# inspect reelection 2021
tmp <- d[d$yr==2018,]
table(tmp$part)
#sel <- which(tmp$part %in% c("indep","loc/oth","pes","pna","pt"))
#tmp$part[sel] <- "other"
#tmp$part <- factor(tmp$part, levels=    c("pan","pri","prd","morena","pvem","mc","pt","pes"))
tmp$part <- factor(tmp$part, levels=rev(c("pan","pri","prd","morena","pvem","mc","pt","pes")), labels=rev(c("PAN","PRI","PRD","MORENA","PVEM","MC","PT","PES")))
## # debug
## sel <- grep("^Reran-out$", tmp$race.after)
## tmp$emm[sel]

tmp2 <- myxtab(tmp$part, tmp$race.after, pct=TRUE, rel=TRUE, digits=2, marginals = 1)
tmp2
#
tmp3 <- myxtab(tmp$part,tmp$race.after, pct=FALSE, rel=FALSE, digits=0, marginals = 1)
colSums(tmp3)
tmp4 <- c(round(colSums(tmp3)*100/300,2), colSums(tmp3)[5])
tmp2 <- rbind(Todos=tmp4, tmp2)
tmp2

tmp2 <- tmp2[order(-tmp2[,1],-tmp2[,2]),]

library(RColorBrewer)
#pdf(file     = "../graph/reel-dipfed2021.pdf", width = 7, height = 5)
#png(filename = "../graph/reel-dipfed2021.png", width = 700, height = 480)
clr <- brewer.pal(n=6, name = 'Paired'); clr <- clr[c(4,3,6,5,2,1)]
par(mar = c(2,0,1.2,0)+.1) # bottom, left, top, right 
plot(x = c(-9,105), y = c(0.4,nrow(tmp2)+1), type = "n", main = "Diputados federales uninominales 2021", axes = FALSE)
axis(1, at=seq(0,100,10),label=FALSE)
axis(1, at=seq(0,100,20),labels=c(seq(0,80,20),"100%"),cex.axis=.9)
polygon(x=c(-20,-20,120,120), y=c(4,5,5,4),col="gray85",border="gray85")
abline(h=1:(nrow(tmp2)+1), lty = 3)
#abline(h=4:5, lty = 1)
for (i in 1:nrow(tmp2)){
    #i <- 2
    l <- c(0,0); r <- rep(tmp2[i,1],2)
    polygon(y = c(i+1/6, i+5/6, i+5/6, i+1/6), x = c(l,r), col = clr[1], border = clr[1])
    if (tmp2[i,1]>.5) text(y = i+1/2, x = (l+r)[1]/2, labels = paste0(round(tmp2[i,1]),"%"), cex = .67, col = "white")
    l <- r; r <- r+rep(tmp2[i,2],2)
    polygon(y = c(i+1/6, i+5/6, i+5/6, i+1/6), x = c(l,r), col = clr[2], border = clr[2])
    if (tmp2[i,2]>.5) text(y = i+1/2, x = (l+r)[1]/2, labels = paste0(round(tmp2[i,2]),"%"), cex = .67, col = "gray50")
    l <- r; r <- r+rep(tmp2[i,3],2)
    polygon(y = c(i+1/6, i+5/6, i+5/6, i+1/6), x = c(l,r), col = clr[5], border = clr[5])
    if (tmp2[i,3]>.5) text(y = i+1/2, x = (l+r)[1]/2, labels = paste0(round(tmp2[i,3]),"%"), cex = .67, col = "white")
    l <- r; r <- r+rep(tmp2[i,4],2)
    polygon(y = c(i+1/6, i+5/6, i+5/6, i+1/6), x = c(l,r), col = clr[6], border = clr[6])
    if (tmp2[i,4]>.5) text(y = i+1/2, x = (l+r)[1]/2, labels = paste0(round(tmp2[i,4]),"%"), cex = .67, col = "gray50")
    #l <- r; r <- r+rep(tmp2[i,5],2)
    #polygon(y = c(i+1/6, i+5/6, i+5/6, i+1/6), x = c(l,r), col = clr[3], border = clr[3])
    #if (tmp2[i,5]>.5) text(y = i+1/2, x = (l+r)[1]/2, labels = paste0(round(tmp2[i,5]),"%"), cex = .67, col = "white")
    #l <- r; r <- r+rep(tmp2[i,6],2)
    #polygon(y = c(i+1/6, i+5/6, i+5/6, i+1/6), x = c(l,r), col = clr[4], border = clr[4])
    #if (tmp2[i,6]>.5) text(y = i+1/2, x = (l+r)[1]/2, labels = paste0(round(tmp2[i,6]),"%"), cex = .67, col = "gray50")
}
text(x=-7 , y=c(1:nrow(tmp2))+.5, labels = rownames(tmp2), cex = .85)#, srt = 90)
text(x=105, y=c(1:nrow(tmp2))+.5, labels = paste0("N=", tmp2[,6]), cex = .75)
#legend(x = 0, y = 0.75, legend = c("Ocupante reelecto","derrotado","Silla vacía ganó","perdió","Term limit ganó","perdió"), fill = clr, cex = .67, border = clr, bty = "n", horiz = TRUE)
## legend(x = 0,  y = 0.85, legend = c("reelecto","derrotado"), title = "Ocupante contendió"  , fill = clr[1:2], cex = .85, border = clr[1:2], bty = "n", horiz = TRUE)
## legend(x = 40, y = 0.85, legend = c("ganó","perdió")       , title = "Silla vacía, partido", fill = clr[3:4], cex = .85, border = clr[3:4], bty = "n", horiz = TRUE)
## legend(x = 75, y = 0.85, legend = c("ganó","perdió")       , title = "Term limit, partido" , fill = clr[5:6], cex = .85, border = clr[5:6], bty = "n", horiz = TRUE)
legend(x = 15,  y = 0.85, legend = c("reelecto","derrotado"), title = "Ocupante contendió"  , fill = clr[1:2], cex = .85, border = clr[1:2], bty = "n", horiz = TRUE)
legend(x = 60, y = 0.85, legend = c("ganó","perdió")       , title = "Silla vacía, partido", fill = clr[5:6], cex = .85, border = clr[5:6], bty = "n", horiz = TRUE)
text(x = 105, y = .85, "@emagar", col = "gray", cex = .7)
#dev.off()



d$part <- factor(d$part, levels=c("pan","pri","prd","morena","pvem","mc","pt","pes"))

sel <- which(d$yr==2018)

myxtab(c=d$race.after[sel],     r=d$part[sel], rel=TRUE, pct=TRUE, digits = 0)

colSums(myxtab(c=d$race.after[sel],     r=d$part[sel], rel=FALSE,          digits = 0))

round(colSums(myxtab(c=d$race.after[sel],     r=d$part[sel], rel=FALSE,          digits = 0))*100/300)



