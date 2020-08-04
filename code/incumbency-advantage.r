rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
gd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/data/"
setwd("/home/eric/Desktop/MXelsCalendGovt/reelec/data/")

options(width = 130)

# load incumbent data since 1989
inc <- read.csv(paste(dd, "aymu1989-present.incumbents.csv", sep = ""), stringsAsFactors = FALSE)
# drop before 1997
sel <- which(inc$yr<1997)
inc <- inc[-sel,]

# load municipal votes
vot <- read.csv(paste(dd, "aymu1989-present.coalAgg.csv", sep = ""), stringsAsFactors = FALSE)
vot[1,]
vot$fuente <- vot$notas <- vot$tot <- vot$nr <- vot$nulos <- NULL
# drop before 1997
sel <- which(vot$yr<1997)
vot <- vot[-sel,]

# keep runoffs only in cases where first round was held in san luis potosí
sel <- grep("san-[0-9]+b", vot$emm) # these are first round races that led to runoff
vot <- vot[-sel,]

## # drop runoffs in san luis potosí, keeping winner
## sel.r1 <- grep("san-[0-9]+b", vot$emm) # these are first round races that led to runoff
## # tmp is emm without the "b"
## tmp <- vot$emm[sel.r1]
## tmp <- sub(pattern = "(san-[0-9]+)b([.][0-9]+)", replacement = "\\1\\2", tmp)
## sel.r2 <- which(vot$emm %in% tmp) # second round cases
## ##data.frame(vot$emm[sel.r1], vot$emm[sel.r2]) # verify match
## # replace first-round vote keeping winner
## vot$win[sel.r1] <- vot$win[sel.r2]
## vot$emm[sel.r1] <- tmp
## # drop runoffs
## vot <- vot[-sel.r2,]
## rm(sel.r1, sel.r2)

vot[1,]

# drop cases that became usos y costumbres
tmp <- c(1, 3, 8, 11, 12, 15, 17:20, 22, 24, 27, 29, 31, 33, 35:38, 42, 45:48,
         50, 51, 54, 58, 60:65, 69, 71, 72, 74, 76:78, 83:87, 91:93, 94:98,
         99:101, 104:111, 113:115, 117:123, 125:129, 131:133, 135, 137, 138:140,
         142, 144:149, 151:156, 158, 159, 161:165, 167, 170, 172:176, 178, 179, 183, 186,
         189:197, 200:224, 226:231, 233:236, 238:244, 246:253, 255:258, 260, 262:276, 279:281,
         282:284, 286:289, 291:293, 296, 297, 299, 301, 303, 304,
         306, 311, 313, 314:318, 320:323, 325, 326, 328:333, 335:337, 340:344, 346, 347, 349:359,
         361:363, 365, 366, 368:374, 376, 378:380, 382:384, 386, 388, 389, 391:396, 398, 399,
         401, 403:412, 416, 419, 420, 422:426, 428:430, 432, 433, 435, 437, 438, 440, 442:446,
         448:454, 457, 458, 460, 461, 463:466, 468, 470, 471, 473, 475:481, 487, 488, 490:504,
         506, 509:512, 514, 516:519, 521:524, 526:536, 538, 541:544, 546:548, 550, 552:554,
         556, 560:564, 566, 568, 569, 16, 82, 310, 348, 367, 400)
# ojo: 88 returned from uyc in 2013
sel <- which(vot$edon==20 & vot$munn %in% tmp)
## tmp <- vot$emm[sel]
## tmp[order(tmp)]
if (length(sel)>0) vot <- vot[-sel,]

# drop hidalgo 2020, not yet here
sel <- grep("hgo-16", vot$emm)
vot <- vot[-sel,]

# drop void elections
sel <- grep("[0-9]+a[.]", vot$emm) # anuladas
#vot$emm[sel]
vot <- vot[-sel,]

# re-compute efec
sel <- grep("v[0-9]+", colnames(vot))
v <- vot[,sel]
tmp <- which(rowSums(v) - vot$efec !=0)
vot[tmp,] # cases where efec does not match sum
vot$efec <- rowSums(v) # re-compute
# vote shares
v <- v/rowSums(v)
vot[,sel] <- v
rm(v)

## # win matches does not match l01 in san luis runoff cases if those were dropped above, therefore l01 and win should be retained
## sel <- which(vot$win != vot$l01)
## vot$emm[sel] # all should be san luis potosi

## # lo usé para detectar casos en aymu.incumbents con info que no correspondía con aymu.coalAgg
## sel <- c("emm", "edon", "mun", "munn", "ife", "inegi", "yr", "dy", "mo", "win")
## i <- merge(x = inc, y = vot[,sel],
##            by = c("emm", "edon", "mun", "munn", "ife", "inegi", "yr", "mo", "dy", "win"),
##            all = TRUE)
## write.csv(i, file = paste(dd, "tmp.csv", sep = ""), row.names = FALSE)

# prepare object with pan pri left morena oth votes
v5 <- vot
v5$ord <- v5$mun <- v5$munn <- v5$inegi <- v5$ife <- v5$status <- v5$dy <- v5$mo <- v5$ncand <- v5$dcoal <- v5$ncoal <- v5$win <- v5$efec <- v5$lisnom <- NULL # remove unneeded cols
v5$status <- NA
# narrow v01..v14 into long vector
v5$n <- 1:nrow(v5)
v5$r <- 1
v5$v <- v5$v01
v5$l <- v5$l01
#
tmp.orig <- v5 # duplicate
tmp <- tmp.orig
tmp$r <- 2
tmp$v <- tmp$v02; tmp$l <- tmp$l02;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 3
tmp$v <- tmp$v03; tmp$l <- tmp$l03;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 4
tmp$v <- tmp$v04; tmp$l <- tmp$l04;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 5
tmp$v <- tmp$v05; tmp$l <- tmp$l05;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 6
tmp$v <- tmp$v06; tmp$l <- tmp$l06;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 7
tmp$v <- tmp$v07; tmp$l <- tmp$l07;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 8
tmp$v <- tmp$v08; tmp$l <- tmp$l08;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 9
tmp$v <- tmp$v09; tmp$l <- tmp$l09;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 10
tmp$v <- tmp$v10; tmp$l <- tmp$l10;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 11
tmp$v <- tmp$v11; tmp$l <- tmp$l11;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 12
tmp$v <- tmp$v12; tmp$l <- tmp$l12;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 13
tmp$v <- tmp$v13; tmp$l <- tmp$l13;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 14
tmp$v <- tmp$v14; tmp$l <- tmp$l14;
v5 <- rbind(v5,tmp)
#
v5$v01 <- v5$v02 <- v5$v03 <- v5$v04 <- v5$v05 <- v5$v06 <- v5$v07 <- v5$v08 <- v5$v09 <- v5$v10 <- v5$v11 <- v5$v12 <- v5$v13 <- v5$v14 <- NULL
v5$l01 <- v5$l02 <- v5$l03 <- v5$l04 <- v5$l05 <- v5$l06 <- v5$l07 <- v5$l08 <- v5$l09 <- v5$l10 <- v5$l11 <- v5$l12 <- v5$l13 <- v5$l14 <- NULL
#
v5$oth <- v5$morena <- v5$prd <- v5$pri <- v5$pan <- 0
v5$dmajcoal <- 0 # will indicate major party coalitions
#
rm(tmp, tmp.orig)
#
# deal with major-party coalition below
sel <- grep("(?=.*pan)(?=.*prd)", v5$l, perl = TRUE)
v5$status[sel] <- "majors"
sel <- grep("(?=.*pan)(?=.*pri)", v5$l, perl = TRUE)
v5$status[sel] <- "majors"
sel <- grep("(?=.*pri)(?=.*prd)", v5$l, perl = TRUE)
v5$status[sel] <- "majors"
#
sel1 <- which(is.na(v5$status))
sel <- grep("pan-|-pan|^pan$", v5$l[sel1])
v5$pan[sel1][sel] <- v5$v[sel1][sel]
v5$v[sel1][sel] <- 0; v5$l[sel1][sel] <- "0"; v5$status[sel1][sel] <- "done"
#
sel1 <- which(is.na(v5$status))
sel <- grep("pri-|-pri|^pri$", v5$l[sel1])
v5$pri[sel1][sel] <- v5$v[sel1][sel]
v5$v[sel1][sel] <- 0; v5$l[sel1][sel] <- "0"; v5$status[sel1][sel] <- "done"
#
sel1 <- which(is.na(v5$status))
sel <- grep("prd-|-prd|^prd$", v5$l[sel1])
v5$prd[sel1][sel] <- v5$v[sel1][sel]
v5$v[sel1][sel] <- 0; v5$l[sel1][sel] <- "0"; v5$status[sel1][sel] <- "done"
#
sel1 <- which(is.na(v5$status))
sel <- grep("morena-|-morena|^morena$", v5$l[sel1])
v5$morena[sel1][sel] <- v5$v[sel1][sel]
v5$v[sel1][sel] <- 0; v5$l[sel1][sel] <- "0"; v5$status[sel1][sel] <- "done"
#
# rest are other
sel1 <- which(is.na(v5$status)) 
v5$oth[sel1] <- v5$v[sel1]
v5$v[sel1] <- 0; v5$l[sel1] <- "0"; v5$status[sel1] <- "done"
#
# 3-majors coalition in mun split in thirds
sel <- which(v5$status=="majors") 
sel1 <- grep("(?=.*pan)(?=.*pri)(?=.*prd)", v5$l[sel], perl = TRUE) # 
v5$pan[sel][sel1] <- v5$v[sel][sel1] / 3; v5$pri[sel][sel1] <- v5$v[sel][sel1] / 3; v5$prd[sel][sel1] <- v5$v[sel][sel1] / 3; v5$v[sel][sel1] <- 0; v5$l[sel][sel1] <- "0"; v5$status[sel][sel1] <- "done"
v5$dmajcoal[sel][sel1] <- 1
#
# pan-pri to pri (19 cases in mic07 mic11 mic15)
sel <- which(v5$status=="majors" & v5$edon==16) 
sel1 <- grep("(?=.*pan)(?=.*pri)", v5$l[sel], perl = TRUE) # 
v5$pri[sel][sel1] <- v5$v[sel][sel1]; v5$v[sel][sel1] <- 0; v5$l[sel][sel1] <- "0"; v5$status[sel][sel1] <- "done"
v5$dmajcoal[sel][sel1] <- 1
#
# pri-prd to pri (chihuahua and guanajuato)
sel <- which(v5$status=="majors") 
sel1 <- grep("(?=.*pri)(?=.*prd)", v5$l[sel], perl = TRUE) # 
v5$pri[sel][sel1] <- v5$v[sel][sel1]; v5$v[sel][sel1] <- 0; v5$l[sel][sel1] <- "0"; v5$status[sel][sel1] <- "done"
v5$dmajcoal[sel][sel1] <- 1
#
# rest are pan-prd
#
# pan-prd to pan (bc coa2009 coa col00 col18 cua dgo jal que san sin son tam yuc)
sel <- which(v5$status=="majors" & (v5$edon==2 | v5$edon==5 | v5$edon==6 | v5$edon==8 | v5$edon==10 | v5$edon==14 | v5$edon==22 | v5$edon==24 | v5$edon==25 | v5$edon==26 | v5$edon==28  | v5$edon==31)) 
v5$pan[sel] <- v5$v[sel]; v5$v[sel] <- 0; v5$l[sel] <- "0"; v5$status[sel] <- "done"
v5$dmajcoal[sel] <- 1
#
# pan-prd in 2018 to pan (bcs cps df gue mex mic oax pue qui tab zac)
sel <- which(v5$status=="majors" & v5$yr==2018) 
v5$pan[sel] <- v5$v[sel]; v5$v[sel] <- 0; v5$l[sel] <- "0"; v5$status[sel] <- "done"
v5$dmajcoal[sel][sel1] <- 1
#
# pan-prd to prd (cps2004, cps2010)
sel <- which(v5$status=="majors" & v5$edon==7 & v5$yr<=2010) 
v5$prd[sel] <- v5$v[sel]; v5$v[sel] <- 0; v5$l[sel] <- "0"; v5$status[sel] <- "done"
v5$dmajcoal[sel] <- 1
#
# pan-prd to pan (nay1999 nay2017 ver2000 ver2017)
sel <- which(v5$status=="majors" & (v5$edon==18 | v5$edon==30) & (v5$yr==1999 | v5$yr==2000 | v5$yr==2017)) 
v5$pan[sel] <- v5$v[sel]; v5$v[sel] <- 0; v5$l[sel] <- "0"; v5$status[sel] <- "done"
v5$dmajcoal[sel] <- 1
#
# pan-prd split halfway (qui2016)
sel <- which(v5$status=="majors" & v5$edon==23 & v5$yr==2016)
v5$pan[sel] <- v5$v[sel] / 2; 
v5$prd[sel] <- v5$v[sel] / 2; v5$v[sel] <- 0; v5$l[sel] <- "0"; v5$status[sel] <- "done"
v5$dmajcoal[sel] <- 1
#
# pan-prd split halfway (pue2010 pue2013 qui2013 qui2010)
sel <- which(v5$status=="majors" & (v5$edon==21 | v5$edon==23))
v5$pan[sel] <- v5$v[sel]; v5$v[sel] <- 0; v5$l[sel] <- "0"; v5$status[sel] <- "done"
v5$dmajcoal[sel] <- 1
#
# pan-prd split halfway (gue2015 hgo2011 mic2015 oax2010 oax2013 oax2016 zac2013, zac2016)
sel <- which(v5$status=="majors" & (v5$edon==12 | v5$edon==13 | v5$edon==16 | v5$edon==20 | v5$edon==32) & v5$yr<2018) 
v5$pan[sel] <- v5$v[sel] / 2; 
v5$prd[sel] <- v5$v[sel] / 2; v5$v[sel] <- 0; v5$l[sel] <- "0"; v5$status[sel] <- "done"
v5$dmajcoal[sel] <- 1
#
# pan-prd split halfway (mex2006)
sel <- which(v5$status=="majors" & v5$edon==15 & v5$yr==2006) 
v5$pan[sel] <- v5$v[sel] / 2; 
v5$prd[sel] <- v5$v[sel] / 2; v5$v[sel] <- 0; v5$l[sel] <- "0"; v5$status[sel] <- "done"
v5$dmajcoal[sel] <- 1
#
## # used to check by hand
## sel <- which(v5$status=="majors") 
## sel1 <- grep("(?=.*pan)(?=.*prd)", v5$l[sel], perl = TRUE) # 
## table(v5$edon[sel])
## table(v5$yr[sel])
## table(v5$emm[sel][sel1], v5$l[sel][sel1])
## table(v5$emm[sel][sel1], v5$yr[sel][sel1])
## x
#
# consolidate
tmp <- v5[v5$r==1,] # will receive consolidated data
dim(tmp)

for (i in 1:max(v5$n)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, max(v5$n)))
    tmp2 <- v5[v5$n==i, c("pan","pri","prd","morena","oth","dmajcoal")]
    tmp2 <- colSums(tmp2)
    tmp[tmp$n==i, c("pan","pri","prd","morena","oth","dmajcoal")] <- tmp2 # plug colsolidated data
}
v5 <- tmp
v5$n <- v5$r <- v5$v <- v5$l <- v5$status <- NULL
rm(tmp,tmp2)
# return to vot
vot <- cbind(vot, v5[,c("pan","pri","prd","morena","oth","dmajcoal")])
# keep 123 places, drop rest
#vot$v01 <- vot$v02 <- vot$v03 <-
vot$v04 <- vot$v05 <- vot$v06 <- vot$v07 <- vot$v08 <- vot$v09 <- vot$v10 <- vot$v11 <- vot$v12 <- vot$v13 <- vot$v14 <- NULL
#vot$l01 <- vot$l02 <- vot$l03 <-
vot$l04 <- vot$l05 <- vot$l06 <- vot$l07 <- vot$l08 <- vot$l09 <- vot$l10 <- vot$l11 <- vot$l12 <- vot$l13 <- vot$l14 <- NULL

## # debug
## save.image(file = "tmp.RData")
## #
## rm(list = ls()) # clean
## dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
## gd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/data/"
## setwd("/home/eric/Desktop/MXelsCalendGovt/reelec/data/")
## load(file = "tmp.RData")

# get electoral histories
vot$vhat.pan <- vot$vhat.pri  <- vot$vhat.left <- vot$alpha.pan  <- vot$alpha.pri <- vot$alpha.left <- NA # open slots
# 2006
his <- read.csv(paste(gd, "dipfed-municipio-vhat-2006.csv", sep = ""), stringsAsFactors = FALSE)
# drop unneeded columns
sel <- which(colnames(his) %in%
             c("edon","pan", "pri", "left", "efec",
               "d.pan", "d.pri", "d.left",
               "bhat.pan", "bhat.left",
               "betahat.pan", "betahat.left"))
his <- his[,-sel]
colnames(his) <- sub("ahat","a",colnames(his)) # shorten alpa names
#
sel <- which(vot$yr==2005 | vot$yr==2006 | vot$yr==2007)
tmp <- vot[sel,]
tmp$vhat.pan <- tmp$vhat.pri <- tmp$vhat.left <- tmp$alpha.pan <- tmp$alpha.pri <- tmp$alpha.left <- NULL # remove to merge them again
tmp <- merge(x = tmp, y = his, by = c("inegi", "ife"), all.x = TRUE, all.y = FALSE, sort = FALSE)
tmp <- tmp[,colnames(vot)] # sort columns to match vot's before merging
vot[sel,] <- tmp # return to data
#
# 2009
his <- read.csv(paste(gd, "dipfed-municipio-vhat-2009.csv", sep = ""), stringsAsFactors = FALSE)
# drop unneeded columns
sel <- which(colnames(his) %in%
             c("edon","pan", "pri", "left", "efec",
               "d.pan", "d.pri", "d.left",
               "bhat.pan", "bhat.left",
               "betahat.pan", "betahat.left"))
his <- his[,-sel]
colnames(his) <- sub("ahat","a",colnames(his)) # shorten alpa names
#
sel <- which(vot$yr==2008 | vot$yr==2009 | vot$yr==2010)
tmp <- vot[sel,]
tmp$vhat.pan <- tmp$vhat.pri <- tmp$vhat.left <- tmp$alpha.pan <- tmp$alpha.pri <- tmp$alpha.left <- NULL # remove to merge them again
tmp <- merge(x = tmp, y = his, by = c("inegi", "ife"), all.x = TRUE, all.y = FALSE, sort = FALSE)
tmp <- tmp[,colnames(vot)] # sort columns to match vot's before merging
vot[sel,] <- tmp # return to data
#
# 2012
his <- read.csv(paste(gd, "dipfed-municipio-vhat-2012.csv", sep = ""), stringsAsFactors = FALSE)
# drop unneeded columns
sel <- which(colnames(his) %in%
             c("edon","pan", "pri", "left", "efec",
               "d.pan", "d.pri", "d.left",
               "bhat.pan", "bhat.left",
               "betahat.pan", "betahat.left"))
his <- his[,-sel]
colnames(his) <- sub("ahat","a",colnames(his)) # shorten alpa names
#
sel <- which(vot$yr==2011 | vot$yr==2012 | vot$yr==2013)
tmp <- vot[sel,]
tmp$vhat.pan <- tmp$vhat.pri <- tmp$vhat.left <- tmp$alpha.pan <- tmp$alpha.pri <- tmp$alpha.left <- NULL # remove to merge them again
tmp <- merge(x = tmp, y = his, by = c("inegi", "ife"), all.x = TRUE, all.y = FALSE, sort = FALSE)
tmp <- tmp[,colnames(vot)] # sort columns to match vot's before merging
vot[sel,] <- tmp # return to data
#
# 2015
his <- read.csv(paste(gd, "dipfed-municipio-vhat-2015.csv", sep = ""), stringsAsFactors = FALSE)
# drop unneeded columns
sel <- which(colnames(his) %in%
             c("edon","pan", "pri", "left", "efec",
               "d.pan", "d.pri", "d.left",
               "bhat.pan", "bhat.left",
               "betahat.pan", "betahat.left"))
his <- his[,-sel]
colnames(his) <- sub("ahat","a",colnames(his)) # shorten alpa names
#
sel <- which(vot$yr==2014 | vot$yr==2015 | vot$yr==2016)
tmp <- vot[sel,]
tmp$vhat.pan <- tmp$vhat.pri <- tmp$vhat.left <- tmp$alpha.pan <- tmp$alpha.pri <- tmp$alpha.left <- NULL # remove to merge them again
tmp <- merge(x = tmp, y = his, by = c("inegi", "ife"), all.x = TRUE, all.y = FALSE, sort = FALSE)
tmp <- tmp[,colnames(vot)] # sort columns to match vot's before merging
vot[sel,] <- tmp # return to data
#
# 2018
his <- read.csv(paste(gd, "dipfed-municipio-vhat-2018.csv", sep = ""), stringsAsFactors = FALSE)
# drop unneeded columns
sel <- which(colnames(his) %in%
             c("edon","pan", "pri", "left", "efec",
               "d.pan", "d.pri", "d.left",
               "bhat.pan", "bhat.left",
               "betahat.pan", "betahat.left"))
his <- his[,-sel]
colnames(his) <- sub("ahat","a",colnames(his)) # shorten alpa names
#
sel <- which(vot$yr==2017 | vot$yr==2018 | vot$yr==2019)
tmp <- vot[sel,]
tmp$vhat.pan <- tmp$vhat.pri <- tmp$vhat.left <- tmp$alpha.pan <- tmp$alpha.pri <- tmp$alpha.left <- NULL # remove to merge them again
tmp <- merge(x = tmp, y = his, by = c("inegi", "ife"), all.x = TRUE, all.y = FALSE, sort = FALSE)
tmp <- tmp[,colnames(vot)] # sort columns to match vot's before merging
vot[sel,] <- tmp # return to data
#
# clean
rm(tmp,i,sel,sel1,v5,his)

# left residuals: use prd-vhat.left and morena-vhat.left (pan-prd went to pan in 2017 and 2018)
vot$res.pan <- vot$pan - vot$vhat.pan
vot$res.pri <- vot$pri - vot$vhat.pri
vot$res.prd <- vot$prd - vot$vhat.left
vot$res.morena <- vot$morena - vot$vhat.left
sel <- which(vot$yr<2015)
vot$res.morena[sel] <- NA

# plug incumbent data
# was there an incumbent on the ballot? did party reelect?
inc$race.current <- inc$race.after
inc$round <- sub(pattern = "[\\w\\D-]+([0-9]{2})[.][0-9]{3}", replacement = "\\1", inc$emm, perl = TRUE)
inc$round <- as.numeric(inc$round)
#table(inc$round)
# lag race after
inc <- inc[order(inc$emm),] # sort
inc$tmp <- as.numeric(sub(pattern = "[\\w\\D-]+[0-9]{2}[.]([0-9]{3})", replacement = "\\1", inc$emm, perl = TRUE)) # munn from emm
for (e in 1:32){
    #e <- 1 # debug
    message(sprintf("loop %s of %s", e, 32))
    sel  <- which(inc$edon==e)
    for (i in unique(inc$tmp[sel])){
        #i <- 10 # debug
        sel1 <- which(inc$tmp[sel]==i)
        last <- length(sel1)
        inc$race.current[sel][sel1][1]  <- NA
        inc$race.current[sel][sel1][-1] <- inc$race.after[sel][sel1][-last]
    }
}
inc <- inc[order(inc$edon, inc$tmp, inc$round),] # re-sort
inc$tmp <- NULL
#
inc$dopenseat <- 1
inc$dptyreel <- 0
inc$dtermlim <- 1
table(inc$race.current)
sel <- grep("Beaten|Reelected", inc$race.current)
inc$dopenseat[sel] <- 0
sel <- grep("Reelected|won", inc$race.current)
inc$dptyreel[sel] <- 1
sel <- grep("^(?!Term-lim).*$", inc$race.current, perl = TRUE)
inc$dtermlim[sel] <- 0

# paste incumbent data into vot
vot <- merge(x = vot, y = inc[,c("emm","race.current","dopenseat","dptyreel","dtermlim")], by = "emm", all.x = TRUE, all.y = FALSE)

# clean
rm(e,i,last,sel,sel1,sel2,tmp)

# debug
save.image(file = "tmp.RData")
#
rm(list = ls()) # clean
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
gd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/data/"
setwd("/home/eric/Desktop/MXelsCalendGovt/reelec/data/")
load(file = "tmp.RData")

# compute winner's margin
vot$mg <- round(vot$v01 - vot$v02, 4)
vot$round <- sub(pattern = "[\\w\\D-]+([0-9]{2})[.][0-9]{3}", replacement = "\\1", vot$emm, perl = TRUE)
vot$round <- as.numeric(vot$round)


# duplicate for pan analysis
tmp <- vot

# pan or oth incumbent --- complement is open seat
sel <- grep("pan", tmp$win)
tmp$dpan <- 0; tmp$dpan[sel] <- 1
table(tmp$dpan)
tmp$dpaninc  <-      tmp$dpan  * (1 - tmp$dopenseat)
tmp$dothinc  <- (1 - tmp$dpan) * (1 - tmp$dopenseat)
tmp$dpanopen <-      tmp$dpan  *      tmp$dopenseat

# subset yr >= 2018
sel <- which(tmp$yr >= 2018)
tmp <- tmp[sel,]


summary(lm(pan ~ dpaninc + dothinc, data = tmp))



vot[85,]

ls()
vot[33:34,]
vot$yr[1:100]
inc[29:33,]
x

## # computing v.hat with municipal vote returns would require fixing coalitions... leave for later
## # last round by state (circa 2018)
## round <- c(17, 17, 16, 17, 16, 17, 17, 17, 17, 17, 17, 16, 16, 17, 16, 16, 17, 16, 17, 17, 16, 17, 17, 17, 17, 17, 17, 17, 16, 16, 17, 17)
## # need previous (circa 2015)
## round <- round - 1
## # data prep for regressions
## tmp.dat <- data.frame()
## for (e in 1:32){
##     tmp <- vot[which(vot$edon==3),]; # keep state obs only
##     sel <- grep(pattern = paste("[a-z]+-", round[e], sep = ""), tmp$emm)
##     tmp <- tmp[sel,]; # keep round only
##     tmp.dat <- rbind(tmp.dat, tmp)
## }
## # separate pan, pri, left, oth
## # then select/paste round-1, round-2, ...


which(
grep(pattern = )



x 

edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")



# vote data to plug into incumbent data
#sel <- c("emm", "dy", "mo", "yr", "win", "v01", "l01", "l02", "l03")
sel <- c("emm", "v01", "l01", "l02", "l03")
v <- vot[,sel]
v$mg <- vot$v01 - vot$v02
v$sf <- round(vot$v03 / vot$v02, 2)

# merge votes into incumbent data
i <- merge(x = inc, y = v, by = "emm", all = TRUE)
# i should have same obs as inc (extras would be election obs not in incumbents) 
nrow(i)==nrow(inc) # must be true
write.csv(i, file = paste(dd, "tmp.csv", sep = ""), row.names = FALSE)

i[1,]

sel <- which(i$emm %in% inc$emm)
i$yr.y[-sel]

dim(v)

head(inc)
head(i)

# get electoral history
hist <- read.csv(paste(gd, "dipfed-municipio-vhat-2006.csv", sep = ""), stringsAsFactors = FALSE)


x

tmp <- inc[, c("edon","yr")]
tmp$tmp <- paste(tmp$edon, tmp$yr, sep = "-")
tmp$tmp2 <- duplicated(tmp$tmp)
tmp <- tmp[tmp$tmp2==FALSE,]
tmp$dinc <- 1 # identify original obs
tmp$tmp <- tmp$tmp2 <- NULL
el.yrs <- tmp # rename


# determine election years for all states --- inafed has yrIn instead of yr
calendar <- list(
    ags=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016),
    bc= c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016),
    bcs=c(     1990,          1993,          1996,          1999,          2002,          2005,          2008,          2011,               2015,          2018),
    cam=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    coa=c(     1990,          1993,          1996,          1999,          2002,          2005,               2009,               2013,               2017,2018),
    col=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    cps=c(          1991,               1995,          1998,          2001,          2004,          2007,          2010,     2012,          2015,          2018),
    cua=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018),
    df= c(                                                       2000,          2003,          2006,          2009,          2012,          2015,          2018),
    dgo=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016),
    gua=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    gue=c(1989,               1993,          1996,          1999,          2002,          2005,          2008,               2012,          2015,          2018),
    hgo=c(     1990,          1993,          1996,          1999,          2002,          2005,          2008,          2011,                    2016),
    jal=c(               1992,          1995,     1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    mex=c(     1990,          1993,          1996,               2000,          2003,          2006,          2009,          2012,          2015,          2018),
    mic=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,               2011,               2015,          2018),
    mor=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    nay=c(     1990,          1993,          1996,          1999,          2002,          2005,          2008,          2011,          2014,          2017),
    nl= c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    oax=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018),
    pue=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,                    2018),
    que=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    qui=c(     1990,          1993,          1996,          1999,          2002,          2005,          2008,     2010,          2013,          2016,     2018),
    san=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    sin=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018),
    son=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    tab=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    tam=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018),
    tla=c(          1991,          1994,               1998,          2001,          2004,          2007,          2010,          2013,          2016),
    ver=c(          1991,          1994,          1997,          2000,               2004,          2007,          2010,          2013,                2017),
    yuc=c(     1990,          1993,     1995,          1998,          2001,          2004,          2007,          2010,     2012,          2015,          2018),
    zac=c(               1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018)
)
# extract min yr by state
min.cal <- unlist(lapply(calendar, min))
# make a data frame
cal <- data.frame() # will receive vectorized data
for (i in 1:32){
    tmp <- data.frame(yr=calendar[[i]])
    tmp$edo <- names(calendar)[i]
    tmp$edon <- i
    cal <- rbind(cal, tmp)
}


# party won/lost dummy
# - was used to recode term-limit to pty-won or -lost but this is now redundant (race.after pasted to csv file 14aug2019)
# - changes to race.after are now redundant in bloc below
# - when more elections added, party greps may need manipulation (dhit verifies)
library(DataCombine) # easy lags
inc <- inc[order(inc$inegi, inc$yr),] # verify sorted before lags
inc <- slide(inc, Var = "win", GroupVar = "inegi", slideBy = +1) # lead win by one period
inc <- slide(inc, Var = "incumbent", GroupVar = "inegi", slideBy = +1) # lead win by one period
#
inc$dpwin <- NA
sel <- which(inc$win=="0" | inc$win1=="0"); inc$dpwin[sel] <- 99   # will change to NAs when all is node
# cherán ayutla before uyc
sel <- grep("mic-13.024|gue-15.012", inc$emm); inc$win1[sel] <- "uyc"; inc$race.after[sel] <- "uyc"
sel <- which(inc$win=="uyc");  inc$dpwin[sel] <- 0 # current usos coded as party not reelecting
sel <- which(inc$win1=="uyc"); inc$dpwin[sel] <- 0 # future usos coded as party not reelecting
# last cycle still NA
sel <- which(is.na(inc$win1) & is.na(inc$dpwin)); inc$dpwin[sel] <- 99 # will change to NAs when all is node
# independents
sel <- which(is.na(inc$dpwin) & inc$win=="indep")
inc$dpwin[sel][inc$incumbent[sel]==inc$incumbent1[sel]] <- 1
inc$dpwin[sel][inc$incumbent[sel]!=inc$incumbent1[sel]] <- 0
sel <- which(is.na(inc$dpwin) & inc$win1=="indep") # if indep won next in cases remaining, then party lost
inc$dpwin[sel] <- 0
#
# checked for false positives  
sel <- which(inc$win==inc$win1 & is.na(inc$dpwin))
#table(inc$win[sel]) # check for false positives  
inc$dpwin[sel] <- 1
# pri
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pri", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pri", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pan
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pan", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pan", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# prd
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("prd", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("prd", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pvem
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pvem", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pvem", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# morena
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("morena", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("morena", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pt
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pt1", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pt1", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# mc
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("^mc$|-mc|mc-|conve", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("^mc$|-mc|mc-|conve", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pna
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pna", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pna", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pps
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pps", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pps", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# ave
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("ave", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("ave", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pfcrn
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pfcrn", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pfcrn", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# parm
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("parm", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("parm", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# prt
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("prt", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("prt", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# prs
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("prs", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("prs", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# psd pasd
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("psd|pasd", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("psd|pasd", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pmch
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pmch", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pmch", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pchu
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pchu", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pchu", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# prv
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("prv", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("prv", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pup
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pup", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pup", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pcp
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pcp", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pcp", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# psi
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("psi", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("psi", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# psn
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("psn", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("psn", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# ps1
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("ps1", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("ps1", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pes
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pes", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pes", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pver
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pver", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pver", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pcd1
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pcd1", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pcd1", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pdm
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pdm", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pdm", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pcdt
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pcdt", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pcdt", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# npp
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("npp", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("npp", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# fc1
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("fc1", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("fc1", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pac1
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pac1", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pac1", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pcm2
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pcm2", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pcm2", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pd1
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pd1", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pd1", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pec
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pec", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pec", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pjs
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pjs", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pjs", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pmp
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pmp", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pmp", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pmt
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pmt", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pmt", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# poc
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("poc", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("poc", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# ppt
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("ppt", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("ppt", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# ppg
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("ppg", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("ppg", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# ph
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("ph", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("ph", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# via radical
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("via_radical", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("via_radical", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# pmr
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("pmr", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("pmr", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# mas
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("mas", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("mas", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
# paz
sel <- which(is.na(inc$dpwin))  # subset uncoded cases
sel1 <- grep("paz", inc$win[sel])
#table(inc$win[sel][sel1]) # check false positives
sel2 <- grep("paz", inc$win1[sel])
#drop#inc$dpwin[sel][sel1] <- 0
inc$dpwin[sel][intersect(sel1, sel2)] <- 1
#
# verify
inc$dhit <- 0
sel <- which(inc$win=="uyc"); inc$dhit[sel] <- 1
sel <- which(inc$win=="0"); inc$dhit[sel] <- 1
sel <- which(inc$emm=="mic-13.024|gue-15.012"); inc$dhit[sel] <- 1
sel <- which(is.na(inc$win1) & is.na(inc$dpwin)); inc$dhit[sel] <- 1
sel <- which(inc$win=="indep"); inc$dhit[sel] <- 1
#sel <- which(is.na(inc$dpwin) & inc$win1=="indep") # if indep won next in cases remaining, then party lost
sel <- which(inc$win==inc$win1); inc$dhit[sel] <- 1
sel <- grep("pri", inc$win); inc$dhit[sel] <- 1
sel <- grep("pan", inc$win); inc$dhit[sel] <- 1
sel <- grep("prd", inc$win); inc$dhit[sel] <- 1
sel <- grep("pvem", inc$win); inc$dhit[sel] <- 1
sel <- grep("morena", inc$win); inc$dhit[sel] <- 1
sel <- grep("pt1", inc$win); inc$dhit[sel] <- 1
sel <- grep("^mc$|-mc|mc-|conve", inc$win); inc$dhit[sel] <- 1
sel <- grep("pna", inc$win); inc$dhit[sel] <- 1
sel <- grep("pps", inc$win); inc$dhit[sel] <- 1
sel <- grep("ave", inc$win); inc$dhit[sel] <- 1
sel <- grep("pfcrn", inc$win); inc$dhit[sel] <- 1
sel <- grep("parm", inc$win); inc$dhit[sel] <- 1
sel <- grep("prt", inc$win); inc$dhit[sel] <- 1
sel <- grep("prs", inc$win); inc$dhit[sel] <- 1
sel <- grep("psd|pasd", inc$win); inc$dhit[sel] <- 1
sel <- grep("pmch", inc$win); inc$dhit[sel] <- 1
sel <- grep("pchu", inc$win); inc$dhit[sel] <- 1
sel <- grep("prv", inc$win); inc$dhit[sel] <- 1
sel <- grep("pup", inc$win); inc$dhit[sel] <- 1
sel <- grep("pcp", inc$win); inc$dhit[sel] <- 1
sel <- grep("psi", inc$win); inc$dhit[sel] <- 1
sel <- grep("psn", inc$win); inc$dhit[sel] <- 1
sel <- grep("ps1", inc$win); inc$dhit[sel] <- 1
sel <- grep("pes", inc$win); inc$dhit[sel] <- 1
sel <- grep("pver", inc$win); inc$dhit[sel] <- 1
sel <- grep("pcd1", inc$win); inc$dhit[sel] <- 1
sel <- grep("pdm", inc$win); inc$dhit[sel] <- 1
sel <- grep("pcdt", inc$win); inc$dhit[sel] <- 1
sel <- grep("npp", inc$win); inc$dhit[sel] <- 1
sel <- grep("fc1", inc$win); inc$dhit[sel] <- 1
sel <- grep("pac1", inc$win); inc$dhit[sel] <- 1
sel <- grep("pcm2", inc$win); inc$dhit[sel] <- 1
sel <- grep("pd1", inc$win); inc$dhit[sel] <- 1
sel <- grep("pec", inc$win); inc$dhit[sel] <- 1
sel <- grep("pjs", inc$win); inc$dhit[sel] <- 1
sel <- grep("pmp", inc$win); inc$dhit[sel] <- 1
sel <- grep("pmt", inc$win); inc$dhit[sel] <- 1
sel <- grep("poc", inc$win); inc$dhit[sel] <- 1
sel <- grep("ppt", inc$win); inc$dhit[sel] <- 1
sel <- grep("ppg", inc$win); inc$dhit[sel] <- 1
sel <- grep("ph", inc$win); inc$dhit[sel] <- 1
sel <- grep("via_radical", inc$win); inc$dhit[sel] <- 1
sel <- grep("pmr", inc$win); inc$dhit[sel] <- 1
sel <- grep("mas", inc$win); inc$dhit[sel] <- 1
sel <- grep("paz", inc$win); inc$dhit[sel] <- 1
table(inc$dhit, useNA = "always") # all must be 1
#sel <- which(inc$dhit==0)
#inc[sel,]
inc$dhit <- NULL # clean
# all others must therefore be dpwin=0
sel <- which(is.na(inc$dpwin)); inc$dpwin[sel] <- 0
#
print("NA must be zero")
table(inc$dpwin, useNA = "always")
# recode 99s to NAs
sel <- which(inc$dpwin==99)
inc$dpwin[sel] <- NA
#
## # recode term limited
## # redundant
## sel <- which(inc$race.after=="Term-limited")
## sel1 <- which(inc$dpwin==1)
## inc$race.after[intersect(sel,sel1)] <- "Term-limited-p-won"
## sel1 <- which(inc$dpwin==0)
## inc$race.after[intersect(sel,sel1)] <- "Term-limited-p-lost"
#
# special cases
sel <- which(inc$emm=="cps-16.120") ; inc$dpwin[sel] <- 0; inc$race.after[sel] <- "Term-limited-p-lost" # litigio coded as term limited 
sel <- which(inc$emm=="dgo-16.035") ; inc$dpwin[sel] <- 1 # reran under pvem only and lost
sel <- which(inc$emm=="oax-16.130") ; inc$dpwin[sel] <- 0 # conflicto postelectoral
#
## # pending cases
## # redundant
## sel <- which(inc$race.after=="Term-limited")
## inc$race.after[sel] <- "Term-limited-pending"
#
# result
table(inc$race.after, inc$dpwin, useNA = "always")
#
# check cases
#sel1 <- which(inc$emm=="cps-17.064")
sel1 <- which(inc$race.after=="Beaten" & inc$dpwin==1)
inc[sel1,c("emm","yr","win","incumbent","win1","race.after","note","dpwin")]
sel1 <- which(inc$race.after=="Reelected" & inc$dpwin==0)
inc[sel1,c("emm","yr","win","incumbent","win1","race.after","note","dpwin")]
#

# change conve for mc to avoid false negatives
sel <- grep("conve", inc$win)
inc$win[sel] <- sub(pattern="conve", replacement="mc", inc$win[sel])
sel <- grep("conve", inc$win1)
inc$win1[sel] <- sub(pattern="conve", replacement="mc", inc$win1[sel])

# which party/ies reelected
inc$returned <- NA
sel <- which(inc$dpwin==1)
inc$returned[-sel] <- "none"
#
tmp0 <- inc$win[sel]
tmp1 <- inc$win1[sel]
for (i in 1:length(tmp0)){
    message(sprintf("loop %s of %s", i, length(tmp0)))
    #i <- 10 # debug
    tmp00 <- unlist(x = strsplit(x = tmp0[i], split = "-")) # break obs i's coals t into vector
    tmp11 <- unlist(x = strsplit(x = tmp1[i], split = "-")) # break obs i's coals t+1 into vector
    index <- tmp00 %in% tmp11
    common <- tmp00[index]
    common <- paste(common, collapse = "-")
    inc$returned[sel][i] <- common
}



# check
sel <- which(inc$returned=="")
inc[sel,c("emm","yr","win","incumbent","win1","race.after","note","dpwin")]

# did a major party return (or none/opportunist only)
sel <- grep("pan|pri|prd|morena", inc$returned)
inc$dmajret <- 0; inc$dmajret[sel] <- 1
table(inc$returned[inc$dmajret==0], useNA = "ifany")

# clean
inc$win1 <- inc$incumbent1 <- NULL
rm(tmp,tmp0,tmp00,tmp1,tmp11,i,sel,sel1,sel2)
head(inc)
#
## # export race.after column to paste it in csv file (done 14ago2019)
## inc <- inc[order(inc$ord),] # sort
## getwd()
## write.csv(inc$race.after, file = "tmp-ra.csv", row.names = FALSE)




# change 0s to NAs
sel <- which(inc$incumbent=="0")
inc$incumbent[sel] <- NA


# recode some win labels
table(inc$win)
sel <- which(inc$win %in% c("via_radical"))
inc$win[sel] <- "vrad"



## # get elected governors
## gob <- read.csv(paste(dd, "goed1985-present.incumbents.csv", sep = ""), stringsAsFactors = FALSE)
## gob$yr <- gob$yr_el; gob$mo <- gob$mo_el; gob$dy <- gob$dy_el
## gob$win <- gob$part; gob$incumbent <- gob$gober
## gob$mun <- "gobernador"
## gob$race.after <- gob$note <- gob$fuente <- gob$returned <- ""
## gob$dpwin <- NA
## gob$inegi <- gob$ife <- gob$munn <- 0
## gob <- gob[,c("emm","yr","mun","ord","inegi","edon","munn","ife","mo","dy","win","incumbent","race.after","note","fuente","dpwin","returned")]
## ## head(gob)
## ## head(inc)
## # add dummy to drop gobernadores after names have been searched
## gob$dgob <- 1; inc$dgob <- 0
## # merge
## inc <- rbind(inc, gob) # paste governors into incumbents to search for their names too (eg. Monreal was zac governor)
## rm(gob)
## tail(inc)

## # get elected diputados federales
## dip <- read.csv(paste(dd, "dfdf2000-present-incumbents.csv", sep = ""), stringsAsFactors = FALSE)
## dip$mo <- dip$dy <- NA; dip$win <- dip$part; dip$incumbent <- dip$nom
## dip$mun <- "dipfed"
## dip$race.after <- dip$note <- dip$fuente <- dip$returned <- ""
## dip$dpwin <- NA
## dip$inegi <- dip$ife <- dip$munn <- dip$dgob <- 0
## dip <- dip[,c("emm","yr","mun","ord","inegi","edon","munn","ife","mo","dy","win","incumbent","race.after","note","fuente","dpwin","returned","dgob")]
## ## head(dip)
## ## head(inc)
## # add dummy to drop dipernadores after names have been searched
## dip$ddip <- 1; inc$ddip <- 0
## # merge
## inc <- rbind(inc, dip) # paste governors into incumbents to search for their names too (eg. Monreal was zac governor)
## rm(dip)
## tail(inc)

## # get senadores --- under construction

# clean names
inc$incumbent <- gsub("  ", " ", inc$incumbent)  # drop double spaces
inc$incumbent <- sub("^ | $", "", inc$incumbent) # drop trainling/heading spaces
inc$original.name <- inc$incumbent # duplicate
#inc$incumbent <- inc$original.name # restore
inc$incumbent <- gsub("[.]", "", inc$incumbent)  # drop periods
inc$incumbent <- gsub("[()]", "", inc$incumbent)  # drop parentheses
inc$incumbent <- gsub("[,]", "", inc$incumbent)  # drop commas
inc$incumbent <- gsub("[|]", " ", inc$incumbent)  # change | with a space
inc$incumbent <- gsub("\\[ÜU\\]|\\[UÜ\\]", "U", inc$incumbent)  # change or with a space
## sel <- grep("|", inc$incumbent)
## inc$incumbent[sel]


# simplify some words in names to save memory in name search function 
#
inc$incumbent <- gsub(" VDA DE", " VDADE", inc$incumbent)
inc$incumbent <- gsub(" DE LA O ", " DELAO ", inc$incumbent)
inc$incumbent <- gsub(" DE LA O$", " DELAO", inc$incumbent)
inc$incumbent <- gsub(" DE LA ", " ", inc$incumbent)
inc$incumbent <- gsub(" DE L[OA]S ", " ", inc$incumbent)
inc$incumbent <- gsub(" DE DIOS", " DEDIOS", inc$incumbent)
inc$incumbent <- gsub(" MONTES DE OCA$", " MONTESDEOCA", inc$incumbent)
inc$incumbent <- gsub(" MONTES DE OCA ", " MONTESDEOCA ", inc$incumbent)
inc$incumbent <- gsub(" DE JESUS", " DEJESUS", inc$incumbent)
inc$incumbent <- gsub(" DE LEON", " DELEON", inc$incumbent)
inc$incumbent <- gsub(" DE LUIS", " DELUIS", inc$incumbent)
inc$incumbent <- gsub(" DE ", " ", inc$incumbent)
inc$incumbent <- gsub(" DEL RIO$", " DELRIO", inc$incumbent)
inc$incumbent <- gsub(" DEL RIO ", " DELRIO ", inc$incumbent)
inc$incumbent <- gsub(" DEL ", " ", inc$incumbent)
inc$incumbent <- gsub(" Y ", " ", inc$incumbent)
## # debug
## tmp <- grep(" DE L[OA]S ", inc$incumbent)
## tmp <- 1:100
## inc$incumbent[tmp]


# load my name-searching function
source("../code/search_names.r")

# will receive repeated names with whatever method chosen
inc$drep <- 0
# DROP#who.was.hit <- as.list(rep(NA,32))
inc$who <- NA # useful to contrast exact and grep/fuzzy
meth <- c("exact","grep","fuzzy")[1] # pick 1 2 or 3
#
# grep match within state alcaldes only (+ govs)
for (i in 1:32){
    #i <- 5 # debug
    #message(sprintf("loop %s of %s", i, 32))
    sel1 <- which(inc$edon %in% i);
    sel2 <- which(inc$ddip==1);
    sel3 <- which(inc$dgob==1);
    sel <- intersect(sel1, sel2); # state's diputados
    sel <- union(sel, sel1);      # state's alcaldes+diputados
    sel <- union(sel, sel3);      # plus all governors
    tmp1 <- search.names(
        within.records = inc$incumbent[sel],
        ids = inc$emm[sel],
        method = meth
    )
    sh.hits <- tmp1$sh.hits # extract share hits matrix
    sh.hits <- sh.hits * (1-diag(nrow(sh.hits))) # diag to 0
    #sh.hits <- matrix(c(1,1,0,.5,0,0,1,0,0), nrow = 3, ncol = 3) # debug
    exact.hits <- apply(X = sh.hits, 2, function(x) length(which(x==1)))
    sel.col <- which(exact.hits>0)
    sel.ids <- tmp1$ids[sel.col]
    sel.names <- tmp1$names[sel.col]
    sel.yrs <- inc$yr[sel][sel.col]
    #
    # get ids of the hits only to debug
    tmp2 <- Map(function(x){y <- which(x==1); return(y);}, split(t(sh.hits), seq(nrow(sh.hits))))
    hits.ids <- unlist(tmp2)
    hits.ids <- tmp1$ids[hits.ids]
    hits.ids <- relist(hits.ids, tmp2)
    tmp.ss <- hits.ids[which(lapply(hits.ids, length)>1)]
    tmp.ss <- sapply(tmp.ss, paste, collapse = " ", simplify = FALSE) # collapse multiple hits into single string
    hits.ids[which(lapply(hits.ids, length)>1)] <- tmp.ss
    hits.ids[which(lapply(hits.ids, length)==0)] <- "0" # fill empty with 0
    #DROP#who.was.hit[[i]] <- hits.ids
    inc$who[sel] <- unlist(hits.ids)
    #
    sh.hits <- tmp1$sh.hits # restore sh.hits
    sh.hits.ss <- sh.hits[,sel.col]
    # function to report indexes of exact hit elements
    sh.hits.ss <- t(sh.hits.ss) # needed because lapply operates on rows and we need hits in each column
    tmp3 <- Map(function(x){y <- which(x==1); return(y)}, split(sh.hits.ss, seq(nrow(sh.hits.ss))))
    # DROP #tmp2 <- tmp2[lapply(tmp2, length)>0] # drop empty elements
    names(tmp3) <- sel.ids
    #
    tmp.ids  <- Map(function(x) tmp1$ids[unlist(x)]   , tmp3)
    #
    tmp.names<- Map(function(x) tmp1$names[unlist(x)] , tmp3)
    #
    tmp.yrs  <- Map(function(x) inc$yr[sel][unlist(x)], tmp3)
    #
    # identifies min(year)'s index
    tmp.drop <- Map(function(x) which(x==min(x)), tmp.yrs)
    # drops indices of early years from id vectors 
    tmp.ones <- Map(function(x, y) x[-y], tmp.ids, tmp.drop)
    # turn into vector
    tmp.ones <- unlist(tmp.ones)
    #
    # record hits in data
    sel4 <- which(inc$emm %in% tmp.ones)
    inc$drep[sel4] <- 1
}
# fix special cases by hand
sel <- which(inc$emm=="mex-09.086")
inc$drep[sel] <- 0 # homónimo en tenango del valle 1990
sel <- which(inc$emm=="mex-16.083")
inc$drep[sel] <- 0 # homónimo en chapa de mota
sel <- which(inc$emm=="mex-14.119")
inc$drep[sel] <- 0 # homónimo en sn felipe progreso
sel <- which(inc$emm=="mex-14.051")
inc$drep[sel] <- 0 # homónimo en tenango del valle 1990
sel <- which(inc$emm=="mex-14.082")
inc$drep[sel] <- 0 # homónimo grep en chicoloapan
# fills-in the appropriate
if (meth=="exact") inc$drepe <- inc$drep
if (meth=="grep")  inc$drepg <- inc$drep
if (meth=="fuzzy") inc$drepf <- inc$drep
inc$drep <- NULL


# parece que sí jala!
table(inc$drepe, inc$drepg)
table(inc$drepe, inc$drepf) # fuzzy doesn't work

sel <- which(inc$drepe==0 & inc$drepg==1)
inc$emm[sel]

tmp <- inc[,c("ord","drepe","drepg", "who")]
head(tmp)
tmp$who

write.csv(tmp, file = "tmp.csv")









sel <- which(inc$edon %in% 21)
length(sel)
x
ags-gue ~4600
hgo-mic ~4300
mor-pue ~4800
que-tla ~4100
ver-zac ~2900

x

# count number of words in names
inc$words <- gsub("[^ ]", "", inc$incumbent); inc$words <- nchar(inc$words) + 1
table(inc$words)
# add column for incumbents with prvious elected municipal office
inc$drep <- NA # will receive dummy = 1 if name repeated in other obs
# sorts by decreasing words to process mem-intensive cases 1st, then drop to speed loops
inc <- inc[order(-inc$words),]

## # load incumbents from inafed data <-- NO LONGER NEEDED, aymu1989-present.incumbents.csv already has them in
## inafed <- read.csv("../../municipiosInafed/alcaldes/alcaldes.csv", stringsAsFactors = FALSE)
## inafed <- inafed[, c("edon","inegi","yr","incumb")] # select columns
## #inafed$edon <- as.integer(inafed$inegi/1000)
## inafed$incumb <- gsub("  ", " ", inafed$incumb) # drop double spaces
## inafed$incumb <- sub("^ | $", "", inafed$incumb) # drop trainling/heading spaces
## inafed$incumb <- gsub("[.]", "", inafed$incumb)  # drop periods
## inafed$incumb <- gsub("[(|)]", "", inafed$incumb)  # drop parentheses
## head(inafed)
## #
## # merge inafed names
## inc <- merge(x = inc, y = inafed[, c("inegi","yr","incumb")], by = c("inegi","yr"), all = TRUE)
## # how many words in names
## inc$spc1 <- gsub(pattern = "[^ ]", replacement = "", inc$incumbent, perl = TRUE) # keep spaces only
## inc$spc2 <- gsub(pattern = "[^ ]", replacement = "", inc$incumb, perl = TRUE) # keep spaces only
## inc$spc1 <- sapply(inc$spc1, nchar) # count them
## inc$spc2 <- sapply(inc$spc2, nchar) # count them
## inc$spc1[is.na(inc$spc1)] <- 0
## inc$spc2[is.na(inc$spc2)] <- 0
## # will receive name difference info
## inc$ddif <- NA
## sel <- which(inc$spc1>1 & inc$spc2>1) # only those names with at least two words manipulated
## tmp2 <- mapply(search.names, inc[sel, c("incumb")], inc[sel, c("incumbent")])
## tmp3 <- rep(1, length(tmp2)); tmp3[tmp2==TRUE] <- 0
## inc$ddif[sel] <- tmp3
## # consolida nombres
## table(is.na(inc$incumbent))
## table(inc$incumbent=="")
## table(is.na(inc$incumb))
## sel <- which((inc$incumbent=="" | is.na(inc$incumbent)) & is.na(inc$incumb)==FALSE)
## inc$incumbent[sel] <- inc$incumb[sel]
## inc$spc1[sel] <- inc$spc2[sel]
## inc$incumb <- inc$spc2 <- NULL
## sel <- which(inc$yr==0)
## inc <- inc[-sel,]
## inc$race.after[inc$yr<2016] <- "Term-limited"
## # add missing mun info
## sel <- which(is.na(inc$edon)==TRUE)
## inc$edon[sel] <- as.integer(inc$inegi[sel]/1000)
## inc$munn[sel] <- inc$inegi[sel] - as.integer(inc$inegi[sel]/1000)*1000
## # smthg wrong, check words
## sel <- which(is.na(inc$words))
## table(inc$words[-sel] - inc$spc1[-sel])


# subset to explore name-searching performance
sel <- which(inc$edon==14)
inc.jal <- inc[sel,]
inc.jal[1,]

# keep full version of inc in order to work w/o NAs (plug manipulation later)
inc.full <- inc # duplicate
inc <- inc.jal
sel.full <- which(inc$incumbent!="" & inc$words>1)
inc <- inc[sel.full,] # subset



# initialize for while
work <- which(is.na(inc$drep)==TRUE) # will be updated to FALSE after a hit recorded
ss <- inc[work,c("incumbent","drep")] # subset
ss$hit <- NA
tmp <- nrow(ss)
#i <- 1

while (tmp > 1){
    message(sprintf("%s loops, %s records left", i, tmp))
    ss$hit[-1] <- search.names(find_name = ss$incumbent[1], within.records = ss$incumbent[-1])
    sel <- which(ss$hit==TRUE)
    if (length(sel)>0) {
        ss$drep[1]   <- i # i allows finding where repeated name is
        ss$drep[sel] <- i
    } else {
        ss$drep[1] <- 0
    }
    # return to data
    inc$drep[work] <- ss$drep
    # update for next loop
    work <- which(is.na(inc$drep)==TRUE) # will be updated to FALSE after a hit recorded
    ss <- inc[work,c("incumbent","drep")] # subset
    ss$hit <- NA
    tmp <- nrow(ss)
    i <- i+1 # prepare for next hit
}

getwd()
save.image(file = "drep-cut1.RData")

rm(list = ls())
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data/"
setwd(wd)
load(file = "drep-cut1.RData")
ls()
options(width = 130)



table(inc$drep)
sel <- which(inc$drep==15)
inc[sel,c("edon","mun","incumbent")]
x


inafed$spcs <- gsub(pattern = "[^ ]", replacement = "", inafed$incumb, perl = TRUE) # keep spaces only
inafed$spcs <- sapply(inafed$spcs, nchar) # count them
3 sort by spcs, export to csv
4 manually input commas to separate name, patronyn, matronym

head(inafed$spcs)
head(inafed$incumb)

# fuzzy matching of names
N <- nrow(inafed)
tmp <- rep(0, N) #will receive dummy

hits <- agrep(pattern = inafed$incumb[1], x = inafed$incumb, ignore.case = TRUE)
n.hits <- length(hits)
names <- agrep(pattern = inafed$incumb[1], x = inafed$incumb, ignore.case = TRUE, value = TRUE)

apply(inafed$incumb[-1], function(x) = agrep(pattern = inafed$incumb[1], inafed$incumb[-1]))

agrep(pattern = "CUAUHTEMOC CALDERON GALVAN", x = "xxAUHTEMOC xxLDERON GALVAN", ignore.case = TRUE)
agrep("lasy", "1 lazy 2", max = list(sub = 0))
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2)
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2, value = TRUE)
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2, ignore.case = TRUE)

# determine/fix mismatches btw yrIn and yr
tmp <- inafed; tmp$tmp <- paste(tmp$edon, tmp$yrIn, sep = "-")
tmp <- inafed[duplicated(tmp$tmp)==FALSE,] # state-yrs in inafed 
tmp$inafed <- tmp$inegi <- NULL
tmp$yr <- tmp$yrIn # emulate column for merge
tmp$dinaf <- 1 # identify obs

el.yrs <- merge(x = el.yrs, y = tmp, by = c("edon","yr"), all = TRUE)
el.yrs <- el.yrs[order(el.yrs$edon, el.yrs$yr),]
el.yrs$dinaf[is.na(el.yrs$dinaf)] <- 0
el.yrs$dinc[is.na(el.yrs$dinc)] <- 0


#merge to incumb election yrs
cal$dcal <- 1
dim(el.yrs)
el.yrs <- merge(x = el.yrs, y = cal, by = c("edon","yr"), all = TRUE)
dim(el.yrs)
el.yrs$dinaf[is.na(el.yrs$dinaf)] <- 0
el.yrs$dinc[is.na(el.yrs$dinc)] <- 0
el.yrs$dcal[is.na(el.yrs$dcal)] <- 0
head(el.yrs)
el.yrs$where <- "none"
el.yrs$where[el.yrs$dcal==0 & el.yrs$dinc==0 & el.yrs$dinaf==1] <- "inaf-only" # 001
el.yrs$where[el.yrs$dcal==0 & el.yrs$dinc==1 & el.yrs$dinaf==0] <- "inc-only"  # 010
el.yrs$where[el.yrs$dcal==0 & el.yrs$dinc==1 & el.yrs$dinaf==1] <- "inc-inaf"  # 011
el.yrs$where[el.yrs$dcal==1 & el.yrs$dinc==0 & el.yrs$dinaf==0] <- "cal-only"  # 100
el.yrs$where[el.yrs$dcal==1 & el.yrs$dinc==0 & el.yrs$dinaf==1] <- "cal-inaf"  # 101
el.yrs$where[el.yrs$dcal==1 & el.yrs$dinc==1 & el.yrs$dinaf==0] <- "cal-inc"   # 110
el.yrs$where[el.yrs$dcal==1 & el.yrs$dinc==1 & el.yrs$dinaf==1] <- "all"       # 111

table(el.yrs$where)
table(el.yrs$where, el.yrs$edon)
table(el.yrs$where, el.yrs$yr)

i <- i+1
sel <- which(el.yrs$edon==i)
table(el.yrs$where[sel], el.yrs$yr[sel])
el.yrs[sel,]
calendar[[i]]


# keep only years missing
el.yrs <- el.yrs[el.yrs$dinc==0,]
el.yrs$where <- el.yrs$dinc <- el.yrs$dcal <- el.yrs$edo <- NULL
# add missing columns to rbind to inc
el.yrs$emm <- el.yrs$ord <- el.yrs$munn <- el.yrs$inegi <- el.yrs$ife <- el.yrs$mo <- el.yrs$dy <- el.yrs$mun <- el.yrs$win <- el.yrs$incumbent <- el.yrs$race.after <- el.yrs$nota <- el.yrs$suplente <- NA
el.yrs <- el.yrs[, c("emm", "ord", "edon", "munn", "inegi", "ife", "yr", "mo", "dy", "mun", "win", "incumbent", "race.after", "nota", "suplente")] # sort columns
# paste missing yrs to inc
inc <- rbind(inc, el.yrs)
inc <- inc[order(inc$edon, inc$yr),]


head(inc)
head(el.yrs)

x

dim(inafed)
inafed <- merge(x = inafed, y = cal, by = c("edon","yr"), all = TRUE)
dim(inafed)
x




head(inafed)
head(cal)

dim(inc)


inc <- merge(x = inc, y = inafed, by = c("inegi","yr"), all = TRUE)



edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")
edon <- 18
edo <- edos[edon]
#
inc$inafed <- NA # add empty column to receive incumbents
#



# load elec data for margin
mv <- read.csv(paste(dd, "aymu1997-present.coalAgg.csv", sep = ""), stringsAsFactors = FALSE)
sel <- grep("^v[0-9]+", colnames(mv))
v <- mv[,sel] # select vote columns only
mv$efec <- rowSums(v) # recompute efec --- should be redundant
mv$mg <- round((mv$v01 - mv$v02) / mv$efec, 3) # compute margin of victory
# prep margin for merge
mv <- mv[,c("emm","mg","lisnom","efec")]
# merge
inc <- merge(x = inc, y = mv, by = "emm", all.x = TRUE, all.y = FALSE)

head(inc)

#############################################################################################
## get lisnom from federal els (taken from naylum code)                                    ##
## ignores vote code bec aggregating multi-district munics w partial coals not straightfwd ##
#############################################################################################
### get seccion equivalencias to fill missing municipio names
pth <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv"
eq <- read.csv(pth, stringsAsFactors = FALSE)
eq <- eq[,c("edon","seccion","ife","inegi","mun")]
### 2006 president has lisnom 
pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/pre2006.csv"
tmp <- read.csv(pth, stringsAsFactors = FALSE)
tmp <- tmp[-which(tmp$seccion==0),] # drop voto extranjero
tmp$lisnom[is.na(tmp$lisnom)] <- 0 # make NAs zeroes because ave() somehow not ignoring them
tmp$lisnom <- ave(tmp$lisnom, as.factor(tmp$edon*10000+tmp$seccion), FUN=sum, na.rm=TRUE) # aggregate secciones
tmp <- tmp[duplicated(tmp$edon*10000+tmp$seccion)==FALSE,] # drop reduntant obs --- keep seccion structure to merge to 09
tmp$lisnom <- ave(tmp$lisnom, as.factor(tmp$munn), FUN=sum, na.rm=TRUE) # aggregate municipios
tmp <- tmp[duplicated(tmp$munn)==FALSE,] # drop reduntant obs --- keep seccion structure to merge to 09
tmp$lisnom.06 <- tmp$lisnom
ln <- tmp[, c("edon","munn","lisnom.06")]
#
# 2009 has mun only, merge to eq for all munn (slight mistakes likely due to remunicipalización)
pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/dip2009.csv"
tmp <- read.csv(pth, stringsAsFactors = FALSE)
tmp$lisnom[is.na(tmp$lisnom)] <- 0 # make NAs zeroes because ave() somehow not ignoring them
tmp$lisnom.09 <- tmp$lisnom # rename
tmp$mun <- tmp$munn # wrong name
tmp <- tmp[,c("edon","mun","seccion","lisnom.09")] # data has no munn
tmp$lisnom.09 <- ave(tmp$lisnom.09, as.factor(tmp$edon*10000+tmp$seccion), FUN=sum, na.rm=TRUE) # aggregate secciones
tmp <- tmp[duplicated(tmp$edon*10000+tmp$seccion)==FALSE,] # drop reduntant obs
tmp$lisnom.09 <- ave(tmp$lisnom.09, as.factor(paste(tmp$edon, tmp$mun, sep = ".")), FUN=sum, na.rm=TRUE) # aggregate municipios
tmp <- merge(x = tmp, y = eq, by = c("edon","seccion"), all.x = TRUE, all.y = FALSE) # merge to have munn
tmp <- tmp[duplicated(as.factor(paste(tmp$edon, tmp$mun.x, sep = ".")))==FALSE,]
tmp$munn <- tmp$ife
tmp$mun <- tmp$mun.y
tmp <- tmp[,c("edon","munn","inegi","mun","lisnom.09")]
#
ln <- merge(x = ln, y = tmp, by = c("edon","munn"), all = TRUE)
#
# 2012 presid
pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/pre2012.csv"
tmp <- read.csv(pth, stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","mun","lisnom")]
tmp <- tmp[-which(tmp$seccion==0),] # drop voto extranjero
tmp[tmp=="-"] <- 0 # remove "-"
# agrega secciones
tmp$lisnom   <- ave(tmp$lisnom,   as.factor(tmp$edon*10000+tmp$seccion), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edon*10000+tmp$seccion)==FALSE,] # drop reduntant obs
tmp$lisnom.12 <- tmp$lisnom; tmp$lisnom <- NULL
# add munn
tmp <- merge(x = tmp, y = eq, by = c("edon","seccion"), all.x = TRUE, all.y = FALSE) 
tmp$munn <- tmp$ife; tmp$ife <- NULL # rename
# aggregate municipios
tmp$lisnom.12   <- ave(tmp$lisnom.12,   as.factor(paste(tmp$edon, tmp$mun.x)), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(as.factor(paste(tmp$edon, tmp$mun.x)))==FALSE,] # drop reduntant obs
tmp$mun <- tmp$mun.y; tmp$mun.x <- tmp$mun.y <- NULL
tmp <- tmp[,c("edon","munn","inegi","mun","lisnom.12")]
#
ln <- merge(x = ln, y = tmp, by = c("edon","munn"), all = TRUE)
# plug mun.y when mun.x is missing
sel <- which(is.na(ln$mun.x))
ln$mun.x[sel] <- ln$mun.y[sel]
sel <- which(is.na(ln$inegi.x))
ln$inegi.x[sel] <- ln$inegi.y[sel]
ln$mun <- ln$mun.x; ln$mun.x <- ln$mun.y <- NULL
ln$inegi <- ln$inegi.x; ln$inegi.x <- ln$inegi.y <- NULL
#
# 2015 dip fed
pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/dip2015.csv"
tmp <- read.csv(pth, stringsAsFactors = FALSE)
#tmp <- tmp[-which(tmp$seccion==0),] # drop voto extranjero
tmp$ord <- tmp$OBSERVACIONES <- tmp$ID_CASILLA <- tmp$TIPO_CASILLA <- tmp$EXT_CONTIGUA <- tmp$nr <- tmp$nul <- tmp$tot <- NULL # clean
tmp$lisnom[is.na(tmp$lisnom)] <- 0
tmp$lisnom.15 <- tmp$lisnom
tmp <- tmp[,c("edon","seccion","lisnom.15")]
# agrega secciones
tmp$lisnom.15   <- ave(tmp$lisnom.15,   as.factor(paste(tmp$edon, tmp$seccion, sep = ".")), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(as.factor(paste(tmp$edon, tmp$seccion, sep = ".")))==FALSE,] # drop reduntant obs
tmp <- merge(x = tmp, y = eq, by = c("edon","seccion"), all.x = TRUE, all.y = FALSE) # add munn
tmp$munn <- tmp$ife; tmp$ife <- NULL # rename
tmp$lisnom.15   <- ave(tmp$lisnom.15,   as.factor(tmp$munn), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$munn)==FALSE,] # drop reduntant obs
tmp <- tmp[,c("edon","munn","mun","lisnom.15")]
#
ln <- merge(x = ln, y = tmp, by = c("edon","munn"), all = TRUE)
# plug mun.y when mun.x is missing
sel <- which(is.na(ln$mun.x))
ln$mun.x[sel] <- ln$mun.y[sel]
ln$mun <- ln$mun.x; ln$mun.x <- ln$mun.y <- NULL
#
# 2018 presid
pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/pre2018.csv"
tmp <- read.csv(pth, stringsAsFactors = FALSE)
tmp <- tmp[-which(tmp$seccion==0),] # drop voto extranjero
tmp[tmp=="-"] <- 0 # remove "-"
tmp <- tmp[, c("edon","seccion","lisnom")]
# agrega secciones
tmp$lisnom   <- ave(tmp$lisnom,   as.factor(tmp$edon*10000+tmp$seccion), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edon*10000+tmp$seccion)==FALSE,] # drop reduntant obs
tmp$lisnom.18 <- tmp$lisnom; tmp$lisnom <- NULL
# add munn
tmp <- merge(x = tmp, y = eq, by = c("edon","seccion"), all.x = TRUE, all.y = FALSE) 
tmp$munn <- tmp$ife; tmp$ife <- NULL # rename
sel <- which(is.na(tmp$munn)) # reseccionamiento, drop handful of secciones not in equiv secciones
tmp <- tmp[-sel,]
# aggregate municipios
tmp$lisnom.18   <- ave(tmp$lisnom.18,   as.factor(tmp$munn), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$munn)==FALSE,] # drop reduntant obs
tmp <- tmp[,c("edon","munn","mun","lisnom.18")]
#
ln <- merge(x = ln, y = tmp, by = c("edon","munn"), all = TRUE)
ln$mun <- ln$mun.x; ln$mun.x <- ln$mun.y <- NULL
#
# sort
ln <- ln[order(ln$edon, ln$munn), c("edon","munn","inegi","mun","lisnom.06","lisnom.09","lisnom.12","lisnom.15","lisnom.18")]
rm(eq,tmp)

# will need to project lisnom from fed elecs (cf naylum), too many missings here
head(ln)
with(inc, table(is.na(mg)))
with(inc, table(is.na(lisnom)))
ls()

# adds object cen.yr (and mpv, unneeded) with yearly census projections
load(file="/home/eric/Desktop/naylum/data/electoral/nay2002-on.RData") 
rm(vpm)


unique(sub("[.][0-9]{2}", "", colnames(cen.yr))) # names omitting yrs
cen.yr$lisnom.12


# lag margin (measure of candidate quality)
library(DataCombine) # easy lags
inc$tmp <- sub("([a-z]+)[-][0-9]{2}([.][0-9]{3})", "\\1\\2", inc$emm) # drop elec cycle from emm
inc <- slide(data = inc, TimeVar = "yr", GroupVar = "tmp", Var = "mg", NewVar = "mg.lag",    slideBy = -1)
inc <- inc[order(inc$ord),] # sort
inc$tmp <- inc$suplente <- NULL

head(inc)

