#########################################
## Code for ayuntamiento vote analysis ##
## Date started: 26nov2023             ##
## By emagar                           ##
#########################################

rm(list = ls())
##
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data"
setwd(wd)

## useful functions
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
## Reads function
source( paste(pth, "moveme.r", sep = "/") )
source( paste(pth, "notin.r", sep = "/") )
rm(pth)


#########################
## get municipal votes ##
#########################
dat  <- read.csv(paste0(dd, "aymu1970-on.coalSplit.csv"), stringsAsFactors = FALSE)
## get coal agg version to take some info
vcoa <- read.csv(paste0(dd, "aymu1970-on.coalAgg.csv"),   stringsAsFactors = FALSE)
table(dat$emm==vcoa$emm) # same order?
## clean
dat <- within(dat, notas <- NULL)
## add info from vcoa
dat$dextra <- vcoa$dextra
dat$win    <- vcoa$win
dat$mg <- round((vcoa$v01 - vcoa$v02) / vcoa$efec, digits = 4)
## compute coal dummies
sel <- grep("l[0-9]+", colnames(vcoa))
l <- vcoa[,sel]
dat$dcoalpan <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-pan|pan-", x)) > 0, 1, 0))
dat$dcoalpri <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-pri|pri-", x)) > 0, 1, 0))
dat$dcoalprd <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-prd|prd-", x)) > 0, 1, 0))
dat$dcoalmor <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-morena|morena-", x)) > 0, 1, 0))
dat$dcoalpve <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-pvem|pvem-", x)) > 0, 1, 0))
dat$dcoalpt  <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-pt|pt-", x)) > 0, 1, 0))
dat$dcoalmc  <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-mc|mc-", x)) > 0, 1, 0))
##
## drop before 1994
sel <- which(dat$yr<1994)
dat <- dat[-sel,]
## clean
rm(l,vcoa)
##
## drop Belisario Domínguez, litigio after 2nd election
drop.r <- grep("xxx", dat$emm)
dat <- dat[-drop.r,]
rm(drop.r)
## drop these obs from analysis: races cancelled, missing, pending and others
## 6ago2020: check which cancelled don't have extraord data. drop them? would break lags... check in incumbents block too
table(dat$status)
drop.r <- grep("cancelled|missing|litigio|pending", dat$status)
dat <- dat[-drop.r,]
##
## drop runoffs where held in san luis potosí (win/mg retain eventual winner/margin)
sel <- grep("san-[0-9]+b", dat$emm) # these are first round races that led to runoff
tmp <- dat$emm[sel] ## get their ids
tmp <- sub(pattern = "^([a-z]+[-][0-9]+)b([.0-9]+)$", replacement = "\\1\\2", tmp) ## drop bs
sel2 <- which(dat$emm %in% tmp) ## select runoffs
dat$win[sel] <- dat$win[sel2] ## replace 1st round winners with runoff winners
dat$mg[sel]  <- dat$mg[sel2]  ## replace 1st round margin with runoff margin
dat <- dat[-sel2,] ## and drop runoffs
rm(sel,sel2,tmp)
##
## drop oaxaca cases that eventually became usos y costumbres
tmp <- c(1, 3, 8, 11, 12, 15, 17:20, 22, 24, 27, 29, 31, 33, 35:38, 42, 45:48, 50, 51, 54, 58, 60:65, 69, 71, 72, 74, 76:78, 83:87, 91:93, 94:98, 99:101, 104:111, 113:115, 117:123, 125:129, 131:133, 135, 137, 138:140, 142, 144:149, 151:156, 158, 159, 161:165, 167, 170, 172:176, 178, 179, 183, 186, 189:197, 200:224, 226:231, 233:236, 238:244, 246:253, 255:258, 260, 262:276, 279:281, 282:284, 286:289, 291:293, 296, 297, 299, 301, 303, 304, 306, 311, 313, 314:318, 320:323, 325, 326, 328:333, 335:337, 340:344, 346, 347, 349:359, 361:363, 365, 366, 368:374, 376, 378:380, 382:384, 386, 388, 389, 391:396, 398, 399, 401, 403:412, 416, 419, 420, 422:426, 428:430, 432, 433, 435, 437, 438, 440, 442:446, 448:454, 457, 458, 460, 461, 463:466, 468, 470, 471, 473, 475:481, 487, 488, 490:504, 506, 509:512, 514, 516:519, 521:524, 526:536, 538, 541:544, 546:548, 550, 552:554, 556, 560:564, 566, 568, 569, 16, 82, 310, 348, 367, 400, 88)
## ojo: 88 returned from uyc in 2013
tmp <- tmp + 20000
tmp1 <- as.numeric(dat$inegi)
sel <- which(tmp1 %in% tmp)
dat$emm[sel]
## tmp <- dat$emm[sel]
## tmp[order(tmp)]
if (length(sel)>0) dat <- dat[-sel,]

## # lo usé para detectar casos en aymu.incumbents con info que no correspondía con aymu.coalAgg
## sel <- c("emm", "edon", "mun", "munn", "ife", "inegi", "yr", "dy", "mo", "win")
## i <- merge(x = inc, y = dat[,sel],
##            by = c("emm", "edon", "mun", "munn", "ife", "inegi", "yr", "mo", "dy", "win"),
##            all = TRUE)
## write.csv(i, file = paste(dd, "tmp.csv", sep = ""), row.names = FALSE)

#################################################################
## prepare object with pan pri prd pvem pt mc morena oth votes ##
#################################################################
v7 <- dat # duplicate for manipulation
v7 <- within(v7, edon <- date <- dextra <- mg <- mun <- inegi <- ife <- ncand <- ncoal <- win <- efec <- lisnom <- NULL) # drop cols
v7 <- within(v7, dcoalpan <- dcoalpri <- dcoalprd <- dcoalmor <- dcoalpve <- dcoalpt <- dcoalmc <- NULL) # can be dropped, all coals split
v7$status <- NA ## may be used to record manip progress
v7[1,]
dim(v7)
## narrow v01..v25 into single column pair
v7$n <- 1:nrow(v7) # obs no
v7$r <- 1          # round
v7$v <- v7$v01     # vote
v7$l <- v7$l01     # label
#
tmp.orig <- v7 # duplicate
tmp <- tmp.orig
tmp$r <- 2
tmp$v <- tmp$v02; tmp$l <- tmp$l02;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 3
tmp$v <- tmp$v03; tmp$l <- tmp$l03;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 4
tmp$v <- tmp$v04; tmp$l <- tmp$l04;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 5
tmp$v <- tmp$v05; tmp$l <- tmp$l05;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 6
tmp$v <- tmp$v06; tmp$l <- tmp$l06;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 7
tmp$v <- tmp$v07; tmp$l <- tmp$l07;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 8
tmp$v <- tmp$v08; tmp$l <- tmp$l08;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 9
tmp$v <- tmp$v09; tmp$l <- tmp$l09;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 10
tmp$v <- tmp$v10; tmp$l <- tmp$l10;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 11
tmp$v <- tmp$v11; tmp$l <- tmp$l11;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 12
tmp$v <- tmp$v12; tmp$l <- tmp$l12;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 13
tmp$v <- tmp$v13; tmp$l <- tmp$l13;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 14
tmp$v <- tmp$v14; tmp$l <- tmp$l14;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 15
tmp$v <- tmp$v15; tmp$l <- tmp$l15;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 16
tmp$v <- tmp$v16; tmp$l <- tmp$l16;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 17
tmp$v <- tmp$v17; tmp$l <- tmp$l17;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 18
tmp$v <- tmp$v18; tmp$l <- tmp$l18;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 19
tmp$v <- tmp$v19; tmp$l <- tmp$l19;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 20
tmp$v <- tmp$v20; tmp$l <- tmp$l20;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 21
tmp$v <- tmp$v21; tmp$l <- tmp$l21;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 22
tmp$v <- tmp$v22; tmp$l <- tmp$l22;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 23
tmp$v <- tmp$v23; tmp$l <- tmp$l23;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 24
tmp$v <- tmp$v24; tmp$l <- tmp$l24;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 25
tmp$v <- tmp$v25; tmp$l <- tmp$l25;
v7 <- rbind(v7,tmp)
v7[1,]
## make sure all v and l columns have been added
v7$v01 <- v7$v02 <- v7$v03 <- v7$v04 <- v7$v05 <- v7$v06 <- v7$v07 <- v7$v08 <- v7$v09 <- v7$v10 <- v7$v11 <- v7$v12 <- v7$v13 <- v7$v14 <- v7$v15 <- v7$v16 <- v7$v17 <- v7$v18 <- v7$v19 <- v7$v20 <- v7$v21 <- v7$v22 <- v7$v23 <- v7$v24 <- v7$v25 <- NULL
v7$l01 <- v7$l02 <- v7$l03 <- v7$l04 <- v7$l05 <- v7$l06 <- v7$l07 <- v7$l08 <- v7$l09 <- v7$l10 <- v7$l11 <- v7$l12 <- v7$l13 <- v7$l14 <- v7$l15 <- v7$l16 <- v7$l17 <- v7$l18 <- v7$l19 <- v7$l20 <- v7$l21 <- v7$l22 <- v7$l23 <- v7$l24 <- v7$l25 <- NULL
##
## rebrand conve to mc etc
table(v7$l)
v7$l[v7$l=="conve|cdppn"] <- "mc"
v7$l[v7$l %in% c("pt1","ptc")] <- "pt"
v7$l[v7$l %in% c("pesm","pest")] <- "pes"
##v7$l[grep("indep|ci_", v7$l)] <- "indep"
## prepare object with pan pri prd pvem pt mc morena oth votes ##
v7$oth <- v7$morena <- v7$mc <- v7$pt <- v7$pvem <- v7$prd <- v7$pri <- v7$pan <- 0
## check if unbroken coalitions left
paste("No unbroken coalitions remaining?", length(v7$emm[grep("-", v7$l)])==0)
#
rm(tmp, tmp.orig)
#
## # change prd/morena to left
## sel <- which(v7$yr<=2015)
## v7$l[sel] <- sub("prd","left",v7$l[sel])
## sel <- which(v7$yr>=2015)
## v7$l[sel] <- sub("morena","left",v7$l[sel])
#
## # deal with major-party coalition below
## sel <- grep("(?=.*pan)(?=.*prd)", v7$l, perl = TRUE)
## v7$status[sel] <- "majors"
## ## sel <- grep("(?=.*pan)(?=.*left)", v7$l, perl = TRUE)
## ## v7$status[sel] <- "majors"
## sel <- grep("(?=.*pan)(?=.*pri)", v7$l, perl = TRUE)
## v7$status[sel] <- "majors"
## sel <- grep("(?=.*pri)(?=.*prd)", v7$l, perl = TRUE)
## v7$status[sel] <- "majors"
## ## sel <- grep("(?=.*pri)(?=.*left)", v7$l, perl = TRUE)
## ## v7$status[sel] <- "majors"
## #
## sel1 <- which(is.na(v7$status))
## sel <- grep("pan-|-pan|^pan$", v7$l[sel1])
## v7$pan[sel1][sel] <- v7$v[sel1][sel]
## v7$v[sel1][sel] <- 0; v7$l[sel1][sel] <- "0"; v7$status[sel1][sel] <- "done"
## #
## sel1 <- which(is.na(v7$status))
## sel <- grep("pri-|-pri|^pri$", v7$l[sel1])
## v7$pri[sel1][sel] <- v7$v[sel1][sel]
## v7$v[sel1][sel] <- 0; v7$l[sel1][sel] <- "0"; v7$status[sel1][sel] <- "done"
## #
## ## sel1 <- which(is.na(v7$status))
## ## sel <- grep("left-|-left|^left$", v7$l[sel1])
## ## v7$morena[sel1][sel] <- v7$v[sel1][sel]
## ## v7$v[sel1][sel] <- 0; v7$l[sel1][sel] <- "0"; v7$status[sel1][sel] <- "done"
## #
## sel1 <- which(is.na(v7$status))
## sel <- grep("prd-|-prd|^prd$", v7$l[sel1])
## v7$prd[sel1][sel] <- v7$v[sel1][sel]
## v7$v[sel1][sel] <- 0; v7$l[sel1][sel] <- "0"; v7$status[sel1][sel] <- "done"
## #
## sel1 <- which(is.na(v7$status))
## sel <- grep("morena-|-morena|^morena$", v7$l[sel1])
## v7$morena[sel1][sel] <- v7$v[sel1][sel]
## v7$v[sel1][sel] <- 0; v7$l[sel1][sel] <- "0"; v7$status[sel1][sel] <- "done"
## #
## # rest are other
## sel1 <- which(is.na(v7$status)) 
## v7$oth[sel1] <- v7$v[sel1]
## v7$v[sel1] <- 0; v7$l[sel1] <- "0"; v7$status[sel1] <- "done"
## #
## # 3-majors coalition in mun split in thirds
## sel <- which(v7$status=="majors") 
## sel1 <- grep("(?=.*pan)(?=.*pri)(?=.*prd)", v7$l[sel], perl = TRUE) # 
## v7$pan[sel][sel1] <- v7$v[sel][sel1] / 3; v7$pri[sel][sel1] <- v7$v[sel][sel1] / 3; v7$prd[sel][sel1] <- v7$v[sel][sel1] / 3; v7$v[sel][sel1] <- 0; v7$l[sel][sel1] <- "0"; v7$status[sel][sel1] <- "done"
## v7$dmajcoal[sel][sel1] <- 1
## #
## sel <- which(v7$status=="majors") 
## sel1 <- grep("(?=.*pan)(?=.*pri)(?=.*prd)", v7$l[sel], perl = TRUE) # 
## v7$pan[sel][sel1] <- v7$v[sel][sel1] / 3; v7$pri[sel][sel1] <- v7$v[sel][sel1] / 3; v7$prd[sel][sel1] <- v7$v[sel][sel1] / 3; v7$v[sel][sel1] <- 0; v7$l[sel][sel1] <- "0"; v7$status[sel][sel1] <- "done"
## v7$dmajcoal[sel][sel1] <- 1
## #
## # pan-pri to pri (19 cases in mic07 mic11 mic15)
## sel <- which(v7$status=="majors" & v7$edon==16) 
## sel1 <- grep("(?=.*pan)(?=.*pri)", v7$l[sel], perl = TRUE) # 
## v7$pri[sel][sel1] <- v7$v[sel][sel1]; v7$v[sel][sel1] <- 0; v7$l[sel][sel1] <- "0"; v7$status[sel][sel1] <- "done"
## v7$dmajcoal[sel][sel1] <- 1
## #
## # pri-prd to pri (chihuahua and guanajuato)
## sel <- which(v7$status=="majors") 
## sel1 <- grep("(?=.*pri)(?=.*prd)", v7$l[sel], perl = TRUE) # 
## v7$pri[sel][sel1] <- v7$v[sel][sel1]; v7$v[sel][sel1] <- 0; v7$l[sel][sel1] <- "0"; v7$status[sel][sel1] <- "done"
## v7$dmajcoal[sel][sel1] <- 1
## #
## # rest are pan-prd
## #
## # pan-prd to pan (bc coa2009 coa col00 col18 cua dgo jal que san sin son tam yuc)
## sel <- which(v7$status=="majors" & (v7$edon==2 | v7$edon==5 | v7$edon==6 | v7$edon==8 | v7$edon==10 | v7$edon==14 | v7$edon==22 | v7$edon==24 | v7$edon==25 | v7$edon==26 | v7$edon==28  | v7$edon==31)) 
## v7$pan[sel] <- v7$v[sel]; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## # pan-prd in 2018 to pan (bcs cps df gue mex mic oax pue qui tab zac)
## sel <- which(v7$status=="majors" & v7$yr==2018) 
## v7$pan[sel] <- v7$v[sel]; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## # pan-prd to prd (cps2004, cps2010)
## sel <- which(v7$status=="majors" & v7$edon==7 & v7$yr<=2010) 
## v7$prd[sel] <- v7$v[sel]; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## # pan-prd to pan (nay1999 nay2017 ver2000 ver2017)
## sel <- which(v7$status=="majors" & (v7$edon==18 | v7$edon==30) & (v7$yr==1999 | v7$yr==2000 | v7$yr==2017)) 
## v7$pan[sel] <- v7$v[sel]; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## # pan-prd split halfway (qui2016)
## sel <- which(v7$status=="majors" & v7$edon==23 & v7$yr==2016)
## v7$pan[sel] <- v7$v[sel] / 2; 
## v7$prd[sel] <- v7$v[sel] / 2; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## # pan-prd split halfway (pue2010 pue2013 qui2013 qui2010)
## sel <- which(v7$status=="majors" & (v7$edon==21 | v7$edon==23))
## v7$pan[sel] <- v7$v[sel]; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## # pan-prd split halfway (gue2015 hgo2011 mic2015 oax2010 oax2013 oax2016 zac2013, zac2016)
## sel <- which(v7$status=="majors" & (v7$edon==12 | v7$edon==13 | v7$edon==16 | v7$edon==20 | v7$edon==32) & v7$yr<2018) 
## v7$pan[sel] <- v7$v[sel] / 2; 
## v7$prd[sel] <- v7$v[sel] / 2; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## # pan-prd split halfway (mex2006)
## sel <- which(v7$status=="majors" & v7$edon==15 & v7$yr==2006) 
## v7$pan[sel] <- v7$v[sel] / 2; 
## v7$prd[sel] <- v7$v[sel] / 2; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## ## # used to check by hand
## ## sel <- which(v7$status=="majors") 
## ## sel1 <- grep("(?=.*pan)(?=.*prd)", v7$l[sel], perl = TRUE) # 
## ## table(v7$edon[sel])
## ## table(v7$yr[sel])
## ## table(v7$emm[sel][sel1], v7$l[sel][sel1])
## ## table(v7$emm[sel][sel1], v7$yr[sel][sel1])
## ## x

## move vote to proper column
sel.r <- which(v7$l=="pan")
v7$pan[sel.r] <- v7$v[sel.r]
v7$v[sel.r] <- 0 # to zero, has been moved
##
sel.r <- which(v7$l=="pri")
v7$pri[sel.r] <- v7$v[sel.r]
v7$v[sel.r] <- 0 # to zero, has been moved
##
sel.r <- which(v7$l=="prd")
v7$prd[sel.r] <- v7$v[sel.r]
v7$v[sel.r] <- 0 # to zero, has been moved
##
sel.r <- which(v7$l=="pvem")
v7$pvem[sel.r] <- v7$v[sel.r]
v7$v[sel.r] <- 0 # to zero, has been moved
##
sel.r <- which(v7$l=="pt")
v7$pt[sel.r] <- v7$v[sel.r]
v7$v[sel.r] <- 0 # to zero, has been moved
##
sel.r <- which(v7$l=="mc")
v7$mc[sel.r] <- v7$v[sel.r]
v7$v[sel.r] <- 0 # to zero, has been moved
##
sel.r <- which(v7$l=="morena")
v7$morena[sel.r] <- v7$v[sel.r]
v7$v[sel.r] <- 0 # to zero, has been moved
##
## remainder non-zero vs are other
v7$oth <- v7$v
##

tail(v7[v7$r==1,])
tail(v7[v7$r==2,])
tail(v7[v7$r==3,])
tail(v7[v7$r==4,])
tail(v7[v7$r==5,])
tail(v7[v7$r==6,])
tail(v7[v7$r==7,])
tail(v7[v7$r==8,])
tail(v7[v7$r==9,])

## consolidate
tmp <- v7[v7$r==1,] # will receive consolidated data
tmp$v <- tmp$l <- tmp$r <- NULL
dim(tmp)
##
for (i in 1:max(v7$n)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, max(v7$n)))
    tmp2 <- v7[v7$n==i, c("pan","pri","prd","pvem","pt","mc","morena","oth")]
    tmp2 <- colSums(tmp2)
    ##sum(tmp2)
    tmp[tmp$n==i,       c("pan","pri","prd","pvem","pt","mc","morena","oth")] <- tmp2 # plug consolidated data
}
v7 <- tmp
## clean
v7$n <- v7$status <- NULL
##
rm(tmp,drop.r,i,sel,sel.r,tmp1,tmp2)

## compute vote shares
v7[1,]
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","oth")
v <- v7[,sel.c] # subset votes columns
efec <- rowSums(v) # re-compute effective vote to remove rounding
efec[which(efec==0)] <- 1 # put 1 vote to missing races to avoid indeterminate shares
v[1,]
v <- round(v / efec, digits = 4)
table(rowSums(v))
v$oth <- 1 - rowSums(v[,-8]) # oth will absorb any dif from 1 due to rounding
table(rowSums(v))
v7[,sel.c] <- v # return vote shares to data
v7$efec <- efec
v7[1,]
dat[1,]
rm(efec,sel.c,v)


## return to dat
dat <- cbind(dat, v7[,c("pan","pri","prd","pvem","pt","mc","morena","oth")])
summary(dat$efec - v7$efec)
##dat$efec <- v7$efec # commenting this line keeps rounded efec
# keep 123 places, drop rest
dat <- within(dat, v01 <- v02 <- v03 <- v04 <- v05 <- v06 <- v07 <- v08 <- v09 <- v10 <- v11 <- v12 <- v13 <- v14 <- v15 <- v16 <- v17 <- v18 <- v19 <- v20 <- v21 <- v22 <- v23 <- v24 <- v25 <- NULL)
dat <- within(dat, l01 <- l02 <- l03 <- l04 <- l05 <- l06 <- l07 <- l08 <- l09 <- l10 <- l11 <- l12 <- l13 <- l14 <- l15 <- l16 <- l17 <- l18 <- l19 <- l20 <- l21 <- l22 <- l23 <- l24 <- l25 <- NULL)
## inspect
dat[1,]
table(dat$status)
##
## clean
rm(v7)

## Change san luis ids followed by runoff
sel <- grep("san-[0-9]+b", dat$emm) # these are first round races that led to runoff
tmp <- dat$emm[sel]
tmp <- sub(pattern = "(san-[0-9]+)b([.0-9]+$)", replacement = "\\1\\2", tmp)
dat$emm[sel] <- tmp
rm(tmp,sel)

## Save data
getwd()
save.image(file = "ay-mu-vote-analysis.RData")

## read saved image
rm(list = ls())
##
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data"
setwd(wd)
load(file = "ay-mu-vote-analysis.RData")

#####################################################################
## Get incumbency data                                             ##
## relies on prior.inc.part, added to aymu.incumbent.csv, for this ##
#####################################################################
inc <- read.csv(paste0(dd, "aymu1989-on.incumbents.csv"), stringsAsFactors = FALSE)
## drop pre-1995 and some cols
sel.r <- which(inc$yr < 1994)
sel.c <- which(colnames(inc) %in% c("ord","dextra","edon","source","dmujer","runnerup","part2nd","mg","dlegacy","who"))
inc <- inc[-sel.r,-sel.c]
## verify time series' structure (for lags)
library(DataCombine) # easy lags
inc$cycle <- as.numeric(sub("^[a-z]+[-]([0-9]+)[.].+$", "\\1", inc$emm))
inc <- inc[order(inc$emm),] # verify sorted before lags
inc <- slide(inc, Var = "cycle", NewVar = "cycle.lag", TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
verif <- inc$cycle - inc$cycle.lag
table(verif) # verify: all should be 1 then ok to lag
rm(verif)
tail(inc) ## check lag or lead
inc$cycle.lag <- NULL
##
## lag to create race-prior variables ##
inc <- slide(inc, Var = "race.after", NewVar = "race.prior",    TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
##
## merge inc into dat
#sel <- which(inc$emm %notin% dat$emm)
#data.frame(emm=inc$emm[sel], yr=inc$yr[sel], mun=inc$mun[sel], incumbent=inc$incumbent[sel])
dat <- merge(x = dat, y = inc[,c("emm","race.prior","prior.inc.part")], by = "emm", all.x = TRUE, all.y = FALSE)
## compute incumbent dummies
dat <- within(dat, {
    dincoth <- 0
    dincmorena <- 0
    dincmc <- 0
    dincpt <- 0
    dincpvem <- 0
    dincprd <- 0
    dincpri <- 0
    dincpan <- 0
})
sel.r <- grep("Reelected|Reran", dat$race.prior)
dat2 <- dat[sel.r,] # subset for manipulation
dat2[1,]
prior.inc.part <- dat2$prior.inc.part ## indicate progress here
## code dummies (emptying race.prior as it proceeds)
sel <- grep("pan",    prior.inc.part); dat2$dincpan   [sel] <- 1; prior.inc.part <- sub("pan",    "", prior.inc.part)
sel <- grep("pri",    prior.inc.part); dat2$dincpri   [sel] <- 1; prior.inc.part <- sub("pri",    "", prior.inc.part)
sel <- grep("prd",    prior.inc.part); dat2$dincprd   [sel] <- 1; prior.inc.part <- sub("prd",    "", prior.inc.part)
sel <- grep("pvem",   prior.inc.part); dat2$dincpvem  [sel] <- 1; prior.inc.part <- sub("pvem",   "", prior.inc.part)
sel <- grep("pt",     prior.inc.part); dat2$dincpt    [sel] <- 1; prior.inc.part <- sub("pt",     "", prior.inc.part)
sel <- grep("mc",     prior.inc.part); dat2$dincmc    [sel] <- 1; prior.inc.part <- sub("mc",     "", prior.inc.part)
sel <- grep("morena", prior.inc.part); dat2$dincmorena[sel] <- 1; prior.inc.part <- sub("morena", "", prior.inc.part)
##
prior.inc.part <- sub("^(-+)$", "", prior.inc.part) ## slashes with no text to empty (will become dincoth=0)
prior.inc.part <- sub("^concejo municipal$", "", prior.inc.part) ## will become dincoth=0
prior.inc.part <- sub("^incumbent out$", "", prior.inc.part) ## will become dincoth=0
##
table(prior.inc.part, useNA = "ifany") ## only minor parties or none should remain
##
## code dummy
dat2$dincoth <- ifelse(prior.inc.part=="", 0, 1) ## empty to 0, rest to 1
## return to data
dat[sel.r,] <- dat2
## clean
rm(prior.inc.part,dat2,sel,sel.c,sel.r)
dat$prior.inc.part <- NULL
## inspect
tail(dat)

## Generate lags
## dat xsts not square, use inc (which is) to add missing obs
## add cycle
tmp <- dat$emm
tmp <- sub("^[a-z]+-([0-9]{2})[ab]?[.][0-9]+$", "\\1", tmp) ## ab for anuladas en pre-runoffs
table(tmp)
tmp <- as.numeric(tmp)
dat$cycle <- tmp
rm(tmp)
##
table(dat$cycle)
table(inc$cycle)
##
tmp <- inc[, c("emm","cycle")] ## keep ids and temp ids only
tmp <- merge(x=dat, y=tmp, by=c("emm","cycle"), all=TRUE)
tmp <- tmp[order(tmp$emm),]
dim(tmp)
dim(inc)
dim(dat)
## drop these after merge
sel.r <- which(tmp$emm %in% c("cps-15.xxx", "cps-16.xxx", "cps-17.xxx", "oax-09.088", "oax-10.088", "oax-11.088", "oax-12.088", "oax-13.088", "oax-14.088", "oax-15.088", "oax-16.088", "oax-17.088", "oax-18.088"))
tmp <- tmp[-sel.r,]
## fill missing ids in new obs
sel.r <- which(is.na(tmp$inegi))
edon <- sub("^([a-z]+)-.+$", "\\1", tmp$emm[sel.r])
edon <- ifelse(edon=="oax", 20, 32)
tmp$edon[sel.r] <- edon
inegi <- as.numeric(sub("^[a-z]+-[0-9]+[.]([0-9]+)$", "\\1", tmp$emm[sel.r]))
inegi <- edon*1000 + inegi
tmp$inegi[sel.r] <- inegi
## replace dat with manipulatd object
dat <- tmp
rm(edon, inegi, tmp, sel.r, inc)
## lag vote by one period
dat <- dat[order(dat$emm),] # sort mun-chrono
dat <- slide(dat, Var = "pan",    NewVar = "panlag",    TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
dat <- slide(dat, Var = "pri",    NewVar = "prilag",    TimeVar = "cycle", GroupVar = "inegi", slideBy = -1, keepInvalid = FALSE)
dat <- slide(dat, Var = "prd",    NewVar = "prdlag",    TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
dat <- slide(dat, Var = "pvem",   NewVar = "pvemlag",   TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
dat <- slide(dat, Var = "pt",     NewVar = "ptlag",     TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
dat <- slide(dat, Var = "mc",     NewVar = "mclag",     TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
dat <- slide(dat, Var = "morena", NewVar = "morenalag", TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
##
## cómo lidio con missing periods? generan NAs en la serie del municipio donde ocurren
summary(dat$pri)
summary(dat$prilag)
table(is.na(dat$prilag))

## Compute electoral calendar variables
## add missing dates manually
date <- dat$date
sel.r <- which(is.na(date))
date[sel.r] <- c(rep(19951112, 16), 20011007, rep(20041003, 2), 20071007, 19950806)
dat$date <- date
## date format
library(lubridate)
dat$date <- ymd(dat$date)
date <- ymd(date)
summary(date) # no NAs
##
## get electoral calendar
cal <- read.csv(file = paste0(dd, "../../calendariosReelec/data/fechasEleccionesMexicoDesde1994.csv"))
sel.r <- which(cal$elec=="gob" | cal$elec=="dip"); cal <- cal[sel.r,] # subset dip and gob
cal[6,]
## translate months to english
library(stringr)
to.eng <- function(x) str_replace_all(x, c("ene"="jan", "abr"="apr", "ago"="aug", "dic"="dec"))
cal <- as.data.frame(sapply(cal, to.eng))
## to date format
sel.c <- grep("^y[0-9]{4}", colnames(cal))
year <- as.numeric(sub("y", "", colnames(cal)[sel.c]))
for (i in 1:nrow(cal)) cal[i,sel.c] <- paste0(cal[i,sel.c], year)
for (i in 1:nrow(cal)) cal[i,sel.c] <- sub("--[0-9]{4}", "", cal[i,sel.c])
for (i in 1:nrow(cal)) cal[i,sel.c] <- sub("([0-9]+)([a-z]+)([0-9]{4})", "\\3-\\2-\\1", cal[i,sel.c])
for (i in 1:ncol(cal)) cal[,sel.c[i]] <- ymd(cal[,sel.c[i]]) ## proceed columnwise to retain date format
cal[1,]
## dummies
dat$dconcdf <- 0
dat$dconcgo <- 0
for (i in 1:32){
    #i <- 5
    sel <- cal[cal$edon==i, sel.c]
    sel.r <- which(dat$date[dat$edon==i] %in% sel)
    if (length(sel.r)>0) dat$dconcgo[dat$edon==i][sel.r] <- 1
    sel <- cal[cal$elec=="dip", sel.c]
    sel.r <- which(dat$date[dat$edon==i] %in% sel)
    if (length(sel.r)>0) dat$dconcdf[dat$edon==i][sel.r] <- 1
}
table(dat$dconcgo)
table(dat$dconcdf)
rm(cal)
## code reform date
## get electoral calendar
cal <- read.csv(file = paste0(dd, "../../calendariosReelec/data/fechasEleccionesMexicoDesde1994.csv"))
sel.r <- which(cal$elec=="ayun"); cal <- cal[sel.r, c("edon","elec","yr1st")] # subset ayun
cal$elec <- NULL
cal$yr1st[cal$edon==13] <- 3000
cal$yr1st <- as.numeric(cal$yr1st)
dat <- merge(x = dat, y = cal, by = "edon", all = TRUE)
rm(cal,sel.r,date)

## Get governor parties
gov <- read.csv(file = paste0(dd, "../../mxBranches/statesBranches/32stategovts.csv"))
head(gov)
sel.r <- which(is.na(gov$govin) | gov$govin < 1000000) ## drop redundant rows
gov <- gov[-sel.r, c("edon","yr","govin","govpty")]    ## keep selected columns
gov <- slide(gov, Var = "govin", NewVar = "govout", TimeVar = "govin", GroupVar = "edon", slideBy = 1) ## lag 1
gov$govin <- ymd(gov$govin)
gov$govout <- ymd(gov$govout) - days(1) ## end of term
sel.r <- which(is.na(gov$govout))
gov$govout[sel.r] <- gov$govin[sel.r] + years(6) - days(1) ## add future ends of term
## Import gov pty to dat
dat$govpty <- NA
for (i in 1:32){ ## loop over states
    #i <- 1
    gov2 <- gov[gov$edon==i,]
    dat2 <- dat[dat$edon==i,]
    for (j in 1:nrow(gov2)){ ## loop over state cycles
        #j <- 2
        sel.r <- which(dat2$date >= gov2$govin[j] & dat2$date <= gov2$govout[j])
        if (length(sel.r) > 0) dat2$govpty[sel.r] <- gov2$govpty[j]
    }
    dat[dat$edon==i,] <- dat2 ## return to dat
}
table(dat$govpty, useNA = "ifany")
## clean
rm(dat2, date, gov, gov2, i, j, sel, sel.c, sel.r, year, to.eng)
## Code gov dummies
dat <- within(dat, {
    dgovmorena <- as.numeric(govpty=="morena")
    dgovmc     <- as.numeric(govpty=="mc")
    dgovpvem   <- as.numeric(govpty=="pvem")
    dgovprd    <- as.numeric(govpty=="prd")
    dgovpri    <- as.numeric(govpty=="pri")
    dgovpan    <- as.numeric(govpty=="pan")
})
dat <- within(dat, {
    dpresmorena <- as.numeric(date >= ymd("20181201") & date < ymd("20241001"))
    dprespri    <- as.numeric( date <  ymd("20001201") |
                              (date >= ymd("20121201") & date < ymd("20181201")))
    dprespan    <- as.numeric(date >= ymd("20001201") & date < ymd("20121201"))
})

## Incumbent ayuntamiento party
dat <- dat[order(dat$emm),]
dat <- slide(dat, Var = "win", NewVar = "winlast", TimeVar = "cycle", GroupVar = "inegi", slideBy = -1) ## lag 1
## compute dummies --- equal 1 if party won alone or in coalition last cycle
dat$daymorena <- dat$daymc     <- dat$daypt     <- dat$daypvem   <- dat$dayprd    <- dat$daypri    <- dat$daypan    <- 0
sel.r <- grep("pan",    dat$winlast); dat$daypan   [sel.r] <- 1
sel.r <- grep("pri",    dat$winlast); dat$daypri   [sel.r] <- 1
sel.r <- grep("prd",    dat$winlast); dat$dayprd   [sel.r] <- 1
sel.r <- grep("pvem",   dat$winlast); dat$daypvem  [sel.r] <- 1
sel.r <- grep("pt",     dat$winlast); dat$daypt    [sel.r] <- 1
sel.r <- grep("mc",     dat$winlast); dat$daymc    [sel.r] <- 1
sel.r <- grep("morena", dat$winlast); dat$daymorena[sel.r] <- 1
rm(sel.r)


dat2 <- dat ## duplicate
dat2  <-  within(dat2, dincballot <- as.numeric(dincpan==1 | dincpri==1 | dincprd==1 | dincpvem==1 | dincpt==1 | dincmc==1 | dincmorena==1 | dincoth==1))
dat2 <- within(dat2, {
    dincpan    <- daypan
    dincpri    <- daypri
    dincprd    <- dayprd
    dincmorena <- daymorena
})
summary(lm(pan ~ (dincpan * dincballot) + dgovpan + dprespan + panlag + as.factor(cycle), data = dat2))
summary(lm(pri ~ (dincpri * dincballot) + dgovpri + dprespri + prilag + as.factor(cycle), data = dat2))
summary(lm(morena ~ (dincmorena * dincballot) + dgovmorena + dpresmorena + morenalag, data = dat2, subset = yr>2014))

              
dat2 <- within(dat2, dincnopan <- as.numeric(dincpri==1 | dincprd==1 | dincpvem==1 | dincpt==1 | dincmc==1 | dincmorena==1 | dincoth==1))
summary(lm(pan ~ dincpan + dincnopan + dgovpan + dprespan + daypan + as.numeric(yr>=yr1st) + as.factor(yr) + as.factor(edon), data = dat2))
dat2 <- within(dat2, dincnopri <- as.numeric(dincpan==1 | dincprd==1 | dincpvem==1 | dincpt==1 | dincmc==1 | dincmorena==1 | dincoth==1))
summary(lm(pri ~ dincpri + dincnopri + dgovpri + dprespri + as.numeric(yr>=yr1st) + as.factor(yr) + as.factor(edon), data = dat2))



summary(lm(pan ~ dcoalpan + dcoalpri, data = dat)) ## Para ilustrar endogeneidad
ls()
dim(dat)



