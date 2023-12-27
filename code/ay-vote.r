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
vot  <- read.csv(paste0(dd, "aymu1970-on.coalSplit.csv"), stringsAsFactors = FALSE)
## get coal agg version to take some info
vcoa <- read.csv(paste0(dd, "aymu1970-on.coalAgg.csv"),   stringsAsFactors = FALSE)
table(vot$emm==vcoa$emm) # same order?
## clean
vot <- within(vot, notas <- NULL)
## add info from vcoa
vot$dextra <- vcoa$dextra
vot$win    <- vcoa$win
vot$mg <- round((vcoa$v01 - vcoa$v02) / vcoa$efec, digits = 4)
## compute coal dummies
sel <- grep("l[0-9]+", colnames(vcoa))
l <- vcoa[,sel]
vot$dcoalpan <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-pan|pan-", x)) > 0, 1, 0))
vot$dcoalpri <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-pri|pri-", x)) > 0, 1, 0))
vot$dcoalprd <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-prd|prd-", x)) > 0, 1, 0))
vot$dcoalmor <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-morena|morena-", x)) > 0, 1, 0))
vot$dcoalpve <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-pvem|pvem-", x)) > 0, 1, 0))
vot$dcoalpt  <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-pt|pt-", x)) > 0, 1, 0))
vot$dcoalmc  <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-mc|mc-", x)) > 0, 1, 0))
##
## drop before 1994
sel <- which(vot$yr<1994)
vot <- vot[-sel,]
## clean
rm(l,vcoa)
##
## drop Belisario Domínguez, litigio after 2nd election
drop.r <- grep("xxx", vot$emm)
vot <- vot[-drop.r,]
rm(drop.r)
## drop these obs from analysis
table(vot$status)
##drop.r <- grep("cancelled|missing|litigio|pending", vot$status)
drop.r <- grep("cancelled|litigio", vot$status)
vot <- vot[-drop.r,]
rm(drop.r)
## Fill NAs missing cases
sel.r <- grep("missing|pending|appoint", vot$status)
sel.c <- grep("^v[0-9]{2}", colnames(vot))
vot[sel.r, sel.c] <- NA
##
## drop runoffs held in san luis potosí (win/mg retain eventual winner/margin)
sel <- grep("san-[0-9]+b", vot$emm) # these are first round races that led to runoff
tmp <- vot$emm[sel] ## get their ids
tmp <- sub(pattern = "^([a-z]+[-][0-9]+)b([.0-9]+)$", replacement = "\\1\\2", tmp) ## drop bs
sel2 <- which(vot$emm %in% tmp) ## select runoffs
vot$win[sel] <- vot$win[sel2] ## replace 1st round winners with runoff winners
vot$mg[sel]  <- vot$mg[sel2]  ## replace 1st round margin with runoff margin
vot <- vot[-sel2,] ## and drop runoffs
rm(sel,sel2,tmp)
##
## drop oaxaca cases that eventually became usos y costumbres
tmp <- c(1, 3, 8, 11, 12, 15, 17:20, 22, 24, 27, 29, 31, 33, 35:38, 42, 45:48, 50, 51, 54, 58, 60:65, 69, 71, 72, 74, 76:78, 83:87, 91:93, 94:98, 99:101, 104:111, 113:115, 117:123, 125:129, 131:133, 135, 137, 138:140, 142, 144:149, 151:156, 158, 159, 161:165, 167, 170, 172:176, 178, 179, 183, 186, 189:197, 200:224, 226:231, 233:236, 238:244, 246:253, 255:258, 260, 262:276, 279:281, 282:284, 286:289, 291:293, 296, 297, 299, 301, 303, 304, 306, 311, 313, 314:318, 320:323, 325, 326, 328:333, 335:337, 340:344, 346, 347, 349:359, 361:363, 365, 366, 368:374, 376, 378:380, 382:384, 386, 388, 389, 391:396, 398, 399, 401, 403:412, 416, 419, 420, 422:426, 428:430, 432, 433, 435, 437, 438, 440, 442:446, 448:454, 457, 458, 460, 461, 463:466, 468, 470, 471, 473, 475:481, 487, 488, 490:504, 506, 509:512, 514, 516:519, 521:524, 526:536, 538, 541:544, 546:548, 550, 552:554, 556, 560:564, 566, 568, 569, 16, 82, 310, 348, 367, 400, 88)
## ojo: 88 returned from uyc in 2013
tmp <- tmp + 20000
tmp1 <- as.numeric(vot$inegi)
sel <- which(tmp1 %in% tmp)
vot$emm[sel]
## tmp <- vot$emm[sel]
## tmp[order(tmp)]
if (length(sel)>0) vot <- vot[-sel,]

## # lo usé para detectar casos en aymu.incumbents con info que no correspondía con aymu.coalAgg
## sel <- c("emm", "edon", "mun", "munn", "ife", "inegi", "yr", "dy", "mo", "win")
## i <- merge(x = inc, y = vot[,sel],
##            by = c("emm", "edon", "mun", "munn", "ife", "inegi", "yr", "mo", "dy", "win"),
##            all = TRUE)
## write.csv(i, file = paste(dd, "tmp.csv", sep = ""), row.names = FALSE)

#################################################################
## prepare object with pan pri prd pvem pt mc morena oth votes ##
#################################################################
v7 <- vot # duplicate for manipulation
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
## make sure all v and l columns have been added before next commands
v7$v01 <- v7$v02 <- v7$v03 <- v7$v04 <- v7$v05 <- v7$v06 <- v7$v07 <- v7$v08 <- v7$v09 <- v7$v10 <- v7$v11 <- v7$v12 <- v7$v13 <- v7$v14 <- v7$v15 <- v7$v16 <- v7$v17 <- v7$v18 <- v7$v19 <- v7$v20 <- v7$v21 <- v7$v22 <- v7$v23 <- v7$v24 <- v7$v25 <- NULL
v7$l01 <- v7$l02 <- v7$l03 <- v7$l04 <- v7$l05 <- v7$l06 <- v7$l07 <- v7$l08 <- v7$l09 <- v7$l10 <- v7$l11 <- v7$l12 <- v7$l13 <- v7$l14 <- v7$l15 <- v7$l16 <- v7$l17 <- v7$l18 <- v7$l19 <- v7$l20 <- v7$l21 <- v7$l22 <- v7$l23 <- v7$l24 <- v7$l25 <- NULL
##
## rebrand conve to mc etc
table(v7$l)
v7$l[v7$l %in% c("conve","cdppn")] <- "mc"
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
rm(tmp,i,sel,sel.r,sel.c,tmp1,tmp2)

## compute vote shares
v7[1,]
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","oth")
v <- v7[,sel.c] # subset votes columns
efec <- rowSums(v) # re-compute effective vote to remove rounding
efec[which(efec==0)] <- NA # put NA vote to missing races
v <- round(v / efec, digits = 4)
table(rowSums(v))
v$oth <- 1 - rowSums(v[,-8]) # oth will absorb any dif from 1 due to rounding
table(rowSums(v))
v7[,sel.c] <- v # return vote shares to data
v7$efec <- efec
v7[1,]
vot[1,]
rm(efec,sel.c,v)

## ## Transform left=prd up to morena, left=morena+prd < 2018, then left=morena since then
## v6 <- v7
## v6[1,]
## v6$left <- v6$prd
## sel <- which(v6$yr<2015)
## v6$prd[sel] <- 0 ## remove duplicates
## sel <- which(v6$yr>=2015 & v6$yr<2018)
## v6$left[sel] <- v6$prd[sel] + v6$morena[sel]
## v6$prd[sel] <- 0 ## remove duplicate
## sel <- which(v6$yr>=2018)
## v6$left[sel] <- v6$morena[sel]
## v6$morena <- NULL ## remove duplicate
## v6 <- v6[moveme(names(v6), "left before prd")]
## v6[1,]

## return to vot
vot[1,]
vot <- cbind(vot, v7[,c("pan","pri","prd","pvem","pt","mc","morena","oth")])
summary(vot$efec - v7$efec)
##vot$efec <- v7$efec # commenting this line keeps rounded efec
# keep named vote cols, drop rest
vot <- within(vot, v01 <- v02 <- v03 <- v04 <- v05 <- v06 <- v07 <- v08 <- v09 <- v10 <- v11 <- v12 <- v13 <- v14 <- v15 <- v16 <- v17 <- v18 <- v19 <- v20 <- v21 <- v22 <- v23 <- v24 <- v25 <- NULL)
vot <- within(vot, l01 <- l02 <- l03 <- l04 <- l05 <- l06 <- l07 <- l08 <- l09 <- l10 <- l11 <- l12 <- l13 <- l14 <- l15 <- l16 <- l17 <- l18 <- l19 <- l20 <- l21 <- l22 <- l23 <- l24 <- l25 <- NULL)
## inspect
vot[1,]
table(vot$status)
##
## clean
rm(v7)

## Change san luis ids followed by runoff
sel <- grep("san-[0-9]+b", vot$emm) # these are first round races that led to runoff
tmp <- vot$emm[sel]
tmp <- sub(pattern = "(san-[0-9]+)b([.0-9]+$)", replacement = "\\1\\2", tmp)
vot$emm[sel] <- tmp
rm(tmp,sel)

## Save data
getwd()
save.image(file = "tmp.RData")

######################
## read saved image ##
######################
library(DataCombine) # easy lags
rm(list = ls())
##
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data"
setwd(wd)
load(file = "tmp.RData")

## Fill missing NAs
sel.r <- which(vot$efec==0)
vot[sel.r,c("pan","pri","prd","pvem","pt","mc","morena","oth","efec")] <- NA

#####################################################################
## Get incumbency data                                             ##
## relies on prior.inc.part, added to aymu.incumbent.csv, for this ##
#####################################################################
inc <- read.csv(paste0(dd, "aymu1989-on.incumbents.csv"), stringsAsFactors = FALSE)
## drop pre-1995 and unanalyzed cols
sel.r <- which(inc$yr < 1994)
sel.c <- which(colnames(inc) %in% c("ord","dextra","edon","source","dmujer","runnerup","dlegacy","who","drepe","drepg"))
inc <- inc[-sel.r,-sel.c]
## change conve top mc
inc$part <- sub("conve|cdppn", "mc", inc$part)
inc$prior.inc.part <- sub("conve|cdppn", "mc", inc$prior.inc.part)
inc$inc.part.after <- sub("conve|cdppn", "mc", inc$inc.part.after)
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
inc <- slide(inc, Var = "race.after", NewVar = "race.prior", TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
inc <- slide(inc, Var = "part",       NewVar = "win.prior",  TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
inc <- slide(inc, Var = "part2nd",    NewVar = "run.prior",  TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
inc <- slide(inc, Var = "mg",         NewVar = "mg.prior",   TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
##
## merge inc into dat
#sel <- which(inc$emm %notin% vot$emm)
#data.frame(emm=inc$emm[sel], yr=inc$yr[sel], mun=inc$mun[sel], incumbent=inc$incumbent[sel])
vot <- merge(x = vot, y = inc[,c("emm","race.prior","prior.inc.part","part2nd","win.prior","run.prior","mg.prior")], by = "emm", all.x = TRUE, all.y = FALSE)
vot <- vot[,moveme(colnames(vot), "part2nd after win; mg.prior after mg; run.prior after mg; win.prior after mg")]
## compute incumbent dummies
vot <- within(vot, {
    dincoth <- 0
    dincmorena <- 0
    dincmc <- 0
    dincpt <- 0
    dincpvem <- 0
    dincprd <- 0
    dincpri <- 0
    dincpan <- 0
})
sel.r <- grep("Reelected|Reran", vot$race.prior)
vot2 <- vot[sel.r,] # subset for manipulation
prior.inc.part <- vot2$prior.inc.part ## indicate progress here
## code dummies (emptying race.prior as it proceeds)
sel <- grep("pan",    prior.inc.part); vot2$dincpan   [sel] <- 1; prior.inc.part <- sub("pan",    "", prior.inc.part)
sel <- grep("pri",    prior.inc.part); vot2$dincpri   [sel] <- 1; prior.inc.part <- sub("pri",    "", prior.inc.part)
sel <- grep("prd",    prior.inc.part); vot2$dincprd   [sel] <- 1; prior.inc.part <- sub("prd",    "", prior.inc.part)
sel <- grep("pvem",   prior.inc.part); vot2$dincpvem  [sel] <- 1; prior.inc.part <- sub("pvem",   "", prior.inc.part)
sel <- grep("pt",     prior.inc.part); vot2$dincpt    [sel] <- 1; prior.inc.part <- sub("pt",     "", prior.inc.part)
sel <- grep("mc",     prior.inc.part); vot2$dincmc    [sel] <- 1; prior.inc.part <- sub("mc",     "", prior.inc.part)
sel <- grep("morena", prior.inc.part); vot2$dincmorena[sel] <- 1; prior.inc.part <- sub("morena", "", prior.inc.part)
##
prior.inc.part <- sub("^(-+)$", "", prior.inc.part) ## slashes with no text to empty (will become dincoth=0)
prior.inc.part <- sub("^concejo municipal$", "", prior.inc.part) ## will become dincoth=0
prior.inc.part <- sub("^incumbent out$", "", prior.inc.part) ## will become dincoth=0
##
table(prior.inc.part, useNA = "ifany") ## only minor parties or none should remain
##
## code dummy
vot2$dincoth <- ifelse(prior.inc.part=="", 0, 1) ## empty to 0, rest to 1
## return to data
vot[sel.r,] <- vot2
## clean
rm(prior.inc.part,vot2,sel,sel.c,sel.r)
vot$prior.inc.part <- NULL
## inspect
tail(vot)

## For lags: vot xsts not square, use inc (which is) to add missing obs
## add cycle
tmp <- vot$emm
tmp <- sub("^[a-z]+-([0-9]{2})[ab]?[.][0-9]+$", "\\1", tmp) ## ab for anuladas and pre-runoffs
table(tmp)
tmp <- as.numeric(tmp)
vot$cycle <- tmp
rm(tmp)
##
table(vot$cycle)
table(inc$cycle)
##
tmp <- inc[, c("emm","cycle")] ## keep ids and temp ids only
tmp <- merge(x=vot, y=tmp, by=c("emm","cycle"), all=TRUE)
tmp <- tmp[order(tmp$emm),]
dim(tmp)
dim(inc)
dim(vot)
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
## function to complete missing ifes
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
source( paste(pth, "inegi2ife.r", sep = "/") )
rm(pth)
tmp$ife[sel.r] <- inegi2ife(tmp$inegi[sel.r])
rm(inegi2ife,ife2inegi,inegi2mun,ife2mun)
## fill missin yr
tmp$yr[sel.r][grep("oax-09", tmp$emm[sel.r])] <- 1995
tmp$yr[sel.r][grep("oax-11", tmp$emm[sel.r])] <- 2001
tmp$yr[sel.r][grep("oax-12", tmp$emm[sel.r])] <- 2004
tmp$yr[sel.r][grep("oax-13", tmp$emm[sel.r])] <- 2007
tmp$yr[sel.r][grep("zac-09", tmp$emm[sel.r])] <- 1995
## replace dat with manipulatd object
vot <- tmp
rm(edon, inegi, tmp, sel.r, inc)

## Compute electoral calendar variables
## add missing dates manually
date <- vot$date
sel.r <- which(is.na(date))
date[sel.r] <- c(rep(19951112, 16), 20011007, rep(20041003, 2), 20071007, 19950806)
vot$date <- date
## ## commented: dates stopped workibng with R upgrade
## ## date format
## library(lubridate)
## vot$date <- ymd(vot$date)
## date <- ymd(date)
## summary(date) # no NAs
##
## get electoral calendar
cal <- read.csv(file = paste0(dd, "../../calendariosReelec/data/fechasEleccionesMexicoDesde1994.csv"))
sel.r <- which(cal$elec=="gob" | cal$elec=="dip"); cal <- cal[sel.r,] # subset dip and gob
cal[6,]
## translate months to english

library(stringr)
to.eng <- function(x) str_replace_all(x, c("ene"="jan", "abr"="apr", "ago"="aug", "dic"="dec"))
cal <- as.data.frame(sapply(cal, to.eng))
## dates to yyyymmdd
sel.c <- grep("^y[0-9]{4}", colnames(cal))
year <- as.numeric(sub("y", "", colnames(cal)[sel.c]))
for (i in 1:nrow(cal)) cal[i,sel.c] <- paste0(cal[i,sel.c], year)
for (i in 1:nrow(cal)) cal[i,sel.c] <- sub("--[0-9]{4}", "", cal[i,sel.c])
for (i in 1:nrow(cal)) cal[i,sel.c] <- sub("([0-9]+)([a-z]+)([0-9]{4})", "\\3\\2\\1", cal[i,sel.c])
for (i in 1:nrow(cal)) cal[i,sel.c] <- sub("([a-z]{1})([1-9]{1})$", "\\10\\2", cal[i,sel.c]) # add heading zeroes for single-digit days
to.num <- function(x) str_replace_all(x, c("jan"="01", "feb"="02", "mar"="03", "apr"="04", "may"="05", "jun"="06", "jul"="07", "aug"="08", "sep"="09", "oct"="10", "nov"="11", "dec"="12"))
cal <- as.data.frame(sapply(cal, to.num))
##for (i in 1:length(sel.c)) cal[,sel.c[i]] <- ymd(cal[,sel.c[i]]) ## proceed columnwise to retain date format
## dummies
vot$dconcdf <- 0
vot$dconcgo <- 0
for (i in 1:32){
    #i <- 26
    sel <- cal[cal$edon==i, sel.c]
    sel.r <- which(vot$date[dat$edon==i] %in% sel)
    if (length(sel.r)>0) vot$dconcgo[vot$edon==i][sel.r] <- 1
    sel <- cal[cal$elec=="dip", sel.c]
    sel.r <- which(vot$date[vot$edon==i] %in% sel)
    if (length(sel.r)>0) vot$dconcdf[vot$edon==i][sel.r] <- 1
}
table(vot$dconcgo)
table(vot$dconcdf)
rm(cal)
## code reform date
## get electoral calendar
cal <- read.csv(file = paste0(dd, "../../calendariosReelec/data/fechasEleccionesMexicoDesde1994.csv"))
sel.r <- which(cal$elec=="ayun"); cal <- cal[sel.r, c("edon","elec","yr1st")] # subset ayun
cal$elec <- NULL
cal$yr1st[cal$edon==13] <- 3000
cal$yr1st <- as.numeric(cal$yr1st)
vot <- merge(x = vot, y = cal, by = "edon", all = TRUE)
rm(cal,sel.r,date)

## Get governor parties
gov <- read.csv(file = paste0(dd, "../../mxBranches/statesBranches/32stategovts.csv"))
head(gov)
sel.r <- which(is.na(gov$govin) | gov$govin < 1000000) ## drop redundant rows
gov <- gov[-sel.r, c("edon","yr","govin","govpty")]    ## keep selected columns
gov <- slide(gov, Var = "govin", NewVar = "govout", TimeVar = "govin", GroupVar = "edon", slideBy = 1) ## lag 1
##gov$govin <- ymd(gov$govin)
##gov$govout <- ymd(gov$govout) - days(1) ## end of term
sel.r <- which(is.na(gov$govout))
##gov$govout[sel.r] <- gov$govin[sel.r] + years(6) - days(1) ## add future ends of term
gov$govout[sel.r] <- gov$govin[sel.r] + 60000 ## add future ends of term (6 yrs)
## Import gov pty to vot
vot$govpty <- NA
for (i in 1:32){ ## loop over states
    #i <- 1
    gov2 <- gov[gov$edon==i,]
    vot2 <- vot[vot$edon==i,]
    for (j in 1:nrow(gov2)){ ## loop over state cycles
        #j <- 2
        sel.r <- which(vot2$date >= gov2$govin[j] & vot2$date < gov2$govout[j])
        if (length(sel.r) > 0) vot2$govpty[sel.r] <- gov2$govpty[j]
    }
    vot[vot$edon==i,] <- vot2 ## return to vot
}
table(vot$govpty, useNA = "ifany")
## clean
rm(vot2, gov, gov2, i, j, sel, sel.c, sel.r, year, to.eng)
## Code gov dummies
vot <- within(vot, {
    dgovmorena <- as.numeric(govpty=="morena")
    dgovmc     <- as.numeric(govpty=="mc")
    dgovpvem   <- as.numeric(govpty=="pvem")
    dgovprd    <- as.numeric(govpty=="prd")
    dgovpri    <- as.numeric(govpty=="pri")
    dgovpan    <- as.numeric(govpty=="pan")
})
## vot <- within(vot, {
##     dpresmorena <- as.numeric(vote >= ymd("20181201") & vote < ymd("20241001"))
##     dprespri    <- as.numeric( vote <  ymd("20001201") |
##                               (vote >= ymd("20121201") & vote < ymd("20181201")))
##     dprespan    <- as.numeric(vote >= ymd("20001201") & vote < ymd("20121201"))
## })
vot <- within(vot, {
    dpresmorena <- as.numeric(vote >= 20181201 & vote < 20241001)
    dprespri    <- as.numeric(vote <  20001201 |
                             (vote >= 20121201 & vote < 20181201))
    dprespan    <- as.numeric(vote >= 20001201 & vote < 20121201)
})

## Incumbent ayuntamiento party
vot <- vot[order(vot$emm),]
vot <- slide(vot, Var = "win", NewVar = "winlast", TimeVar = "cycle", GroupVar = "inegi", slideBy = -1) ## lag 1
## compute dummies --- equal 1 if party won alone or in coalition last cycle
vot$daymorena <- vot$daymc     <- vot$daypt     <- vot$daypvem   <- vot$dayprd    <- vot$daypri    <- vot$daypan    <- 0
sel.r <- grep("pan",    vot$winlast); vot$daypan   [sel.r] <- 1
sel.r <- grep("pri",    vot$winlast); vot$daypri   [sel.r] <- 1
sel.r <- grep("prd",    vot$winlast); vot$dayprd   [sel.r] <- 1
sel.r <- grep("pvem",   vot$winlast); vot$daypvem  [sel.r] <- 1
sel.r <- grep("pt",     vot$winlast); vot$daypt    [sel.r] <- 1
sel.r <- grep("mc",     vot$winlast); vot$daymc    [sel.r] <- 1
sel.r <- grep("morena", vot$winlast); vot$daymorena[sel.r] <- 1
rm(sel.r)

## triennium cat var (cycle breaks sequence when state calendars change)
vot$trienio <- cut(vot$yr, 
                   breaks=c(-Inf, seq(1992,2028,3), Inf),
                   labels=seq(from=1991, to=2030, by=3))
vot <- vot[,moveme(names(vot), "trienio before status")]

## state capital municipalities
sel <- which(vot$ife  %in%  c( 1001,
                               2002,
                               3003,
                               4001,
                               5030,
                               6001,
                               7102,
                               8019,
                               9015, # makes Cuauhtémoc DF's capital
                              10005,
                              11015,
                              12001,
                              13047,
                              14041,
                              15107,
                              16054,
                              17007,
                              18017,
                              19040,
                              20066,
                              21115,
                              22014,
                              23007,
                              24028,
                              25006,
                              26049,
                              27004,
                              28041,
                              29033,
                              30089,
                              31050,
                              32056))
vot$dcapital <- 0; vot$dcapital[sel] <- 1
rm(sel)

##################################################
## Get altitude and population from localidades ##
##################################################
## ## With 2010 censo
## ## unzip, read, then delete unzipped file
## system("unzip /home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/2010censo/00_nacional_2010_iter_zip.ZIP")
## ## get municipio altitude variance (from censo 2010 @ localidad level)
## library(foreign)
## alt <- read.dbf("ITER_nalDBF10/ITER_NALDBF10.dbf", as.is = TRUE)
## table(is.na(alt$X))
## dim(alt)
## ## erase unzipped file from disk, too large
## system("rm -r ITER_nalDBF10/")
## # selected columns only
## colnames(alt)
## sel <- grep("entidad|mun|loc|longitud|latitud|altitud|pobtot", colnames(alt), ignore.case = TRUE, perl = TRUE)
## alt <- alt[,sel]
## # rename variables
## colnames(alt) <- c("edon","inegi","mun","locn","localidad","long","lat","alt","ptot","drop")
## alt$drop <- NULL
## alt[1:15,]
##
## With 2020 censo
alt <- data.frame() # prep
for (i in 1:9){
    tmp <- read.csv(paste0("../../censos/raw/2020censo/localidades/poblacion/ITER_0", i, "CSV20.csv"))
    ## Keep selected indicators only
    tmp <- tmp[, c("ENTIDAD","MUN","NOM_MUN","LOC","NOM_LOC","LONGITUD","LATITUD","ALTITUD","POBTOT","P_18YMAS","P_5YMAS","P5_HLI")]
    alt <- rbind(alt, tmp)
}
for (i in 10:32){
    tmp <- read.csv(paste0("../../censos/raw/2020censo/localidades/poblacion/ITER_", i, "CSV20.csv"))
    ## Keep selected indicators only
    tmp <- tmp[, c("ENTIDAD","MUN","NOM_MUN","LOC","NOM_LOC","LONGITUD","LATITUD","ALTITUD","POBTOT","P_18YMAS","P_5YMAS","P5_HLI")]
    alt <- rbind(alt, tmp)
}
colnames(alt) <- c("edon","inegi","mun","locn","localidad","long","lat","alt","ptot","p18","p5","p5li")
alt[5:10,]
## 2020 long lat need manip
alt$long <- gsub("[°'\" WN]", "", alt$long)
alt$lat <- gsub("[°'\" WN]", "", alt$lat)
## 2020 alt
sel.r <- grep("^00[-]", alt$alt)
table(alt$alt[sel.r])
alt$alt <- gsub("^00[-]", "-", alt$alt)
## numeric vars
alt$edon <- as.numeric(as.character(alt$edon))
alt$inegi <- as.numeric(as.character(alt$inegi))
alt$mun <- as.character(alt$mun)
alt$locn <- as.numeric(as.character(alt$locn))
alt$localidad <- as.character(alt$localidad)
alt$long <- as.numeric(as.character(alt$long))
alt$lat <- as.numeric(as.character(alt$lat))
alt$alt <- as.numeric(as.character(alt$alt))
alt$ptot <- as.numeric(as.character(alt$ptot))
## alt$p18 <- as.numeric(as.character(alt$p18))
## alt$p5 <- as.numeric(as.character(alt$p5))
## alt$p5li <- as.numeric(as.character(alt$p5li))
## add edon to inegi
alt$inegi <- alt$edon*1000 + alt$inegi
## drop aggregate rows
sel <- which(alt$locn==0|alt$locn==9998|alt$locn==9999)
alt <- alt[-sel,]
## standardize long lat to 0=further E/S to 1=further W/N
alt <- within(alt, {
    longs <- (long - (min(long))) / (max(long) - min(long))
    lats  <- (lat  - (min(lat)))  / (max(lat)  - min(lat))
})
## mun pop share in localidad
alt$tmp      <- ave(alt$ptot, as.factor(alt$inegi), FUN=sum, na.rm=TRUE)
alt$popsh    <- alt$ptot / alt$tmp
alt$tmp <- NULL
summary(alt$popsh)
## Effective num of localidades (pop-wise)
alt$effloc <- ave(alt$popsh, as.factor(alt$inegi), FUN=function(x) 1/sum(x^2))
summary(alt$effloc)
##
## Population share in cabecera
## should capture center/periphery conflict
## ## 2010 localidad name has char encoding issues, this imports a previously exported version converted to utf-8
## localidades <- read.csv("../ancillary/localidades-2010-utf8.csv")
## table(localidades$locn==alt$locn) ## check all same order
## alt$localidad <- localidades$localidad
## rm(localidades)
## Cabecera is locn=1 in municipio, with very few exceptions. Most cabeceras share mun's name (not all: eg. Santa Rosalía for Mulegé)
alt$dcabecera <- as.numeric(alt$locn==1)
sel.r <- which(alt$inegi== 7033 & alt$locn== 42);  alt$dcabecera[sel.r] <- 1 ## inegi== 7033 Francisco León
sel.r <- which(alt$inegi==24056 & alt$locn== 2);   alt$dcabecera[sel.r] <- 1 ## inegi==24056 Villa de Arista
## ## cabeceras of municipios created after 2010 (for use when generating variables with censo 2010)
## sel.r <- which(alt$inegi== 2001 & alt$locn==857); alt$dcabecera[sel.r] <- 1 ## inegi== 2006 SAN QUINTIN
## sel.r <- which(alt$inegi== 7072 & alt$locn== 54;  alt$dcabecera[sel.r] <- 1 ## inegi== 7121 RINCON CHAMULA SAN PEDRO
## sel.r <- which(alt$inegi== 7080 & alt$locn== 13;  alt$dcabecera[sel.r] <- 1 ## inegi== 7125 HONDURAS DE LA SIERRA
## sel.r <- which(alt$inegi== 7092 & alt$locn== 31;  alt$dcabecera[sel.r] <- 1 ## inegi== 7124 MEZCALAPA
## sel.r <- which(alt$inegi== 7102 & alt$locn== 71;  alt$dcabecera[sel.r] <- 1 ## inegi== 7123 EMILIANO ZAPATA
## sel.r <- which(alt$inegi== 7107 & alt$locn==166;  alt$dcabecera[sel.r] <- 1 ## inegi== 7122 PARRAL--EL
## sel.r <- which(alt$inegi==14093 & alt$locn== 58;  alt$dcabecera[sel.r] <- 1 ## inegi==14126 CAPILLA DE GUADALUPE
## sel.r <- which(alt$inegi==23004 & alt$locn== 11;  alt$dcabecera[sel.r] <- 1 ## inegi==23010 BACALAR
## sel.r <- which(alt$inegi==23005 & alt$locn== 24;  alt$dcabecera[sel.r] <- 1 ## inegi==23011 PUERTO MORELOS
## ## cabeceras of municipios created after 2020 (for use when generating variables with censo 2020)
sel.r <- which(alt$inegi==4001 & alt$locn== 7);  alt$dcabecera[sel.r] <- 1 ## inegi==4013 DZITBALCHE
sel.r <- which(alt$inegi==4001 & alt$locn %in% c(3,7));  alt$inegi[sel.r] <- 4013 ## inegi==4013 localidades in DZITBALCHE
## sel.r <- which(alt$inegi==14093 & alt$locn== 58;  alt$dcabecera[sel.r] <- 1 ## inegi==14126 CAPILLA DE GUADALUPE
alt$popshincab <- alt$popsh * alt$dcabecera
alt$popshincab <- ave(alt$popshincab, as.factor(alt$inegi), FUN=sum) ## put value in all localidades
summary(alt$popshincab)
## weighted mean(alt) and sd(alt)
alt$altpopsh <- alt$popsh * alt$alt 
alt$wmeanalt <- ave(alt$altpopsh, as.factor(alt$inegi), FUN=sum, na.rm=TRUE)
alt$altpopsh <- alt$popsh * (alt$alt - alt$wmeanalt)^2
alt$wsdalt <-   ave(alt$altpopsh, as.factor(alt$inegi), FUN=sum, na.rm=TRUE)
alt$wsdalt <-   sqrt(alt$wsdalt)
alt$altpopsh <- NULL # clean
# mean(alt) and sd(alt)
alt$meanalt <- ave(alt$alt, as.factor(alt$inegi), FUN=mean, na.rm=TRUE)
alt$sdalt <-   ave(alt$alt, as.factor(alt$inegi), FUN=sd,   na.rm=TRUE)
# cases with single localidad sd=NA
sel <- which(is.na(alt$sdalt)==TRUE & is.na(alt$meanalt)==FALSE)
alt$inegi[sel]
alt$wsdalt[sel] <- 0
alt$sdalt[sel] <- 0
##
## drop redundant rows and cols
alt <- alt[-duplicated(as.factor(alt$inegi))==FALSE,]
alt[1,]
alt$locn <- alt$localidad <- alt$alt <- alt$mun <- alt$edon <- alt$popsh <- alt$dcabecera <- NULL
alt$wmeanalt <- round(alt$wmeanalt, 1)
alt$wsdalt <- round(alt$wsdalt, 1)
alt$meanalt <- round(alt$meanalt, 1)
alt$sdalt <- round(alt$sdalt, 1)
#summary(alt$sdalt)
#summary(alt$wsdalt)
##
## add geo vars to vot
dim(alt)
alt[1,]
vot[1,]
sel.c <- c("inegi","longs","lats","effloc","popshincab","wmeanalt","wsdalt","meanalt","sdalt") # columns to merge into vot
vot <- merge(x = vot, y = alt[,sel.c], by = "inegi", all.x = TRUE, all.y = FALSE)
rm(i,sel,sel.r,sel.c,tmp) # clean
summary(vot$sdalt)
##

## re-define party-by-party incumbency variables
## rename vars: dincpan will now mean outgoing mayor is pan etc, dincballotpan that outgoing mayior is in ballot again
colnames(vot) <- gsub("^dinc", "dincballot", colnames(vot))
## generic dummy dincballot=1 if outgoing mayor (regardless of party) is in ballot again
vot <- within(vot, {
    dincballot <- as.numeric(dincballotpan==1 | dincballotpri==1 | dincballotprd==1 | dincballotpvem==1 | dincballotpt==1 | dincballotmc==1 | dincballotmorena==1 | dincballototh==1)
})
## rename accordingly daypan to dincpan etc
colnames(vot) <- gsub("^day", "dinc", colnames(vot))
vot[1,]

## Separate id and non-time varying vars into own data.frame
colnames(vot)
sel.c <- which(colnames(vot) %in% c("inegi","edon","cycle","yr","ife","mun","date","trienio","status","dcapital","longs","lats","effloc","popshincab","wmeanalt","wsdalt","meanalt","sdalt"))
ids <- vot$emm
ids <- cbind(emm=ids, vot[,sel.c])
vot <- vot[,-sel.c]
ids[1,]
vot[1,]

## Import municipal electoral histories for ids
elhis <- data.frame()
for (yr in seq(1994,2024,3)){
    #yr <- 2009
    tmp <- read.csv(file = paste0(dd, "v-hats-etc/mun/dipfed-municipio-vhat-", yr, ".csv"))
    tmp[1,]
    elhis <- rbind(elhis, tmp)
}
## keep selected vars only
sel.c <- grep("yr|ife|vhat|alpha|beta", colnames(elhis))
elhis <- elhis[, sel.c]
## alpha and beta to ids
tmp <- elhis[which(duplicated(elhis$ife)==FALSE), c("ife","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")]
ids <- merge(x = ids, y = tmp, by = "ife", all.x = TRUE, all.y = FALSE)
## vhats to vot
## function to complete missing ifes
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
source( paste(pth, "inegi2ife.r", sep = "/") )
rm(pth)
elhis$inegi <- ife2inegi(elhis$ife)
## use info from elecRetrns/ancillary/mun-yrs to deduct state cycle from federal yr
elhis$edon <- as.integer(elhis$inegi/1000)
elhis$cycle <- elhis$yr
sel <- which(elhis$edon==1)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==1],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019, 2021, 2024
                              to = 4:19)
sel <- which(elhis$edon==2)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==2],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==3)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==3],
                              from = c(1979, 1982, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2015, 2018, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==4)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==4],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==5)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==5],
                              from = c(1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012,       2018, 2021, 2024),
                           ##state els 1981, 1984, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2009, 2013, 2017, 2018, 2021, 2024),
                              to = c(4:14,16:18)) ## skip 15 that will be a manual duplicate of 16 below
sel <- which(elhis$edon==6)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==6],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==7)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==7],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1995, 1998, 2001, 2004, 2007, 2010, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==8)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==8],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==9)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==9],
                              from = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 11:19)
sel <- which(elhis$edon==10)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==10],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019, 2022, 2025),
                              to = 4:19)
sel <- which(elhis$edon==11)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==11],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==12)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==12],
                              from = c(1979, 1982, 1985, 1988, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1993, 1996, 1999, 2002, 2005, 2008, 2012, 2015, 2018, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==13)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==13],
                              from = c(1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2021, 2024),
                           ##state els 1981, 1984, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2016, 2020, 2024),
                              to = 4:17)
sel <- which(elhis$edon==14)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==14],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1992, 1995, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==15)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==15],
                              from = c(1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1981, 1984, 1987, 1990, 1993, 1996, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==16)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==16],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2011, 2015, 2018, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==17)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==17],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==18)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==18],
                              from = c(1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1981, 1984, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2014, 2017, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==19)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==19],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==20)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==20],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==21)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==21],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2018, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==22)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==22],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==23)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==23],
                              from = c(1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006,       2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1981, 1984, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2010, 2013, 2016, 2018, 2021, 2024),
                              to = c(4:12,14:19)) ## skip 13 that will be a manual duplicate of 14 below
sel <- which(elhis$edon==24)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==24],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==25)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==25],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==26)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==26],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==27)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==27],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==28)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==28],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==29)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==29],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==30)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==30],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2004, 2007, 2010, 2013, 2017, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==31)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==31],
                              from = c(1982, 1985, 1988, 1991, 1994,       1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1981, 1984, 1987, 1990, 1993, 1995, 1998, 2001, 2004, 2007, 2010, 2012, 2016, 2018, 2021, 2024),
                              to = c(4:8,10:19))  ## skip 9 that will be a manual duplicate of 8 below
sel <- which(elhis$edon==32)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==32],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024),
                              to = 4:19)
## duplicate cycle 16 in yucatán and make it cycle 15
sel <- which(elhis$edon==5 & elhis$cycle==16)
tmp <- elhis[sel,]
tmp$cycle <- 15
elhis <- rbind(elhis, tmp)
## duplicate cycle 14 in quintana roo and make it cycle 13
sel <- which(elhis$edon==23 & elhis$cycle==14)
tmp <- elhis[sel,]
tmp$cycle <- 13
elhis <- rbind(elhis, tmp)
## duplicate cycle 8 in yucatán and make it cycle 9
sel <- which(elhis$edon==31 & elhis$cycle==8)
tmp <- elhis[sel,]
tmp$cycle <- 9
elhis <- rbind(elhis, tmp)
## build emm for merging
elhis$edo <- mapvalues(x=elhis$edon,
                          from = 1:32,
                          to = c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac"))
tmp <- sub('.*(?=.{3}$)', '', elhis$inegi, perl=TRUE) ## last three characters of inegi
elhis$emm <- paste0(elhis$edo, "-0", elhis$cycle, ".", tmp) ## adds zero heading cycle
elhis$emm <- sub("-0([1-9][0-9])", "-\\1", elhis$emm)       ## drop heading zero in emm when followed by non-zero and another digit
elhis <- within(elhis, edo <- alphahat.pan <- alphahat.pri <- alphahat.left <- betahat.pan <- betahat.left <- NULL)
##
vot <- merge(x = vot, y = elhis[, c("emm","vhat.pan","vhat.pri","vhat.left")], by = "emm", all.x = TRUE, all.y = FALSE)


## duplicate vot for lucardi-rosas selection criteria
sel.c <- which(colnames(vot) %in% c("emm", "win", "part2nd", "mg", "win.prior", "run.prior", "mg.prior"))
luro <- vot[, sel.c]
vot$win.prior <- vot$run.prior <- vot$mg.prior <- NULL
rm(alt,elhis,ife2inegi,ife2mun,inegi2ife,inegi2mun,sel,sel.c,tmp,to.num,yr) ## clean


## Save data
getwd()
save.image(file = "ay-mu-vote-analysis.RData")

######################
## read saved image ##
######################
library(DataCombine) # easy lags
rm(list = ls())
##
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data"
setwd(wd)
load(file = "ay-mu-vote-analysis.RData")

## alternative to interaction
vot[1,]
vot <- within(vot, {
    #dinomorena     <- as.numeric(dincmorena==0 & dincballot==0) ## omited
    dimorena       <- as.numeric(dincmorena==1 & dincballot==0)
    diballnomorena <- as.numeric(dincmorena==0 & dincballot==1)
    diballmorena   <- as.numeric(dincmorena==1 & dincballot==1)
    ##
    diprd       <- as.numeric(dincprd==1 & dincballot==0)
    diballnoprd <- as.numeric(dincprd==0 & dincballot==1)
    diballprd   <- as.numeric(dincprd==1 & dincballot==1)
    ##
    #dinopri     <- as.numeric(dincpri==0 & dincballot==0) ## omited
    dipri       <- as.numeric(dincpri==1 & dincballot==0)
    diballnopri <- as.numeric(dincpri==0 & dincballot==1)
    diballpri   <- as.numeric(dincpri==1 & dincballot==1)
    ##
    #dinopan     <- as.numeric(dincpan==0 & dincballot==0) ## omited
    dipan       <- as.numeric(dincpan==1 & dincballot==0)
    diballnopan <- as.numeric(dincpan==0 & dincballot==1)
    diballpan   <- as.numeric(dincpan==1 & dincballot==1)
})


#############################################################
## Prepare different specifications of the DV for analysis ##
#############################################################
colnames(vot)
vot[1,]
## vot has raw vote shares
res <- vot ## residual v - vhat
v4t <- vot ## pan pri left oth shares
r4t <- vot ## pan/pri left/pri oth/pri ratios
##

## duplicate vot for pan pri left oth breakdown
vot <- vot[order(vot$emm),]; ids <- ids[order(ids$emm),]
table(vot$emm==ids$emm)
v4t <- vot
v4t$left <- NA
v4t$left[ids$yr<2015]                  <- vot$prd[ids$yr<2015]
v4t$prd[ids$yr<2015]                   <- 0
v4t$left[ids$yr>=2015 & ids$yr<2018]   <- v4t$prd[ids$yr>=2015 & ids$yr<2018] + v4t$morena[ids$yr>=2015 & ids$yr<2018]
v4t$prd[ids$yr>=2015 & ids$yr<2018]    <- 0
v4t$morena[ids$yr>=2015 & ids$yr<2018] <- 0
v4t$left[ids$yr>=2018]                 <- v4t$morena[ids$yr>=2018]
v4t$morena[ids$yr>=2018]               <- 0
v4t <- within(v4t, oth <- 1 - pan - pri - left)
v4t <- within(v4t, prd <- morena <- pvem <- pt <- mc <- NULL)
v4t <- v4t[, moveme(colnames(v4t), "left after pri")]
##
v4t$dincballotleft <- NA
v4t$dincballotleft[ids$yr<2015]                  <- vot$dincballotprd[ids$yr<2015]
v4t$dincballotprd[ids$yr<2015]                   <- 0
v4t$dincballotleft[ids$yr>=2015 & ids$yr<2018]   <- v4t$dincballotprd[ids$yr>=2015 & ids$yr<2018] + v4t$dincballotmorena[ids$yr>=2015 & ids$yr<2018]
v4t$dincballotprd[ids$yr>=2015 & ids$yr<2018]    <- 0
v4t$dincballotmorena[ids$yr>=2015 & ids$yr<2018] <- 0
v4t$dincballotleft[ids$yr>=2018]                 <- v4t$dincballotmorena[ids$yr>=2018]
v4t$dincballotmorena[ids$yr>=2018]               <- 0
v4t <- within(v4t, dincballototh <- dincballototh + dincballotprd + dincballotmorena + dincballotpvem + dincballotpt + dincballotmc)
v4t <- within(v4t, dincballototh <- as.numeric(dincballototh > 0))
v4t <- within(v4t, dincballotprd <- dincballotmorena <- dincballotpvem <- dincballotpt <- dincballotmc <- NULL)
v4t <- v4t[, moveme(colnames(v4t), "dincballotleft after dincballotpri")]
##
v4t$dgovprd <- v4t$dgovprd + v4t$dgovmorena
colnames(v4t)[grep("dgovprd", colnames(v4t))] <- "dgovleft"
v4t <- within(v4t, dgovoth <- as.numeric((dgovpvem + dgovmc) > 0))
v4t <- within(v4t, dgovmorena <- dgovpvem <- dgovmc <- NULL)
v4t <- v4t[, moveme(colnames(v4t), "dgovoth after dgovleft")]
##
colnames(v4t)[grep("dpresmorena", colnames(v4t))] <- "dpresleft"
##
v4t$dincleft <- NA
v4t$dincleft[ids$yr<2015]                  <- vot$dincprd[ids$yr<2015]
v4t$dincprd[ids$yr<2015]                   <- 0
v4t$dincleft[ids$yr>=2015 & ids$yr<2018]   <- v4t$dincprd[ids$yr>=2015 & ids$yr<2018] + v4t$dincmorena[ids$yr>=2015 & ids$yr<2018]
v4t$dincprd[ids$yr>=2015 & ids$yr<2018]    <- 0
v4t$dincmorena[ids$yr>=2015 & ids$yr<2018] <- 0
v4t$dincleft[ids$yr>=2018]                 <- v4t$dincmorena[ids$yr>=2018]
v4t$dincmorena[ids$yr>=2018]               <- 0
v4t <- within(v4t, dincoth <- dincprd + dincmorena + dincpvem + dincpt + dincmc)
v4t <- within(v4t, dincoth <- as.numeric(dincoth > 0))
v4t <- within(v4t, dincprd <- dincmorena <- dincpvem <- dincpt <- dincmc <- NULL)
v4t <- v4t[, moveme(colnames(v4t), "dincleft after dincpri")]
##
v4t <- within(v4t, {
    dimorena <- dimorena + diprd
    diballmorena <- diballmorena + diballprd
    diballnomorena <- as.numeric((diballnomorena + diballnoprd) > 0)
})
sel.c <- grep("di(ball)?(no)?morena", colnames(v4t))
tmp <- colnames(v4t)[sel.c]
tmp <- sub("di(ball)?(no)?morena", "di\\1\\2left", tmp)
colnames(v4t)[sel.c] <- tmp
v4t <- within(v4t, diprd <- diballprd <- diballnoprd <- NULL)
rm(tmp,sel.c)
##
v4t$dcoalleft <- NA
v4t$dcoalleft[ids$yr<2015]                <- v4t$dcoalprd[ids$yr<2015]
v4t$dcoalleft[ids$yr>=2015 & ids$yr<2018] <- v4t$dcoalprd[ids$yr>=2015 & ids$yr<2018] + v4t$dcoalmor[ids$yr>=2015 & ids$yr<2018]
v4t$dcoalleft[ids$yr>=2015 & ids$yr<2018] <- as.numeric(v4t$dcoalleft[ids$yr>=2015 & ids$yr<2018]>0)
v4t$dcoalleft[ids$yr>=2018]               <- v4t$dcoalmor[ids$yr>=2018]
v4t <- within(v4t, dcoalprd <- dcoalmor <- dcoalpve <- dcoalpt <- dcoalmc <- NULL)
v4t <- v4t[, moveme(colnames(v4t), "dcoalleft after dcoalpri")]

## Deal with zeroes as Aitchison
## If .0005 is the maximum rounding error and unit u has C zeroes and D-C non-zeroes add
## .0005 (C+1)(D-C) / D^2
## to zero categories and subtract
## .0005 (C+1)C / D^2
## to each non-zero categories
v4 <- v4t[, c("pan","pri","left","oth")] # take vote columns for manipulation
tmp <- function(x){
    C <- length(x[x==0])
    if (is.na(sum(x))==TRUE) C <- 0
    return(C)
}
C <- apply(v4, 1, tmp)
## C = 3
plus  <- .0005 * 4 * 1 / 16
minus <- .0005 * 4 * 3 / 16
sel.r <- which(C==3)
for (i in 1:nrow(v4[sel.r,])){
    sel.plus  <- which(v4[sel.r,][i,]==0)
    sel.minus <- which(v4[sel.r,][i,]>0)
    v4[sel.r,][i, sel.plus]  <- v4[sel.r,][i, sel.plus]  + plus
    v4[sel.r,][i, sel.minus] <- v4[sel.r,][i, sel.minus] - minus
}
## C = 2
plus  <- .0005 * 3 * 2 / 16
minus <- .0005 * 3 * 2 / 16
sel.r <- which(C==2)
for (i in 1:nrow(v4[sel.r,])){
    sel.plus  <- which(v4[sel.r,][i,]==0)
    sel.minus <- which(v4[sel.r,][i,]>0)
    v4[sel.r,][i, sel.plus]  <- v4[sel.r,][i, sel.plus]  + plus
    v4[sel.r,][i, sel.minus] <- v4[sel.r,][i, sel.minus] - minus
}
## C = 1
plus  <- .0005 * 2 * 3 / 16
minus <- .0005 * 2 * 1 / 16
sel.r <- which(C==1)
for (i in 1:nrow(v4[sel.r,])){
    sel.plus  <- which(v4[sel.r,][i,]==0)
    sel.minus <- which(v4[sel.r,][i,]>0)
    v4[sel.r,][i, sel.plus]  <- v4[sel.r,][i, sel.plus]  + plus
    v4[sel.r,][i, sel.minus] <- v4[sel.r,][i, sel.minus] - minus
}
rm(i,plus,minus,sel.r,sel.minus, sel.plus)
##
## generate ratios (pri is denom)
r4 <- v4t
r4[,c("pan","left","oth")] <- v4[,-2] / v4$pri
r4$pri <- NULL
r4 <- within(r4, {
    vhat.oth  <- (1 - vhat.pan - vhat.pri - vhat.left) / vhat.pri ## re-specify as ratio while keeping names
    vhat.left <- vhat.left / vhat.pri                             ## re-specify as ratio while keeping names
    vhat.pan  <- vhat.pan  / vhat.pri                             ## re-specify as ratio while keeping names
})
r4 <- within(r4, vhat.pan <- vhat.pri <- vhat.left <- NULL)
rm(C, tmp, v4)

## residual DVs
res <- vot
res <- within(res, {
    pan    <- pan    - vhat.pan
    pri    <- pri    - vhat.pri
    morena <- morena - vhat.left
    oth    <- oth - (1 - vhat.pan - vhat.pri - vhat.left)
})

## function to simplify lagging and deltas
inegi.cycle.fr.emm <- function(emm){
    library(plyr)
    #emm <- vot$emm[1:1000] # debug
    tmp <- data.frame(emm=emm)
    tmp$edo <- sub("^([a-z]{2,3})[-][0-9]{2}[.][0-9]{3}$", "\\1", emm)
    tmp$edon <- mapvalues(x=tmp$edo,
                          from = c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac"),
                          to = 1:32)
    tmp$inegi <- sub("^[a-z]+[-][0-9]+[.]([0-9]{3})$", "\\1", emm)
    tmp$inegi <- as.numeric(tmp$edon)*1000 + as.numeric(tmp$inegi)
    tmp$cycle <- sub("^[a-z]+[-]([0-9]+)[.][0-9]{3}", "\\1", emm)
    tmp$cycle <- as.numeric(tmp$cycle)
    return(tmp[, c("inegi","cycle")])
    #head(tmp)
    }

#################################
## lag variables by one period ##
#################################
vot <- vot[order(vot$emm),] # sort mun-chrono
ids <- ids[order(ids$emm),]
vot2 <- vot
colnames(vot2)[-1] <- paste0(colnames(vot2)[-1], "2") # rename cols except emm
drop.c <- colnames(vot2)[-1] # save names to drop after manipulation
vot2 <- cbind(vot2, inegi.cycle.fr.emm(vot2$emm))
vot2[1,]
##
vot2 <- slide(vot2, Var = "lisnom2",          NewVar = "lisnom",          TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "efec2",            NewVar = "efec",            TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "ncoal2",           NewVar = "ncoal",           TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "ncand2",           NewVar = "ncand",           TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dextra2",          NewVar = "dextra",          TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "win2",             NewVar = "win",             TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "mg2",              NewVar = "mg",              TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dcoalpan2",        NewVar = "dcoalpan",        TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dcoalpri2",        NewVar = "dcoalpri",        TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dcoalprd2",        NewVar = "dcoalprd",        TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dcoalmor2",        NewVar = "dcoalmor",        TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dcoalpve2",        NewVar = "dcoalpve",        TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dcoalpt2",         NewVar = "dcoalpt",         TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dcoalmc2",         NewVar = "dcoalmc",         TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "pan2",             NewVar = "pan",             TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "pri2",             NewVar = "pri",             TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "prd2",             NewVar = "prd",             TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "pvem2",            NewVar = "pvem",            TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "pt2",              NewVar = "pt",              TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "mc2",              NewVar = "mc",              TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "morena2",          NewVar = "morena",          TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "oth2",             NewVar = "oth",             TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "race.prior2",      NewVar = "race.prior",      TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincballotpan2",   NewVar = "dincballotpan",   TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincballotpri2",   NewVar = "dincballotpri",   TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincballotprd2",   NewVar = "dincballotprd",   TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincballotpvem2",  NewVar = "dincballotpvem",  TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincballotpt2",    NewVar = "dincballotpt",    TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincballotmc2",    NewVar = "dincballotmc",    TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincballotmorena2",NewVar = "dincballotmorena",TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincballototh2",   NewVar = "dincballototh",   TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dconcdf2",         NewVar = "dconcdf",         TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dconcgo2",         NewVar = "dconcgo",         TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "yr1st2",           NewVar = "yr1st",           TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "govpty2",          NewVar = "govpty",          TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dgovpan2",         NewVar = "dgovpan",         TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dgovpri2",         NewVar = "dgovpri",         TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dgovprd2",         NewVar = "dgovprd",         TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dgovpvem2",        NewVar = "dgovpvem",        TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dgovmc2",          NewVar = "dgovmc",          TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dgovmorena2",      NewVar = "dgovmorena",      TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dprespan2",        NewVar = "dprespan",        TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dprespri2",        NewVar = "dprespri",        TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dpresmorena2",     NewVar = "dpresmorena",     TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "winlast2",         NewVar = "winlast",         TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincpan2",         NewVar = "dincpan",         TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincpri2",         NewVar = "dincpri",         TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincprd2",         NewVar = "dincprd",         TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincpvem2",        NewVar = "dincpvem",        TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincpt2",          NewVar = "dincpt",          TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincmc2",          NewVar = "dincmc",          TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincmorena2",      NewVar = "dincmorena",      TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dincballot2",      NewVar = "dincballot",      TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "vhat.pan2",        NewVar = "vhat.pan",        TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "vhat.pri2",        NewVar = "vhat.pri",        TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "vhat.left2",       NewVar = "vhat.left",       TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "diballmorena2",    NewVar = "diballmorena",    TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "diballnomorena2",  NewVar = "diballnomorena",  TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dimorena2",        NewVar = "dimorena",        TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "diballpri2",       NewVar = "diballpri",       TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "diballnopri2",     NewVar = "diballnopri",     TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dipri2",           NewVar = "dipri",           TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "diballpan2",       NewVar = "diballpan",       TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "diballnopan2",     NewVar = "diballnopan",     TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "dipan2",           NewVar = "dipan",           TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "morenares2",       NewVar = "morenares",       TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "prires2",          NewVar = "prires",          TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
vot2 <- slide(vot2, Var = "panres2",          NewVar = "panres",          TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
##
drop.c <- which(colnames(vot2) %in% drop.c)
vot2 <- vot2[, -drop.c]
vot2$inegi <- vot2$cycle <- NULL
votlag <- vot2
rm(vot2, drop.c) # clean
##
## cómo lidio con missing periods? generan NAs en la serie del municipio donde ocurren
summary(vot$pri)
summary(votlag$pri)

## deltas for cross-temp regs
## sort all
table(colnames(vot)==colnames(votlag))
data.frame(vot=colnames(vot), lag=colnames(votlag))
vot[1,]
votlag[1,]
vot    <- vot   [order(vot$emm),   ]
votlag <- votlag[order(votlag$emm),]
sel.c <- setdiff(2:ncol(vot), grep("win|race.prior|govpty|winlast", colnames(vot))) ## ignore non-numeric vars in 1st diff
delta <- vot
for (i in sel.c){
    delta[, i] <- vot[, i] - votlag[, i]
}
rm(i,sel.c,inegi.cycle.fr.emm)
##
table(delta$dincpan)
table(delta$dincpri)
table(delta$dincprd)
table(delta$dincmorena)


## use left nor prd/morena: lines 1469:1536 1966:1971 2024:2177
## run regs: lines 2356:2642


## Lucardi/Rosas case selector needed
summary(votlag$mg)
table(votlag$mg<.15) ## éste es el que aparentemente usan
table(votlag$mg<.1)
table(votlag$mg<.05)
table(votlag$mg<.025, useNA = "ifany") ## éste suena mucho mejor
sel.lr <- which(votlag$mg<.15)
table(vot$winlast[sel.lr])


## single yr
vot <- vot[order(vot$emm),]; votlag <- votlag[order(votlag$emm),]; ids <- ids[order(ids$emm),] ## sort all objects
## add ids to vot
vot2 <- cbind(ids, vot[,-1]) ## duplicate
vot2[1,]
##
## get lags
vot2$panlag <-    votlag$pan
vot2$prilag <-    votlag$pri
vot2$prdlag <-    votlag$prd
vot2$pvemlag <-   votlag$pvem
vot2$ptlag <-     votlag$pt
vot2$mclag <-     votlag$mc
vot2$morenalag <- votlag$morena
##
colnames(vot2)
summary(lm(pan ~    (dincpan * dincballot)    + dgovpan    + dprespan    + vhat.pan  + popshincab + wsdalt + lats + as.factor(trienio), data = vot2, subset = yr>2005))
summary(lm(pri ~    (dincpri * dincballot)    + dgovpri    + dprespri    + vhat.pri  + popshincab + wsdalt + lats + as.factor(trienio), data = vot2, subset = yr>2005))
summary(lm(morena ~ (dincmorena * dincballot) + dgovmorena + dpresmorena + vhat.left + popshincab + wsdalt + lats + as.factor(trienio), data = vot2, subset = yr>2014))
##
## alternative to interactions
##
summary(lm(pan    ~ dipan + diballnopan + diballpan   + dgovpan    + dprespan    + vhat.pan  + popshincab + wsdalt + lats + as.factor(trienio), data = vot2, subset = yr>2005))
##
summary(lm(pri    ~ dipri + diballnopri + diballpri   + dgovpri    + dprespri    + vhat.pri  + popshincab + wsdalt + lats + as.factor(trienio), data = vot2, subset = yr>2005))
##
summary(lm(morena    ~ dimorena + diballnomorena + diballmorena   + dgovmorena    + dpresmorena    + vhat.left  + popshincab + wsdalt + lats + as.factor(trienio), data = vot2, subset = yr>2014))
x

## cross-temp
delta <- delta[order(delta$emm),]; ids <- ids[order(ids$emm),] ## sort all objects
## add ids to vot
delta2 <- cbind(ids, delta[,-1]) ## duplicate
delta2[10000,]
summary(lm(pan    ~ (dincpan * dincballot)    + dgovpan    + dprespan    + as.factor(trienio), data = delta2, subset = yr>1996))
summary(lm(pri    ~ (dincpri * dincballot)    + dgovpri    + dprespri    + as.factor(trienio), data = delta2, subset = yr>2001))
summary(lm(morena ~ (dincmorena * dincballot) + dgovmorena + dpresmorena + as.factor(trienio), data = delta2, subset = yr>2013))

summary(lm(pan ~ dipan + diballnopan + diballpan + dgovpan    + dprespan + dconcgo   + as.factor(trienio), data = delta2, subset = yr>1996))
summary(lm(pri ~ dipri + diballnopri + diballpri + dgovpri    + dprespri    + as.factor(trienio), data = delta2, subset = yr>1996))
summary(lm(morena    ~ dimorena + diballnomorena + diballmorena + dgovmorena    + dpresmorena    + as.factor(trienio), data = delta2, subset = yr>2014))

summary(lm(panres ~ dipan + diballnopan + diballpan + dgovpan    + dprespan + dconcgo   + as.factor(trienio), data = delta2, subset = yr>1996))
summary(lm(prires ~ dipri + diballnopri + diballpri + dgovpri    + dprespri    + as.factor(trienio), data = delta2, subset = yr>1996))
summary(lm(morenares ~ dimorena + diballnomorena + diballmorena + dgovmorena    + dpresmorena    + dconcgo + as.factor(trienio), data = delta2, subset = yr>2014))

summary(lm(pan ~ dcoalpan + dcoalpri, data = vot)) ## Para ilustrar endogeneidad
ls()
dim(vot)




