
#########################
## get municipal votes ##
#########################
vot <- read.csv(paste0(dd, "aymu1989-present.coalAgg.csv"), stringsAsFactors = FALSE)
# clean
vot <- within(vot, notas <- lisnom <- NULL)
# drop before 1997
sel <- which(vot$yr<1997)
vot <- vot[-sel,]
#
# keep runoffs only in cases where first round was held in san luis potosí
sel <- grep("san-[0-9]+b", vot$emm) # these are first round races that led to runoff
vot <- vot[-sel,]
#
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
#
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
tmp <- tmp + 20000
# ojo: 88 returned from uyc in 2013
tmp1 <- as.numeric(vot$inegi)
sel <- which(tmp1 %in% tmp)
## tmp <- vot$emm[sel]
## tmp[order(tmp)]
if (length(sel)>0) vot <- vot[-sel,]
#
# drop oxchuc in 2018, went usos y costumbres
sel <- which(vot$emm=="cps-17.064")
vot <- vot[-sel,]
#
#
# drop void elections
# 6ago2020: check which don't have extraord data. drop them? would break lags... check in incumbents block too
sel <- grep("[0-9]+a[.]", vot$emm) # anuladas
tmp <- vot$emm[sel]
tmp <- sub("(^.+[0-9])+a[.]", "\\1.", tmp) # drop the a from emm, are all these obs in data?
table(tmp[which(tmp %in% vot$emm)]==tmp)   # all have extra data if output only TRUEs wo error
vot <- vot[-sel,]
#
# re-compute efec
sel <- grep("v[0-9]+", colnames(vot))
v <- vot[,sel]
# cases where efec does not match sum, if any
sel1 <- which(rowSums(v) - vot$efec !=0)
if (length(sel1)>0) vot[sel1,]
vot$efec <- rowSums(v) # re-compute
vot$efec[which(vot$efec==0)] <- 1 # put 1 vote to missing races to avoid indeterminate shares
# vote shares
v <- round(v/rowSums(v), digits = 4)
vot[,sel] <- v
rm(v)
#
## # win matches does not match l01 in san luis runoff cases if those were dropped above, therefore l01 and win should be retained
## sel <- which(vot$win != vot$l01)
## vot$emm[sel] # all should be san luis potosi
#
## # lo usé para detectar casos en aymu.incumbents con info que no correspondía con aymu.coalAgg
## sel <- c("emm", "edon", "mun", "munn", "ife", "inegi", "yr", "dy", "mo", "win")
## i <- merge(x = inc, y = vot[,sel],
##            by = c("emm", "edon", "mun", "munn", "ife", "inegi", "yr", "mo", "dy", "win"),
##            all = TRUE)
## write.csv(i, file = paste(dd, "tmp.csv", sep = ""), row.names = FALSE)
#
# prepare object with pan pri left morena oth votes
# 4ago2020: add pvem?
v5 <- vot # duplicate
#v5[1,]
v5 <- within(v5, ord <- mun <- inegi <- ife <- status <- dy <- mo <- ncand <- dcoal <- ncoal <- win <- efec <- lisnom <- NULL) # drop cols
v5$status <- NA
# narrow v01..v18 into long vector
v5$n <- 1:nrow(v5) # obs no
v5$r <- 1          # round
v5$v <- v5$v01     # vote
v5$l <- v5$l01     # label
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
tmp <- tmp.orig
tmp$r <- 15
tmp$v <- tmp$v15; tmp$l <- tmp$l15;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 16
tmp$v <- tmp$v16; tmp$l <- tmp$l16;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 17
tmp$v <- tmp$v17; tmp$l <- tmp$l17;
v5 <- rbind(v5,tmp)
tmp <- tmp.orig
tmp$r <- 18
tmp$v <- tmp$v18; tmp$l <- tmp$l18;
v5 <- rbind(v5,tmp)
# make sure all v columns have been added
#
v5$v01 <- v5$v02 <- v5$v03 <- v5$v04 <- v5$v05 <- v5$v06 <- v5$v07 <- v5$v08 <- v5$v09 <- v5$v10 <- v5$v11 <- v5$v12 <- v5$v13 <- v5$v14 <- v5$v15 <- v5$v16 <- v5$v17 <- v5$v18 <- NULL
v5$l01 <- v5$l02 <- v5$l03 <- v5$l04 <- v5$l05 <- v5$l06 <- v5$l07 <- v5$l08 <- v5$l09 <- v5$l10 <- v5$l11 <- v5$l12 <- v5$l13 <- v5$l14 <- v5$l15 <- v5$l16 <- v5$l17 <- v5$l18 <- NULL
#
v5$oth <- v5$morena <- v5$prd <- v5$pri <- v5$pan <- 0
v5$dmajcoal <- 0 # will indicate major party coalitions
#
rm(tmp, tmp.orig)
#
## # change prd/morena to left
## sel <- which(v5$yr<=2015)
## v5$l[sel] <- sub("prd","left",v5$l[sel])
## sel <- which(v5$yr>=2015)
## v5$l[sel] <- sub("morena","left",v5$l[sel])
#
# deal with major-party coalition below
sel <- grep("(?=.*pan)(?=.*prd)", v5$l, perl = TRUE)
v5$status[sel] <- "majors"
## sel <- grep("(?=.*pan)(?=.*left)", v5$l, perl = TRUE)
## v5$status[sel] <- "majors"
sel <- grep("(?=.*pan)(?=.*pri)", v5$l, perl = TRUE)
v5$status[sel] <- "majors"
sel <- grep("(?=.*pri)(?=.*prd)", v5$l, perl = TRUE)
v5$status[sel] <- "majors"
## sel <- grep("(?=.*pri)(?=.*left)", v5$l, perl = TRUE)
## v5$status[sel] <- "majors"
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
## sel1 <- which(is.na(v5$status))
## sel <- grep("left-|-left|^left$", v5$l[sel1])
## v5$morena[sel1][sel] <- v5$v[sel1][sel]
## v5$v[sel1][sel] <- 0; v5$l[sel1][sel] <- "0"; v5$status[sel1][sel] <- "done"
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
v5$dmajcoal[sel] <- 1
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
#
for (i in 1:max(v5$n)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, max(v5$n)))
    tmp2 <- v5[v5$n==i, c("pan","pri","prd","morena","oth","dmajcoal")]
    tmp2 <- colSums(tmp2)
    tmp[tmp$n==i, c("pan","pri","prd","morena","oth","dmajcoal")] <- tmp2 # plug colsolidated data
}
v5 <- tmp
#
# debug inspect v5, all vs and ls should be 0
table(v5$v, useNA = "always")
#
#
# clean, data in pan pri morena prd oth
v5$n <- v5$r <- v5$v <- v5$l <- v5$status <- NULL
rm(tmp,tmp2)
# return to vot
vot <- cbind(vot, v5[,c("pan","pri","prd","morena","oth","dmajcoal")])
# keep 123 places, drop rest
vot <- within(vot, v04 <- v05 <- v06 <- v07 <- v08 <- v09 <- v10 <- v11 <- v12 <- v13 <- v14 <- NULL)
vot <- within(vot, l04 <- l05 <- l06 <- l07 <- l08 <- l09 <- l10 <- l11 <- l12 <- l13 <- l14 <- NULL)
# inspect
vot[1,]
#
# clean
rm(i,sel,sel1,v5)



