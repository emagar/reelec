# get state-level gob elections (will need to replace with mu-level)
# script reads data, cleans, and aggregates coalitions, returning object vot
source("/home/eric/Desktop/MXelsCalendGovt/elecReturns/code/go.r")


# prepare object with pan pri left morena oth votes
vot[1,]
v5 <- vot # duplicate
sel <- grep("^[vl][0-9]{2}", colnames(v5))
v5 <- v5[,sel] # keep votes and labels only #within(v5, ord <- edon <- yr <- mo <- dy <- mun <- munn <- inegi <- ife <- status <- dy <- mo <- ncand <- dcoal <- ncoal <- win <- efec <- lisnom <- NULL) # drop cols
v5$status <- NA
# narrow v01..v14 into long vector
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
#
v5$v01 <- v5$v02 <- v5$v03 <- v5$v04 <- v5$v05 <- v5$v06 <- v5$v07 <- v5$v08 <- v5$v09 <- v5$v10 <- v5$v11 <- NULL
v5$l01 <- v5$l02 <- v5$l03 <- v5$l04 <- v5$l05 <- v5$l06 <- v5$l07 <- v5$l08 <- v5$l09 <- v5$l10 <- v5$l11 <- NULL
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

for (i in 1:max(v5$n)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, max(v5$n)))
    tmp2 <- v5[v5$n==i, c("pan","pri","prd","morena","oth","dmajcoal")]
    tmp2 <- colSums(tmp2)
    tmp[tmp$n==i, c("pan","pri","prd","morena","oth","dmajcoal")] <- tmp2 # plug colsolidated data
}
v5 <- tmp

# debug inspect v5, all vs and ls should be 0
table(v5$v, useNA = "always")

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
