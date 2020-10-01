#####################################
## deal with major-party coalition ##
#####################################
inc$status <- NA
sel <- grep("(?=.*pan)(?=.*prd)", inc$win, perl = TRUE)
inc$status[sel] <- "majors"
sel <- grep("(?=.*pan)(?=.*pri)", inc$win, perl = TRUE)
inc$status[sel] <- "majors"
sel <- grep("(?=.*pri)(?=.*prd)", inc$win, perl = TRUE)
inc$status[sel] <- "majors"
#
# 3-majors coalition in mun split in thirds
sel <- which(inc$status=="majors") 
sel1 <- grep("(?=.*pan)(?=.*pri)(?=.*prd)", inc$win[sel], perl = TRUE) # 
inc$inegi[sel][sel1]; inc$mun[sel][sel1]; inc$yr[sel][sel1] # which?
# assign to strong party (coal vs narco it seems)
inc$win2[which(inc$inegi==16056 & inc$yr==2015)] <- "pri"  # Nahuatzén to pri
inc$win2[which(inc$inegi==16083 & inc$yr==2015)] <- "pan"  # Tancítaro to pan
inc$status[sel][sel1] <- "done"
#
# pan-pri to pri (19 cases in mic07 mic11 mic15)
sel <- which(inc$status=="majors" & inc$edon==16) 
sel1 <- grep("(?=.*pan)(?=.*pri)", inc$win[sel], perl = TRUE) # 
inc$win2[sel][sel1] <- "pri"
inc$status[sel][sel1] <- "done"
#
# pri-prd to pri (chihuahua and guanajuato)
sel <- which(inc$status=="majors") 
sel1 <- grep("(?=.*pri)(?=.*prd)", inc$win[sel], perl = TRUE) # 
inc$win2[sel][sel1] <- "pri"
inc$status[sel][sel1] <- "done"
#
# rest are pan-prd
#
# pan-prd to pan (bc coa2009 coa col00 col18 cua dgo jal que san sin son tam yuc)
sel <- which(inc$status=="majors" & (inc$edon==2 | inc$edon==5 | inc$edon==6 | inc$edon==8 | inc$edon==10 | inc$edon==14 | inc$edon==22 | inc$edon==24 | inc$edon==25 | inc$edon==26 | inc$edon==28  | inc$edon==31)) 
inc$win2[sel] <- "pan"
inc$status[sel] <- "done"
#
# pan-prd in 2018 to pan (bcs cps df gue mex mic oax pue qui tab zac)
sel <- which(inc$status=="majors" & inc$yr==2018) 
inc$win2[sel] <- "pan"
inc$status[sel] <- "done"
#
# pan-prd to prd (cps2004, cps2010)
sel <- which(inc$status=="majors" & inc$edon==7 & inc$yr<=2010) 
inc$win2[sel] <- "prd"
inc$status[sel] <- "done"
#
# pan-prd to pan (nay1999 nay2017 ver2000 ver2017)
sel <- which(inc$status=="majors" & (inc$edon==18 | inc$edon==30) & (inc$yr==1999 | inc$yr==2000 | inc$yr==2017)) 
inc$win2[sel] <- "pan"
inc$status[sel] <- "done"
#
# pan-prd to prd (votes split halfway, qui2016)
sel <- which(inc$status=="majors" & inc$edon==23 & inc$yr==2016)
inc$win2[sel] <- "prd"
inc$status[sel] <- "done"
#
# pan-prd to pan (votes split halfway, pue2010 pue2013)
sel <- which(inc$status=="majors" & inc$edon==21)
inc$win2[sel] <- "pan"
inc$status[sel] <- "done"
#
# pan-prd to prd (votes split halfway, pue2010 pue2013 qui2013 qui2010)
sel <- which(inc$status=="majors" & (inc$edon==21 | inc$edon==23))
inc$win2[sel] <- "prd"
inc$status[sel] <- "done"
#
# pan-prd to prd (votes split halfway, gue2015 hgo2011 mic2015 oax2010 oax2013 oax2016 zac2013, zac2016)
sel <- which(inc$status=="majors" & (inc$edon==12 | inc$edon==13 | inc$edon==16 | inc$edon==20 | inc$edon==32) & inc$yr<2018) 
inc$win2[sel] <- "prd"
inc$status[sel] <- "done"
#
# pan-prd to pan (votes split halfway, mex2006)
sel <- which(inc$status=="majors" & inc$edon==15 & inc$yr==2006) 
inc$win2[sel] <- "pan"
inc$status[sel] <- "done"
#
# clean
inc$status <- NULL
