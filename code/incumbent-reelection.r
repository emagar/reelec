#####################################################################
## ################################################################ ##
## ## SCRIPT FROM ../elecReturns/code/incumbents.r *STARTS* HERE ## ##
## ## 1aug2020                                                   ## ##
## ################################################################ ##
######################################################################
#
rm(list = ls())
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data/"
setwd(dd)

# get useful functions
source("~/Dropbox/data/useful-functions/myxtab.r")
source("~/Dropbox/data/useful-functions/inegi2ife.r")
source("~/Dropbox/data/useful-functions/sortBy.r") ## winner (sorts data to have largest vote-winning party in column 1)
##
###########################################
## ##################################### ##
## ##                                 ## ##
## ## INCUMBENT DATA PREP STARTS HERE ## ##
## ##      |         |         |      ## ##
## ##      |         |         |      ## ##
## ##      V         V         V      ## ##
## ##################################### ##
###########################################
#
###################
## read alcaldes ##
###################

inc <- read.csv(file = "aymu1989-on.incumbents.csv", stringsAsFactors = FALSE)
inc <- inc[order(inc$ord),]
library(plyr)
inc$edo <- mapvalues(inc$edon, from = 1:32, to = c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac"))
##
####################################################
## rename part win for compatibility w code below ##
####################################################
colnames(inc)[grep("^part$", colnames(inc))]    <- "win"
colnames(inc)[grep("^part2nd$", colnames(inc))] <- "pty2nd"
##
#########################
## Drop pre-1970 in bc ##
#########################
sel <- which(inc$yr < 1970)
if (length(sel)>0) inc <- inc[-sel,]
##
###########################################################################
## Add federal cycle based on yr (OJO: extras might shift to next cycle) ##
###########################################################################
##inc$cyclef <- cut(inc$yr, breaks = seq(1970,2027, by=3), right = FALSE)
inc$cyclef <- inc$yr
inc$cyclef[inc$cyclef==1970 | inc$cyclef==1971 | inc$cyclef==1972] <- 1970
inc$cyclef[inc$cyclef==1973 | inc$cyclef==1974 | inc$cyclef==1975] <- 1973
inc$cyclef[inc$cyclef==1976 | inc$cyclef==1977 | inc$cyclef==1978] <- 1976
inc$cyclef[inc$cyclef==1979 | inc$cyclef==1980 | inc$cyclef==1981] <- 1979
inc$cyclef[inc$cyclef==1982 | inc$cyclef==1983 | inc$cyclef==1984] <- 1982
inc$cyclef[inc$cyclef==1985 | inc$cyclef==1986 | inc$cyclef==1987] <- 1985
inc$cyclef[inc$cyclef==1988 | inc$cyclef==1989 | inc$cyclef==1990] <- 1988
inc$cyclef[inc$cyclef==1991 | inc$cyclef==1992 | inc$cyclef==1993] <- 1991
inc$cyclef[inc$cyclef==1994 | inc$cyclef==1995 | inc$cyclef==1996] <- 1994
inc$cyclef[inc$cyclef==1997 | inc$cyclef==1998 | inc$cyclef==1999] <- 1997
inc$cyclef[inc$cyclef==2000 | inc$cyclef==2001 | inc$cyclef==2002] <- 2000
inc$cyclef[inc$cyclef==2003 | inc$cyclef==2004 | inc$cyclef==2005] <- 2003
inc$cyclef[inc$cyclef==2006 | inc$cyclef==2007 | inc$cyclef==2008] <- 2006
inc$cyclef[inc$cyclef==2009 | inc$cyclef==2010 | inc$cyclef==2011] <- 2009
inc$cyclef[inc$cyclef==2012 | inc$cyclef==2013 | inc$cyclef==2014] <- 2012
inc$cyclef[inc$cyclef==2015 | inc$cyclef==2016 | inc$cyclef==2017] <- 2015
inc$cyclef[inc$cyclef==2018 | inc$cyclef==2019 | inc$cyclef==2020] <- 2018
inc$cyclef[inc$cyclef==2021 | inc$cyclef==2022 | inc$cyclef==2023] <- 2021
inc$cyclef[inc$cyclef==2024 | inc$cyclef==2025 | inc$cyclef==2026] <- 2024
##
##############################
## keep dead incumbent info ##
##############################
table(inc$race.after)
sel <- grep("Dead", inc$race.after, ignore.case = TRUE)
inc$ddead <- 0
inc$ddead[sel] <- 1
##
## recode race.after to simplify
inc$race.after <- gsub("Dead-term-limited", "Term-limited"        , inc$race.after)
inc$race.after <- gsub("Dead-reran-"      , "Out-"                , inc$race.after)
inc$race.after <- gsub("Dead-202.|Dead-$" , "Pending"             , inc$race.after)
inc$race.after <- gsub("Dead-p-"          , "Out-p-"              , inc$race.after)
inc$race.after <- gsub("Reran-dead-p-"    , "Out-p-"              , inc$race.after)
inc$race.after <- gsub("Impeached-p-"     , "Out-p-"              , inc$race.after)
inc$race.after <- gsub("Term-limited-202.", "Term-limited-pending", inc$race.after)
inc$race.after <- gsub("^202.$"           , "Pending"             , inc$race.after)
sel <- which(inc$race.after==""); inc$race.after[sel] <- "Pending"
sel <- which(inc$race.after=="Reran-2025"); inc$race.after[sel] <- "Pending"
##
table(inc$race.after)
## ##
## ## explore
## sel <- which(inc$race.after=="")
## sel <- grep("uyc", inc$race.after)
## inc[sel,]
## inc$emm[sel]
## x
##
#############################################
## verify time series' structure (for lags) ##
#############################################
library(DataCombine) # easy lags
tmp <- inc[,c("ord","inegi","emm")]
tmp$cycle <- as.numeric(sub("^[a-z]+[-]([0-9]+)[.].+$", "\\1", tmp$emm))
tmp <- tmp[order(tmp$ord),] # verify sorted before lags
tmp <- slide(tmp, Var = "cycle", NewVar = "cycle.lag", TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
tmp$verif <- tmp$cycle - tmp$cycle.lag
table(tmp$verif) # verify: all should be 1 then ok to lag
## ## inspect
## sel <- which(tmp$verif==0)
## tmp[sel,]
## 
full.xsts <- tmp # keep as list of all observations (to recover them after they were drop to ease code/analysis)
# 
###################
## pending cases ##
###################
sel <- grep("^[0-9]+$", inc$race.after)
table(pending=inc$race.after[sel])
if (length(sel)>0) inc$race.after[sel] <- "Pending"
# code missing race.after as pending (drop this bit of code once all case are known)
#inc[inc$ife==20442, c("mun","yr","ife","inegi","win","incumbent","race.after")] # used to debug
sel <- grep("[?]", inc$race.after)
inc[sel,]
if (length(sel)>0) inc$race.after[sel] <- "Pending"
#
#################################################################
## recode race after for munic that turned usos in next round  ##
#################################################################
sel <- grep("uyc", inc$race.after)
if (length(sel)>0) inc$race.after[sel] <- "Out-p-lost"
########################################################
## drop observations that became to usos y costumbres ##
########################################################
sel <- grep("uyc", inc$win, ignore.case = TRUE)
if (length(sel)>0) inc <- inc[-sel,]
################################################
## drop consejos municipales (void elections) ##
################################################
sel <- grep("con[sc]ejo", inc$win, ignore.case = TRUE)
if (length(sel)>0) inc <- inc[-sel,]
##########################################
## drop new municipio in border dispute ##
##########################################
sel <- grep("7xxx", inc$inegi)
if (length(sel)>0) inc <- inc[-sel,]
##
##############################################################################################
## manipulate cases where incumbent switched parties so coded as party won/lost accordingly ##
##############################################################################################
#table(inc$race.after)
sel1 <- grep("-dif-p", inc$race.after) # round t
#
# add one round to emm codes to select next round obs
tmp <- inc$emm[sel1]
tmp1 <-            sub("^([a-z]+[-])([0-9]{2})([.][0-9]{3})$", "\\1", tmp, perl = TRUE)
tmp2 <- as.numeric(sub("^([a-z]+[-])([0-9]{2})([.][0-9]{3})$", "\\2", tmp, perl = TRUE))
tmp3 <-            sub("^([a-z]+[-])([0-9]{2})([.][0-9]{3})$", "\\3", tmp, perl = TRUE)
tmp4 <- paste(tmp1, tmp2+1, tmp3, sep = "")
sel2 <- which(inc$emm %in% tmp4)       # round t+1
##
## fish switched-to party from note
#data.frame(emm=inc$emm[sel1], note=inc$note[sel1])

tmp <- sub("only ", "", inc$note[sel1]) # remove "only"
tmp <- sub("extraordinaria ", "", inc$note[sel1]) # remove "only"
tmp1 <- sub("^(?:re)?ran as ([a-z-]+).*", "\\1", tmp, perl = TRUE)
table(tmp1)
##
# paste switched-to party as the original
##inc$emm[sel1]
##data.frame(inc$win[sel1], tmp1) ## check
inc$win[sel1] <- tmp1
# 
# recode race.after given swithed-to party
#data.frame(emm.pre=inc$emm[sel1], win.pre=inc$win[sel1], race.after.pre=inc$race.after[sel1], win.post=inc$win[sel2]) # debug: verify that race.after will be ok by just dropping -dif-p
tmp <- inc$race.after[sel1]
tmp <- sub("-dif-p", "", tmp)
inc$race.after[sel1] <- tmp
# verify
colnames(inc)
table(inc$yr, inc$race.after)
# clean
rm(tmp,tmp1,tmp2,tmp3,tmp4,sel,sel1,sel2)
##
## # merge a new coalAgg into incumbents
## cagg <- read.csv(file = "aymu1997-present.coalAgg.csv", stringsAsFactors = FALSE)
## colnames(cagg)
## cagg <- cagg[, c("emm","win")]
## inc <- merge(x = inc, y = cagg, by = "emm", all = TRUE)
## sel <- grep("pt1", inc$win.y, ignore.case = TRUE)  # change labels
## inc$win.y[sel] <- gsub("pt1", "pt", inc$win.y[sel])# change labels
## sel <- grep("pmc$|pmc-", inc$win.y, ignore.case = TRUE) # change labels
## inc$win.y[sel] <- gsub("pmc", "mc", inc$win.y[sel])                             # change labels
## sel <- grep("panal", inc$win.y, ignore.case = TRUE)# change labels
## inc$win.y[sel] <- gsub("panal", "pna", inc$win.y[sel])                           # change labels
## sel <- grep("[a-z]+[-][0-9]{2}[ab].*", inc$emm) # find anuladas/ballotage
## inc <- inc[-sel,] # drop them
## write.csv(inc, file = "tmp.csv", row.names = FALSE) # verify what tmp.csv looks like
##
######################
## simplify parties ##
######################
inc$win <- sub("conve", "mc",  inc$win, ignore.case = TRUE)
inc$win <- sub("panal", "pna", inc$win, ignore.case = TRUE)
##
## duplicate for manipulation
inc$win2 <- inc$win
##
## assign panc to pan, then pric to pri, then prdc to prd; major party coalitions dealt as exceptions below
sel <- grep("pan-", inc$win2, ignore.case = TRUE)
if (length(sel)>0) inc$win2[sel] <- "pan"
sel <- grep("pri-", inc$win2, ignore.case = TRUE)
if (length(sel)>0) inc$win2[sel] <- "pri"
sel <- grep("prd-|-prd", inc$win2, ignore.case = TRUE)
if (length(sel)>0) inc$win2[sel] <- "prd"
sel <- grep("morena-|-morena", inc$win2, ignore.case = TRUE)
if (length(sel)>0) inc$win2[sel] <- "morena"
sel <- grep("pvem-|-pvem", inc$win2, ignore.case = TRUE)
if (length(sel)>0) inc$win2[sel] <- "pvem"
sel <- grep("mc-|-mc", inc$win2, ignore.case = TRUE)
if (length(sel)>0) inc$win2[sel] <- "mc"
sel <- grep("pt-", inc$win2, ignore.case = TRUE)
if (length(sel)>0) inc$win2[sel] <- "pt"
sel <- grep("-pes", inc$win2, ignore.case = TRUE)
if (length(sel)>0) inc$win2[sel] <- "pes"
sel <- grep("cardenista|eso|fuerciud|futuro|fxm|hag|mas|mexa|mexpos|ml|mujer|pac|parm|pas|paz|pbg|pcd|pcp|pd|pes|pfcrn|phm|pmm|pmt|pmr|pna|podemos|pp1|ppg|prt|ps|psd1|psd|psg|psi|pst|pudc|pueblo|pup|via_|pchu|pmch|pver|prs|prv|ps1|poc|pjs|pd1|pec|pasd|pac1|npp|pcu|pcdt|pmac|pcm2|pdm|pps|ppt|ph|pmp|rhr|txver|uc|fc1|psn|ave|hagamos|rsp|somos|indep", inc$win2, ignore.case = TRUE)
if (length(sel)>0) inc$win2[sel] <- "loc/oth"
table(inc$win2)
##
#####################################
## deal with major-party coalition ## OJO 11mar21: 2021 will have many pan-pri-prd coalitions, need a decisions on how to code those!!!
#####################################
#############
## winners ##
#############
# explore
table(inc$race.after, useNA = "always")

#OLD WAY OF DEALING WITH MAJOR PTY COALS SPLIT CASE-BY-CASE
#DIFT METHOD IMMEDIATELY BELOW
inc$status <- NA
sel <- grep("(?=.*pan)(?=.*prd)", inc$win, perl = TRUE)
inc$status[sel] <- "majors"
sel <- grep("(?=.*pan)(?=.*pri)", inc$win, perl = TRUE)
inc$status[sel] <- "majors"
sel <- grep("(?=.*pri)(?=.*prd)", inc$win, perl = TRUE)
inc$status[sel] <- "majors"
table(inc$status)
##
# 3-majors coalition in mun split in thirds
sel <- which(inc$status=="majors") 
sel1 <- grep("(?=.*pan)(?=.*pri)(?=.*prd)", inc$win[sel], perl = TRUE) # 
table(inc$edo[sel][sel1])
data.frame(inegi = inc$inegi[sel][sel1], mun=inc$mun[sel][sel1], yr=inc$yr[sel][sel1]) # which?
# assign to strong party (coal vs narco it seems)
inc$win2[which(inc$inegi==16056 & inc$yr==2015)] <- "pri"  # Nahuatzén to pri
inc$win2[which(inc$inegi==16083 & inc$yr==2015)] <- "pan"  # Tancítaro to pan
#
inc$win2[which(inc$edon==3  & inc$yr==2021)] <- "pan"  # bcs to pan
inc$win2[which(inc$edon==4  & inc$yr==2021)] <- "pri"  # cam to pri
inc$win2[which(inc$edon==6  & inc$yr==2021)] <- "pri"  # col to pri
inc$win2[which(inc$edon==9  & inc$yr==2021)] <- "pan"  # df  to pan
inc$win2[which(inc$edon==15 & inc$yr==2021)] <- "pri"  # mex to pri
inc$win2[which(inc$edon==16 & inc$yr==2021)] <- "pri"  # mic to pri
inc$win2[which(inc$edon==23 & inc$yr==2021)] <- "pri"  # qui to pri
inc$win2[which(inc$edon==24 & inc$yr==2021)] <- "pan"  # san to pan
inc$win2[which(inc$edon==26 & inc$yr==2021)] <- "pri"  # son to pri
inc$win2[which(inc$edon==32 & inc$yr==2021)] <- "pri"  # zac to pri
#
inc$status[sel][sel1] <- "done"
#
# pan-pri to pri (19 cases in mic07 mic11 mic15 tab21)
sel <- which(inc$status=="majors" & inc$edon==16) 
sel1 <- grep("(?=.*pan)(?=.*pri)", inc$win[sel], perl = TRUE) # 
inc$win2[sel][sel1] <- "pri"
inc$status[sel][sel1] <- "done"
#
# pri-prd to pri (cua and gua before 2021, gua mor nl yuc in 2021)
sel <- which(inc$status=="majors") 
sel1 <- grep("(?=.*pri)(?=.*prd)", inc$win[sel], perl = TRUE) # 
inc$win2[sel][sel1] <- "pri"
inc$status[sel][sel1] <- "done"
#
# pan-pri to pan (san in 2021)
sel <- which(inc$status=="majors") 
sel1 <- grep("(?=.*pan)(?=.*pri)", inc$win[sel], perl = TRUE) # 
inc$win2[sel][sel1] <- "pan"
inc$status[sel][sel1] <- "done"
#
# rest are pan-prd
#
# pan-prd in 2021 to pan (ags bcs col cua que)
sel <- which(inc$status=="majors" & inc$yr==2021) 
inc$win2[sel] <- "pan"
inc$status[sel] <- "done"
#
# pan-prd in 2020 to pan (hgo mex)
sel <- which(inc$status=="majors" & inc$yr==2020) 
inc$win2[sel] <- "pan"
inc$status[sel] <- "done"
#
# pan-prd to pan (mex2015)
sel <- which(inc$status=="majors" & inc$edon==15 & inc$yr==2015) 
inc$win2[sel] <- "pan"
inc$status[sel] <- "done"
#
# pan-prd to pan (bc coa2009 coa col00 col18 cua dgo jal que san sin son tam yuc)
sel <- which(inc$status=="majors" & (inc$edon==2 | inc$edon==5 | inc$edon==6 | inc$edon==8 | inc$edon==10 | inc$edon==14 | inc$edon==22 | inc$edon==24 | inc$edon==25 | inc$edon==26 | inc$edon==28  | inc$edon==31)) 
inc$win2[sel] <- "pan"
inc$status[sel] <- "done"
##
# pan-prd in 2024 to prd (oax)
sel <- which(inc$status=="majors" & inc$edon==20 & inc$yr==2024) 
inc$win2[sel] <- "prd"
inc$status[sel] <- "done"
##
# pan-prd in 2024 to pan (mic)
sel <- which(inc$status=="majors" & inc$edon==16 & inc$yr==2024) 
inc$win2[sel] <- "prd"
inc$status[sel] <- "done"
##
## pan-prd in 2018 to pan (bcs cps df gue mex mic oax pue qui tab zac)
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
# pan-prd to prd (qui2016)
sel <- which(inc$status=="majors" & inc$edon==23 & inc$yr==2016)
inc$win2[sel] <- "prd"
inc$status[sel] <- "done"
#
# pan-prd to pan (pue2010 pue2013)
sel <- which(inc$status=="majors" & inc$edon==21)
inc$win2[sel] <- "pan"
inc$status[sel] <- "done"
#
# pan-prd to prd (qui2013 qui2010)
sel <- which(inc$status=="majors" & inc$edon==23)
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
# verify
table(inc$status)
inc$emm[which(inc$status=="majors")]
##
## clean
inc$status <- NULL
##
## #THIS IS ANOTHER WAY: GIVES VOTE TO PAN WHEN PAN IS MEMBER, TO PRI WHEN PAN ABSENT
## inc$status <- NA
## sel <- grep("(?=.*pan)(?=.*prd)", inc$win, perl = TRUE)
## inc$status[sel] <- "majors"
## sel <- grep("(?=.*pan)(?=.*pri)", inc$win, perl = TRUE)
## inc$status[sel] <- "majors"
## sel <- grep("(?=.*pri)(?=.*prd)", inc$win, perl = TRUE)
## inc$status[sel] <- "majors"
## #
## # 3-majors coalition in mun to pan
## sel <- which(inc$status=="majors") 
## sel1 <- grep("(?=.*pan)(?=.*pri)(?=.*prd)", inc$win[sel], perl = TRUE)
## data.frame(inegi = inc$inegi[sel][sel1], mun=inc$mun[sel][sel1], yr=inc$yr[sel][sel1]) # which?
## inc$win2[sel[sel1]] <- "pan"
## inc$status[sel][sel1] <- "done"
## #
## # pan-pri to pan
## sel <- which(inc$status=="majors" & inc$edon==16) 
## sel1 <- grep("(?=.*pan)(?=.*pri)", inc$win[sel], perl = TRUE) # 
## inc$win2[sel][sel1] <- "pan"
## inc$status[sel][sel1] <- "done"
## #
## # pri-prd to pri
## sel <- which(inc$status=="majors") 
## sel1 <- grep("(?=.*pri)(?=.*prd)", inc$win[sel], perl = TRUE) # 
## inc$win2[sel][sel1] <- "pri"
## inc$status[sel][sel1] <- "done"
## #
## # rest are pan-prd
## #
## # pan-prd to pan (bc coa2009 coa col00 col18 cua dgo jal que san sin son tam yuc)
## sel <- which(inc$status=="majors") 
## inc$win2[sel] <- "pan"
## inc$status[sel] <- "done"
## #
## # pan-prd in 2018 to pan (bcs cps df gue mex mic oax pue qui tab zac)
## # clean
## inc$status <- NULL
##
#inc$win.long <- inc$win # retain unsimplified version
inc$win <- inc$win2; inc$win2 <- NULL # keep manipulated version only
table(inc$win)

####################################
## this block inspects reelection ##
####################################
tmp <- inc ## duplicates data for manipulation
# drop litigios uyc anuladas
sel <- grep("uyc|litigio|anulada", tmp$race.after)
if (length(sel)>0) tmp <- tmp[-sel,] # drop one uyc munic 
sel <- grep("con[cs]ejo", tmp$win, ignore.case=TRUE)
if (length(sel)>0) tmp <- tmp[-sel,] # drop consejos municipales
## # 10 and 13 had no els in 2021, 18 29 and 30 had term limits still in place
## tmp <- tmp[tmp$edon %in% c(1:9,11:12,14:17,19:28,31:32),] # keep states with race.after coded only

# recode race.after categories
library(plyr)
table(tmp$race.after)
sel <- grep("pending", tmp$race.after, ignore.case = TRUE) # drop cases pending after election
##tmp$emm[sel] # which?
tmp <- tmp[-sel,] 
tmp$race.after <- mapvalues(tmp$race.after, from = c("Reelected","Reran-beaten","Term-limited-p-won","Term-limited-p-lost","Out-p-won","Out-p-lost"), to = c("1Reel","2Beaten","3Term-pwon","4Term-plost","5Out-pwon","6Out-plost"))
table(tmp$race.after, useNA = "always")
#
tmp[1,]
table(tmp$win, useNA = "always")
sel <- which(tmp$win=="loc/oth")
tmp$win[sel] <- "other"
#tmp$win <- factor(tmp$win, levels=    c("pan","pri","prd","morena","pvem","mc","other"))
tmp$win <- factor(tmp$win, levels=rev(c("pan","pri","prd","morena","pvem","pt","mc","other")), labels=rev(c("PAN","PRI","PRD","MORENA","PVEM","PT","MC","Otros")))

tmp2 <- myxtab(tmp$cyclef, tmp$race.after, pct=TRUE, rel=TRUE, digits=2, marginals = 1)
sel <- which(tmp$cyclef==2021)
tmp2 <- myxtab(tmp$win[sel], tmp$race.after[sel], pct=TRUE, rel=TRUE, digits=1, marginals = 1)
tmp3 <- myxtab(tmp$win[sel], tmp$race.after[sel], pct=FALSE, rel=FALSE, digits=0, marginals = 1)
colSums(tmp3)
tmp4 <- c(round(colSums(tmp3)*100/colSums(tmp3)[7]), colSums(tmp3)[7]); names(tmp4)[8] <- "N"
tmp2 <- rbind(Todos=tmp4, tmp2)
tmp2
##
tmp2 <- tmp2[order(-tmp2[,1]),] # sort by reelected
tmp2

## this will become part of the 2024--2026 breakdown 
sel <- which(inc$yr>2023 & inc$edon!=10 & inc$edon!=30 & inc$win!=0  & inc$win!="") # ver and dgo incumbent defined in 2025 (not yet)
tmp2 <- myxtab(inc$win[sel], inc$race.after[sel], pct=TRUE,  rel=TRUE,  digits=1, marginals = 1)
tmp3 <- myxtab(inc$win[sel], inc$race.after[sel], pct=FALSE, rel=FALSE, digits=0, marginals = 1)
tmp4 <- c(round(colSums(tmp3)*100/colSums(tmp3)[3]), colSums(tmp3)[3]); names(tmp4)[4] <- "N"
tmp2 <- rbind(Todos=tmp4, tmp2)
tmp2
table(inc$win[sel], inc$race.after[sel], useNA = "ifany")

## clean
rm(sel,sel1,tmp,tmp2,tmp3,tmp4)

##############################################
## get/merge v7, vs, va for margin analysis ##
##############################################
setwd(wd)
v7 <- read.csv("aymu1988-on-v7-coalSplit.csv", stringsAsFactors = FALSE)
v7$ord <- NULL ## drop ord to import inc$ord below
setwd(dd)
va <- read.csv(file = "aymu1970-on.coalAgg.csv", stringsAsFactors = FALSE)
va$ord <- NULL ## drop ord to import inc$ord below
vs <- read.csv(file = "aymu1970-on.coalSplit.csv", stringsAsFactors = FALSE)
vs$ord <- NULL ## drop ord to import inc$ord below

##########################################
## sort coalition-split data columnwise ##
##########################################
## Extract vote and label objects for manip
sel.l <- grep("^l[0-9]{2}", colnames(vs))
sl <- vs[,sel.l] # subset label columns
sel.v <- grep("^v[0-9]{2}", colnames(vs))
sv <- vs[,sel.v] # subset vote columns
#########################################
tail(sv)
tail(sl)
###########################################
sv.sorted <- sortBy(target = sv, By = sv) # slow! better wait for process end before continuing  
###########################################
sl.sorted <- sortBy(target = sl, By = sv) # slow! better wait for process end before continuing
###########################################
sv.sorted <- as.data.frame(sv.sorted, stringsAsFactors = FALSE) # return matrix to dataframe
sl.sorted <- as.data.frame(sl.sorted, stringsAsFactors = FALSE) # return matrix to dataframe
colnames(sv.sorted) <- colnames(sv); colnames(sl.sorted) <- colnames(sl)
sv.sorted <- transform(sv.sorted, v01 = as.numeric(v01), v02 = as.numeric(v02), v03 = as.numeric(v03), v04 = as.numeric(v04), v05 = as.numeric(v05), v06 = as.numeric(v06), v07 = as.numeric(v07), v08 = as.numeric(v08), v09 = as.numeric(v09), v10 = as.numeric(v10), v11 = as.numeric(v11), v12 = as.numeric(v12), v13 = as.numeric(v13), v14 = as.numeric(v14), v15 = as.numeric(v15), v16 = as.numeric(v16), v17 = as.numeric(v17), v18 = as.numeric(v18), v19 = as.numeric(v19) , v20 = as.numeric(v20) , v21 = as.numeric(v21) , v22 = as.numeric(v22) , v23 = as.numeric(v23) , v24 = as.numeric(v24) , v25 = as.numeric(v25)) # return to numeric format
tail(sv.sorted)
tail(sl.sorted)
## Return manipulated columns to data
#sel.l <- grep("^l[0-9]{2}", colnames(vs))
sl.sorted -> vs[,sel.l] # subset label columns
#sel.v <- grep("^v[0-9]{2}", colnames(vs))
sv.sorted -> vs[,sel.v] # subset vote columns
rm(sl, sv, sl.sorted, sv.sorted)
##
#################
## vote shares ##
#################
sel.v <- grep("^v[0-9]{2}", colnames(vs))
v <- vs[,sel.v] # subset vote columns
v <- v / rowSums(v, na.rm = TRUE)
v -> vs[,sel.v] # return manipulation
##
## keep only obs in inc too
tmp <- inc[,c("ord","emm")]
v7 <- merge(x = tmp, y = v7, by = "emm", all.x = TRUE, all.y = FALSE)
va <- merge(x = tmp, y = va, by = "emm", all.x = TRUE, all.y = FALSE)
vs <- merge(x = tmp, y = vs, by = "emm", all.x = TRUE, all.y = FALSE)
## sort
v7  <- v7 [order(v7 $ord),]
va  <- va [order(va $ord),]
vs  <- vs [order(vs $ord),]
inc <- inc[order(inc$ord),]
## check: these are cases where incumbent switched parties, which is recorded in inc but not in coalAgg, so margins need to be recomputed.
## Plus, margins computed with coalSplit. 
table(va=va$win[which(va$win=="fxm")], inc=inc$win[which(va$win=="fxm")]) ### OJO 18feb2025 check how to compute margin for these cases
table(va=va$win,                       inc=inc$win)
##
## compute party margins (vs 2nd when pty won, vs 1st when it didn't)
v7$mg.pan <- NA
v7$mg.pan[inc$win=="pan"]       <- vs$v01   [inc$win=="pan"]    - vs$v02[inc$win=="pan"]
v7$mg.pan[inc$win!="pan"]       <- v7$pan   [inc$win!="pan"]    - vs$v01[inc$win!="pan"]
v7$mg.pan[v7$pan==0] <- NA ## make margin NA when party did not enter the race
v7$mg.pri <- NA
v7$mg.pri[inc$win=="pri"]       <- vs$v01   [inc$win=="pri"]    - vs$v02[inc$win=="pri"]
v7$mg.pri[inc$win!="pri"]       <- v7$pri   [inc$win!="pri"]    - vs$v01[inc$win!="pri"]
v7$mg.pri[v7$pri==0] <- NA ## make margin NA when party did not enter the race
v7$mg.prd <- NA
v7$mg.prd[inc$win=="prd"]       <- vs$v01   [inc$win=="prd"]    - vs$v02[inc$win=="prd"]
v7$mg.prd[inc$win!="prd"]       <- v7$prd   [inc$win!="prd"]    - vs$v01[inc$win!="prd"]
v7$mg.prd[v7$prd==0] <- NA ## make margin NA when party did not enter the race
v7$mg.morena <- NA
v7$mg.morena[inc$win=="morena"] <- vs$v01   [inc$win=="morena"] - vs$v02[inc$win=="morena"]
v7$mg.morena[inc$win!="morena"] <- v7$morena[inc$win!="morena"] - vs$v01[inc$win!="morena"]
v7$mg.morena[v7$morena==0] <- NA ## make margin NA when party did not enter the race
v7$mg.pt <- NA
v7$mg.pt[inc$win=="pt"]        <- vs$v01    [inc$win=="pt"]     - vs$v02[inc$win=="pt"]
v7$mg.pt[inc$win!="pt"]        <- v7$pt     [inc$win!="pt"]     - vs$v01[inc$win!="pt"]
v7$mg.pt[v7$pt==0] <- NA ## make margin NA when party did not enter the race
v7$mg.pvem <- NA
v7$mg.pvem[inc$win=="pvem"]    <- vs$v01    [inc$win=="pvem"]   - vs$v02[inc$win=="pvem"]
v7$mg.pvem[inc$win!="pvem"]    <- v7$pvem   [inc$win!="pvem"]   - vs$v01[inc$win!="pvem"]
v7$mg.pvem[v7$pvem==0] <- NA ## make margin NA when party did not enter the race
v7$mg.mc <- NA
v7$mg.mc[inc$win=="mc"]        <- vs$v01    [inc$win=="mc"]     - vs$v02[inc$win=="mc"]
v7$mg.mc[inc$win!="mc"]        <- v7$mc     [inc$win!="mc"]     - vs$v01[inc$win!="mc"]
v7$mg.mc[v7$mc==0] <- NA ## make margin NA when party did not enter the race
## import margins to inc
inc$mg.pan    <- v7$mg.pan
inc$mg.pri    <- v7$mg.pri
inc$mg.prd    <- v7$mg.prd
inc$mg.morena <- v7$mg.morena
inc$mg.pvem   <- v7$mg.pvem
inc$mg.pt     <- v7$mg.pt
inc$mg.mc     <- v7$mg.mc
inc[1,]

########################################
## lag to create race-prior variables ##
########################################
tmp <- inc
# add dropped observations
tmp <- merge(x = tmp, y = full.xsts[,c("ord","emm","cycle","inegi")], by = "emm", all = TRUE)
dim(inc)
dim(tmp)
tmp$ddrop <- as.numeric(is.na(tmp$inegi.x)) # will drop these obs after lag to retain dimensionality
tmp$ord.x[is.na(tmp$inegi.x)] <- tmp$ord.y[is.na(tmp$inegi.x)] # get missing ords for sorting
tmp$inegi.x[is.na(tmp$inegi.x)] <- tmp$inegi.y[is.na(tmp$inegi.x)] # get missing inegi codes for grouping
tmp$inegi.y <- tmp$ord.y <- NULL
colnames(tmp)[which(colnames(tmp)=="inegi.x")] <- "inegi" # rename back to inegi
colnames(tmp)[which(colnames(tmp)=="ord.x")] <- "ord"     # rename back to ord
#
library(DataCombine) # easy lags
tmp <- tmp[order(tmp$ord),] # verify sorted before lags
tmp <- slide(tmp, Var = "race.after", NewVar = "race.prior",    TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
tmp <- slide(tmp, Var = "win",        NewVar = "win.prior",     TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
#tmp <- slide(tmp, Var = "ddied",      NewVar = "ddied.prior",   TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
#tmp$drep <- as.numeric(tmp$drepe==1 | tmp$drepg==1) # join drep info into one and dichotomize
#tmp <- slide(tmp, Var = "drep",       NewVar = "drep.prior",    TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
#tmp <- slide(tmp, Var = "dlegacy",    NewVar = "dlegacy.prior", TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
#
tmp <- tmp[-which(tmp$ddrop==1), -which(colnames(tmp)=="ddrop")] # drop added obs
#tmp[which(tmp$inegi==9004), c("emm","mun","win.prior","win","incumbent","race.prior","race.after")] # verify
inc <- tmp # replace manipulated object

##########################################################################################
## MANIPULATE WIN.PRIOR IN NEW MUNICS... looked at win in parent municipio and used it  ##
##########################################################################################
## # read master list of municipal parents/offspring in case needed for debugging
## tmp <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/ancillary/new-mun-parents-1989on.csv"
## tmp <- read.csv(tmp, stringsAsFactors = FALSE)
## head(tmp)
#
sel <- which(inc$emm=="ags-08.010");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="ags-08.011");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="bc-10.005");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pan"
sel <- which(inc$emm=="bc-19.006");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun morena"
sel <- which(inc$emm=="bc-19.007");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun morena"
sel <- which(inc$emm=="bcs-08.009");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cam-08.009");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cam-10.010");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cam-11.011");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cam-18.013");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cam-18.012");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pan"
sel <- which(inc$emm=="cps-08.112");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cps-11.113");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cps-11.114");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cps-11.115");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cps-11.116");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cps-11.117");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cps-11.118");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cps-11.119");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cps-15.122"); # me lo saqué de la manga
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cps-15.123");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pvem"
sel <- which(inc$emm=="cps-15.124"); # mezcalapa 2012 to pri (won secciones in 2010)
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cps-17.120"); # cap luis vidal 2018 to pvem (won secciones in 2015, pchu had won mun)
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pvem"
sel <- which(inc$emm=="cps-17.121"); # rincon chamula 2018 to pvem (won secciones in 2015, prd had won mun)
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pvem"
sel <- which(inc$emm=="cps-18.125");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cps-15.xxx"); # ojo needs inegi code
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- grep("df-11.002", inc$emm) # df2000 (first municipal election)
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- grep("df-11.003", inc$emm) # 
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- grep("df-11.004", inc$emm) # 
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- grep("df-11.005", inc$emm) # 
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- grep("df-11.006", inc$emm) # 
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- grep("df-11.007", inc$emm) # 
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- grep("df-11.008", inc$emm) # 
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- grep("df-11.009", inc$emm) # milpa alta to pri
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- grep("df-11.010", inc$emm) # 
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- grep("df-11.011", inc$emm) # 
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- grep("df-11.012", inc$emm) # 
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- grep("df-11.013", inc$emm) # 
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- grep("df-11.014", inc$emm) # benito juarez to pan
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pan"
sel <- grep("df-11.015", inc$emm) # 
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- grep("df-11.016", inc$emm) # miguel hidalgo to pan
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pan"
sel <- grep("df-11.017", inc$emm) # 
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- which(inc$emm=="dgo-07.039");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="gue-09.076");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="gue-12.077"); # pan en cuajinicualapa
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="gue-13.078");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- which(inc$emm=="gue-13.079");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="gue-13.080");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="gue-13.081"); # pri en san luis acatlan
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- which(inc$emm=="gue-18.082"); # prd san marcos
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- which(inc$emm=="gue-18.084"); # pes en malinaltepec
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pes"
sel <- which(inc$emm=="gue-18.085"); # pvem en cuajinicuilapa
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pvem"
sel <- which(inc$emm=="jal-13.125");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pan"
#sel <- which(inc$emm=="jal-xx.xxx"); # capilla de guadalupe might eventually appear
#inc$race.prior[sel] <- "new mun"; win.prior[sel] <- "new mun "
sel <- which(inc$emm=="mex-08.122");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="mex-11.123");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="mex-11.124");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="mex-12.125");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="nay-07.020");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="qui-09.008");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="qui-13.009");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- which(inc$emm=="qui-15.010");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="qui-16.011");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="san-10.057");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="san-10.058");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pan"
sel <- which(inc$emm=="sin-19.019"); ## morena in culiacán
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun morena"
sel <- which(inc$emm=="sin-19.020"); ## morena in guasave
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun morena"
sel <- which(inc$emm=="son-08.070"); # usé puerto peñasco
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="son-10.071");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="son-10.072");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.045");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.046");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.047");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.048");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.049");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.050");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.051");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.052");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.053");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.054");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.055");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.056");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.057");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.058");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.059");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="tla-09.060");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="ver-08.204");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="ver-08.205");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="ver-08.206");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="ver-08.207");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="ver-10.208");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="ver-10.209");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="ver-10.210");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="ver-12.211");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pan"
sel <- which(inc$emm=="ver-12.212");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="zac-11.057");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
sel <- which(inc$emm=="zac-13.058");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun prd"
#
# there are NAs before 1993 ignored because analysis drops those years
# fix them in future when needed
table(is.na(inc$win.prior), inc$yr)
#
# spot missing cases post 1994, if any
sel <- which(is.na(inc$win.prior) & inc$yr>1994)
with(inc[sel,], data.frame(emm, inegi, ife, mun, yr, win.prior, race.prior, win))
# code them by hand
sel <- which(inc$emm=="cps-17.058"|inc$emm=="cps-09.072"|inc$emm=="cps-09.107"|inc$emm=="oax-13.053"|inc$emm=="oax-18.130"|inc$emm=="oax-10.141"|inc$emm=="oax-14.472"|inc$emm=="san-11.031")
inc$win.prior[sel] <- inc$race.after[sel] <- "consejoMunic"
sel <- which(inc$emm=="oax-15.088")
inc$win.prior[sel] <- inc$race.after[sel] <- "was-uyc"

## code incumbent running, open seat or term limit in current race
inc$inc.current <- inc$race.prior
inc$inc.current [grep("out", inc$inc.current, ignore.case = TRUE)]           <- "open" ## Open seat
inc$inc.current [grep("term", inc$inc.current, ignore.case = TRUE)]          <- "term" ## Term limit
inc$inc.current [grep("reran|reelect", inc$inc.current, ignore.case = TRUE)] <- "ran"  ## Incumbent running
inc$inc.current [grep("pending", inc$inc.current, ignore.case = TRUE)]       <- NA
inc$inc.current [inc$yr < 2018]                                              <- "term"
inc$inc.current [inc$inc.current=="new mun" & inc$yr >= 2018] <- "open"
table(inc$inc.current, useNA = "ifany")
## code party currently defends the seat 
table(inc$win.prior)
inc$dpanin    <- 0; inc$dpanin   [grep("pan"   , inc$win.prior)] <- 1
inc$dpriin    <- 0; inc$dpriin   [grep("pri"   , inc$win.prior)] <- 1
inc$dprdin    <- 0; inc$dprdin   [grep("prd"   , inc$win.prior)] <- 1
inc$dmorenain <- 0; inc$dmorenain[grep("morena", inc$win.prior)] <- 1
inc$dpvemin   <- 0; inc$dpvemin  [grep("pvem"  , inc$win.prior)] <- 1
inc$dptin     <- 0; inc$dptin    [grep("pt"    , inc$win.prior)] <- 1
inc$dmcin     <- 0; inc$dmcin    [grep("mc"    , inc$win.prior)] <- 1

## dummy concurred with gubernatorial race since 1996
inc$dgub <- 0
inc$dgub[inc$yr==2024 & inc$edon %in% c(7, 9, 11, 14, 17, 21, 27, 30, 31)] <- 1
inc$dgub[inc$yr==2023 & inc$edon %in% c(5, 15)] <- 1
inc$dgub[inc$yr==2022 & inc$edon %in% c(1, 10, 13, 20, 23, 28)] <- 1
inc$dgub[inc$yr==2021 & inc$edon %in% c(2, 3, 4, 6, 8, 12, 16, 18, 19, 22, 24, 25, 26, 29, 32)] <- 1
inc$dgub[inc$yr==2019 & inc$edon %in% c(2, 21)] <- 1
inc$dgub[inc$yr==2018 & inc$edon %in% c(7, 9, 11, 14, 17, 21, 27, 30, 31)] <- 1
inc$dgub[inc$yr==2017 & inc$edon %in% c(5, 15, 18)] <- 1
inc$dgub[inc$yr==2016 & inc$edon %in% c(1, 6, 8, 10, 13, 20, 21, 23, 25, 28, 29, 30, 32)] <- 1
inc$dgub[inc$yr==2015 & inc$edon %in% c(3, 4, 6, 12, 16, 19, 22, 24, 26)] <- 1
inc$dgub[inc$yr==2013 & inc$edon %in% c(2)] <- 1
inc$dgub[inc$yr==2012 & inc$edon %in% c(7, 9, 11, 14, 17, 27, 31)] <- 1
inc$dgub[inc$yr==2011 & inc$edon %in% c(3, 5, 12, 15, 16, 18)] <- 1
inc$dgub[inc$yr==2010 & inc$edon %in% c(1, 8, 10, 13, 20, 21, 23, 25, 28, 29, 30, 32)] <- 1
inc$dgub[inc$yr==2009 & inc$edon %in% c(4, 6, 19, 22, 24, 26)] <- 1
inc$dgub[inc$yr==2007 & inc$edon %in% c(2, 16, 31)] <- 1
inc$dgub[inc$yr==2006 & inc$edon %in% c(7, 9, 11, 14, 17, 27)] <- 1
inc$dgub[inc$yr==2005 & inc$edon %in% c(3, 5, 6, 12, 13, 15, 18, 23)] <- 1
inc$dgub[inc$yr==2004 & inc$edon %in% c(1, 8, 10, 20, 21, 25, 28, 29, 30, 32)] <- 1
inc$dgub[inc$yr==2003 & inc$edon %in% c(4, 6, 19, 22, 24, 26)] <- 1
inc$dgub[inc$yr==2001 & inc$edon %in% c(16, 31, 27, 2)] <- 1
inc$dgub[inc$yr==2000 & inc$edon %in% c(7, 9, 11, 14, 17, 27)] <- 1
inc$dgub[inc$yr==1999 & inc$edon %in% c(13, 23, 5, 12, 15, 18, 3)] <- 1
inc$dgub[inc$yr==1998 & inc$edon %in% c(1, 8, 10, 20, 21, 25, 28, 29, 30, 32)] <- 1
inc$dgub[inc$yr==1997 & inc$edon %in% c(9, 6, 4, 19, 22, 24, 26)] <- 1
inc$dgub[inc$yr==1995 & inc$edon %in% c(2, 11, 14, 16, 31)] <- 1
inc$dgub[inc$yr==1994 & inc$edon %in% c(7, 17, 27)] <- 1
inc$dgub[inc$yr==1993 & inc$edon %in% c(3, 5, 12, 13, 15, 18, 23, 24, 31)] <- 1
inc$dgub[inc$yr==1992 & inc$edon %in% c(1, 8, 10, 16, 20, 21, 25, 28, 29, 30, 32)] <- 1
inc$dgub[inc$yr==1991 & inc$edon %in% c(4, 6, 11, 19, 22, 24, 26)] <- 1
inc$dgub[inc$yr==1991 & inc$edon %in% c(4, 6, 11, 19, 22, 24, 26)] <- 1
inc$dgub[inc$yr==1989 & inc$edon %in% c(2)] <- 1
inc$dgub[inc$yr==1988 & inc$edon %in% c(7, 14, 17, 27)] <- 1

## incumbent gov party at election
inc$gpty <- NA
inc$gpty[is.na(inc$gpty) & inc$yr<=1989] <- "pri"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=1992 & inc$edon!=2] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2019 & inc$edon==2] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr> 2019 & inc$edon==2] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=1998 & inc$edon==1] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2010 & inc$edon==1] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr<=2016 & inc$edon==1] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2016 & inc$edon==1] <- "pan"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=1999 & inc$edon==3] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2011 & inc$edon==3] <- "prd"
inc$gpty[is.na(inc$gpty) & inc$yr<=2021 & inc$edon==3] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr> 2021 & inc$edon==3] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2021 & inc$edon==4] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2021 & inc$edon==4] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$edon==5] <- "pri"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2021 & inc$edon==6] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2021 & inc$edon==6] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2000 & inc$edon==7] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2012 & inc$edon==7] <- "prd"   ## salazar madiguchia coded as prd
inc$gpty[is.na(inc$gpty) & inc$yr<=2018 & inc$edon==7] <- "pvem"
inc$gpty[is.na(inc$gpty) & inc$yr> 2018 & inc$edon==7] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=1992 & inc$edon==8] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=1998 & inc$edon==8] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr<=2016 & inc$edon==8] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2016 & inc$edon==8] <- "pan"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=1997 & inc$edon==9] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2018 & inc$edon==9] <- "prd"
inc$gpty[is.na(inc$gpty) & inc$yr> 2018 & inc$edon==9] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2016 & inc$edon==10] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2022 & inc$edon==10] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr> 2022 & inc$edon==10] <- "pri"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=1991 & inc$edon==11] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 1991 & inc$edon==11] <- "pan"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2005 & inc$edon==12] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2015 & inc$edon==12] <- "prd"
inc$gpty[is.na(inc$gpty) & inc$yr<=2021 & inc$edon==12] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2021 & inc$edon==12] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2021 & inc$edon==13] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2021 & inc$edon==13] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=1995 & inc$edon==14] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2013 & inc$edon==14] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr<=2018 & inc$edon==14] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2018 & inc$edon==14] <- "mc"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2023 & inc$edon==15] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2023 & inc$edon==15] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2001 & inc$edon==16] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2012 & inc$edon==16] <- "prd"
inc$gpty[is.na(inc$gpty) & inc$yr<=2015 & inc$edon==16] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2021 & inc$edon==16] <- "prd"
inc$gpty[is.na(inc$gpty) & inc$yr> 2021 & inc$edon==16] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2000 & inc$edon==17] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2012 & inc$edon==17] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr<=2018 & inc$edon==17] <- "prd"
inc$gpty[is.na(inc$gpty) & inc$yr> 2018 & inc$edon==17] <- "morena" ## cuauhtémoc blanco coded morena
##
inc$gpty[is.na(inc$gpty) & inc$yr<=1999 & inc$edon==18] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2005 & inc$edon==18] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr<=2017 & inc$edon==18] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2021 & inc$edon==18] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr> 2021 & inc$edon==18] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=1997 & inc$edon==19] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2003 & inc$edon==19] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr<=2015 & inc$edon==19] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2021 & inc$edon==19] <- "indep"
inc$gpty[is.na(inc$gpty) & inc$yr> 2021 & inc$edon==19] <- "mc"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2010 & inc$edon==20] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2016 & inc$edon==20] <- "prd" ## gabino cue coded prd
inc$gpty[is.na(inc$gpty) & inc$yr<=2022 & inc$edon==20] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2022 & inc$edon==20] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2011 & inc$edon==21] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2019 & inc$edon==21] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr> 2019 & inc$edon==21] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=1997 & inc$edon==22] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2009 & inc$edon==22] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr<=2015 & inc$edon==22] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2015 & inc$edon==22] <- "pan"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2016 & inc$edon==23] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2022 & inc$edon==23] <- "prd"
inc$gpty[is.na(inc$gpty) & inc$yr> 2022 & inc$edon==23] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2003 & inc$edon==24] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2009 & inc$edon==24] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr<=2021 & inc$edon==24] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2021 & inc$edon==24] <- "pvem"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2011 & inc$edon==25] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2016 & inc$edon==25] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr<=2021 & inc$edon==25] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2021 & inc$edon==25] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2009 & inc$edon==26] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2015 & inc$edon==26] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr<=2021 & inc$edon==26] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2021 & inc$edon==26] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2013 & inc$edon==27] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2019 & inc$edon==27] <- "prd"
inc$gpty[is.na(inc$gpty) & inc$yr> 2019 & inc$edon==27] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2015 & inc$edon==28] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2021 & inc$edon==28] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr> 2021 & inc$edon==28] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=1999 & inc$edon==29] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2005 & inc$edon==29] <- "prd"
inc$gpty[is.na(inc$gpty) & inc$yr<=2011 & inc$edon==29] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr<=2021 & inc$edon==29] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2021 & inc$edon==29] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2016 & inc$edon==30] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2018 & inc$edon==30] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr> 2018 & inc$edon==30] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=2001 & inc$edon==31] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2007 & inc$edon==31] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr<=2018 & inc$edon==31] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2024 & inc$edon==31] <- "pan"
inc$gpty[is.na(inc$gpty) & inc$yr> 2024 & inc$edon==31] <- "morena"
##
inc$gpty[is.na(inc$gpty) & inc$yr<=1998 & inc$edon==32] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr<=2010 & inc$edon==32] <- "prd"
inc$gpty[is.na(inc$gpty) & inc$yr<=2021 & inc$edon==32] <- "pri"
inc$gpty[is.na(inc$gpty) & inc$yr> 2021 & inc$edon==32] <- "morena"
## ## check
## data.frame(inc$edon, inc$yr, inc$gpty)[inc$edon==30,]
table(inc$gpty)

## midterm governor
inc$dmidpan    <- (1 - inc$dgub) * as.numeric(inc$gpty=="pan")
inc$dmidpri    <- (1 - inc$dgub) * as.numeric(inc$gpty=="pri")
inc$dmidprd    <- (1 - inc$dgub) * as.numeric(inc$gpty=="prd")
inc$dmidmorena <- (1 - inc$dgub) * as.numeric(inc$gpty=="morena")
inc$dmidpvem   <- (1 - inc$dgub) * as.numeric(inc$gpty=="pvem")
inc$dmidmc     <- (1 - inc$dgub) * as.numeric(inc$gpty=="mc")

aggregate(mg.pan    ~ inc.current * dpanin    * dmidpan,    data = inc, subset = yr>2017, FUN = function(x) round(mean(x, na.rm = TRUE)*100, 1))
aggregate(mg.pri    ~ inc.current * dpriin    * dmidpri,    data = inc, subset = yr>2017, FUN = function(x) round(mean(x, na.rm = TRUE)*100, 1))
aggregate(mg.prd    ~ inc.current * dprdin    * dmidprd,    data = inc, subset = yr>2017, FUN = function(x) round(mean(x, na.rm = TRUE)*100, 1))
aggregate(mg.morena ~ inc.current * dmorenain * dmidmorena, data = inc, subset = yr>2017, FUN = function(x) round(mean(x, na.rm = TRUE)*100, 1))
aggregate(mg.pvem   ~ inc.current * dpvemin   * dmidpvem,   data = inc, subset = yr>2017, FUN = function(x) round(mean(x, na.rm = TRUE)*100, 1))
aggregate(mg.mc     ~ inc.current * dmcin     * dmidmc,     data = inc, subset = yr>2017, FUN = function(x) round(mean(x, na.rm = TRUE)*100, 1))

sel <- which(inc$yr>1978)
print("copy-here")
sel <- which(inc$inc.current == "term" & inc$dpanin == 1)
round(tapply(inc$mg.pan[sel], inc$cyclef[sel], FUN = function(x) mean(x, na.rm = TRUE))*100, 1)
sel <- which(inc$inc.current == "term" & inc$dpanin == 0)
round(tapply(inc$mg.pan[sel], inc$cyclef[sel], FUN = function(x) mean(x, na.rm = TRUE))*100, 1)
sel <- which(inc$inc.current == "ran" & inc$dpanin == 1)
round(tapply(inc$mg.pan[sel], inc$cyclef[sel], FUN = function(x) mean(x, na.rm = TRUE))*100, 1)
sel <- which(inc$inc.current == "ran" & inc$dpanin == 0)
round(tapply(inc$mg.pan[sel], inc$cyclef[sel], FUN = function(x) mean(x, na.rm = TRUE))*100, 1)
sel <- which(inc$inc.current == "open" & inc$dpanin == 1)
round(tapply(inc$mg.pan[sel], inc$cyclef[sel], FUN = function(x) mean(x, na.rm = TRUE))*100, 1)
sel <- which(inc$inc.current == "open" & inc$dpanin == 0)
round(tapply(inc$mg.pan[sel], inc$cyclef[sel], FUN = function(x) mean(x, na.rm = TRUE))*100, 1)
##


compute mean party margin depending on incumbent status
myxtab
x



library(RColorBrewer)
#pdf(file =     "../graph/reel-munic2021.pdf", width = 7, height = 6)
#png(filename = "../graph/reel-munic2021.png", width = 700, height = 480)
clr <- brewer.pal(n=6, name = 'Paired'); clr <- clr[c(4,3,6,5,2,1)]
par(mar = c(2,0,1.2,0)+.1) # bottom, left, top, right 
plot(x = c(-9,105), y = c(0.4,nrow(tmp2)+1), type = "n", main = "Municipios con reelección 2021", axes = FALSE)
axis(1, at=seq(0,100,10),label=FALSE)
axis(1, at=seq(0,100,20),labels=c(seq(0,80,20),"100%"),cex.axis=.9)
polygon(x=c(-20,-20,120,120), y=c(5,6,6,5),col="gray85",border="gray85")
abline(h=1:(nrow(tmp2)+1), lty = 3)
#abline(h=5:6)
for (i in 1:nrow(tmp2)){
    #i <- 1
    l <- c(0,0); r <- rep(tmp2[i,1],2)
    polygon(y = c(i+1/6, i+5/6, i+5/6, i+1/6), x = c(l,r), col = clr[1], border = clr[1])
    if (tmp2[i,1]>.5) text(y = i+1/2, x = (l+r)[1]/2, labels = paste0(round(tmp2[i,1]),"%"), cex = .67, col = "white")
    l <- r; r <- r+rep(tmp2[i,2],2)
    polygon(y = c(i+1/6, i+5/6, i+5/6, i+1/6), x = c(l,r), col = clr[2], border = clr[2])
    if (tmp2[i,2]>.5) text(y = i+1/2, x = (l+r)[1]/2, labels = paste0(round(tmp2[i,2]),"%"), cex = .67, col = "gray50")
    l <- r; r <- r+rep(tmp2[i,3],2)
    polygon(y = c(i+1/6, i+5/6, i+5/6, i+1/6), x = c(l,r), col = clr[3], border = clr[3])
    if (tmp2[i,3]>.5) text(y = i+1/2, x = (l+r)[1]/2, labels = paste0(round(tmp2[i,3]),"%"), cex = .67, col = "white")
    l <- r; r <- r+rep(tmp2[i,4],2)
    polygon(y = c(i+1/6, i+5/6, i+5/6, i+1/6), x = c(l,r), col = clr[4], border = clr[4])
    if (tmp2[i,4]>.5) text(y = i+1/2, x = (l+r)[1]/2, labels = paste0(round(tmp2[i,4]),"%"), cex = .67, col = "gray50")
    l <- r; r <- r+rep(tmp2[i,5],2)
    polygon(y = c(i+1/6, i+5/6, i+5/6, i+1/6), x = c(l,r), col = clr[5], border = clr[5])
    if (tmp2[i,5]>.5) text(y = i+1/2, x = (l+r)[1]/2, labels = paste0(round(tmp2[i,5]),"%"), cex = .67, col = "white")
    l <- r; r <- r+rep(tmp2[i,6],2)
    polygon(y = c(i+1/6, i+5/6, i+5/6, i+1/6), x = c(l,r), col = clr[6], border = clr[6])
    if (tmp2[i,6]>.5) text(y = i+1/2, x = (l+r)[1]/2, labels = paste0(round(tmp2[i,6]),"%"), cex = .67, col = "gray50")
}
text(x=-7 , y=c(1:nrow(tmp2))+.5, labels = rownames(tmp2), cex = .85)#, srt = 90)
text(x=105, y=c(1:nrow(tmp2))+.5, labels = paste0("N=", tmp2[,8]), cex = .75)
#legend(x = 0, y = 0.75, legend = c("Ocupante reelecto","derrotado","Silla vacía ganó","perdió","Term limit ganó","perdió"), fill = clr, cex = .67, border = clr, bty = "n", horiz = TRUE)
legend(x = -2,  y = 0.85, legend = c("reelecto","derrotado"), title = "Ocupante contendió"  , fill = clr[1:2], cex = .85, border = clr[1:2], bty = "n", horiz = TRUE)
legend(x = 40, y = 0.85, legend = c("ganó","perdió")       , title = "Term limit, partido", fill = clr[3:4], cex = .85, border = clr[3:4], bty = "n", horiz = TRUE)
legend(x = 72, y = 0.85, legend = c("ganó","perdió")       , title = "Silla vacía, partido" , fill = clr[5:6], cex = .85, border = clr[5:6], bty = "n", horiz = TRUE)
text(x = 105, y = .9, "@emagar", col = "gray", cex = .7)
#dev.off()

##########################################
## Get municipality electoral histories ##
##########################################
hd <- "/home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/data/" # where histories are stored
#
# wrap reading in a function
tmp <- function(y){
    v <- read.csv(file = paste0(hd,"dipfed-municipio-vhat-",y,".csv"), stringsAsFactors = FALSE)
    v$inegi <- ife2inegi(v$ife)
    v$yr <- y
    return(v)
}
vhis <- data.frame()
vhis <- rbind(vhis, tmp(2006))
vhis <- rbind(vhis, tmp(2009))
vhis <- rbind(vhis, tmp(2012))
vhis <- rbind(vhis, tmp(2015))
vhis <- rbind(vhis, tmp(2018))
vhis <- rbind(vhis, tmp(2021))
tail(vhis)

#####################
## add emm to vhis ##
#####################
tmp <- inc[,c("emm","ife","yr")]
tmp <- tmp[tmp$yr>=2005,] # will use dipfed2006 hat for ay els 2005:2007, etc so drop prior to 2005
tmp$dfyr <-   ifelse(tmp$yr>=2005 & tmp$yr<=2007, 2006 # which dipfed closest to ayuntamiento election?
            , ifelse(tmp$yr>=2008 & tmp$yr<=2010, 2009
            , ifelse(tmp$yr>=2011 & tmp$yr<=2013, 2012
            , ifelse(tmp$yr>=2014 & tmp$yr<=2016, 2015
            , ifelse(tmp$yr>=2017 & tmp$yr<=2019, 2018
            , ifelse(tmp$yr>=2020 & tmp$yr<=2022, 2021, 0
                     ))))))
tmp1 <- vhis
tmp1$ife.yr <-            tmp1$ife + tmp1$yr/10000
tmp$ife.yr  <- as.numeric(tmp$ife) +  tmp$dfyr/10000
tmp1 <- merge(x = tmp1, y = tmp[,c("ife.yr","emm")], by = "ife.yr", all.x = TRUE, all.y = FALSE)
# verify
sel <- which(tmp1$inegi==29001)grep("tla-...001", tmp1$emm)
tmp1[sel,] # NA in emm is correct, that row will not be used given ayun elec in 2004 2007 2010 2013 2016 2021
vhis <- tmp1
vhis$ife.yr <- NULL

inc[1,]
vhis[1,]
x


## #############################################
## # subset: cases allowing reelection in 2018 #
## # esto lo reporté en el blog de Nexos       #
## #############################################
## sel <- which(inc$yr==2018 & inc$edon!=9 & inc$edon!=21)
## inc.sub <- inc[sel,]
## dim(inc.sub)
## #
## sel <- which(inc.sub$race.prior=="pending"|inc.sub$race.prior=="")
## inc.sub$emm[sel]
## if (length(sel)>0) inc.sub <- inc.sub[-sel,] # drop cases with pending election
## #
## table(inc.sub$edon, inc.sub$race.prior) # by state
## table(              inc.sub$race.prior)
## nrow(inc.sub)
## round(table(inc.sub$race.prior) / nrow(inc.sub),2)
## #
## table(inc.sub$win, inc.sub$race.prior) # by incumbent party
## tab <- table(inc.sub$win.prior, inc.sub$race.prior)
## rowSums(tab)
## sum(rowSums(tab))
## round(table(inc.sub$win.prior, inc.sub$race.prior) *100 / rowSums(tab), 1)
## round(colSums(tab) *100 / sum(rowSums(tab)), 1)
## #
## # subset: cases NOT allowing reelection in 2018
## sel <- which(inc$yr==2018 & (inc$edon==9 | inc$edon==21))
## inc.sub <- inc[sel,]
## #
## table(inc.sub$edon, inc.sub$race.prior) # by state
## table(              inc.sub$race.prior)
## nrow(inc.sub)
## round(table(inc.sub$race.prior) / nrow(inc.sub),2)
## #
## table(inc.sub$win, inc.sub$race.prior) # by incumbent party
## tab <- table(inc.sub$win.prior, inc.sub$race.prior)
## rowSums(tab)
## round(table(inc.sub$win.prior, inc.sub$race.prior) *100 / rowSums(tab), 0)
## #
## sel <- which(inc$yr==2018 & inc$edon!=16)
## inc.sub <- inc[sel,]
## #
## ####################
## ## end blog nexos ##
## ####################
#
################################################
## ########################################## ##
## ## SCRIPT FROM incumbents.r *ENDS* HERE ## ##
## ########################################## ##
################################################
#
# CLEAN MESS
rm(clr,i,l,r,sel,sel1,tmp,tmp1,tmp2,tmp3,tmp4)
#
###################################
## ############################# ##
## ## party reelected dummies ## ##
## ############################# ##
###################################

## NOTE 5-7-2021 --- lines 773-1023:
## new categorization of pwon/dif-p in race.after offers much easier way of coding these dummies than bloc below!
## Skip this bloc until all final 2021 returns are in and have full combination of pre/post winners 

save.image(file = "/home/eric/Desktop/MXelsCalendGovt/reelec/data/tmp.RData")
aqui voy

## NOTE 19-3-2021 --- ~ lines 590-666
## Ya he tenido esta duda: dpwon se refiere a t+1. Sospecho que la terminología sería más clara si:
## (1) t se refiere al trienio del incumbent y también a la elección que ganó (win) 
## (2) win.prior = t-1 partido del incumbent previo 
## (3) win.post = partido que ganó la siguiente elección
## (4) dpwin = 1 if win==win.prior
## (5) dpwin.post = 1 if win==win.post
## x
## Solución: variables con nombres más claros
## dpty.same.as.last
## dpty.same.as.next
## win --> incumbent.pty
#
# will receive dichotomous vars
inc$dpty.same.as.last <- NA
inc$dpty.same.as.next <- NA

###########################################################################
## NAs due to lack of info to 99 (returned to NA later, easier to debug) ##
###########################################################################
sel  <- which(is.na(inc$dpty.same.as.last)==TRUE)  # only NAs need manipulation
manip <- inc$dpty.same.as.last[sel]                # extract for manipulation
tail(inc)
sel1 <- which(is.na(inc$win.long.prior[sel])==TRUE)
sel2 <- which(is.na(inc$win.long[sel])==TRUE)
if (length(union(sel1,sel2))>0) manip[union(sel1,sel2)] <- 99 # either is NA
sel1 <- which(inc$win.long.prior[sel]=="0")
sel2 <- which(inc$win.long[sel]=="0")
if (length(union(sel1,sel2))>0) manip[union(sel1,sel2)] <- 99 # either is 0
inc$dpty.same.as.last[sel] <- manip # return to data after manipulation
#
sel  <- which(is.na(inc$dpty.same.as.next)==TRUE)  # only NAs need manipulation
manip <- inc$dpty.same.as.next[sel]                # extract for manipulation
sel1 <- which(is.na(inc$win.long.post[sel])==TRUE)
sel2 <- which(is.na(inc$win.long[sel])==TRUE)
if (length(union(sel1,sel2))>0) manip[union(sel1,sel2)] <- 99 # either is NA
sel1 <- which(inc$win.long.post[sel]=="0")
sel2 <- which(inc$win.long[sel]=="0")
if (length(union(sel1,sel2))>0) manip[union(sel1,sel2)] <- 99 # either is 0
inc$dpty.same.as.next[sel] <- manip # return to data after manipulation 
################################
## unelected authorities to 0 ##
## uses win not win.long      ##
################################

sel <- which(is.na(inc$dpty.same.as.last)==TRUE)  # only NAs need manipulation
manip <- inc$dpty.same.as.last[sel]               # extract for manipulation
sel1 <- grep("anulada|consejoMun|litigio|uyc|0",inc$win.prior[sel])
sel2 <- grep("anulada|consejoMun|litigio|uyc|0",inc$win[sel])
if (length(union(sel1,sel2))>0) manip[union(sel1,sel2)] <- 0 # either
inc$dpty.same.as.last[sel] <- manip # return to data after manipulation
#
sel <- which(is.na(inc$dpty.same.as.next)==TRUE)  # only NAs need manipulation
manip <- inc$dpty.same.as.next[sel]               # extract for manipulation
sel1 <- grep("anulada|consejoMun|litigio|uyc|0",inc$win.post[sel])
sel2 <- grep("anulada|consejoMun|litigio|uyc|0",inc$win[sel])
if (length(union(sel1,sel2))>0) manip[union(sel1,sel2)] <- 0 # either
inc$dpty.same.as.next[sel] <- manip # return to data after manipulation
###########################
## independents to 0     ##
## uses win not win.long ##
###########################
sel <- which(is.na(inc$dpty.same.as.last)==TRUE)  # only NAs need manipulation
manip <- inc$dpty.same.as.last[sel]               # extract for manipulation
sel1 <- grep("indep",inc$win.prior[sel])
sel2 <- grep("indep",inc$win[sel])
manip[union(sel1,sel2)] <- 0 # one or the other (or both, changed next)
sel3 <- intersect(sel1,sel2) # both
sel4 <- grep("Incumb-remained", inc$race.prior[sel])
manip[intersect(sel3,sel4)] <- 1 # both (independent incumbent reelected coded as party won)
inc$dpty.same.as.last[sel] <- manip # return to data after manipulation 
#
sel <- which(is.na(inc$dpty.same.as.next)==TRUE)  # only NAs need manipulation
manip <- inc$dpty.same.as.next[sel]               # extract for manipulation
sel1 <- grep("indep",inc$win.post[sel])
sel2 <- grep("indep",inc$win[sel])
manip[union(sel1,sel2)] <- 0 # one or the other (or both, changed next)
sel3 <- intersect(sel1,sel2) # both
sel4 <- grep("Reelected", inc$race.after[sel])
manip[intersect(sel3,sel4)] <- 1 # both (independent incumbent reelected coded as party won)
inc$dpty.same.as.next[sel] <- manip # return to data after manipulation
#
#####################################################################################################
## by covering all individual winning parties (solo or in coalition), remaining NAs must be zeroes ##
#####################################################################################################
indiv.pties <- unique(inc$win.long)
indiv.pties <- unlist(strsplit(indiv.pties, split = "-")) # split coalition constituent parties
indiv.pties <- unique(indiv.pties)
indiv.pties <- indiv.pties[order(indiv.pties)] # all these must be individually processed with my.fun 
##################################################
## define function to process dpty.same.as.last ##
##################################################
my.fun <- function(target = NA){
    #target <- "pt" # debug
    target <- paste("^", target, "$|-", target, "|", target, "-", sep = "") # target solo or in coalition
    sel <- which(is.na(inc$dpty.same.as.last)==TRUE) # only NAs need manipulation
    manip <- inc$dpty.same.as.last[sel]              # extract for manipulation
    sel1 <- grep(target, inc$win.long.prior[sel])    # is target in win.long.prior? indexes?
    sel2 <- grep(target, inc$win.long      [sel])    # is target in win.long? indexes?
    manip[intersect(sel1,sel2)] <- 1                 # in both
    inc$dpty.same.as.last[sel] <- manip              # return to data after manipulation
    return(inc$dpty.same.as.last)                    # function does not modify inc, so send as output
}
# run my.fun in each element of indiv.pties (individual party list)
for (i in 1:length(indiv.pties)){
    # my.fun("prd") # debug
    inc$dpty.same.as.last <- my.fun(indiv.pties[i])
}
##################################################
## define function to process dpty.same.as.next ##
##################################################
my.fun <- function(target = NA){
    #target <- "pt" # debug
    target <- paste("^", target, "$|-", target, "|", target, "-", sep = "") # target solo or in coalition
    sel <- which(is.na(inc$dpty.same.as.next)==TRUE) # only NAs need manipulation
    manip <- inc$dpty.same.as.next[sel]              # extract for manipulation
    sel1 <- grep(target, inc$win.long.post[sel])     # is target in win.long.prior? indexes?
    sel2 <- grep(target, inc$win.long      [sel])    # is target in win.long? indexes?
    manip[intersect(sel1,sel2)] <- 1                 # in both
    inc$dpty.same.as.next[sel] <- manip              # return to data after manipulation
    return(inc$dpty.same.as.next)                    # function does not modify inc, so send as output
}
# run my.fun in each element of indiv.pties (individual party list)
for (i in 1:length(indiv.pties)){
    # my.fun("prd") # debug
    inc$dpty.same.as.next <- my.fun(indiv.pties[i])
}

rm(indiv.pties, my.fun)
##################################
## remaining NAs must be zeroes ##
##################################
sel <- which(is.na(inc$dpty.same.as.last)==TRUE);
if (length(sel)>0) inc$dpty.same.as.last[sel] <- 0
sel <- which(is.na(inc$dpty.same.as.next)==TRUE);
if (length(sel)>0) inc$dpty.same.as.next[sel] <- 0
#
# return 99s to NA
sel <- which(inc$dpty.same.as.last==99);
if (length(sel)>0) inc$dpty.same.as.last[sel] <- NA
#
sel <- which(inc$dpty.same.as.next==99);
if (length(sel)>0) inc$dpty.same.as.next[sel] <- NA

# clean
rm(i,min.cal,sel,sel1,sel2,sel3,sel4,tmp,tmp2,manip)
sel <- which(colnames(inc) %in% c("race.prior","win.prior","win.long.prior","win.post","win.long.post"))
inc <- inc[,-sel]

colnames(inc)
ls()

# these lag NAs can be filled with current info
sel <- which(is.na(inc$dpty.same.as.next) & inc$race.after=="uyc")
if (length(sel)>0) inc$dpty.same.as.next[sel] <- 0

# verify
table(inc$race.after)
table(inc$race.after, factor(inc$dpty.same.as.next, labels=c("dift","same")), useNA = "always")
# prd incumbent reelected as indep
sel <- which(inc$race.after=="Reelected" & inc$dpty.same.as.next==0)
inc[inc$inegi==inc$inegi[sel],c("emm", "yr","win","incumbent", "race.after")]

sel <- which(inc$race.after=="Term-limited-p-won" & inc$dpty.same.as.next==0)
inc[sel,]
#
# the dpwon dummy identifies cases where Beaten but party won
sel <- which(inc$race.after=="Beaten" & inc$dpty.same.as.next==1)
data.frame(inc$emm[sel], inc$note[sel])
# and cases where Reelected but party lost
sel <- which(inc$race.after=="Reelected" & inc$dpty.same.as.next==0)
data.frame(inc$emm[sel], inc$note[sel])
#
## # original attempt to code 1s went thus (deprecated)
## sel <- which(is.na(inc$dpty.same.as.last)==TRUE); tmp <- inc$dpty.same.as.last[sel] # extract for manipulation
## sel1 <- grep("pan",inc$win.long.prior[sel])
## sel2 <- grep("pan",inc$win.long[sel])
## tmp[intersect(sel1,sel2)] <- 1 # both
## inc$dpty.same.as.last[sel] <- tmp # return to data after manipulation 
#
# if a party returns after current term, which is it?
# dpty.same.as.last==1 if race.prior winner includes incumbent 
19-3-2021: VERIFY THIS AFTER CHECKING same.last same.next LOOK GOOD
inc$returning.p.prior <- NA
sel <- which(is.na(inc$dpty.same.as.last) | is.na(inc$dpty.same.as.next))
inc$returning.p.prior[sel] <- "pending"
sel <- which(inc$dpty.same.as.last==0)
inc$returning.p.prior[sel] <- "none"
#
sel <- which(is.na(inc$returning.p.prior)); tmp <- inc$returning.p.prior[sel] # extract for manipulation
# for majors use win not win.long
tmp0 <- inc$win.prior[sel]
tmp1 <- inc$win[sel]
sel0 <- which(tmp0=="pan")
sel1 <- which(tmp1=="pan")
tmp[intersect(sel0,sel1)] <- "pan"
#
tmp0 <- inc$win.prior[sel]
tmp1 <- inc$win[sel]
sel0 <- which(tmp0=="pri")
sel1 <- which(tmp1=="pri")
tmp[intersect(sel0,sel1)] <- "pri"
#
tmp0 <- inc$win.prior[sel]
tmp1 <- inc$win[sel]
sel0 <- which(tmp0=="prd")
sel1 <- which(tmp1=="prd")
tmp[intersect(sel0,sel1)] <- "prd"
#
## tmp0 <- inc$win.prior[sel]
## tmp1 <- inc$win[sel]
## sel0 <- which(tmp0=="left")
## sel1 <- which(tmp1=="left")
## tmp[intersect(sel0,sel1)] <- "left"
## #
tmp0 <- inc$win.prior[sel]
tmp1 <- inc$win[sel]
sel0 <- which(tmp0=="morena")
sel1 <- which(tmp1=="morena")
tmp[intersect(sel0,sel1)] <- "morena"
#
inc$returning.p.prior[sel] <- tmp # return to data after manipulation
#
# rest are minor parties
sel <- which(is.na(inc$returning.p.prior)); inc$returning.p.prior[sel] <- "minor"
table(inc$returning.p.prior, useNA = "always")
#
# unlag
inc <- inc[order(inc$ife),] # verify sorted before lags
inc <- slide(inc, Var = "returning.p.prior", NewVar = "returning.p", GroupVar = "ife", slideBy = +1) # lead by one period
#
# clean: drop lags
#inc <- inc[,-grep("prior", colnames(inc))]
inc$returning.p.prior <- NULL
inc$win.long.prior <- NULL
rm(tmp,tmp0,tmp1,tmp2,i,sel,sel0,sel1,sel2,sel3,sel4,min.cal)
inc$fuente <- NULL
options(width=190)
inc[1,]

# rename win
sel <- which(colnames(inc)=="win")
colnames(inc)[sel] <- "incumbent.pty"





## ######################################
## ## BLOCK TO SEARCH FOR NAME REPEATS ##
## ######################################
## ## # get elected governors
## ## gob <- read.csv(paste(dd, "goed1985-present.incumbents.csv", sep = ""), stringsAsFactors = FALSE)
## ## gob$yr <- gob$yr_el; gob$mo <- gob$mo_el; gob$dy <- gob$dy_el
## ## gob$win <- gob$part; gob$incumbent <- gob$gober
## ## gob$mun <- "gobernador"
## ## gob$race.after <- gob$note <- gob$fuente <- gob$returning.p <- ""
## ## gob$dpwin <- NA
## ## gob$inegi <- gob$ife <- gob$munn <- 0
## ## gob <- gob[,c("emm","yr","mun","ord","inegi","edon","munn","ife","mo","dy","win","incumbent","race.after","note","fuente","dpwin","returning.p")]
## ## ## head(gob)
## ## ## head(inc)
## ## # add dummy to drop gobernadores after names have been searched
## ## gob$dgob <- 1; inc$dgob <- 0
## ## # merge
## ## inc <- rbind(inc, gob) # paste governors into incumbents to search for their names too (eg. Monreal was zac governor)
## ## rm(gob)
## ## tail(inc)
## #
## ## # get elected diputados federales
## ## dip <- read.csv(paste(dd, "dfdf2000-present-incumbents.csv", sep = ""), stringsAsFactors = FALSE)
## ## dip$mo <- dip$dy <- NA; dip$win <- dip$part; dip$incumbent <- dip$nom
## ## dip$mun <- "dipfed"
## ## dip$race.after <- dip$note <- dip$fuente <- dip$returning.p <- ""
## ## dip$dpwin <- NA
## ## dip$inegi <- dip$ife <- dip$munn <- dip$dgob <- 0
## ## dip <- dip[,c("emm","yr","mun","ord","inegi","edon","munn","ife","mo","dy","win","incumbent","race.after","note","fuente","dpwin","returning.p","dgob")]
## ## ## head(dip)
## ## ## head(inc)
## ## # add dummy to drop gobernadores after names have been searched
## ## dip$ddip <- 1; inc$ddip <- 0
## ## # merge
## ## inc <- rbind(inc, dip) # paste governors into incumbents to search for their names too (eg. Monreal was zac governor)
## ## rm(dip)
## ## tail(inc)
## #
## ## # get senadores --- under construction
## 
## # clean names
## inc$incumbent <- gsub("  ", " ", inc$incumbent)  # drop double spaces
## inc$incumbent <- sub("^ | $", "", inc$incumbent) # drop trainling/heading spaces
## inc$original.name <- inc$incumbent # duplicate
## #inc$incumbent <- inc$original.name # restore
## inc$incumbent <- gsub("[.]", "", inc$incumbent)  # drop periods
## inc$incumbent <- gsub("[()]", "", inc$incumbent)  # drop parentheses
## inc$incumbent <- gsub("[,]", "", inc$incumbent)  # drop commas
## inc$incumbent <- gsub("[|]", " ", inc$incumbent)  # change | with a space
## inc$incumbent <- gsub("\\[ÜU\\]|\\[UÜ\\]", "U", inc$incumbent)  # change or with a space
## ## sel <- grep("|", inc$incumbent)
## ## inc$incumbent[sel]
## 
## # simplify some words in names to save memory in name search function 
## #
## inc$incumbent <- gsub(" VDA DE", " VDADE", inc$incumbent)
## inc$incumbent <- gsub(" DE LA O ", " DELAO ", inc$incumbent)
## inc$incumbent <- gsub(" DE LA O$", " DELAO", inc$incumbent)
## inc$incumbent <- gsub(" DE LA ", " ", inc$incumbent)
## inc$incumbent <- gsub(" DE L[OA]S ", " ", inc$incumbent)
## inc$incumbent <- gsub(" DE DIOS", " DEDIOS", inc$incumbent)
## inc$incumbent <- gsub(" MONTES DE OCA$", " MONTESDEOCA", inc$incumbent)
## inc$incumbent <- gsub(" MONTES DE OCA ", " MONTESDEOCA ", inc$incumbent)
## inc$incumbent <- gsub(" DE JESUS", " DEJESUS", inc$incumbent)
## inc$incumbent <- gsub(" DE LEON", " DELEON", inc$incumbent)
## inc$incumbent <- gsub(" DE LUIS", " DELUIS", inc$incumbent)
## inc$incumbent <- gsub(" DE ", " ", inc$incumbent)
## inc$incumbent <- gsub(" DEL RIO$", " DELRIO", inc$incumbent)
## inc$incumbent <- gsub(" DEL RIO ", " DELRIO ", inc$incumbent)
## inc$incumbent <- gsub(" DEL ", " ", inc$incumbent)
## inc$incumbent <- gsub(" Y ", " ", inc$incumbent)
## ## # debug
## ## tmp <- grep(" DE L[OA]S ", inc$incumbent)
## ## tmp <- 1:100
## ## inc$incumbent[tmp]
## 
## # load my name-searching function
## source("../code/search_names.r")
## 
## # will receive repeated names with whatever method chosen
## inc$drep <- 0
## # DROP#who.was.hit <- as.list(rep(NA,32))
## inc$who <- NA # useful to contrast exact and grep/fuzzy
## meth <- c("exact","grep","fuzzy")[1] # pick 1 2 or 3
## #
## # grep match within state alcaldes only (+ govs)
## for (i in 1:32){
##     #i <- 5 # debug
##     #message(sprintf("loop %s of %s", i, 32))
##     sel1 <- which(inc$edon %in% i);
##     sel2 <- which(inc$ddip==1);
##     sel3 <- which(inc$dgob==1);
##     sel <- intersect(sel1, sel2); # state's diputados
##     sel <- union(sel, sel1);      # state's alcaldes+diputados
##     sel <- union(sel, sel3);      # plus all governors
##     tmp1 <- search.names(
##         within.records = inc$incumbent[sel],
##         ids = inc$emm[sel],
##         method = meth
##     )
##     sh.hits <- tmp1$sh.hits # extract share hits matrix
##     sh.hits <- sh.hits * (1-diag(nrow(sh.hits))) # diag to 0
##     #sh.hits <- matrix(c(1,1,0,.5,0,0,1,0,0), nrow = 3, ncol = 3) # debug
##     exact.hits <- apply(X = sh.hits, 2, function(x) length(which(x==1)))
##     sel.col <- which(exact.hits>0)
##     sel.ids <- tmp1$ids[sel.col]
##     sel.names <- tmp1$names[sel.col]
##     sel.yrs <- inc$yr[sel][sel.col]
##     #
##     # get ids of the hits only to debug
##     tmp2 <- Map(function(x){y <- which(x==1); return(y);}, split(t(sh.hits), seq(nrow(sh.hits))))
##     hits.ids <- unlist(tmp2)
##     hits.ids <- tmp1$ids[hits.ids]
##     hits.ids <- relist(hits.ids, tmp2)
##     tmp.ss <- hits.ids[which(lapply(hits.ids, length)>1)]
##     tmp.ss <- sapply(tmp.ss, paste, collapse = " ", simplify = FALSE) # collapse multiple hits into single string
##     hits.ids[which(lapply(hits.ids, length)>1)] <- tmp.ss
##     hits.ids[which(lapply(hits.ids, length)==0)] <- "0" # fill empty with 0
##     #DROP#who.was.hit[[i]] <- hits.ids
##     inc$who[sel] <- unlist(hits.ids)
##     #
##     sh.hits <- tmp1$sh.hits # restore sh.hits
##     sh.hits.ss <- sh.hits[,sel.col]
##     # function to report indexes of exact hit elements
##     sh.hits.ss <- t(sh.hits.ss) # needed because lapply operates on rows and we need hits in each column
##     tmp3 <- Map(function(x){y <- which(x==1); return(y)}, split(sh.hits.ss, seq(nrow(sh.hits.ss))))
##     # DROP #tmp2 <- tmp2[lapply(tmp2, length)>0] # drop empty elements
##     names(tmp3) <- sel.ids
##     #
##     tmp.ids  <- Map(function(x) tmp1$ids[unlist(x)]   , tmp3)
##     #
##     tmp.names<- Map(function(x) tmp1$names[unlist(x)] , tmp3)
##     #
##     tmp.yrs  <- Map(function(x) inc$yr[sel][unlist(x)], tmp3)
##     #
##     # identifies min(year)'s index
##     tmp.drop <- Map(function(x) which(x==min(x)), tmp.yrs)
##     # drops indices of early years from id vectors 
##     tmp.ones <- Map(function(x, y) x[-y], tmp.ids, tmp.drop)
##     # turn into vector
##     tmp.ones <- unlist(tmp.ones)
##     #
##     # record hits in data
##     sel4 <- which(inc$emm %in% tmp.ones)
##     inc$drep[sel4] <- 1
## }
## # fix special cases by hand
## sel <- which(inc$emm=="mex-09.086")
## inc$drep[sel] <- 0 # homónimo en tenango del valle 1990
## sel <- which(inc$emm=="mex-16.083")
## inc$drep[sel] <- 0 # homónimo en chapa de mota
## sel <- which(inc$emm=="mex-14.119")
## inc$drep[sel] <- 0 # homónimo en sn felipe progreso
## sel <- which(inc$emm=="mex-14.051")
## inc$drep[sel] <- 0 # homónimo en tenango del valle 1990
## sel <- which(inc$emm=="mex-14.082")
## inc$drep[sel] <- 0 # homónimo grep en chicoloapan
## # fills-in the appropriate
## if (meth=="exact") inc$drepe <- inc$drep
## if (meth=="grep")  inc$drepg <- inc$drep
## if (meth=="fuzzy") inc$drepf <- inc$drep
## inc$drep <- NULL
## 
## # parece que sí jala!
## table(inc$drepe, inc$drepg)
## table(inc$drepe, inc$drepf) # fuzzy doesn't work
## 
## sel <- which(inc$drepe==0 & inc$drepg==1)
## inc$emm[sel]
## 
## tmp <- inc[,c("ord","drepe","drepg", "who")]
## head(tmp)
## tmp$who
## 
## write.csv(tmp, file = "tmp.csv")
## 
## # count number of words in names
## inc$words <- gsub("[^ ]", "", inc$incumbent); inc$words <- nchar(inc$words) + 1
## table(inc$words)
## # add column for incumbents with prvious elected municipal office
## inc$drep <- NA # will receive dummy = 1 if name repeated in other obs
## # sorts by decreasing words to process mem-intensive cases 1st, then drop to speed loops
## inc <- inc[order(-inc$words),]
## 
## # subset to explore name-searching performance
## sel <- which(inc$edon==14)
## inc.jal <- inc[sel,]
## inc.jal[1,]
## 
## # keep full version of inc in order to work w/o NAs (plug manipulation later)
## inc.full <- inc # duplicate
## inc <- inc.jal
## sel.full <- which(inc$incumbent!="" & inc$words>1)
## inc <- inc[sel.full,] # subset
## 
## # initialize for while
## work <- which(is.na(inc$drep)==TRUE) # will be updated to FALSE after a hit recorded
## ss <- inc[work,c("incumbent","drep")] # subset
## ss$hit <- NA
## tmp <- nrow(ss)
## #i <- 1
## 
## while (tmp > 1){
##     message(sprintf("%s loops, %s records left", i, tmp))
##     ss$hit[-1] <- search.names(find_name = ss$incumbent[1], within.records = ss$incumbent[-1])
##     sel <- which(ss$hit==TRUE)
##     if (length(sel)>0) {
##         ss$drep[1]   <- i # i allows finding where repeated name is
##         ss$drep[sel] <- i
##     } else {
##         ss$drep[1] <- 0
##     }
##     # return to data
##     inc$drep[work] <- ss$drep
##     # update for next loop
##     work <- which(is.na(inc$drep)==TRUE) # will be updated to FALSE after a hit recorded
##     ss <- inc[work,c("incumbent","drep")] # subset
##     ss$hit <- NA
##     tmp <- nrow(ss)
##     i <- i+1 # prepare for next hit
## }
## 
## getwd()
## save.image(file = "drep-cut1.RData")
## 
## rm(list = ls())
## dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
## wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data/"
## setwd(wd)
## load(file = "drep-cut1.RData")
## ls()
## 
## table(inc$drep)
## sel <- which(inc$drep==15)
## inc[sel,c("edon","mun","incumbent")]
## x
## 
## inafed$spcs <- gsub(pattern = "[^ ]", replacement = "", inafed$incumb, perl = TRUE) # keep spaces only
## inafed$spcs <- sapply(inafed$spcs, nchar) # count them
## 3 sort by spcs, export to csv
## 4 manually input commas to separate name, patronyn, matronym
## 
## head(inafed$spcs)
## head(inafed$incumb)
## 
## # fuzzy matching of names
## N <- nrow(inafed)
## tmp <- rep(0, N) #will receive dummy
## 
## hits <- agrep(pattern = inafed$incumb[1], x = inafed$incumb, ignore.case = TRUE)
## n.hits <- length(hits)
## names <- agrep(pattern = inafed$incumb[1], x = inafed$incumb, ignore.case = TRUE, value = TRUE)
## 
## apply(inafed$incumb[-1], function(x) = agrep(pattern = inafed$incumb[1], inafed$incumb[-1]))
## 
## agrep(pattern = "CUAUHTEMOC CALDERON GALVAN", x = "xxAUHTEMOC xxLDERON GALVAN", ignore.case = TRUE)
## agrep("lasy", "1 lazy 2", max = list(sub = 0))
## agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2)
## agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2, value = TRUE)
## agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2, ignore.case = TRUE)
## 
## ####################################
## ## REPEATED NAME SEARCH ENDS HERE ##
## ####################################
#
#########################################
## ################################### ##
## ##      ^         ^         ^    ## ##
## ##      |         |         |    ## ##
## ##      |         |         |    ## ##
## ## INCUMBENT DATA PREP ENDS HERE ## ##
## ##                               ## ##
## ################################### ##
#########################################




# restore: incumbent prep up to line 771 --- missing party-returned dummies, clean when final 2021 data arrives 
rm(list = ls())
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data/"
setwd(wd)
load(file = "tmp.RData")
ls()

###########################################################
## ##################################################### ##
## ##                                                 ## ##
## ## VOTING DATA AND ELEC HISTORIES PREP STARTS HERE ## ##
## ##         |         |         |         |         ## ##
## ##         |         |         |         |         ## ##
## ##         V         V         V         v         ## ##
## ##################################################### ##
###########################################################


vraw <- read.csv(file = paste0(dd, "aymu1989-present.coalSplit.csv"), stringsAsFactors = FALSE)
vraw[1,]
x

#
#############################
## GET ELECTORAL HISTORIES ##
#############################
vot$vhat.pan <- vot$vhat.pri  <- vot$vhat.left <- vot$alpha.pan  <- vot$alpha.pri <- vot$alpha.left <- NA # open slots
vot$d.pan <- vot$d.pri  <- vot$d.left <- NA # open slots
#
## # save a copy
## save.image(paste(wd,"tmp.RData",sep=""))

## # load image
## rm(list = ls())
## dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
## wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data/"
## setwd(dd)
## load(paste(wd,"tmp.RData",sep=""))
#
#
# 2006
his <- read.csv(paste(gd, "dipfed-municipio-vhat-2006.csv", sep = ""), stringsAsFactors = FALSE)
# drop unneeded columns
sel <- which(colnames(his) %in%
             c("edon","pan", "pri", "left", "efec",
#               "d.pan", "d.pri", "d.left",
               "bhat.pan", "bhat.left",
               "betahat.pan", "betahat.left"))
his <- his[,-sel]
colnames(his) <- sub("ahat","a",colnames(his)) # shorten alpha names
#
sel <- which(vot$yr==2005 | vot$yr==2006 | vot$yr==2007)
tmp <- vot[sel,]
tmp$vhat.pan <- tmp$vhat.pri <- tmp$vhat.left <- tmp$alpha.pan <- tmp$alpha.pri <- tmp$alpha.left <- tmp$d.pan <- tmp$d.pri <- tmp$d.left <- NULL # remove to merge them again
# check that merge ids are complete
table(is.na(his$ife), is.na(his$inegi))
table(is.na(tmp$ife), is.na(tmp$inegi))
# merge
tmp <- merge(x = tmp, y = his, by = c("inegi", "ife"), all.x = TRUE, all.y = FALSE, sort = FALSE)
tmp <- tmp[,colnames(vot)] # sort columns to match vot's before merging
vot[sel,] <- tmp # return to data
#
# 2009
his <- read.csv(paste(gd, "dipfed-municipio-vhat-2009.csv", sep = ""), stringsAsFactors = FALSE)
# drop unneeded columns
sel <- which(colnames(his) %in%
             c("edon","pan", "pri", "left", "efec",
#               "d.pan", "d.pri", "d.left",
               "bhat.pan", "bhat.left",
               "betahat.pan", "betahat.left"))
his <- his[,-sel]
colnames(his) <- sub("ahat","a",colnames(his)) # shorten alpa names
#
sel <- which(vot$yr==2008 | vot$yr==2009 | vot$yr==2010)
tmp <- vot[sel,]
tmp$vhat.pan <- tmp$vhat.pri <- tmp$vhat.left <- tmp$alpha.pan <- tmp$alpha.pri <- tmp$alpha.left <- tmp$d.pan <- tmp$d.pri <- tmp$d.left <- NULL # remove to merge them again
tmp <- merge(x = tmp, y = his, by = c("inegi", "ife"), all.x = TRUE, all.y = FALSE, sort = FALSE)
tmp <- tmp[,colnames(vot)] # sort columns to match vot's before merging
vot[sel,] <- tmp # return to data
#
# 2012
his <- read.csv(paste(gd, "dipfed-municipio-vhat-2012.csv", sep = ""), stringsAsFactors = FALSE)
# drop unneeded columns
sel <- which(colnames(his) %in%
             c("edon","pan", "pri", "left", "efec",
#               "d.pan", "d.pri", "d.left",
               "bhat.pan", "bhat.left",
               "betahat.pan", "betahat.left"))
his <- his[,-sel]
colnames(his) <- sub("ahat","a",colnames(his)) # shorten alpa names
#
sel <- which(vot$yr==2011 | vot$yr==2012 | vot$yr==2013)
tmp <- vot[sel,]
tmp$vhat.pan <- tmp$vhat.pri <- tmp$vhat.left <- tmp$alpha.pan <- tmp$alpha.pri <- tmp$alpha.left <- tmp$d.pan <- tmp$d.pri <- tmp$d.left <- NULL # remove to merge them again
tmp <- merge(x = tmp, y = his, by = c("inegi", "ife"), all.x = TRUE, all.y = FALSE, sort = FALSE)
tmp <- tmp[,colnames(vot)] # sort columns to match vot's before merging
vot[sel,] <- tmp # return to data
#
# 2015
his <- read.csv(paste(gd, "dipfed-municipio-vhat-2015.csv", sep = ""), stringsAsFactors = FALSE)
# drop unneeded columns
sel <- which(colnames(his) %in%
             c("edon","pan", "pri", "left", "efec",
#               "d.pan", "d.pri", "d.left",
               "bhat.pan", "bhat.left",
               "betahat.pan", "betahat.left"))
his <- his[,-sel]
colnames(his) <- sub("ahat","a",colnames(his)) # shorten alpa names
#
sel <- which(vot$yr==2014 | vot$yr==2015 | vot$yr==2016)
tmp <- vot[sel,]
tmp$vhat.pan <- tmp$vhat.pri <- tmp$vhat.left <- tmp$alpha.pan <- tmp$alpha.pri <- tmp$alpha.left <- tmp$d.pan <- tmp$d.pri <- tmp$d.left <- NULL # remove to merge them again
tmp <- merge(x = tmp, y = his, by = c("inegi", "ife"), all.x = TRUE, all.y = FALSE, sort = FALSE)
tmp <- tmp[,colnames(vot)] # sort columns to match vot's before merging
vot[sel,] <- tmp # return to data
#
# 2018
his <- read.csv(paste(gd, "dipfed-municipio-vhat-2018.csv", sep = ""), stringsAsFactors = FALSE)
# drop unneeded columns
sel <- which(colnames(his) %in%
             c("edon","pan", "pri", "left", "efec",
#               "d.pan", "d.pri", "d.left",
               "bhat.pan", "bhat.left",
               "betahat.pan", "betahat.left"))
his <- his[,-sel]
colnames(his) <- sub("ahat","a",colnames(his)) # shorten alpa names
#
sel <- which(vot$yr==2017 | vot$yr==2018 | vot$yr==2019)
tmp <- vot[sel,]
tmp$vhat.pan <- tmp$vhat.pri <- tmp$vhat.left <- tmp$alpha.pan <- tmp$alpha.pri <- tmp$alpha.left <- tmp$d.pan <- tmp$d.pri <- tmp$d.left <- NULL # remove to merge them again
tmp <- merge(x = tmp, y = his, by = c("inegi", "ife"), all.x = TRUE, all.y = FALSE, sort = FALSE)
tmp <- tmp[,colnames(vot)] # sort columns to match vot's before merging
vot[sel,] <- tmp # return to data
#
# clean
rm(tmp,sel,his)
#
# left residuals: use prd-vhat.left and morena-vhat.left (pan-prd went to pan in 2017 and 2018)
vot$res.pan <- vot$pan - vot$vhat.pan
vot$res.pri <- vot$pri - vot$vhat.pri
vot$res.prd <- vot$prd - vot$vhat.left
vot$res.morena <- vot$morena - vot$vhat.left
vot$res.oth <- (1 - vot$pan - vot$pri - vot$prd - vot$morena) - (1 - vot$vhat.pan - vot$vhat.pri - vot$vhat.left)
sel <- which(vot$yr<2015)
vot$res.morena[sel] <- NA

# inspect vot
vot[2000,]
dim(vot)
table(is.na(vot$vhat.pri), vot$yr)
# 3oct2020 no veo por qué faltan éstos... ando checho, revisar
sel <- which(is.na(vot$vhat.pri) & vot$yr==2007)
vot[sel,]
eric  x

#########################################################
## ################################################### ##
## ##       ^         ^         ^         ^         ## ##
## ##       |         |         |         |         ## ##
## ##       |         |         |         |         ## ##
## ## VOTING DATA AND ELEC HISTORIES PREP ENDS HERE ## ##
## ##                                               ## ##
## ################################################### ##
#########################################################

# change prd/morena for left
# new rule is as follows:
# all prd or morena mayors are left, regardless of year. Rationale: prd incumbents in 2018 ad 2019 who wanted to reelect could not jump to morena
inc$win.left <- inc$win # duplicate
sel <- grep("morena", inc$win)
inc$win.left[sel] <- "left"
sel <- grep("prd", inc$win)
inc$win.left[sel] <- "left"
## sel1 <- which(inc$yr[sel]<=2017)
## inc$win.left[sel][sel1] <- "left"
inc$win.prior.left <- inc$win.prior # duplicate
sel <- grep("morena", inc$win.prior)
inc$win.prior.left[sel] <- "left"
sel <- grep("prd", inc$win.prior)
inc$win.prior.left[sel] <- "left"
#
library(DataCombine) # easy lags with slide
inc <- inc[order(inc$emm),] # check sorted for lags
inc$cycle <- as.numeric(sub("^[a-z]?[a-z]{2}[-]([0-9]+)[.][0-9]{3}$", "\\1",inc$emm))
table(inc$cycle, useNA = "ifany")
inc <- slide(data = inc, TimeVar = "cycle", GroupVar = "ife", Var = "win.left", NewVar = "win.left.lead", slideBy = 1) # lead by one period
#
# recode race.after.left RULE: prd --> morena coded as p-won unless prd incumbent beaten by morena
inc$race.after.left <- inc$race.after
sel <- which(inc$win.left=="left" & inc$win.left.lead=="left" & inc$race.after=="Out-p-lost")
inc$race.after.left[sel] <- "Out-p-won"
sel <- which(inc$win.left=="left" & inc$win.left.lead=="left" & inc$race.after=="Term-limited-p-lost")
inc$race.after.left[sel] <- "Term-limited-p-won"
#
inc$dpwon.left <- inc$dpwon
sel <- which(inc$win.left=="left" & inc$win.left.lead=="left" & inc$dpty.same.as.last==0)
inc$dpwon.left[sel] <- 1
#
# Morena beat a prd incumbent: win.left into prd in order not to count those as left
sel <- which(inc$win=="prd" & inc$win.left.lead=="left" & inc$race.after=="Beaten") 
#inc$emm[sel]
inc$win.left[sel] <- "prd"
inc$dpwon.left[sel] <- 0
#
# now lag by one period
inc <- inc[order(inc$emm),] # check sorted for lags
inc <- slide(data = inc, TimeVar = "cycle", GroupVar = "ife", Var = "race.after.left", NewVar = "race.prior.left", slideBy = -1) # lag by one period
inc <- slide(data = inc, TimeVar = "cycle", GroupVar = "ife", Var = "dpwon.left", NewVar = "dpty.same.as.last.left", slideBy = -1) # lag by one period
table(inc$race.prior, inc$race.prior.left)
table(inc$dpty.same.as.last, inc$dpty.same.as.last.left)
## # debug
## colnames(inc)[grep("prior|w[io]n|after", colnames(inc))]
## table(inc$dpty.same.as.last, inc$dpty.same.as.last.left)
## getwd()
## write.csv(inc, file = "tmp.csv")

## # change prd/morena for left before/after 2015
## # old rule was as follows:
## # all morena mayors are left
## # all prd mayors up to 2017 are left
## inc$win.left <- inc$win # duplicate
## sel <- grep("morena", inc$win)
## inc$win.left[sel] <- "left"
## sel <- grep("prd", inc$win)
## sel1 <- which(inc$yr[sel]<=2017)
## inc$win.left[sel][sel1] <- "left"
#
# re-compute race.after with left recategorization; compute dincran.after, dincwon.after, and dptywon.after; lag for .current
#inc.dupli <- inc # duplicate for debug
#inc <- inc.dupli # restore
#
### OLD VERSION OF THIS VAR-GENERATION HAD PROBLEMS
# win.left, win.after.left, race.after.left (overestimates reelection cases, just like race.after misses some where perredista went to morena) 
#
# incumbent in ballot, incumbent won
inc$dincran.after <- 0
sel <- grep("Beaten|Reelected", inc$race.after) #sel <- which(inc$race.after=="Beaten" | inc$race.after=="Reelected")
inc$dincran.after[sel] <- 1
inc$dincwon.after <- 0
sel <- grep("Reelected", inc$race.after) #sel <- which(inc$race.after=="Reelected")
inc$dincwon.after[sel] <- 1
# party won
inc$dinptywon.after <- 0
sel <- grep("p-won|Reelected$",inc$race.after)
inc$dinptywon.after[sel] <- 1
inc$dinptywon.after.left <- inc$dinptywon.after
sel <- grep("p-won",inc$race.after.left)
inc$dinptywon.after.left[sel] <- 1
table(inc$dinptywon.after, inc$dinptywon.after.left)
#
# code dtermlim
inc$dtermlim <- 1
sel <- grep("^(?!Term-lim).*$", inc$race.after, perl = TRUE)
inc$dtermlim[sel] <- 0
#
# lag to get current versions
inc <- inc[order(inc$ife, inc$emm),] # verify sorted before lags
inc <- slide(inc, Var = "race.after",           NewVar = "race.current",           GroupVar = "ife", TimeVar = "cycle", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "race.after.left",      NewVar = "race.current.left",      GroupVar = "ife", TimeVar = "cycle", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "dincran.after",        NewVar = "dincran.current",        GroupVar = "ife", TimeVar = "cycle", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "dincwon.after",        NewVar = "dincwon.current",        GroupVar = "ife", TimeVar = "cycle", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "dinptywon.after",      NewVar = "dinptywon.current",      GroupVar = "ife", TimeVar = "cycle", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "dinptywon.after.left", NewVar = "dinptywon.current.left", GroupVar = "ife", TimeVar = "cycle", slideBy = -1) # lag by one period
#
# new municipalities, fill in dincran.current
sel <- which(inc$emm %in% c("gue-12.077", "gue-13.078", "gue-13.079", "gue-13.080", "gue-13.081", "jal-13.125", "mex-12.125", "qui-13.009", "qui-15.010", "qui-16.011", "zac-13.058", "cps-15.120", "cps-15.121", "cps-15.122", "cps-15.123", "cps-17.124", "cps-17.125"))
inc$dincran.current[sel] <- 0
#
# rename current vars
inc$win.current <- inc$win; inc$win <- NULL
inc$win.current.left <- inc$win.left; inc$win.left <- NULL
# drop name-searching ancillary
inc$note <- NULL
inc$drepe <- NULL
inc$drepg <- NULL
inc$who <- NULL
inc$check <- NULL
#
# drop original version of same variable
#table(inc$race.prior, inc$race.current)
inc$race.prior <- NULL      # it's not prior, its current
inc$race.prior.left <- NULL # it's not prior, its current
#table(inc$win.prior, inc$win.current)
inc$win.prior <- NULL      # it is prior, but not needed
inc$win.prior.left <- NULL # it is prior, but not needed
# inc$dptywon.current is original inc$dpty.same.as.last with less info, update
sel <- which(inc$dpty.same.as.last==1 & inc$dinptywon.current==0) # cases where win.long has the returning party
inc$dinptywon.current[sel] <- 1                             # cases where win.long has the returning party
sel <- which(inc$dpty.same.as.last==0 & inc$dinptywon.current==1) # cases where incumbent reelected with other pty
inc$dinptywon.current[sel] <- 0                             # cases where incumbent reelected with other pty
sel <- which(inc$dpty.same.as.last==0 & is.na(inc$dinptywon.current)==TRUE) # new municipalities
inc$dinptywon.current[sel] <- 0                                       # new municipalities
sel <- which(inc$dpty.same.as.last==1 & is.na(inc$dinptywon.current)==TRUE) # new municipalities
inc$dinptywon.current[sel] <- 1                                       # new municipalities
table(inc$dinptywon.current, inc$dpty.same.as.last, useNA = "always")
inc$dpty.same.as.last      <- NULL # drop redundant
inc$dpwon            <- NULL # drop redundant, it's dinptywon.after with more NAs and some inconsistencies
inc$dpwon.left       <- NULL
inc$dpty.same.as.last.left <- NULL
# remaining NAs are all start-of-series in early 1990s
sel <- which(is.na(inc$dinptywon.current)==TRUE) # 
table(sub("^[a-z]+-([0-9]{2})[.0-9]+$", "\\1", inc$emm[sel])) # cycle they occur in
#
#drop after versions, unneeded for analysis of current period
##inc$race.after           <- NULL 
##inc$race.after.left      <- NULL
inc$dinptywon.after.left <- NULL
inc$dincran.after        <- NULL
inc$dincwon.after        <- NULL
inc$dinptywon.after      <- NULL
inc$win.after            <- NULL
inc$win.after.left       <- NULL
inc$race.after.left      <- NULL
inc$win.left.lead <- NULL
#
# sort columns 
sel <- c("emm","ord","mun","yr","dextra","dy","mo","cycle","edon","munn","ife", "inegi", "incumbent","race.after","mg","pty2nd","runnerup", "win.long", "returning.p", "dtermlim", "win.current", "win.current.left", "race.current", "race.current.left", "dinptywon.current", "dinptywon.current.left", "dincran.current", "dincwon.current")
inc <- inc[,sel]
#
# plug incumbent data to vot
inc$round <- inc$cycle
inc$cycle <- NULL
#
# paste incumbent data into vot
vot.dup <- vot # duplicate for debug
#sel.c <- which(colnames(inc) %in% c("emm", "race.current", "dopenseat", "dptyreel", "dtermlim", "win"))
#vot <- merge(x = vot, y = inc[,sel.c], by = "emm", all.x = TRUE, all.y = FALSE)
sel.c <- which(colnames(inc) %in% c("emm", "race.after","race.current", "race.current.left", "dincran.current", "dinptywon.current", "dtermlim", "win.current", "win.current.left"))
vot <- merge(x = vot, y = inc[, sel.c], by = "emm", all.x = TRUE, all.y = FALSE)
#
## # verify that win in vot (win) and in inc (win.current) have no inconsistencies
table(vot$win, vot$win.current)
## sel <- which(vot$win.current=="indep" & vot$win=="pt-morena-pes")
## vot[sel,]
#
# keep inc's version only
vot <- within(vot, {win <- win.current; win.current <- NULL; win.left <- win.current.left; win.current.left <- NULL})
#
# clean
rm(sel,sel1,sel2,sel.c,tmp)
#
#
#################################
## concurrent election dummies ##
#################################
# get calendario
cal <- read.csv("../../calendariosReelec/fechasEleccionesMexicoDesde1994.csv", stringsAsFactors = FALSE) # fechasEleccionesMexicoDesde1994 is csv-friendly version of calendarioConcurrenciasMex05
#cal[1,]
sel <- grep("^ord$|^edon$|^elec$|y[0-9]{4}", colnames(cal), perl = TRUE) # drop useless variables
cal <- cal[,sel]
sel <- which(cal$elec=="dip") # keep ayun lines only
cal.fed <- cal[sel,]
# duplicate rows 5 times to get 32 obs
cal.fed <- rbind(cal.fed, cal.fed); cal.fed <- rbind(cal.fed, cal.fed); cal.fed <- rbind(cal.fed, cal.fed); cal.fed <- rbind(cal.fed, cal.fed); cal.fed <- rbind(cal.fed, cal.fed)
sel <- which(cal$elec=="ayun") # keep ayun lines only
cal.ay <- cal[sel,]
sel <- which(cal$elec=="gob") # keep ayun lines only
cal.gob <- cal[sel,]
# ay concurs with fed
conc.fed <- cal.ay
sel.c <- grep("y[0-9]{4}", colnames(cal.ay), perl = TRUE) # select year columns only
for (i in 1:32){
    #i <- 1
    tmp <- as.numeric(cal.ay[i,sel.c]==cal.fed[i,sel.c])
    conc.fed[i, sel.c] <- tmp
}
#
conc.fed[cal.ay=="--"] <- 0
# ay concurs with gob
conc.gob <- cal.ay
for (i in 1:32){
    #i <- 1
    tmp <- as.numeric(cal.ay[i,sel.c]==cal.gob[i,sel.c])
    conc.gob[i, sel.c] <- tmp
}
conc.gob[cal.ay=="--"] <- 0
# dummify
tmp <- t(conc.gob[,sel.c])
tmp <- apply(tmp, c(1,2), FUN = function(x){as.numeric(x)})
conc.gob <- tmp
tmp <- t(conc.fed[,sel.c])
tmp <- apply(tmp, c(1,2), FUN = function(x){as.numeric(x)})
conc.fed <- tmp
#
# plug into vot
#
vot$dconcgob <- vot$dconcfed <- NA
for (e in 1:32){ # loop across states
    message(sprintf("loop %s", e))
    #e <- 21
    sel.e <- which(vot$edon==e) # state e's indices in vot
    vot.e <- vot[sel.e,]        # subset state e in vot
    for (y in 1994:2023){ # loop over years up to 2023
        #y <- 2018
        sel.y <- which(as.numeric(sub("y", "", names(conc.gob[,e])))==y) # index for year y
        sel.ey <- which(vot.e$yr==y) # subset state's obs in year y
        if (length(sel.ey)>0){
            vot.e$dconcgob[sel.ey] <- conc.gob[sel.y,e]
            vot.e$dconcfed[sel.ey] <- conc.fed[sel.y,e]
        }
    }
    vot[sel.e,] <- vot.e # return manipulated subset to vot
}
# clean
rm(cal,cal.ay,cal.fed,cal.gob,conc.fed,conc.gob,e,i,sel,sel.c,sel.e,sel.ey,sel.y,tmp,vot.e,y)
#
# get governor parties
tmp <- "../../mxBranches/statesBranches/32stategovts.csv"
gob <- read.csv(file = tmp, stringsAsFactors = FALSE)
gob <- gob[, c("edon","yr","yrin","dnewgov","govterm","govpty")]
# fill empty yrins looking at last obs
gob <- gob[order(gob$edon, gob$yr),] # sort to fill NAs
for (i in 2:length(gob$yrin)){
    if (gob$edon[i]==gob$edon[i-1] & is.na(gob$yrin[i])==TRUE) gob$yrin[i] <- gob$yrin[i-1]
}
# lag govpty one year (dnewgov==1 for yrs with new gov, even if sworn dec31)
library(DataCombine) # easy lags with slide
gob <- slide(data = gob, TimeVar = "yr", GroupVar = "edon", Var = "govpty", NewVar = "govpty.lag",    slideBy = -1) # lag by one period
#
# plug into vot
vot$govpty.lag <- NA
for (e in 1:32){ # loop across states
    message(sprintf("loop %s", e))
    #e <- 17
    sel.e <- which(vot$edon==e) # state e's indices in vot
    vot.e <- vot[sel.e,]        # subset state e in vot
    for (y in 1994:2023){ # loop over years up to 2023
        #y <- 2006
        sel.y <- which(gob$edon==e & gob$yr==y) # index for year y
        sel.ey <- which(vot.e$yr==y) # subset state's obs in year y
        if (length(sel.ey)>0){
            vot.e$govpty.lag[sel.ey] <- gob$govpty.lag[sel.y]
        }
    }
    vot[sel.e,] <- vot.e # return manipulated subset to vot
}
# clean
rm(e,gob,i,sel.e,sel.ey,sel.y,tmp,vot.e,y)
#
# get municipio altitude variance (from censo 2010 @ loc level)
library(foreign)
tmp <- read.dbf("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/2010censo/ITER_NALDBF10.dbf", as.is = TRUE)
#table(tmp$X.1)
# selected columns only
sel <- grep("entidad|mun|loc|longitud|latitud|altitud|pobtot", colnames(tmp), ignore.case = TRUE, perl = TRUE)
tmp <- tmp[,sel]
# rename variables
colnames(tmp) <- c("edon","inegi","mun","locn","localidad","long","lat","alt","ptot","drop")
tmp$drop <- NULL
tmp$edon <- as.numeric(as.character(tmp$edon))
tmp$inegi <- as.numeric(as.character(tmp$inegi))
tmp$mun <- as.character(tmp$mun)
tmp$locn <- as.numeric(as.character(tmp$locn))
tmp$localidad <- as.character(tmp$localidad)
tmp$long <- as.numeric(as.character(tmp$long))
tmp$lat <- as.numeric(as.character(tmp$lat))
tmp$alt <- as.numeric(as.character(tmp$alt))
tmp$ptot <- as.numeric(as.character(tmp$ptot))
# drop aggregate rows
sel <- which(tmp$locn==0|tmp$locn==9998|tmp$locn==9999)
tmp <- tmp[-sel,]
# add edon to inegi
tmp$inegi <- tmp$edon*1000 + tmp$inegi
# mun pop share
tmp$tmp      <- ave(tmp$ptot, as.factor(tmp$inegi), FUN=sum, na.rm=TRUE)
tmp$popsh    <- tmp$ptot / tmp$tmp
# weighted mean(alt) and sd(alt)
tmp$altpopsh <- tmp$popsh * tmp$alt 
tmp$wmeanalt <- ave(tmp$altpopsh, as.factor(tmp$inegi), FUN=sum, na.rm=TRUE)
tmp$altpopsh <- tmp$popsh * (tmp$alt - tmp$wmeanalt)^2
tmp$wsdalt <-   ave(tmp$altpopsh, as.factor(tmp$inegi), FUN=sum, na.rm=TRUE)
tmp$wsdalt <-   sqrt(tmp$wsdalt)
tmp$altpopsh <- NULL # clean
# mean(alt) and sd(alt)
tmp$meanalt <- ave(tmp$alt, as.factor(tmp$inegi), FUN=mean, na.rm=TRUE)
tmp$sdalt <-   ave(tmp$alt, as.factor(tmp$inegi), FUN=sd,   na.rm=TRUE)
# cases with single localidad sd=NA
sel <- which(is.na(tmp$sdalt)==TRUE & is.na(tmp$meanalt)==FALSE)
tmp$wsdalt[sel] <- 0
tmp$sdalt[sel] <- 0
# drop redundant rows cols
tmp <- tmp[-duplicated(as.factor(tmp$inegi))==FALSE,]
tmp$locn <- tmp$localidad <- tmp$alt <- tmp$long <- tmp$lat <- tmp$mun <- tmp$edon <- tmp$popsh <- tmp$tmp <- NULL
tmp$wmeanalt <- round(tmp$wmeanalt, 1)
tmp$wsdalt <- round(tmp$wsdalt, 1)
tmp$meanalt <- round(tmp$meanalt, 1)
tmp$sdalt <- round(tmp$sdalt, 1)
#summary(tmp$sdalt)
#summary(tmp$wsdalt)
#
# make discrete altitude variables for mapping exploration
alt <- tmp
rm(tmp,sel)
#
# read sección-municipio equivalencias
tmp <- read.csv("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", stringsAsFactors = FALSE)
tmp <- tmp[,grep("edon|seccion|inegi|ife|mun[0-9]+",colnames(tmp))] # select columns
#tmp[1,]
#tmp[tmp$edon==1 & tmp$inegi==1010,c("seccion","munn")]
censo <- tmp # rename, will receive state-by-state
#
# get censo 2010 ptot p5li etc
edos <- c("ags","bc","bcs","cam","coa","col","cps","cua","df","dgo","gua","gue","hgo","jal","mex","mic",
          "mor","nay","nl","oax","pue","que","qui","san","sin","son","tab","tam","tla","ver","yuc","zac")
tmp.dat <- data.frame() # will receive state's rows
for (i in 1:32){
    #i <- 1 # debug
    tmp.dir <- paste("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/secciones/eceg_2010", edos[i], sep = "/")
    tmp.file <- grep("secciones.+csv", dir(tmp.dir))
    tmp.file <- dir(tmp.dir)[grep("secciones.+csv", dir(tmp.dir))]
    tmp <- read.csv(paste(tmp.dir, tmp.file, sep = "/"))
    sel <- grep("clavegeo|entidad|pobtot|pcatolica|sin_relig|pder|psinder|vivtot|c_elec|drenaj|agua|p5_hli$|p_5ymas$", colnames(tmp), ignore.case = TRUE, perl = TRUE)
    #colnames(tmp)[sel] # debug
    tmp <- tmp[,sel]
    tmp$seccion <- tmp$CLAVEGEO - as.integer(tmp$CLAVEGEO/10000)*10000
    tmp$edon <- tmp$ENTIDAD; tmp$ENTIDAD <- NULL; tmp$CLAVEGEO <- NULL
    # sort columns
    tmp <- tmp[, c("edon", "seccion", "POBTOT", "P_5YMAS", "P5_HLI", "PSINDER", "PDER_SS", "PDER_IMSS", "PDER_ISTE", "PDER_ISTEE", "PDER_SEGP", "PCATOLICA", "PSIN_RELIG", "VIVTOT", "VPH_AGUAFV", "VPH_AGUADV", "VPH_C_ELEC", "VPH_DRENAJ")]
    # add rows merge to main object that will merge to censo for mun aggregations
    #tmp[1,]
    tmp.dat <- rbind(tmp.dat, tmp)
}
# merge to censo
dim(censo)
dim(tmp.dat)
censo <- merge(x = censo, y = tmp.dat, by = c("edon","seccion"), all = TRUE)
#table(censo$POBTOT, useNA = "always")
#censo[which(is.na(censo$POBTOT))[2],]
#x
# change NAs to zero
sel <- which(colnames(censo) %in% c("POBTOT", "P_5YMAS", "P5_HLI", "PSINDER", "PDER_SS", "PDER_IMSS", "PDER_ISTE", "PDER_ISTEE", "PDER_SEGP", "PCATOLICA", "PSIN_RELIG", "VIVTOT", "VPH_AGUAFV", "VPH_AGUADV", "VPH_C_ELEC", "VPH_DRENAJ"))
tmp <- censo[,sel]
tmp[is.na(tmp)] <- 0
censo[,sel] <- tmp
# duplicate to fix new municipios
censo.sec <- censo
#censo <- censo.sec # restore
# aggregate municipios
censo$POBTOT     <- ave(censo$POBTOT    , as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$P_5YMAS    <- ave(censo$P_5YMAS   , as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$P5_HLI     <- ave(censo$P5_HLI    , as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$PSINDER    <- ave(censo$PSINDER   , as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$PDER_SS    <- ave(censo$PDER_SS   , as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$PDER_IMSS  <- ave(censo$PDER_IMSS , as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$PDER_ISTE  <- ave(censo$PDER_ISTE , as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$PDER_ISTEE <- ave(censo$PDER_ISTEE, as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$PDER_SEGP  <- ave(censo$PDER_SEGP , as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$PCATOLICA  <- ave(censo$PCATOLICA , as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$PSIN_RELIG <- ave(censo$PSIN_RELIG, as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$VIVTOT     <- ave(censo$VIVTOT    , as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$VPH_AGUAFV <- ave(censo$VPH_AGUAFV, as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$VPH_AGUADV <- ave(censo$VPH_AGUADV, as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$VPH_C_ELEC <- ave(censo$VPH_C_ELEC, as.factor(censo$ife), FUN=sum, na.rm=TRUE)
censo$VPH_DRENAJ <- ave(censo$VPH_DRENAJ, as.factor(censo$ife), FUN=sum, na.rm=TRUE)
# drop redundant lines cols
censo <- censo[duplicated(censo$ife)==FALSE,]
censo <- censo[, -grep("ife[0-9]", colnames(censo))]
#
###########################################################################################
## go back to censo.sec and fix new municipios and their parents in exact year happened  ##
## at present, seccion-mun map taken from 2018 and projected backwards                   ##
## --> take code from redistrict/code/elec-data-for-maps.r to achieve exact year mapping ##
## --> steps for 2015                                                                    ##
## --> (1) change censo.sec$ife  <- censo.sec$mun2015                                    ##
## --> (2) subset parent.children secciones with target.ife                              ##
## --> (3) aggregate                                                                     ##
## --> (4) paste manipulation in censo                                                   ##
###########################################################################################
#          
# create census variables
censo$ptot <- censo$POBTOT
censo$p5li <- censo$P5_HLI / censo$P_5YMAS
censo$religoth  <- (censo$ptot - censo$PCATOLICA - censo$PSIN_RELIG) / censo$ptot
censo$relignone <-                                 censo$PSIN_RELIG  / censo$ptot
censo$segpop    <-  censo$PDER_SEGP / censo$ptot
censo$imss      <- censo$PDER_IMSS / censo$ptot
censo$issste    <- (censo$PDER_ISTE + censo$PDER_ISTEE) / censo$ptot # proxy bureaucrats
censo$uninsured <- censo$PSINDER   / censo$ptot # no he usado derechohabientes imss
censo$water     <- (censo$VPH_AGUAFV + censo$VPH_AGUADV) / censo$VIVTOT
censo$electric  <- censo$VPH_C_ELEC / censo$VIVTOT
censo$sewage    <- censo$VPH_DRENAJ / censo$VIVTOT
# round 3 digits
censo <- within(censo, {
    p5li      <- round(p5li, 3);     
    religoth  <- round(religoth, 3); 
    relignone <- round(relignone, 3);
    segpop    <- round(segpop, 3);   
    imss      <- round(imss, 3);     
    issste    <- round(issste, 3);   
    uninsured <- round(uninsured, 3);
    water     <- round(water, 3);    
    electric  <- round(electric, 3); 
    sewage    <- round(sewage, 3);
})
# clean
censo <- within(censo, POBTOT <- P_5YMAS <- P5_HLI <- PSINDER <- PDER_SS <- PDER_IMSS <- PDER_ISTE <- PDER_ISTEE <- PDER_SEGP <- PCATOLICA <- PSIN_RELIG <- VIVTOT <- VPH_AGUAFV <- VPH_AGUADV <- VPH_C_ELEC <- VPH_DRENAJ  <- NULL)
censo$seccion <- NULL
#
# merge altitudes
dim(censo)
dim(alt)
alt$ptot <- NULL
#
#tmp <- censo # duplicate for debug
censo <- merge(x = censo, y = alt, by = "inegi", all = TRUE)
# clean
rm(alt)
#
# make discrete altitude variables for mapping exploration
# script mapa-municipios.r draws wsd(alt) etc
#
# merge censo into vot
sel <- which(colnames(censo) %in% c("ife","edon")) # drop towards merge
vot <- merge(x = vot, y = censo[,-sel], by = "inegi", all.x = TRUE, all.y = FALSE)
rm(censo, censo.sec, i, sel, tmp, tmp.dat, tmp.file, tmp.dir)
#
# compute winner's margin
vot$mg <- round(vot$v01 - vot$v02, 4)
vot$round <- sub(pattern = "[\\w\\D-]+([0-9]{2})[.][0-9]{3}", replacement = "\\1", vot$emm, perl = TRUE)
vot$round <- as.numeric(vot$round)
#
# capital municipalities
sel <- which(vot$ife  %in%  c( 1001,
                               2002,
                               3003,
                               4001,
                               5030,
                               6001,
                               7102,
                               8019,
#                               9015, # makes Cuauhtémoc the fake capital of DF  
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
#
# single-term states after reform
vot$dhgover <- as.numeric(vot$edon==13 | vot$edon==30)
# pre-reform dummy
vot$dpreref  <- as.numeric(vot$yr< 2018)
vot$dpostref <- as.numeric(vot$yr>=2018)
#
# left vote is prd pre-2015, morena+prd 2015:17, morena since 2018
vot <- within(vot, {
    left = prd + morena
    left[yr>=2018] = morena[yr>=2018]
    res.left = left - vhat.left # re-compute residuals w manip left 
})
#
#
# save a copy
save.image(paste(wd,"mun-reelection.RData",sep=""))

# load image
rm(list = ls())
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data/"
setwd(dd)
#
load(paste(wd,"mun-reelection.RData",sep=""))
rm(vot.dup)
rm(inc) # drop to avoid confusion, useful data has been merged into vot
#
# elevation variance
vot$varalt <- vot$sdalt^2 /1000
vot$wvaralt <- vot$wsdalt^2 /1000
vot$wmeanalt2 <- vot$wmeanalt^2
vot$logptot <- log(vot$ptot)
#
## # inspect eric  x
## sel <- which(vot$yr>2005 & vot$d)
## colnames(vot)
## plot(vot$left[sel])
## his <- read.csv(paste(gd, "dipfed-municipio-vhat-2018.csv", sep = ""), stringsAsFactors = FALSE)
## plot(his$pan-his$vhat.pan)
## plot(his$pri-his$vhat.pri)
## plot(his$left-his$vhat.left)
## x
#
# get state-level gob elections (will need to replace with mu-level when data available)
# script reads data, cleans, and aggregates coalitions, returning object dat
source("/home/eric/Desktop/MXelsCalendGovt/elecReturns/code/go.r")
#
# subset post 2004
sel <- which(dat$yr>2004)
dat <- dat[sel,]
#
# conve to mc
sel <- grep("l01", colnames(dat)); dat[,sel] <- sub("conver?", "mc", dat[,sel])
sel <- grep("l02", colnames(dat)); dat[,sel] <- sub("conver?", "mc", dat[,sel])
sel <- grep("l03", colnames(dat)); dat[,sel] <- sub("conver?", "mc", dat[,sel])
sel <- grep("l04", colnames(dat)); dat[,sel] <- sub("conver?", "mc", dat[,sel])
sel <- grep("l05", colnames(dat)); dat[,sel] <- sub("conver?", "mc", dat[,sel])
sel <- grep("l06", colnames(dat)); dat[,sel] <- sub("conver?", "mc", dat[,sel])
sel <- grep("l07", colnames(dat)); dat[,sel] <- sub("conver?", "mc", dat[,sel])
sel <- grep("l08", colnames(dat)); dat[,sel] <- sub("conver?", "mc", dat[,sel])
sel <- grep("l09", colnames(dat)); dat[,sel] <- sub("conver?", "mc", dat[,sel])
sel <- grep("l10", colnames(dat)); dat[,sel] <- sub("conver?", "mc", dat[,sel])
sel <- grep("l11", colnames(dat)); dat[,sel] <- sub("conver?", "mc", dat[,sel])
#
# prepare object with pan pri left morena oth votes
v5 <- dat # duplicate
#sel <- grep("^[vl][0-9]{2}", colnames(v5))
#v5 <- v5[,sel] # keep @otes and labels only #
v5 <- within(v5, ord <- mo <- dy <- edo <- win <- ncand <- dcoal <- ncoal <- efec <- lisnom <- imputacion <- distpan <- distpri <- distprd <- seyr <- semo <- sepan <- sepri <- seprd <- seefec <- NULL) # drop cols
v5$status <- NA
# narrow v01..v14 into long vector
v5$n <- 1:nrow(v5) # obs no
v5$r <- 1          # round
v5$v <- v5$v01     # @ote
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
## sel <- grep("(?=.*pan)(?=.*pri)", v5$l, perl = TRUE)
## v5$status[sel] <- "majors"
## sel <- grep("(?=.*pri)(?=.*prd)", v5$l, perl = TRUE)
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
# pan-prd to pan (bc dgo gua nay pue san sin son tam yuc)
sel <- which(v5$status=="majors" & (v5$edon==2 | v5$edon==10 | v5$edon==11 | v5$edon==18 | v5$edon==21 |  v5$edon==25 | v5$edon==26 | v5$edon==28  | v5$edon==31))
v5$pan[sel] <- v5$v[sel]; v5$v[sel] <- 0; v5$l[sel] <- "0"; v5$status[sel] <- "done"
v5$dmajcoal[sel] <- 1
#
# pan-prd split halfway (hgo ver)
sel <- which(v5$status=="majors" & (v5$edon==13  | v5$edon==30))
v5$pan[sel] <- v5$v[sel] / 2; 
v5$prd[sel] <- v5$v[sel] / 2; v5$v[sel] <- 0; v5$l[sel] <- "0"; v5$status[sel] <- "done"
v5$dmajcoal[sel] <- 1
#
# pan-prd to prd (cps df oax qui tab zac)
sel <- which(v5$status=="majors" & (v5$edon==7 | v5$edon==9 | v5$edon==20 | v5$edon==23 | v5$edon==27 | v5$edon==32))
v5$prd[sel] <- v5$v[sel]; v5$v[sel] <- 0; v5$l[sel] <- "0"; v5$status[sel] <- "done"
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
table(v5$v, v5$l, useNA = "always")
#
#
# clean, data in pan pri morena prd oth
v5$n <- v5$r <- v5$v <- v5$l <- v5$status <- NULL
rm(tmp,tmp2)
# return to dat
dat <- cbind(dat, v5[,c("pan","pri","prd","morena","oth","dmajcoal")])
# keep 123 places, drop rest
dat <- within(dat, v04 <- v05 <- v06 <- v07 <- v08 <- v09 <- v10 <- v11 <- v12 <- v13 <- v14 <- NULL)
dat <- within(dat, l04 <- l05 <- l06 <- l07 <- l08 <- l09 <- l10 <- l11 <- l12 <- l13 <- l14 <- NULL)
# inspect
dat[1,]
#
# clean
rm(i,sel,sel1,v5)
#
###################
## END IMPORT GO ##
###################
#
# merge go into vot here
dat$edoyr <- dat$edon*10000+dat$yr
vot$edoyr <- vot$edon*10000+vot$yr
#
vot <- within(vot, gopan <- gopri <- goprd <- gomorena <- goefec <- 0) # will receive dat
#
for (ey in unique(dat$edoyr)){
    #ey <- 72018 # debug
    sel <- which(vot$edoyr==ey)
    if (length(sel)==0) next
    vot$gopan   [sel] <- dat$pan   [dat$edoyr==ey]
    vot$gopri   [sel] <- dat$pri   [dat$edoyr==ey]
    vot$goprd   [sel] <- dat$prd   [dat$edoyr==ey]
    vot$gomorena[sel] <- dat$morena[dat$edoyr==ey]
    vot$goefec  [sel] <- dat$efec  [dat$edoyr==ey]
}
# shares
vot$goefec <- vot$goefec + 1 # avoid indeterminacy
vot <- within(vot, gopan    <- gopan    / goefec)
vot <- within(vot, gopri    <- gopri    / goefec)
vot <- within(vot, goprd    <- goprd    / goefec)
vot <- within(vot, gomorena <- gomorena / goefec)
vot <- within(vot, gooth    <- 1 - gopan - gopri - goprd - gomorena)
vot$goleft <- vot$goprd
vot <- within(vot, goleft[yr>2015] <- gomorena[yr>2015])
#vot[1,] # debug
# clean
rm(dat)
vot$edoyr <- vot$goefec <- NULL
#
#####################################################################
## get municipio-level pres elections to interact with concurrence ##
#####################################################################
pr06 <- read.csv("/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/municipios/pre2006.csv", stringsAsFactors = FALSE)
pr12 <- read.csv("/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/municipios/pre2012.csv", stringsAsFactors = FALSE)
pr18 <- read.csv("/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/municipios/pre2018.csv", stringsAsFactors = FALSE)
pr06 <- within(pr06, {
    efec <- fch + rmp + amlo + pna + asdc # recompute efec
    fch <- round(fch / efec, 3) # shares
    rmp  <- round(rmp / efec, 3)
    amlo <- round(amlo / efec, 3)
    pna <- round(pna / efec, 3)
    asdc <- round(asdc / efec, 3)
    oth <- pna + asdc # particize
    prd <- amlo
    pri <- rmp
    pan <- fch
    fch <- rmp <- amlo <- pna <- asdc <- efec <- lisnom <- NULL # clean
    left <- prd # duplicate to import when left is used
})
pr12 <- within(pr12, {
    efec <- pena + jvm + amlo + pna # recompute efec
    pena <- round(pena / efec, 3) # shares
    jvm  <- round(jvm / efec, 3)
    amlo <- round(amlo / efec, 3)
    pna <- round(pna / efec, 3)
    oth <- pna # particize
    prd <- amlo
    pri <- pena
    pan <- jvm
    jvm <- pena <- amlo <- pna <- mun <- efec <- lisnom <- NULL # clean
    left <- prd # duplicate to import when left is used
})
pr18 <- within(pr18, {
    efec <- rac + jam + amlo + bronco # recompute efec
    rac <- round(rac / efec, 3) # shares
    jam  <- round(jam / efec, 3)
    amlo <- round(amlo / efec, 3)
    bronco <- round(bronco / efec, 3)
    oth <- bronco # particize
    morena <- amlo
    pri <- jam
    pan <- rac
    rac <- jam <- amlo <- bronco <- efec <- lisnom <- NULL # clean
    left <- morena # duplicate to import when left is used
})
#
# merge pr into vot here
pr06 <- within(pr06, ifeyr <- ife*10000 + 2006)
pr12 <- within(pr12, ifeyr <- ife*10000 + 2012)
pr18 <- within(pr18, ifeyr <- ife*10000 + 2018)
vot$ifeyr <- vot$ife*10000+vot$yr
#
vot <- within(vot, prleft <- prmorena <- prprd <- prpri <- prpan <- 0) # will receive dat
#
for (ey in unique(pr06$ifeyr)){
    #ey <- 220012006 # debug
    sel <- which(vot$ifeyr==ey)
    sel2 <- which(pr06$ifeyr==ey)
    if (length(sel)==0) next
    vot$prpan   [sel] <- pr06$pan   [sel2]
    vot$prpri   [sel] <- pr06$pri   [sel2]
    vot$prprd   [sel] <- pr06$prd   [sel2]
    vot$prleft  [sel] <- pr06$left  [sel2]
}
#
for (ey in unique(pr12$ifeyr)){
    #ey <- 220012012 # debug
    sel <- which(vot$ifeyr==ey)
    sel2 <- which(pr12$ifeyr==ey)
    if (length(sel)==0) next
    vot$prpan   [sel] <- pr12$pan   [sel2]
    vot$prpri   [sel] <- pr12$pri   [sel2]
    vot$prprd   [sel] <- pr12$prd   [sel2]
    vot$prleft  [sel] <- pr12$left  [sel2]
}
#
for (ey in unique(pr18$ifeyr)){
    #ey <- 220012018 # debug
    sel <- which(vot$ifeyr==ey)
    sel2 <- which(pr18$ifeyr==ey)
    if (length(sel)==0) next
    vot$prpan   [sel] <- pr18$pan   [sel2]
    vot$prpri   [sel] <- pr18$pri   [sel2]
    vot$prmorena[sel] <- pr18$morena[sel2]
    vot$prleft  [sel] <- pr18$left  [sel2]
}
############################################
## End get municipio-level pres elections ##
############################################
#
#######################
## dummy pty had won ##
#######################
tmp <- data.frame(ife=vot$ife, yr=vot$yr, win=vot$win, dpanpast=0, dpripast=0, dprdpast=0, dmorenapast=0, dleftpast=0, dothpast=0, stringsAsFactors = FALSE)
tmp$ord <- 1:nrow(tmp)
tmp <- tmp[order(tmp$ife,tmp$yr),] # sort
tmp <- slide(data = tmp, Var = "win", TimeVar = "yr", GroupVar = "ife", NewVar = "win1", slideBy = -1, keepInvalid = TRUE) 
tmp <- slide(data = tmp, Var = "win", TimeVar = "yr", GroupVar = "ife", NewVar = "win2", slideBy = -2, keepInvalid = TRUE) 
tmp <- slide(data = tmp, Var = "win", TimeVar = "yr", GroupVar = "ife", NewVar = "win3", slideBy = -3, keepInvalid = TRUE) 
tmp <- slide(data = tmp, Var = "win", TimeVar = "yr", GroupVar = "ife", NewVar = "win4", slideBy = -4, keepInvalid = TRUE) 
tmp <- slide(data = tmp, Var = "win", TimeVar = "yr", GroupVar = "ife", NewVar = "win5", slideBy = -5, keepInvalid = TRUE) 
tmp <- slide(data = tmp, Var = "win", TimeVar = "yr", GroupVar = "ife", NewVar = "win6", slideBy = -6, keepInvalid = TRUE) 
tmp <- slide(data = tmp, Var = "win", TimeVar = "yr", GroupVar = "ife", NewVar = "win7", slideBy = -7, keepInvalid = TRUE) 
tmp <- slide(data = tmp, Var = "win", TimeVar = "yr", GroupVar = "ife", NewVar = "win8", slideBy = -8, keepInvalid = TRUE) 
#tmp <- slide(data = tmp, Var = "win", TimeVar = "yr", GroupVar = "ife", NewVar = "win8", slideBy = -9, keepInvalid = TRUE) 
# fill NAs
tmp[is.na(tmp)] <- 0
tmp[100:110,]
#
# get past winners
sel <- grep("win[0-9]", colnames(tmp))
for (i in 1:nrow(tmp)){
    #i <- 10 # debug
    tmp1 <- unique(c(tmp$win1[i],tmp$win2[i],tmp$win3[i],tmp$win4[i],tmp$win5[i],tmp$win6[i],tmp$win7[i],tmp$win8[i]) )
    if (length(grep("pan", unique(tmp1)))>0) tmp$dpanpast[i] <- 1
    if (length(grep("pri", unique(tmp1)))>0) tmp$dpripast[i] <- 1
    if (length(grep("prd", unique(tmp1)))>0) tmp$dprdpast[i] <- 1
    if (length(grep("morena", unique(tmp1)))>0) tmp$dmorenapast[i] <- 1
    if (length(grep("indep|loc/oth|mc|pes|pna|pt|pvem", unique(tmp1)))>0) tmp$dothpast[i] <- 1
}
tmp$dleftpast[tmp$dprdpast==1|tmp$dmorenapast==1] <- 1
#
# sort back to original order
tmp <- tmp[order(tmp$ord),]
# retain new dummies only to cbind them
tmp <- tmp[,c("dpanpast","dpripast","dleftpast","dothpast")]
vot <- cbind(vot,tmp)
###########################
## end dummy pty had won ##
###########################
#
# re-check shares add to 1
table(with(vot, pan + pri + prd + morena + oth)) # rounding messed some
vot <- within(vot, oth <- 1 - pan - pri - prd - morena) # fix them thru oth
table(with(vot, pan + pri + prd + morena + oth)) # rounding messed some


###################################
## function to estimate ols regs ##
###################################
library(DataCombine) # easy lags with slide
#
estim.mod <- function(pty = "left", y = 2005, ret.data = FALSE, list.NAs = FALSE){
    # duplicate vot for analysis
    tmp <- vot
    #
    # drop indetermined races
    sel <- which(tmp$win %in% c("anulada","consejoMunic","consejoMun","litigio"))
    tmp <- tmp[-sel,]
    #
    ## # change prd/morena to left in win ## DONE IN OBJECTS VOT AND INC
    ## grep("win", colnames(tmp))
    ## table(tmp$win)
    #
    if (pty == "pan"){
        # municipios where pan=0 or pri=0, add 0.001 to avoid indeterminacy in log-ratios
        sel  <- which(tmp$pan==0); tmp$pan[sel] <- 0.001
        sel  <- which(tmp$pri==0); tmp$pri[sel] <- 0.001
        #
        tmp$vot <- tmp$pan
        tmp$res.pty <- tmp$res.pan
        tmp$concgovot <- tmp$dconcgo*tmp$gopan
        tmp$concprvot <- tmp$dconcfed*tmp$prpan # prpan is 0 in midterms, so same as using dconcpr
        tmp$dptypast <- tmp$dpanpast
        tmp$dwin <- as.numeric(tmp$win=="pan")
        sel <- grep("pan", tmp$win); tmp$dpty <- 0; tmp$dpty[sel] <- 1
    }
    if (pty == "pri"){
        tmp$vot <- tmp$pri
        tmp$res.pty <- tmp$res.pri
        tmp$concgovot <- tmp$dconcgo*tmp$gopri
        tmp$concprvot <- tmp$dconcfed*tmp$prpri
        tmp$dptypast <- tmp$dpripast
        tmp$dwin <- as.numeric(tmp$win=="pri")
        sel <- grep("pri", tmp$win); tmp$dpty <- 0; tmp$dpty[sel] <- 1
    }
    if (pty == "left"){
        # municipios where left=0 or pri=0, add 0.001 to avoid indeterminacy in log-ratios
        sel  <- which(tmp$left==0); tmp$left[sel] <- 0.001
        sel  <- which(tmp$pri==0); tmp$pri[sel] <- 0.001
        #
        tmp$vot <- tmp$left
        tmp$res.pty <- tmp$res.left
        tmp$concgovot <- tmp$dconcgo*tmp$goleft
        tmp$concprvot <- tmp$dconcfed*tmp$prleft
        tmp$dptypast <- tmp$dleftpast
        sel <- grep("left", tmp$win.left); tmp$dpty <- 0; tmp$dpty[sel] <- 1
    }
    if (pty == "oth"){
        # municipios where oth=0 or pri=0, add 0.001 to avoid indeterminacy in log-ratios
        sel  <- which(tmp$oth<=0); tmp$oth[sel] <- 0.001
        sel  <- which(tmp$pri==0); tmp$pri[sel] <- 0.001
        #
        tmp$vot <- tmp$oth
        tmp$res.pty <- tmp$res.oth
        tmp$log.vot <- log(tmp$oth/tmp$pri)
        tmp$concgovot <- tmp$dconcgo*tmp$gooth
        tmp$concprvot <- tmp$dconcfed * (1 - tmp$prpan - tmp$prpri - tmp$prleft)
        tmp$dptypast <- tmp$dothpast
        tmp$dwin <- as.numeric(tmp$win!="pan" & tmp$win!="pri" & tmp$win!="prd" & tmp$win!="morena")
        sel <- grep("pan|pri|prd|morena", tmp$win); tmp$dpty <- 1; tmp$dpty[sel] <- 0
    }
    # incumbent x pty dummies (complement is open seat)
    tmp$dptyinc  <-      tmp$dpty  *      tmp$dincran.current #(1 - tmp$dopenseat)
    tmp$dothinc  <- (1 - tmp$dpty) *      tmp$dincran.current
    tmp$dptyopen <-      tmp$dpty  * (1 - tmp$dincran.current)
    tmp$dothopen <- (1-  tmp$dpty) * (1 - tmp$dincran.current) # drop to avoid dummy trap
    #
    # manipulate prd/morena govpty for left
    sel <- which( tmp$govpty.lag=="prd" | tmp$govpty.lag=="morena" )
    tmp$govpty.lag[sel] <- "left"
    # code govpty oth
    sel <- which(tmp$govpty.lag=="pvem" | tmp$govpty.lag=="mc" | tmp$govpty.lag=="ind")
    tmp$govpty.lag[sel] <- "oth"
    # own party governor dummy
    tmp$dsamegov <- 0
    tmp$dsamegov[tmp$govpty.lag == pty] <- 1
    #
    # lag votes
    tmp <- tmp[order(tmp$emm),] # check sorted for lags
    #tmp[1,]
    tmp <- slide(data = tmp, TimeVar = "round", GroupVar = "ife", Var = "vot", NewVar = "vot.lag", slideBy = -1) # lag by one period
    tmp <- slide(data = tmp, TimeVar = "round", GroupVar = "ife", Var = "pri", NewVar = "pri.lag", slideBy = -1) # lag by one period
    #
    # alpha
    if (pty=="pan")  tmp$alpha <- tmp$alpha.pan
    if (pty=="pri")  tmp$alpha <- tmp$alpha.pri
    if (pty=="left") tmp$alpha <- tmp$alpha.left
    #if (pty=="oth")  tmp$alpha <- tmp$alpha.oth # i believe this is not in distributed data
    #
    # years to retain in estimation (vhat histories after 2004 only) --- do after lag to avoid losing obs
    sel <- which(tmp$yr>=y)
    tmp <- tmp[sel,]
    #
    # change var units
    tmp <- within(tmp, {
        wmeanalt <- wmeanalt/1000;
        wsdalt <-  wsdalt/1000;
        ptot <- ptot/100000
    })
    #
    # drop cases with NAs observations---sould be new municialities (missing agged vote) and void races (zapatista muns)
    tmp2 <- gsub(" ", "", form)
    tmp2 <- strsplit(tmp2, split = "[~+]", perl = TRUE)
    tmp2 <- tmp2[[1]]
    tmp2 <- tmp[, which(colnames(tmp) %in% tmp2)]
    sel <- which(! complete.cases(tmp2))
    #cbind(tmp$emm[sel], tmp2[sel,])
    if (list.NAs==TRUE) print(c("Cases with NAs dropped from lm", tmp$emm[sel])) # list of missing cases
    tmp <- tmp[-sel,] 
    #
    tmp.mod <- lm(formula = form, data = tmp, subset = (dhgover==0))
    #
    ## # debug
    ## table(is.na(tmp$vot.lag  ))
    ## table(is.na(tmp$dptyinc  ))
    ## table(is.na(tmp$dothinc  ))
    ## table(is.na(tmp$dptyopen ))
    ## table(is.na(tmp$dsamegov ))
    ## table(is.na(tmp$ptot     ))
    ## table(is.na(tmp$wmeanalt ))
    ## table(is.na(tmp$wsdalt   ))
    ## table(is.na(tmp$dpostref ))
    ## sel  <- which(tmp$pri==0)
    ## sel  <- which(tmp$pri.lag==0)
    ## tmp$emm[sel]
    #
    if (ret.data == TRUE){
        return(tmp)
    } else {
        return(tmp.mod)
    }
}

# three models with residual as DV
# model 1
form <- "res.pty ~ vot.lag  + dptyinc + dothinc + dptyopen             + dsamegov + logptot + wsdalt + dpostref"
pan.dat05.m1 <- estim.mod(pty = "pan", y = 2005, ret.data = TRUE, list.NAs = TRUE)
pan.lag05.m1 <- estim.mod(pty = "pan", y = 2005)
summary(pan.lag05.m1)
pri.dat05.m1 <- estim.mod(pty = "pri", y = 2005, ret.data = TRUE, list.NAs = TRUE)
pri.lag05.m1 <- estim.mod(pty = "pri", y = 2005)
summary(pri.lag05.m1)
left.dat05.m1 <- estim.mod(pty = "left", y = 2005, ret.data = TRUE, list.NAs = TRUE)
left.lag05.m1 <- estim.mod(pty = "left", y = 2005)
summary(left.lag05.m1)
# model 2
form <- "res.pty ~ vot.lag  + dptyinc + dothinc + dptyopen + concgovot + concprvot + dsamegov + logptot + wsdalt + dpostref"
pan.lag05.m2 <- estim.mod(pty = "pan", y = 2005)
summary(pan.lag05.m2)
pri.lag05.m2 <- estim.mod(pty = "pri", y = 2005)
summary(pri.lag05.m2)
left.lag05.m2 <- estim.mod(pty = "left", y = 2005)
summary(left.lag05.m2)
# model 3
form <- "res.pty ~ vot.lag  + dptyinc + dothinc + dptyopen + concgovot + concprvot + dsamegov + logptot + wsdalt + as.factor(yr) + as.factor(edon)"
pan.lag05.m3 <- estim.mod(pty = "pan", y = 2005)
summary(pan.lag05.m3)
pri.lag05.m3 <- estim.mod(pty = "pri", y = 2005)
summary(pri.lag05.m3)
left.lag05.m3 <- estim.mod(pty = "left", y = 2005)
summary(left.lag05.m3)


# models with log(pan/pri) etc as DV --- need oth.lag05.m4 in order to deduce pri
# model 4
form <- "log(vot/pri) ~ vot.lag  + dptyinc + dothinc + dptyopen + concgovot + concprvot  + dsamegov + logptot + wsdalt + dpostref" #  + as.factor(yr) + as.factor(edon)
#form <- "vot ~ vot.lag  + dptyinc + dothinc + dptyopen             + dsamegov + logptot + wsdalt + dpostref"
pan.dat05.m4 <- estim.mod(pty = "pan", y = 2005, ret.data = TRUE)
pan.lag05.m4 <- estim.mod(pty = "pan", y = 2005)
summary(pan.lag05.m4)
left.lag05.m4 <- estim.mod(pty = "left", y = 2005)
summary(left.lag05.m4)
oth.dat05.m4 <- estim.mod(pty = "oth", y = 2005, ret.data = TRUE)
oth.lag05.m4 <- estim.mod(pty = "oth", y = 2005)
summary(oth.lag05.m4)

tmp <- oth.dat05.m4
table(tmp$dsamegov)
x

# predict
tmp <- data.frame(vot.lag = seq(0,1,.01),
                  dptyinc = 1,
                  dothinc = 0,
                  dptyopen = 0,
                  concgovot = 0,
                  concprvot = 0,
                  dsamegov = 1,
                  logptot = 10, # mean
                  wsdalt = 10,  # low
                  dpostref = 1)
r <- data.frame(pan =  exp(predict(pan.lag05.m4,  newdata = tmp)),
                left = exp(predict(left.lag05.m4, newdata = tmp)), 
                oth =  exp(predict(oth.lag05.m4,  newdata = tmp)))
v <- data.frame(pan  = r$pan  / (1 + r$pan + r$left + r$oth),
                pri  = 1      / (1 + r$pan + r$left + r$oth),
                left = r$left / (1 + r$pan + r$left + r$oth),
                oth  = r$oth  / (1 + r$pan + r$left + r$oth))
v

tmp <- pan.dat05.m4
tmp[1,]
x

# inspect find missing cases eric  x
tmp <- vot # duplicate
sel <- which(tmp$win %in% c("anulada","consejoMunic","litigio"))
tmp <- tmp[-sel,]
tmp <- tmp[order(tmp$emm),] # check sorted for lags
tmp <- slide(data = tmp, TimeVar = "round", GroupVar = "ife", Var = "pan", NewVar = "pan.lag", slideBy = -1) # lag by one period
tmp <- slide(data = tmp, TimeVar = "round", GroupVar = "ife", Var = "pri", NewVar = "pri.lag", slideBy = -1) # lag by one period
    #
sel <- which(tmp$yr>=2005)
tmp <- tmp[sel,]
sel <- which(colnames(tmp) %in% c("emm", "pan", "res.pan", "pan.lag", "win", "dincran.current", "govpty.lag", "logptot", "wsdalt", "dpostref"))
tmp <- tmp[,sel]
#
sel <- which(! complete.cases(tmp))
tmp[sel,]
x

colnames(tmp)[grep("win",colnames(tmp))]

# check here:
# 1. openlostpri =  1903 with dummies but 1904 in table
# 2. openlostleft =  1038 with dummies but 1046 in table
tmp <- pan.dat05.m1
table(tmp$win, tmp$race.after)
table(tmp$yr)
table(tmp$dptyinc, tmp$dinptywon.current)
table(tmp$dptyopen, tmp$dinptywon.current)
table(tmp$dothinc, tmp$dinptywon.current)
table(tmp$dothopen, tmp$dinptywon.current)
summary(tmp$res.pty)
#
sel <- which(tmp$win=="morena" & tmp$race.current=="Beaten")
tmp$emm[sel]

x

#
## pan.lag12 <- estim.mod(pty = "pan", y = 2012)
## summary(pan.lag12)
## pri.lag12 <- estim.mod(pty = "pri", y = 2012)
## summary(pri.lag12)
## left.lag12 <- estim.mod(pty = "left", y = 2012)
## summary(left.lag12)

## # alpha has little effect in oth coefs, but seems to overshoot pan and prd
## form <- "res.pty ~ alpha  + dptyinc + dothinc + dptyopen - dconcgob + dsamegov + ptot + wmeanalt*wsdalt + dpostref - dcapital - as.factor(edon)"
## #
## pan.alpha <- estim.mod(pty = "pan")
## summary(pan.alpha)
## pri.alpha <- estim.mod(pty = "pri")
## summary(pri.alpha)
## left.alpha <- estim.mod(pty = "left")
## summary(left.alpha)




library(stargazer)
stargazer(pan.lag05.m1, pan.lag05.m2, pan.lag05.m3, pri.lag05.m1, pri.lag05.m2, pri.lag05.m3, left.lag05.m1, left.lag05.m2, left.lag05.m3, align=TRUE, report = 'vc*s'
#          , title = "Regression results"
          , type = c("text","latex")[2]
#          , out = "tmp-tab.txt"
          , digits = 3
          , dep.var.labels = c("Residual")
          , column.labels = c(rep("PAN",3), rep("PRI",3), rep("Left",3))
 ##          , covariate.labels=
 ## c("vote share (lagged)",
 ##   "party incumbent",
 ##   "other-party incumbent",
 ##   "party open seat",
 ##   "governor",
 ##   "population (log, 10k)",
 ##   "sd elev. (pop. weighted)",
 ##   "post reform",
 ##   "Constant")
          )

## library(apsrtable)
## apsrtable(fit1, fit2)

# some descriptives
tmp <- pan.dat05
colnames(tmp)

# prior win for xtab
library(DataCombine) # easy lags with slide
tmp <- tmp[order(tmp$emm),] # check sorted for lags
tmp <- slide(data = tmp, TimeVar = "cycle", GroupVar = "inegi", Var = "win", NewVar = "win.prior",    slideBy = -1) # lag by one period
tmp <- slide(data = tmp, TimeVar = "cycle", GroupVar = "inegi", Var = "win.left", NewVar = "win.prior.left",    slideBy = -1) # lag by one period

table(tmp$win.prior, tmp$dinptywon.current, tmp$dincran.current)
table(tmp$win.prior.left, tmp$dinptywon.current, tmp$dincran.current)
colnames(tmp)
tmp <- pri.dat05
tmp <- left.dat05

# term-limited / reelected or not through yrs
tmp$status <- NA
tmp$status[tmp$dtermlim==1] <- "1 term limited"
tmp$status[tmp$dtermlim==0] <- "3 can reelect next race"
tmp$status[grep("Reelected", tmp$race.current)] <- "2 reelected last race (term limited)"
table(tmp$yr,tmp$status, useNA = "ifany")
tmp2 <- data.frame(
    yr=2005:2020,
    reelected=c(rep(0,13),283,12,0),
    cannext=c(rep(0,10),987,465,59,1323,48,0),
    termlim=c(234,581,1217,197,622,1114,221,894,924,26,19,84,212,0,0,84)
    )
tmp2$N <- tmp2$termlim + tmp2$reelected + tmp2$cannext
tmp2 <- within(tmp2, {
    termlim <- round(termlim*100/N);
    reelected <- round(reelected*100/N);
    cannext <- round(cannext*100/N)})

######################################################
## term-limited / reelected or not through yrs plot ##
######################################################
vot.dup <- vot # duplicate for debud
#vot <- vot.dup # restore
vot$status <- NA
vot$status[vot$dtermlim==1] <- "1 term limited"
vot$status[vot$dtermlim==0] <- "3 can reelect next race"
vot$status[grep("Reelected", vot$race.current)] <- "2 reelected last race (term limited)"
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
#     xlab = "", ylab = expression("Percent municipalities (N" %~~% "2,030)"))
     xlab = "", ylab = "% municipalities")
                    #expression(paste("% municipalities (N", %~~%, "2,300)")))
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
    polygon(x, c(y1, y2), col = "gray", border = "gray")
#
for (y in 2:7){
    #y <- 1
    x <- c(y-.15,y+.15,y+.15,y-.15)
    y1 <- rep(0,2)
    y2 <- rep(tmp2[y],2)
    polygon(x, c(y1, y2), col = colors[1], border = colors[1])
    y1 <- y2
    y2 <- y2 + tmp3[y]
    polygon(x, c(y1, y2), col = colors[2], border = colors[2])
    y1 <- y2
    y2 <- y2 + tmp1[y]
    polygon(x, c(y1, y2), col = colors[3], border = colors[3])
    y1 <- y2
    y2 <- rep(1,2)
    polygon(x, c(y1, y2), col = "gray", border = "gray") # without usos
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
polygon(x, c(y1, y2), col = colors[1], border = colors[1])
y1 <- y2
y2 <- y2 + tmptmp21
polygon(x, c(y1, y2), col = "white", border = colors[2])#colors[4])
y1 <- y2
y2 <- rep((1-84/colSums(tmp4)[7]),2) # veracruz joins 'can run next' group in 2021
#y2 <- y2 + tmptmp3
polygon(x, c(y1, y2), col = colors[2], border = colors[2])
## y1 <- y2
## y2 <- y2 + tmptmp1
## polygon(x, c(y1, y2), col = colors[3], border = colors[3])
y1 <- y2 # only Hidalgo left in gray
y2 <- rep(1,2)
polygon(x, c(y1, y2), col = "gray", border = "gray") # without usos
#
# footnote
mtext("* forthcoming", side = 1, line = 1.5, outer = FALSE, adj = 1, cex = .7)
#
legend(x=8.5, y=.7,
       legend =  rev(c("reelected","can run again","term limited","to be determined","non-reformers")), # without usos
       fill =    rev(c(colors[1],colors[2],colors[3],"white","gray")),                     # without usos
       border = rev(c(colors[1],colors[2],colors[3],colors[2],"gray")),
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
###############################################################

# relative population of reelected mayors
vot$shptot <- vot$ptot/112336539 # rel population
sel <- which(vot$round==15 | vot$round==16)
sum(vot$shptot[sel][grep("Reelected", vot$race.current)], na.rm = TRUE)

colSums(tmp4)
# clean
rm(tmp,tmp1,tmp2,tmp11,tmp12,tmp21,tmp3,tmp4,tmp5,tmp.ey,tmptmp1,tmptmp12,tmptmp2,tmptmp21,tmptmp3,colors,x,y,y1,y2)
# restore
vot <- vot.dup; rm(vot.dup)

# debug
sel <- which(tmp$status=="1 term limited" & tmp$yr==2018)
tmp$emm[sel]
x chambiando aqui x

# debug
table(tmp$dptyopen)
sel <- which(is.na(tmp$dptyinc))
tmp$emm[sel]
x

table(tmp$win, tmp$dinptywon.current)
sel <- which(tmp$win=="morena" & tmp$dinptywon.current==0)
tmp[sel[1],]
table(tmp$dincran.current)

## need to present morena, prd, and left, need break pan-prd (which i've done elsewhere, win2)
##         inc open
##   pan   182 2532
##   pri   143 4194
##   left   94 1511
##   oth   117  754
##   tot   536 8991
colnames(pan.dat05)
tmp$win2 <- "."
tmp$win2[grep("morena", tmp$win)] <- "morena"
tmp$win2[grep("pri", tmp$win)] <- "pri"
tmp$win2[grep("pan-prd", tmp$win)] <- "pan-prd"
sel <- which(tmp$win2=="."); sel <- intersect(sel, grep("pan", tmp$win)); tmp$win2[sel] <- "pan"
sel <- which(tmp$win2=="."); sel <- intersect(sel, grep("prd", tmp$win)); tmp$win2[sel] <- "prd"
sel <- which(tmp$win2=="."); sel <- intersect(sel, grep("pvem", tmp$win)); tmp$win2[sel] <- "pvem"
table(tmp$win2)
table(tmp$win[tmp$win2=="."]) # does not reflect win in aymuincumbents! why?
x

# explore missingness
table(is.na(tmp$res.pan[tmp$yr>=2006]), is.na(tmp$alpha.pan[tmp$yr>=2006]))
x


# model eric  x
colnames(tmp)
tmp.mod <- lm(formula = res.pan ~ alpha.pan + dpaninc + dothinc + dpanopen, data = tmp)
tmp.mod <- lm(formula = pan.lag ~ alpha.pan, data = tmp)
tmp.mod <- lm(formula = res.pan ~ pan.lag   + dpaninc + dothinc + dpanopen - dconcgob + dsamegov + ptot + wmeanalt*wsdalt + dpostref - dcapital - as.factor(edon), data = tmp, subset = (dhgover==0 & yr>=2006))
summary(tmp.mod)
nobs(tmp.mod)
x

estimate with jags, simulate


## #############################################################################################
## ## MOVE THIS BLOCK TO AFTER READING/MANIPULATING ELECTORAL DATA                            ##
## ## get lisnom from federal els (taken from naylum code)                                    ##
## ## ignores vote code bec aggregating multi-district munics w partial coals not straightfwd ##
## #############################################################################################
## ### get seccion equivalencias to fill missing municipio names
## pth <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv"
## eq <- read.csv(pth, stringsAsFactors = FALSE)
## eq <- eq[,c("edon","seccion","ife","inegi","mun")]
## ### 2006 president has lisnom 
## pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/pre2006.csv"
## tmp <- read.csv(pth, stringsAsFactors = FALSE)
## tmp <- tmp[-which(tmp$seccion==0),] # drop voto extranjero
## tmp$lisnom[is.na(tmp$lisnom)] <- 0 # make NAs zeroes because ave() somehow not ignoring them
## tmp$lisnom <- ave(tmp$lisnom, as.factor(tmp$edon*10000+tmp$seccion), FUN=sum, na.rm=TRUE) # aggregate secciones
## tmp <- tmp[duplicated(tmp$edon*10000+tmp$seccion)==FALSE,] # drop reduntant obs --- keep seccion structure to merge to 09
## tmp$lisnom <- ave(tmp$lisnom, as.factor(tmp$munn), FUN=sum, na.rm=TRUE) # aggregate municipios
## tmp <- tmp[duplicated(tmp$munn)==FALSE,] # drop reduntant obs --- keep seccion structure to merge to 09
## tmp$lisnom.06 <- tmp$lisnom
## ln <- tmp[, c("edon","munn","lisnom.06")]
## #
## # 2009 has mun only, merge to eq for all munn (slight mistakes likely due to remunicipalización)
## pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/dip2009.csv"
## tmp <- read.csv(pth, stringsAsFactors = FALSE)
## tmp$lisnom[is.na(tmp$lisnom)] <- 0 # make NAs zeroes because ave() somehow not ignoring them
## tmp$lisnom.09 <- tmp$lisnom # rename
## tmp$mun <- tmp$munn # wrong name
## tmp <- tmp[,c("edon","mun","seccion","lisnom.09")] # data has no munn
## tmp$lisnom.09 <- ave(tmp$lisnom.09, as.factor(tmp$edon*10000+tmp$seccion), FUN=sum, na.rm=TRUE) # aggregate secciones
## tmp <- tmp[duplicated(tmp$edon*10000+tmp$seccion)==FALSE,] # drop reduntant obs
## tmp$lisnom.09 <- ave(tmp$lisnom.09, as.factor(paste(tmp$edon, tmp$mun, sep = ".")), FUN=sum, na.rm=TRUE) # aggregate municipios
## tmp <- merge(x = tmp, y = eq, by = c("edon","seccion"), all.x = TRUE, all.y = FALSE) # merge to have munn
## tmp <- tmp[duplicated(as.factor(paste(tmp$edon, tmp$mun.x, sep = ".")))==FALSE,]
## tmp$munn <- tmp$ife
## tmp$mun <- tmp$mun.y
## tmp <- tmp[,c("edon","munn","inegi","mun","lisnom.09")]
## #
## ln <- merge(x = ln, y = tmp, by = c("edon","munn"), all = TRUE)
## #
## # 2012 presid
## pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/pre2012.csv"
## tmp <- read.csv(pth, stringsAsFactors = FALSE)
## tmp <- tmp[, c("edon","seccion","mun","lisnom")]
## tmp <- tmp[-which(tmp$seccion==0),] # drop voto extranjero
## tmp[tmp=="-"] <- 0 # remove "-"
## # agrega secciones
## tmp$lisnom   <- ave(tmp$lisnom,   as.factor(tmp$edon*10000+tmp$seccion), FUN=sum, na.rm=TRUE)
## tmp <- tmp[duplicated(tmp$edon*10000+tmp$seccion)==FALSE,] # drop reduntant obs
## tmp$lisnom.12 <- tmp$lisnom; tmp$lisnom <- NULL
## # add munn
## tmp <- merge(x = tmp, y = eq, by = c("edon","seccion"), all.x = TRUE, all.y = FALSE) 
## tmp$munn <- tmp$ife; tmp$ife <- NULL # rename
## # aggregate municipios
## tmp$lisnom.12   <- ave(tmp$lisnom.12,   as.factor(paste(tmp$edon, tmp$mun.x)), FUN=sum, na.rm=TRUE)
## tmp <- tmp[duplicated(as.factor(paste(tmp$edon, tmp$mun.x)))==FALSE,] # drop reduntant obs
## tmp$mun <- tmp$mun.y; tmp$mun.x <- tmp$mun.y <- NULL
## tmp <- tmp[,c("edon","munn","inegi","mun","lisnom.12")]
## #
## ln <- merge(x = ln, y = tmp, by = c("edon","munn"), all = TRUE)
## # plug mun.y when mun.x is missing
## sel <- which(is.na(ln$mun.x))
## ln$mun.x[sel] <- ln$mun.y[sel]
## sel <- which(is.na(ln$inegi.x))
## ln$inegi.x[sel] <- ln$inegi.y[sel]
## ln$mun <- ln$mun.x; ln$mun.x <- ln$mun.y <- NULL
## ln$inegi <- ln$inegi.x; ln$inegi.x <- ln$inegi.y <- NULL
## #
## # 2015 dip fed
## pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/dip2015.csv"
## tmp <- read.csv(pth, stringsAsFactors = FALSE)
## #tmp <- tmp[-which(tmp$seccion==0),] # drop voto extranjero
## tmp$ord <- tmp$OBSERVACIONES <- tmp$ID_CASILLA <- tmp$TIPO_CASILLA <- tmp$EXT_CONTIGUA <- tmp$nr <- tmp$nul <- tmp$tot <- NULL # clean
## tmp$lisnom[is.na(tmp$lisnom)] <- 0
## tmp$lisnom.15 <- tmp$lisnom
## tmp <- tmp[,c("edon","seccion","lisnom.15")]
## # agrega secciones
## tmp$lisnom.15   <- ave(tmp$lisnom.15,   as.factor(paste(tmp$edon, tmp$seccion, sep = ".")), FUN=sum, na.rm=TRUE)
## tmp <- tmp[duplicated(as.factor(paste(tmp$edon, tmp$seccion, sep = ".")))==FALSE,] # drop reduntant obs
## tmp <- merge(x = tmp, y = eq, by = c("edon","seccion"), all.x = TRUE, all.y = FALSE) # add munn
## tmp$munn <- tmp$ife; tmp$ife <- NULL # rename
## tmp$lisnom.15   <- ave(tmp$lisnom.15,   as.factor(tmp$munn), FUN=sum, na.rm=TRUE)
## tmp <- tmp[duplicated(tmp$munn)==FALSE,] # drop reduntant obs
## tmp <- tmp[,c("edon","munn","mun","lisnom.15")]
## #
## ln <- merge(x = ln, y = tmp, by = c("edon","munn"), all = TRUE)
## # plug mun.y when mun.x is missing
## sel <- which(is.na(ln$mun.x))
## ln$mun.x[sel] <- ln$mun.y[sel]
## ln$mun <- ln$mun.x; ln$mun.x <- ln$mun.y <- NULL
## #
## # 2018 presid
## pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/pre2018.csv"
## tmp <- read.csv(pth, stringsAsFactors = FALSE)
## tmp <- tmp[-which(tmp$seccion==0),] # drop voto extranjero
## tmp[tmp=="-"] <- 0 # remove "-"
## tmp <- tmp[, c("edon","seccion","lisnom")]
## # agrega secciones
## tmp$lisnom   <- ave(tmp$lisnom,   as.factor(tmp$edon*10000+tmp$seccion), FUN=sum, na.rm=TRUE)
## tmp <- tmp[duplicated(tmp$edon*10000+tmp$seccion)==FALSE,] # drop reduntant obs
## tmp$lisnom.18 <- tmp$lisnom; tmp$lisnom <- NULL
## # add munn
## tmp <- merge(x = tmp, y = eq, by = c("edon","seccion"), all.x = TRUE, all.y = FALSE) 
## tmp$munn <- tmp$ife; tmp$ife <- NULL # rename
## sel <- which(is.na(tmp$munn)) # reseccionamiento, drop handful of secciones not in equiv secciones
## tmp <- tmp[-sel,]
## # aggregate municipios
## tmp$lisnom.18   <- ave(tmp$lisnom.18,   as.factor(tmp$munn), FUN=sum, na.rm=TRUE)
## tmp <- tmp[duplicated(tmp$munn)==FALSE,] # drop reduntant obs
## tmp <- tmp[,c("edon","munn","mun","lisnom.18")]
## #
## ln <- merge(x = ln, y = tmp, by = c("edon","munn"), all = TRUE)
## ln$mun <- ln$mun.x; ln$mun.x <- ln$mun.y <- NULL
## #
## # sort
## ln <- ln[order(ln$edon, ln$munn), c("edon","munn","inegi","mun","lisnom.06","lisnom.09","lisnom.12","lisnom.15","lisnom.18")]
## rm(eq,tmp)
## 
## # will need to project lisnom from fed elecs (cf naylum), too many missings here
## head(ln)
## with(inc, table(is.na(mg)))
## with(inc, table(is.na(lisnom)))
## ls()
## 
## # adds object cen.yr (and mpv, unneeded) with yearly census projections
## load(file="/home/eric/Desktop/naylum/data/electoral/nay2002-on.RData") 
## rm(vpm)
## 
## unique(sub("[.][0-9]{2}", "", colnames(cen.yr))) # names omitting yrs
## cen.yr$lisnom.12
## 
## # lag margin (measure of candidate quality)
## library(DataCombine) # easy lags
## inc$tmp <- sub("([a-z]+)[-][0-9]{2}([.][0-9]{3})", "\\1\\2", inc$emm) # drop elec cycle from emm
## inc <- slide(data = inc, TimeVar = "yr", GroupVar = "tmp", Var = "mg", NewVar = "mg.lag",    slideBy = -1)
## inc <- inc[order(inc$ord),] # sort
## inc$tmp <- inc$suplente <- NULL


