######################################################################
## ################################################################ ##
## ## SCRIPT FROM ../elecReturns/code/incumbents.r *STARTS* HERE ## ##
## ## 1aug2020                                                   ## ##
## ################################################################ ##
######################################################################

options(width = 80)

rm(list = ls())
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data/"
setwd(dd)

###########################################
## ##################################### ##
## ##                                 ## ##
## ## INCUMBENT DATA PREP STARTS HERE ## ##
## ##                                 ## ##
## ##################################### ##
###########################################

# read alcaldes
inc <- read.csv(file = "aymu1989-present.incumbents.csv", stringsAsFactors = FALSE)
colnames(inc)

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

# simplify parties
inc$win2 <- inc$win
inc$win2 <- sub("conve", "mc", inc$win2, ignore.case = TRUE)
inc$win2 <- grep("panal", "pna", inc$win2, ignore.case = TRUE)
inc$win2 <- grep("pucd", "pudc", inc$win2, ignore.case = TRUE) # typo
sel <- grep("ci_|^ci$|c-i-|ind_|eduardo|luis|oscar|indep", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "indep"
inc$win <- inc$win2 # register changes above in original win to remove false negatives
sel <- grep("pan-", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pan"
sel <- grep("pri-", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pri"
sel <- grep("prd-|-prd", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "prd"
sel <- grep("morena-|-morena", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "morena"
sel <- grep("pvem-|-pvem", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pvem"
sel <- grep("mc-|-mc", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "mc"
sel <- grep("-pes", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pes"
sel <- grep("mas|paz|pcp|phm|pmr|ppg|psd1|psi|pudc|pup|via_|pchu|pmch|pver|prs|prv|ps1|poc|pjs|pd1|pec|pasd|pac1|npp|pcu|pcdt|pmac|pcm2|pdm|pps|ppt|ph|pmp|fc1|pcd1|psn|ave", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "loc/oth"
#
inc$win.long <- inc$win # retain unsimplified version
inc$win <- inc$win2; inc$win2 <- NULL # keep manipulated version only

#############################################################
## lag race.after & win to generate race.prior & win.prior ##
#############################################################
# this function does the job easier
# library(DataCombine) # easy lags with slide
# example
# inc <- inc[order(inc$inegi, inc$yr),] # verify sorted before lags
# inc <- slide(inc, Var = "win", GroupVar = "ife", slideBy = +1) # lead win by one period
inc <- inc[order(inc$emm),] # sort munn-chrono
# open slots for lagged variables
inc$race.prior <- NA
inc$win.prior <- NA
inc$win.long.prior <- NA # to code pty won dummy
#
for (e in 1:32){
    #e <- 8 #debug
    sel.e <- which(inc$edon==e)
    inc.e <- inc[sel.e,]
    mm <- unique(inc.e$munn)
    for (m in mm){
        #m <- 5 #debug
        sel.m <- which(inc.e$munn==m)
        inc.m <- inc.e[sel.m,]
        M <- nrow(inc.m)
        if (M==1) next
        tmp <- inc.m$race.after
        tmp <- c(NA, tmp[-M])
        inc.m$race.prior <- tmp
        tmp <- inc.m$win
        tmp <- c(NA, tmp[-M])
        inc.m$win.prior <- tmp
        tmp <- inc.m$win.long
        tmp <- c(NA, tmp[-M])
        inc.m$win.long.prior <- tmp
        inc.e[sel.m,] <- inc.m
    }
    inc[sel.e,] <- inc.e
}

# drop observations that went to usos y costumbres
sel <- grep("drop-obs", inc$race.after, ignore.case = TRUE)
if (length(sel)>0) inc <- inc[-sel,]
sel <- grep("uyc", inc$win, ignore.case = TRUE)
if (length(sel)>0) inc <- inc[-sel,]

# 31jul2020: NEED TO DEAL WITH WIN.PRIOR IN NEW MUNICS... looked at win in parent municipio and used it 
inc$win.prior[inc$emm=="ags-08.010"] <- inc$win.long.prior[inc$emm=="ags-08.010"] <- "pri"
inc$win.prior[inc$emm=="ags-08.011"] <- inc$win.long.prior[inc$emm=="ags-08.011"] <- "pri"
inc$win.prior[inc$emm=="bc-10.005"]  <- inc$win.long.prior[inc$emm=="bc-10.005"]  <- "pan"
inc$win.prior[inc$emm=="bcs-08.009"] <- inc$win.long.prior[inc$emm=="bcs-08.009"] <- "pri"
inc$win.prior[inc$emm=="cam-08.009"] <- inc$win.long.prior[inc$emm=="cam-08.009"] <- "pri"
inc$win.prior[inc$emm=="cam-10.010"] <- inc$win.long.prior[inc$emm=="cam-10.010"] <- "pri"
inc$win.prior[inc$emm=="cam-11.011"] <- inc$win.long.prior[inc$emm=="cam-11.011"] <- "pri"
inc$win.prior[inc$emm=="cps-08.112"] <- inc$win.long.prior[inc$emm=="cps-08.112"] <- "pri"
inc$win.prior[inc$emm=="cps-11.113"] <- inc$win.long.prior[inc$emm=="cps-11.113"] <- "pri"
inc$win.prior[inc$emm=="cps-11.114"] <- inc$win.long.prior[inc$emm=="cps-11.114"] <- "pri"
inc$win.prior[inc$emm=="cps-11.115"] <- inc$win.long.prior[inc$emm=="cps-11.115"] <- "pri"
inc$win.prior[inc$emm=="cps-11.116"] <- inc$win.long.prior[inc$emm=="cps-11.116"] <- "pri"
inc$win.prior[inc$emm=="cps-11.117"] <- inc$win.long.prior[inc$emm=="cps-11.117"] <- "pri"
inc$win.prior[inc$emm=="cps-11.118"] <- inc$win.long.prior[inc$emm=="cps-11.118"] <- "pri"
inc$win.prior[inc$emm=="cps-11.119"] <- inc$win.long.prior[inc$emm=="cps-11.119"] <- "pri"
inc$win.prior[inc$emm=="cps-15.120"] <- "prd"; inc$win.long.prior[inc$emm=="cps-15.120"] <- "pan-prd-mc-pna"
inc$win.prior[inc$emm=="cps-15.121"] <- inc$win.long.prior[inc$emm=="cps-15.121"] <- "pvem"
inc$win.prior[inc$emm=="cps-15.122"] <- inc$win.long.prior[inc$emm=="cps-15.122"] <- "pri" # me lo saqué de la manga
inc$win.prior[inc$emm=="cps-15.123"] <- inc$win.long.prior[inc$emm=="cps-15.123"] <- "pvem"
inc$win.prior[inc$emm=="cps-17.124"] <- inc$win.long.prior[inc$emm=="cps-17.124"] <- "pvem"
inc$win.prior[inc$emm=="cps-17.125"] <- inc$win.long.prior[inc$emm=="cps-17.125"] <- "prd"
inc$win.prior[inc$emm=="dgo-07.039"] <- inc$win.long.prior[inc$emm=="dgo-07.039"] <- "pri"
inc$win.prior[inc$emm=="gue-09.076"] <- inc$win.long.prior[inc$emm=="gue-09.076"] <- "pri"
inc$win.prior[inc$emm=="gue-12.077"] <- inc$win.long.prior[inc$emm=="gue-12.077"] <- "pri" # pan en cuajinicualapa
inc$win.prior[inc$emm=="gue-13.078"] <- inc$win.long.prior[inc$emm=="gue-13.078"] <- "prd"
inc$win.prior[inc$emm=="gue-13.079"] <- inc$win.long.prior[inc$emm=="gue-13.079"] <- "pri"
inc$win.prior[inc$emm=="gue-13.080"] <- inc$win.long.prior[inc$emm=="gue-13.080"] <- "pri"
inc$win.prior[inc$emm=="gue-13.081"] <- inc$win.long.prior[inc$emm=="gue-13.081"] <- "prd" # pri en san luis acatlan
inc$win.prior[inc$emm=="jal-13.125"] <- inc$win.long.prior[inc$emm=="jal-13.125"] <- "pan"
inc$win.prior[inc$emm=="mex-08.122"] <- inc$win.long.prior[inc$emm=="mex-08.122"] <- "pri"
inc$win.prior[inc$emm=="mex-11.123"] <- inc$win.long.prior[inc$emm=="mex-11.123"] <- "pri"
inc$win.prior[inc$emm=="mex-11.124"] <- inc$win.long.prior[inc$emm=="mex-11.124"] <- "pri"
inc$win.prior[inc$emm=="mex-12.125"] <- "pri"; inc$win.long.prior[inc$emm=="mex-12.125"] <- "pri-pvem"
inc$win.prior[inc$emm=="nay-07.020"] <- inc$win.long.prior[inc$emm=="nay-07.020"] <- "pri"
inc$win.prior[inc$emm=="qui-09.008"] <- inc$win.long.prior[inc$emm=="qui-09.008"] <- "pri"
inc$win.prior[inc$emm=="qui-13.009"] <- "prd"; inc$win.long.prior[inc$emm=="qui-13.009"] <- "prd-pt"
inc$win.prior[inc$emm=="qui-15.010"] <- inc$win.long.prior[inc$emm=="qui-15.010"] <- "pri"
inc$win.prior[inc$emm=="qui-16.011"] <- "pri"; inc$win.long.prior[inc$emm=="qui-16.011"] <- "pri-pvem-pna"
inc$win.prior[inc$emm=="san-10.057"] <- inc$win.long.prior[inc$emm=="san-10.057"] <- "pri"
inc$win.prior[inc$emm=="san-10.058"] <- inc$win.long.prior[inc$emm=="san-10.058"] <- "pan"
inc$win.prior[inc$emm=="son-08.070"] <- inc$win.long.prior[inc$emm=="son-08.070"] <- "pri" # usé puerto peñasco
inc$win.prior[inc$emm=="son-10.071"] <- inc$win.long.prior[inc$emm=="son-10.071"] <- "pri"
inc$win.prior[inc$emm=="son-10.072"] <- inc$win.long.prior[inc$emm=="son-10.072"] <- "pri"
inc$win.prior[inc$emm=="tla-09.045"] <- inc$win.long.prior[inc$emm=="tla-09.045"] <- "pri"
inc$win.prior[inc$emm=="tla-09.046"] <- inc$win.long.prior[inc$emm=="tla-09.046"] <- "pri"
inc$win.prior[inc$emm=="tla-09.047"] <- inc$win.long.prior[inc$emm=="tla-09.047"] <- "pri"
inc$win.prior[inc$emm=="tla-09.048"] <- inc$win.long.prior[inc$emm=="tla-09.048"] <- "pri"
inc$win.prior[inc$emm=="tla-09.049"] <- inc$win.long.prior[inc$emm=="tla-09.049"] <- "pri"
inc$win.prior[inc$emm=="tla-09.050"] <- inc$win.long.prior[inc$emm=="tla-09.050"] <- "pri"
inc$win.prior[inc$emm=="tla-09.051"] <- inc$win.long.prior[inc$emm=="tla-09.051"] <- "pri"
inc$win.prior[inc$emm=="tla-09.052"] <- inc$win.long.prior[inc$emm=="tla-09.052"] <- "pri"
inc$win.prior[inc$emm=="tla-09.053"] <- inc$win.long.prior[inc$emm=="tla-09.053"] <- "pri"
inc$win.prior[inc$emm=="tla-09.054"] <- inc$win.long.prior[inc$emm=="tla-09.054"] <- "pri"
inc$win.prior[inc$emm=="tla-09.055"] <- inc$win.long.prior[inc$emm=="tla-09.055"] <- "pri"
inc$win.prior[inc$emm=="tla-09.056"] <- inc$win.long.prior[inc$emm=="tla-09.056"] <- "pri"
inc$win.prior[inc$emm=="tla-09.057"] <- inc$win.long.prior[inc$emm=="tla-09.057"] <- "pri"
inc$win.prior[inc$emm=="tla-09.058"] <- inc$win.long.prior[inc$emm=="tla-09.058"] <- "pri"
inc$win.prior[inc$emm=="tla-09.059"] <- inc$win.long.prior[inc$emm=="tla-09.059"] <- "pri"
inc$win.prior[inc$emm=="tla-09.060"] <- inc$win.long.prior[inc$emm=="tla-09.060"] <- "pri"
inc$win.prior[inc$emm=="ver-08.204"] <- inc$win.long.prior[inc$emm=="ver-08.204"] <- "pri"
inc$win.prior[inc$emm=="ver-08.205"] <- inc$win.long.prior[inc$emm=="ver-08.205"] <- "pri"
inc$win.prior[inc$emm=="ver-08.206"] <- inc$win.long.prior[inc$emm=="ver-08.206"] <- "pri"
inc$win.prior[inc$emm=="ver-08.207"] <- inc$win.long.prior[inc$emm=="ver-08.207"] <- "pri"
inc$win.prior[inc$emm=="ver-10.208"] <- inc$win.long.prior[inc$emm=="ver-10.208"] <- "pri"
inc$win.prior[inc$emm=="ver-10.209"] <- inc$win.long.prior[inc$emm=="ver-10.209"] <- "pri"
inc$win.prior[inc$emm=="ver-10.210"] <- inc$win.long.prior[inc$emm=="ver-10.210"] <- "pri"
inc$win.prior[inc$emm=="ver-12.211"] <- inc$win.long.prior[inc$emm=="ver-12.211"] <- "pan"
inc$win.prior[inc$emm=="ver-12.212"] <- inc$win.long.prior[inc$emm=="ver-12.212"] <- "pri"
inc$win.prior[inc$emm=="zac-11.057"] <- inc$win.long.prior[inc$emm=="zac-11.057"] <- "prd"
inc$win.prior[inc$emm=="zac-13.058"] <- inc$win.long.prior[inc$emm=="zac-13.058"] <- "prd"
# make prd incumbent party in all df2000 (first municipal election)
sel <- grep("df-11", inc$emm)
inc$win.prior[sel] <- inc$win.long.prior[sel] <- "prd"
# there are NAs before 1993 ignored
# because analysis drops those years
# fix them in future when needed
table(is.na(inc$win.prior), inc$yr, useNA = "always")

# 31jul2020: NEED TO DEAL WITH RACE.PRIOR IN NEW MUNICS... code as new category "new mun"
# deals with post 1993 only
sel <- which(is.na(inc$race.prior)==TRUE & inc$yr>1993)
tmp <- inc[sel,] # subset data for manipulation
sel1 <- grep("pan", tmp$win.prior)
sel2 <- grep("pan", tmp$win)
tmp$race.prior[intersect(sel1,sel2)] <- "Term-limited-p-won"
sel1 <- grep("pri", tmp$win.prior)
sel2 <- grep("pri", tmp$win)
tmp$race.prior[intersect(sel1,sel2)] <- "Term-limited-p-won"
sel1 <- grep("prd", tmp$win.prior)
sel2 <- grep("prd", tmp$win)
tmp$race.prior[intersect(sel1,sel2)] <- "Term-limited-p-won"
sel1 <- grep("pvem", tmp$win.prior)
sel2 <- grep("pvem", tmp$win)
tmp$race.prior[intersect(sel1,sel2)] <- "Term-limited-p-won"
inc[sel,] <- tmp  # return to data after manipulation
# repeat subset for defeats (easier debugging)
sel <- which(is.na(inc$race.prior)==TRUE & inc$yr>1993)
tmp <- inc[sel,] # subset data for manipulation
#table(tmp$win, tmp$win.prior, useNA = "always")
#data.frame(tmp$win.prior, tmp$win, tmp$race.prior) # check that all remaining are defeats
tmp$race.prior <- "Term-limited-p-lost"
inc[sel,] <- tmp  # return to data after manipulation
# check coding
sel <- which(inc$yr>1993)
table(inc$race.prior[sel], useNA = "always")
# rename categories
sel <- which(inc$race.prior=="Reelected")
inc$race.prior[sel] <- "Incumb-remained"
sel <- which(inc$race.prior=="Beaten")
inc$race.prior[sel] <- "Incumb-ousted"
sel <- grep("p-won", inc$race.prior, ignore.case = TRUE) 
inc$race.prior[sel] <- "Open-same-pty"
sel <- grep("p-lost", inc$race.prior, ignore.case = TRUE) 
inc$race.prior[sel] <- "Open-dif-pty"
sel <- grep("pending|out-p-[?]", inc$race.prior, ignore.case = TRUE) 
inc$race.prior[sel] <- "pending"
sel <- which(inc$yr>1993)
table(inc$race.prior[sel], useNA = "always")
table(inc$race.prior, useNA = "always")

#############################################
# subset: cases allowing reelection in 2018 #
# esto lo reporté en el blog de Nexos       #
#############################################
sel <- which(inc$yr==2018 & inc$edon!=9 & inc$edon!=21)
inc.sub <- inc[sel,]
dim(inc.sub)

sel <- which(inc.sub$race.prior=="pending"|inc.sub$race.prior=="")
inc.sub$emm[sel]
if (length(sel)>0) inc.sub <- inc.sub[-sel,] # drop cases with pending election

table(inc.sub$edon, inc.sub$race.prior) # by state
table(              inc.sub$race.prior)
nrow(inc.sub)
round(table(inc.sub$race.prior) / nrow(inc.sub),2)

table(inc.sub$win, inc.sub$race.prior) # by incumbent party
tab <- table(inc.sub$win.prior, inc.sub$race.prior)
rowSums(tab)
sum(rowSums(tab))
round(table(inc.sub$win.prior, inc.sub$race.prior) *100 / rowSums(tab), 1)
round(colSums(tab) *100 / sum(rowSums(tab)), 1)

# subset: cases NOT allowing reelection in 2018
sel <- which(inc$yr==2018 & (inc$edon==9 | inc$edon==21))
inc.sub <- inc[sel,]

table(inc.sub$edon, inc.sub$race.prior) # by state
table(              inc.sub$race.prior)
nrow(inc.sub)
round(table(inc.sub$race.prior) / nrow(inc.sub),2)

table(inc.sub$win, inc.sub$race.prior) # by incumbent party
tab <- table(inc.sub$win.prior, inc.sub$race.prior)
rowSums(tab)
round(table(inc.sub$win.prior, inc.sub$race.prior) *100 / rowSums(tab), 0)

sel <- which(inc$yr==2018 & inc$edon!=16)
inc.sub <- inc[sel,]

####################
## end blog nexos ##
####################

## 1ago2020: THIS APPEARS DEPRECATED, CLASSIF IS DONE IN RACE.AFTER
## ###############################################################
## # classify term-limited cases according to party returned/not #
## ###############################################################
## duplic.win <- inc$win; duplic.win.prior <- inc$win.prior # duplicate
## inc$win <- gsub("conve", "mc", inc$win)
## inc$win.prior <- gsub("conve", "mc", inc$win.prior)
## #
## inc$manipule <- inc$race.prior # manipulate a copy
## table(inc$manipule)
## # 
## sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
## inc.sub <- inc[sel.tl,] # subset
## # n coalition members
## num <- gsub(pattern = "[^-]*", replacement = "", inc.sub$win, perl = TRUE) # keep hyphens only
## num <- sapply(num, function(x) nchar(x)+1); names(num) <- NULL #n hyphens
## sel <- which(num==7) # subset coals with seven members
## for (i in sel){
##     tmp7 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\7", inc.sub$win[i])
##     tmp6 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\6", inc.sub$win[i])
##     tmp5 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\5", inc.sub$win[i])
##     tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
##     tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
##     tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
##     tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
##     if ( length(grep(pattern = tmp7, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp6, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp5, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
##         inc.sub$manipule[i] <- "Term-limited-p-won"
##     } else {
##         inc.sub$manipule[i] <- "Term-limited-p-lost"
##     }
## }
## sel <- which(num==6) # subset coals with six members
## for (i in sel){
##     tmp6 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\6", inc.sub$win[i])
##     tmp5 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\5", inc.sub$win[i])
##     tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
##     tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
##     tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
##     tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
##     if ( length(grep(pattern = tmp6, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp5, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
##         inc.sub$manipule[i] <- "Term-limited-p-won"
##     } else {
##         inc.sub$manipule[i] <- "Term-limited-p-lost"
##     }
## }
## sel <- which(num==5) # subset coals with five members
## for (i in sel){
##     tmp5 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\5", inc.sub$win[i])
##     tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
##     tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
##     tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
##     tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
##     if ( length(grep(pattern = tmp5, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
##         inc.sub$manipule[i] <- "Term-limited-p-won"
##     } else {
##         inc.sub$manipule[i] <- "Term-limited-p-lost"
##     }
## }
## sel <- which(num==4) # subset coals with four members
## for (i in sel){
##     tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
##     tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
##     tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
##     tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
##     if ( length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
##         inc.sub$manipule[i] <- "Term-limited-p-won"
##     } else {
##         inc.sub$manipule[i] <- "Term-limited-p-lost"
##     }
## }
## sel <- which(num==3) # subset coals with three members
## for (i in sel){
##     tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
##     tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
##     tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
##     if ( length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
##         inc.sub$manipule[i] <- "Term-limited-p-won"
##     } else {
##         inc.sub$manipule[i] <- "Term-limited-p-lost"
##     }
## }
## sel <- which(num==2) # subset coals with two members
## for (i in sel){
##     tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
##     tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
##     if ( length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
##         inc.sub$manipule[i] <- "Term-limited-p-won"
##     } else {
##         inc.sub$manipule[i] <- "Term-limited-p-lost"
##     }
## }
## sel <- which(num==1) # subset coals with single member
## for (i in sel){
##     tmp1 <- gsub("^([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
##     if ( length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
##         inc.sub$manipule[i] <- "Term-limited-p-won"
##     } else {
##         inc.sub$manipule[i] <- "Term-limited-p-lost"
##     }
## }
## inc[sel.tl,] <- inc.sub # return to data
## inc$race.prior <- inc$manipule # return to data 
## inc$win <- duplic.win; inc$win.prior <- duplic.win.prior  # return unmanipulated winners
## table(inc$race.prior[inc$win!="" & inc$win.prior!=""]) # check classification
## rm(tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, sel.tl, sel.e, sel.m, sel.7, duplic.win, duplic.win.prior)
## #
## ## table(inc$manipule[inc$win!="" & inc$win.prior!=""]) # debug
## table(inc$yr)                                   # debug
## table(inc.sub$win2)                                   # debug
## table(inc$win2, inc$race.prior)                                   # debug
## inc.sub
## ## data.frame(inc.sub$win.prior[sel], inc.sub$win[sel]) # debug
## ## data.frame(inc.sub$win.prior[sel1], inc.sub$win[sel1]) # debug


## 1ago2020: ALSO DEPRECATED, INFO IS IN RACE.AFTER
## #############################################
## # classify create dummy for reelected party #
## #############################################
## inc$dptyReelected <- NA
## #
## duplic.win <- inc$win; duplic.win.prior <- inc$win.prior # duplicate
## inc$win <- gsub("conve", "mc", inc$win)
## inc$win.prior <- gsub("conve", "mc", inc$win.prior)
## #
## # CASES WITH NO MISSING INFO
## sel.sub <- which(inc$win!="" & inc$win.prior!="" & is.na(inc$win)==FALSE & is.na(inc$win.prior)==FALSE)
## inc.sub <- inc[sel.sub,] # subset
## # n coalition members
## num <- gsub(pattern = "[^-]*", replacement = "", inc.sub$win, perl = TRUE) # keep hyphens only
## num <- sapply(num, function(x) nchar(x)+1); names(num) <- NULL #n hyphens
## table(num)
## sel <- which(num==7) # subset coals with seven members
## for (i in sel){
##     tmp7 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\7", inc.sub$win[i])
##     tmp6 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\6", inc.sub$win[i])
##     tmp5 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\5", inc.sub$win[i])
##     tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
##     tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
##     tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
##     tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
##     if ( length(grep(pattern = tmp7, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp6, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp5, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
##         inc.sub$dptyReelected[i] <- 1
##     } else {
##         inc.sub$dptyReelected[i] <- 0
##     }
## }
## sel <- which(num==6) # subset coals with six members
## for (i in sel){
##     tmp6 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\6", inc.sub$win[i])
##     tmp5 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\5", inc.sub$win[i])
##     tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
##     tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
##     tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
##     tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
##     if ( length(grep(pattern = tmp6, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp5, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
##         inc.sub$dptyReelected[i] <- 1
##     } else {
##         inc.sub$dptyReelected[i] <- 0
##     }
## }
## sel <- which(num==5) # subset coals with five members
## for (i in sel){
##     tmp5 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\5", inc.sub$win[i])
##     tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
##     tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
##     tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
##     tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
##     if ( length(grep(pattern = tmp5, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
##         inc.sub$dptyReelected[i] <- 1
##     } else {
##         inc.sub$dptyReelected[i] <- 0
##     }
## }
## sel <- which(num==4) # subset coals with four members
## for (i in sel){
##     tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
##     tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
##     tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
##     tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
##     if ( length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
##         inc.sub$dptyReelected[i] <- 1
##     } else {
##         inc.sub$dptyReelected[i] <- 0
##     }
## }
## sel <- which(num==3) # subset coals with three members
## for (i in sel){
##     tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
##     tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
##     tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
##     if ( length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
##         inc.sub$dptyReelected[i] <- 1
##     } else {
##         inc.sub$dptyReelected[i] <- 0
##     }
## }
## sel <- which(num==2) # subset coals with two members
## for (i in sel){
##     tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
##     tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
##     if ( length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
##          length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
##         inc.sub$dptyReelected[i] <- 1
##     } else {
##         inc.sub$dptyReelected[i] <- 0
##     }
## }
## sel <- which(num==1) # subset coals with single member
## for (i in sel){
##     tmp1 <- gsub("^([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
##     if ( length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
##         inc.sub$dptyReelected[i] <- 1
##     } else {
##         inc.sub$dptyReelected[i] <- 0
##     }
## }
## inc[sel.sub,] <- inc.sub # return to data
## inc$win <- duplic.win; inc$win.prior <- duplic.win.prior  # return unmanipulated winners
## table(inc$dptyReelected[inc$win!="" & inc$win.prior!="" & is.na(inc$win)==FALSE & is.na(inc$win.prior)==FALSE], useNA = "always") # check classification
## rm(tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, sel.sub, sel.e, sel.m, sel.7, duplic.win, duplic.win.prior)
## #
## ## table(inc$manipule[inc$win!="" & inc$win.prior!=""]) # debug
## ## table(inc.sub$win)                                   # debug
## ## data.frame(inc.sub$win.prior[sel], inc.sub$win[sel]) # debug
## ## data.frame(inc.sub$win.prior[sel1], inc.sub$win[sel1]) # debug

################################################
## ########################################## ##
## ## SCRIPT FROM incumbents.r *ENDS* HERE ## ##
## ########################################## ##
################################################

# HOUSECLEANING
rm(e,inc.e,inc.m,inc.sub,m,M,mm,sel,sel1,sel2,sel.e,sel.m,tab,tmp)


# ORDINARY ELECTION YEARS FOR ALL STATES --- INAFED HAS YRIN INSTEAD OF YR
# 4ago2020: PROBABLY REDUNDANT SINCE emm NOW IDENTIFIES CYCLE REGARDLESS OF YEAR
# extract election yrs
tmp <- inc[, c("edon","yr","dextra")]
tmp <- tmp[tmp$dextra==0,] # drop extraordinaria years
tmp$dextra <- NULL
tmp$tmp <- paste(tmp$edon, tmp$yr, sep = "-")
tmp$tmp2 <- duplicated(tmp$tmp)
tmp <- tmp[tmp$tmp2==FALSE,]
tmp$dinc <- 1 # identify original obs
tmp$tmp <- tmp$tmp2 <- NULL
el.yrs <- tmp # rename
#
## # done by hand (1ago2020: no longer needed, now feeds from calenariosReeleccion)
## calendar <- list(
##     ags=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,          2019),
##     bc= c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,          2019),
##     bcs=c(     1990,          1993,          1996,          1999,          2002,          2005,          2008,          2011,               2015,          2018),
##     cam=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
##     coa=c(     1990,          1993,          1996,          1999,          2002,          2005,               2009,               2013,               2017,2018),
##     col=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
##     cps=c(          1991,               1995,          1998,          2001,          2004,          2007,          2010,     2012,          2015,          2018),
##     cua=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018),
##     df= c(                                                       2000,          2003,          2006,          2009,          2012,          2015,          2018),
##     dgo=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,           2019),
##     gua=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
##     gue=c(1989,               1993,          1996,          1999,          2002,          2005,          2008,               2012,          2015,          2018),
##     hgo=c(     1990,          1993,          1996,          1999,          2002,          2005,          2008,          2011,                    2016),
##     jal=c(               1992,          1995,     1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
##     mex=c(     1990,          1993,          1996,               2000,          2003,          2006,          2009,          2012,          2015,          2018),
##     mic=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,               2011,               2015,          2018),
##     mor=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
##     nay=c(     1990,          1993,          1996,          1999,          2002,          2005,          2008,          2011,          2014,          2017),
##     nl= c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
##     oax=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018),
##     pue=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,                    2018),
##     que=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
##     qui=c(     1990,          1993,          1996,          1999,          2002,          2005,          2008,     2010,          2013,          2016,     2018),
##     san=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
##     sin=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018),
##     son=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
##     tab=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
##     tam=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018),
##     tla=c(          1991,          1994,               1998,          2001,          2004,          2007,          2010,          2013,          2016),
##     ver=c(          1991,          1994,          1997,          2000,               2004,          2007,          2010,          2013,                2017),
##     yuc=c(     1990,          1993,     1995,          1998,          2001,          2004,          2007,          2010,     2012,          2015,          2018),
##     zac=c(               1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018)
## )
#
calendar <- read.csv("../../calendariosReelec/fechasEleccionesMexicoDesde1994.csv", stringsAsFactors = FALSE) # fechasEleccionesMexicoDesde1994 is csv-friendly version of calendarioConcurrenciasMex05
#calendar[1,]
sel <- grep("^ord$|^edon$|^elec$|y[0-9]{4}", colnames(calendar), perl = TRUE) # drop useless variables
calendar <- calendar[,sel]
sel <- which(calendar$elec=="ayun") # keep ayun lines only
calendar <- calendar[sel,]
#
sel <- grep("y[0-9]{4}", colnames(calendar), perl = TRUE) # select year vars
tmp <- list() # will receive elyrs by state
# fill list
for (i in 1:32){
    #i <- 1 # debug
    tmp2 <- calendar[i,sel]
    tmp2 <- as.numeric(sub("y","",colnames(tmp2[which(tmp2!="--")]))) # select cells with dates and read yr from colname
    tmp2 <- tmp2[tmp2<=2020] # keep only up to 2020 (current) year
    tmp[[i]] <- tmp2 # plug into list
}
calendar <- tmp # replace ordinary calendar with list
#
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
calendar <- cal # keep data frame only
rm(cal)

############################
## dummy party reelected  ##
############################
inc$dpwon.prior <- NA # will receive info
#
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- which(is.na(inc$win.long.prior[sel])==TRUE)
sel2 <- which(is.na(inc$win.long[sel])==TRUE)
tmp[union(sel1,sel2)] <- 99 # either is NA (will be returned to NA later, easier to debug)
sel1 <- which(inc$win.long.prior[sel]=="0")
sel2 <- which(inc$win.long[sel]=="0")
tmp[union(sel1,sel2)] <- 99 # either
inc$dpwon.prior[sel] <- tmp # return to data after manipulation 
#
# uses win not win.long
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("anulada|consejoMunic|litigio|uyc|0",inc$win.prior[sel])
sel2 <- grep("anulada|consejoMunic|litigio|uyc|0",inc$win[sel])
tmp[union(sel1,sel2)] <- 0 # either
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
sel <- which(inc$emm=="cps-16.064"); inc[sel,]
x
#
# uses win not win.long
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("indep",inc$win.prior[sel])
sel2 <- grep("indep",inc$win[sel])
tmp[union(sel1,sel2)] <- 0 # one or the other (or both, changed next)
sel3 <- intersect(sel1,sel2) # both
sel4 <- which(inc$race.prior[sel]=="Incumb-remained")
tmp[intersect(sel3,sel4)] <- 1 # both (independent incumbent reelected coded as party won)
inc$dpwon.prior[sel] <- tmp # return to data after manipulation 
#
##############################################################################################################
## if following block covers all individual winning parties (solo or in coalition), then NAs must be zeroes ##
##############################################################################################################
# list winners (break to individual parties separately by hand)
tmp <- unique(inc$win.long)
tmp[order(tmp)]
#
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("adc",inc$win.long.prior[sel])
sel2 <- grep("adc",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("ave",inc$win.long.prior[sel])
sel2 <- grep("ave",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("cc1",inc$win.long.prior[sel])
sel2 <- grep("cc1",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("cp",inc$win.long.prior[sel])
sel2 <- grep("cp",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("cpp",inc$win.long.prior[sel])
sel2 <- grep("cpp",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("dsppn",inc$win.long.prior[sel])
sel2 <- grep("dsppn",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("fc1",inc$win.long.prior[sel])
sel2 <- grep("fc1",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("fc1",inc$win.long.prior[sel])
sel2 <- grep("fc1",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("fc1",inc$win.long.prior[sel])
sel2 <- grep("fc1",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("fdn",inc$win.long.prior[sel])
sel2 <- grep("fdn",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("indep",inc$win.long.prior[sel])
sel2 <- grep("indep",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("mas",inc$win.long.prior[sel])
sel2 <- grep("mas",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("^mc$|-mc|mc-",inc$win.long.prior[sel])
sel2 <- grep("^mc$|-mc|mc-",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("morena",inc$win.long.prior[sel])
sel2 <- grep("morena",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("npp",inc$win.long.prior[sel])
sel2 <- grep("npp",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pac1",inc$win.long.prior[sel])
sel2 <- grep("pac1",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pan",inc$win.long.prior[sel])
sel2 <- grep("pan",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("parm",inc$win.long.prior[sel])
sel2 <- grep("parm",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pasd",inc$win.long.prior[sel])
sel2 <- grep("pasd",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("paz",inc$win.long.prior[sel])
sel2 <- grep("paz",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pcc",inc$win.long.prior[sel])
sel2 <- grep("pcc",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pcd1",inc$win.long.prior[sel])
sel2 <- grep("pcd1",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pcdt",inc$win.long.prior[sel])
sel2 <- grep("pcdt",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pchu",inc$win.long.prior[sel])
sel2 <- grep("pchu",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pcm2",inc$win.long.prior[sel])
sel2 <- grep("pcm2",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pcp",inc$win.long.prior[sel])
sel2 <- grep("pcp",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pcpp",inc$win.long.prior[sel])
sel2 <- grep("pcpp",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pd1",inc$win.long.prior[sel])
sel2 <- grep("pd1",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pdm",inc$win.long.prior[sel])
sel2 <- grep("pdm",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pebc",inc$win.long.prior[sel])
sel2 <- grep("pebc",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pec",inc$win.long.prior[sel])
sel2 <- grep("pec",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pes",inc$win.long.prior[sel])
sel2 <- grep("pes",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pfcrn",inc$win.long.prior[sel])
sel2 <- grep("pfcrn",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pfd1",inc$win.long.prior[sel])
sel2 <- grep("pfd1",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("ph",inc$win.long.prior[sel])
sel2 <- grep("ph",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("ph_bcs",inc$win.long.prior[sel])
sel2 <- grep("ph_bcs",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pj1",inc$win.long.prior[sel])
sel2 <- grep("pj1",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pjs",inc$win.long.prior[sel])
sel2 <- grep("pjs",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pl1",inc$win.long.prior[sel])
sel2 <- grep("pl1",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("plm",inc$win.long.prior[sel])
sel2 <- grep("plm",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pmch",inc$win.long.prior[sel])
sel2 <- grep("pmch",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pmp",inc$win.long.prior[sel])
sel2 <- grep("pmp",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pmr",inc$win.long.prior[sel])
sel2 <- grep("pmr",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pmt",inc$win.long.prior[sel])
sel2 <- grep("pmt",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pna",inc$win.long.prior[sel])
sel2 <- grep("pna",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("poc",inc$win.long.prior[sel])
sel2 <- grep("poc",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("ppc",inc$win.long.prior[sel])
sel2 <- grep("ppc",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("ppg",inc$win.long.prior[sel])
sel2 <- grep("ppg",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("ppro",inc$win.long.prior[sel])
sel2 <- grep("ppro",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pps",inc$win.long.prior[sel])
sel2 <- grep("pps",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pps",inc$win.long.prior[sel])
sel2 <- grep("pps",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("ppt",inc$win.long.prior[sel])
sel2 <- grep("ppt",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("prc",inc$win.long.prior[sel])
sel2 <- grep("prc",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("prd",inc$win.long.prior[sel])
sel2 <- grep("prd",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pri",inc$win.long.prior[sel])
sel2 <- grep("pri",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("prs",inc$win.long.prior[sel])
sel2 <- grep("prs",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("prt",inc$win.long.prior[sel])
sel2 <- grep("prt",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("prv",inc$win.long.prior[sel])
sel2 <- grep("prv",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("ps1",inc$win.long.prior[sel])
sel2 <- grep("ps1",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("psd1",inc$win.long.prior[sel])
sel2 <- grep("psd1",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("psdc",inc$win.long.prior[sel])
sel2 <- grep("psdc",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("psi",inc$win.long.prior[sel])
sel2 <- grep("psi",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("psn",inc$win.long.prior[sel])
sel2 <- grep("psn",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("^pt$|-pt|pt-",inc$win.long.prior[sel])
sel2 <- grep("^pt$|-pt|pt-",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pudc",inc$win.long.prior[sel])
sel2 <- grep("pudc",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pup",inc$win.long.prior[sel])
sel2 <- grep("pup",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pvem",inc$win.long.prior[sel])
sel2 <- grep("pvem",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("pver",inc$win.long.prior[sel])
sel2 <- grep("pver",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("si",inc$win.long.prior[sel])
sel2 <- grep("si",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("trans",inc$win.long.prior[sel])
sel2 <- grep("trans",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
# 
sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
sel1 <- grep("via_radical",inc$win.long.prior[sel])
sel2 <- grep("via_radical",inc$win.long[sel])
tmp[intersect(sel1,sel2)] <- 1 # both
inc$dpwon.prior[sel] <- tmp # return to data after manipulation
#####################
## block ends here ##
#####################
#
# remaining NAs must be zeroes
## sel <- which(is.na(inc$dpwon.prior)==TRUE & inc$win.prior=="pan") # debug
## table(inc$win.long[sel], inc$win.long.prior[sel]) # debug
## table(inc$win.long.prior[sel]) # debug
sel <- which(is.na(inc$dpwon.prior)==TRUE);
inc$dpwon.prior[sel] <- 0
#
# return 99s to NA
sel <- which(inc$dpwon.prior==99);
inc$dpwon.prior[sel] <- NA
#
# unlag
library(DataCombine) # easy lags with slide
inc <- inc[order(inc$ife),] # verify sorted before lags
inc <- slide(inc, Var = "dpwon.prior", NewVar = "dpwon", GroupVar = "ife", slideBy = +1) # lead by one period
# these lag NAs can be filled with current info
sel <- which(is.na(inc$dpwon) & inc$race.after=="uyc")
inc$dpwon[sel] <- 0
#
# verify
table(inc$race.after, inc$dpwon, useNA = "always")
#
# the dpwon dummy identifies cases where Beaten but party won
sel <- which(inc$race.after=="Beaten" & inc$dpwon==1)
data.frame(inc$emm[sel], inc$note[sel])
# and cases where Reelected but party lost
sel <- which(inc$race.after=="Reelected" & inc$dpwon==0)
data.frame(inc$emm[sel], inc$note[sel])
#
## # original attempt to code 1s went thus (deprecated)
## sel <- which(is.na(inc$dpwon.prior)==TRUE); tmp <- inc$dpwon.prior[sel] # extract for manipulation
## sel1 <- grep("pan",inc$win.long.prior[sel])
## sel2 <- grep("pan",inc$win.long[sel])
## tmp[intersect(sel1,sel2)] <- 1 # both
## inc$dpwon.prior[sel] <- tmp # return to data after manipulation 

# if a party returns after current term, which is it?
# dpwon.prior==1 if race.prior winner includes incumbent 
inc$returning.p.prior <- NA
sel <- which(is.na(inc$dpwon.prior) | is.na(inc$dpwon))
inc$returning.p.prior[sel] <- "pending"
sel <- which(inc$dpwon.prior==0)
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

# clean: drop lags
inc <- inc[,-grep("prior", colnames(inc))]
rm(tmp,tmp0,tmp1,tmp2,i,sel,sel0,sel1,sel2,sel3,sel4,min.cal)
inc$fuente <- NULL
head(inc)


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
## ## # add dummy to drop dipernadores after names have been searched
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
## options(width = 130)
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

#########################################
## ################################### ##
## ##                               ## ##
## ## INCUMBENT DATA PREP ENDS HERE ## ##
## ##                               ## ##
## ################################### ##
#########################################




########################################
## ################################## ##
## ##                              ## ##
## ## VOTING DATA PREP STARTS HERE ## ##
## ##                              ## ##
## ################################## ##
########################################

######################################
## ################################ ##
## ##                            ## ##
## ## VOTING DATA PREP ENDS HERE ## ##
## ##                            ## ##
## ################################ ##
######################################


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


