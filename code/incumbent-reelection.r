######################################################################
## ################################################################ ##
## ## SCRIPT FROM ../elecReturns/code/incumbents.r *STARTS* HERE ## ##
## ## 1aug2020                                                   ## ##
## ################################################################ ##
######################################################################

options(width = 120)

rm(list = ls())
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data/"
setwd(dd)

###########################################
## ##################################### ##
## ##                                 ## ##
## ## INCUMBENT DATA PREP STARTS HERE ## ##
## ##      |         |         |      ## ##
## ##      |         |         |      ## ##
## ##      V         V         V      ## ##
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
inc$win2 <- sub("conve", "mc",   inc$win2, ignore.case = TRUE)
inc$win2 <- sub("panal", "pna", inc$win2, ignore.case = TRUE)
inc$win2 <- sub("pucd", "pudc", inc$win2, ignore.case = TRUE) # typo
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
# check that no cycles are missing in any municipio
tmp <- data.frame(emm=inc$emm, cycle=NA)
tmp$cycle <- as.numeric(sub("^.+-([0-9]{2})[.][0-9]+", "\\1", tmp$emm))
tmp$id <- sub("^(.+)-[0-9]{2}[.]([0-9]+)", "\\1\\2", tmp$emm)
library(DataCombine) # easy lags with slide
tmp <- tmp[order(tmp$emm),] # check sorted for lags
tmp <- slide(data = tmp, TimeVar = "cycle", GroupVar = "id", Var = "cycle", NewVar = "cycle.lag",    slideBy = -1) # lag by one period
tmp$dif <- tmp$cycle - tmp$cycle.lag - 1 # zeroes indicate time series ok
tmp$dif[is.na(tmp$dif)] <- 0
tmp$suma <- ave(tmp$dif, as.factor(tmp$id), FUN=sum, na.rm=TRUE)
table(tmp$suma) # all zeroes implies no gaps
#
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
## ##      ^         ^         ^    ## ##
## ##      |         |         |    ## ##
## ##      |         |         |    ## ##
## ## INCUMBENT DATA PREP ENDS HERE ## ##
## ##                               ## ##
## ################################### ##
#########################################




###########################################################
## ##################################################### ##
## ##                                                 ## ##
## ## VOTING DATA AND ELEC HISTORIES PREP STARTS HERE ## ##
## ##         |         |         |         |         ## ##
## ##         |         |         |         |         ## ##
## ##         V         V         V         v         ## ##
## ##################################################### ##
###########################################################

gd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/data/"

# load municipal votes
vot <- read.csv("aymu1989-present.coalAgg.csv", stringsAsFactors = FALSE)
#vot[1,]
vot <- within(vot, fuente <- notas <- tot <- nr <- nulos <- NULL)
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
# ojo: 88 returned from uyc in 2013
sel <- which(vot$edon==20 & vot$munn %in% tmp)
## tmp <- vot$emm[sel]
## tmp[order(tmp)]
if (length(sel)>0) vot <- vot[-sel,]

# drop oxchuc in 2018, went usos y costumbres
sel <- which(vot$emm=="cps-17.064")
vot <- vot[-sel,]

# drop hidalgo 2020, will be held 18oct2020
sel <- grep("hgo-16", vot$emm)
vot <- vot[-sel,]

# drop void elections
# 6ago2020: check which don't have extraord data. drop them? would break lags... check in incumbents block too
sel <- grep("[0-9]+a[.]", vot$emm) # anuladas
tmp <- vot$emm[sel]
tmp <- sub("(^.+[0-9])+a[.]", "\\1.", tmp) # drop the a from emm, are these obs in data?
tmp[which(tmp %in% vot$emm)]==tmp # all have extra data if output only TRUEs wo error
vot <- vot[-sel,]

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
# 4ago2020: add pvem?
v5 <- vot # duplicate
v5[1,]
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
vot <- within(vot, v04 <- v05 <- v06 <- v07 <- v08 <- v09 <- v10 <- v11 <- v12 <- v13 <- v14 <- NULL)
vot <- within(vot, l04 <- l05 <- l06 <- l07 <- l08 <- l09 <- l10 <- l11 <- l12 <- l13 <- l14 <- NULL)

## # debug
## save.image(file = "tmp.RData")
## #
## rm(list = ls()) # clean
## dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
## gd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/data/"
## setwd("/home/eric/Desktop/MXelsCalendGovt/reelec/data/")
## load(file = "tmp.RData")

# clean
rm(i,sel,sel1,v5)





#############################
## GET ELECTORAL HISTORIES ##
#############################
vot$vhat.pan <- vot$vhat.pri  <- vot$vhat.left <- vot$alpha.pan  <- vot$alpha.pri <- vot$alpha.left <- NA # open slots
vot$d.pan <- vot$d.pri  <- vot$d.left <- NA # open slots

# save a copy
save.image(paste(wd,"tmp.RData",sep=""))
# load image
rm(list = ls())
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data/"
setwd(dd)
load(paste(wd,"tmp.RData",sep=""))


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

# left residuals: use prd-vhat.left and morena-vhat.left (pan-prd went to pan in 2017 and 2018)
vot$res.pan <- vot$pan - vot$vhat.pan
vot$res.pri <- vot$pri - vot$vhat.pri
vot$res.prd <- vot$prd - vot$vhat.left
vot$res.morena <- vot$morena - vot$vhat.left
sel <- which(vot$yr<2015)
vot$res.morena[sel] <- NA

# inspect vot
vot[2000,]
dim(vot)
table(is.na(vot$vhat.pri), vot$yr)
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
ls()
rm(e,i,last,sel,sel1)
rm(inc) # drop to avoid confusion, useful data has been merged into vot

# save a copy
save.image(paste(wd,"mun-reelection.RData",sep=""))

# load image
rm(list = ls())
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data/"
setwd(dd)

load(paste(wd,"mun-reelection.RData",sep=""))
options(width = 130)

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

## THIS CAN BE DROPPED
## # merge into vot
## vot$sdalt <- vot$meanalt <- vot$wsdalt <- vot$wmeanalt <- NA # open slot for new vars
## for (i in 1:nrow(tmp)){
##     #i <- 1 # debug
##     sel <- which(vot$inegi %in% tmp$inegi[i])
##     if (length(sel)>0){
##         vot$wmeanalt[sel] <- tmp$wmeanalt[i]
##         vot$wsdalt[sel]   <- tmp$wsdalt[i]
##         vot$meanalt[sel] <- tmp$meanalt[i]
##         vot$sdalt[sel]   <- tmp$sdalt[i]
##     }
## }

# make discrete altitude variables for mapping exploration
alt <- tmp
rm(tmp,i,sel)

# read sección-municipio equivalencias
tmp <- read.csv("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", stringsAsFactors = FALSE)
tmp <- tmp[,grep("edon|seccion|inegi|ife|mun[0-9]+",colnames(tmp))] # select columns
#tmp[1,]
#tmp[tmp$edon==1 & tmp$inegi==1010,c("seccion","munn")]
censo <- tmp # rename, will receive state-by-state

# get censo 2010 ptot p5li etc
edos <- c("ags","bc","bcs","cam","coa","col","cps","cua","df","dgo","gua","gue","hgo","jal","mex","mic",
          "mor","nay","nl","oax","pue","que","qui","san","sin","son","tab","tam","tla","ver","yuc","zac")
tmp.dat <- data.frame() # will receive state's rows
for (i in 1:32){
    #i <- 1 # debug
    tmp.dir <- paste("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/secciones/censo2010", edos[i], sep = "/")
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
censo <- censo[, -grep("mun[0-9]", colnames(censo))]

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

# merge altitudes
dim(censo)
dim(alt)
alt$ptot <- NULL

#tmp <- censo # duplicate for debug
censo <- merge(x = censo, y = alt, by = "inegi", all = TRUE)

# make discrete altitude variables for mapping exploration
# script mapa-municipios.r draws wsd(alt) etc

# merge censo into vot
options(width = 175)
sel <- which(colnames(censo) %in% c("ife","edon")) # drop towards merge
vot <- merge(x = vot, y = censo[,-sel], by = "inegi", all.x = TRUE, all.y = FALSE)
rm(censo, censo.sec, i, sel, tmp, tmp.dat, tmp.file, tmp.dir)

# need dipfed and gub votes to interact with concurrence, preferably at mun level, else at state
--> get edo-level first while mun-level available

# compute winner's margin
vot$mg <- round(vot$v01 - vot$v02, 4)
vot$round <- sub(pattern = "[\\w\\D-]+([0-9]{2})[.][0-9]{3}", replacement = "\\1", vot$emm, perl = TRUE)
vot$round <- as.numeric(vot$round)
# dincumbent
vot$dincumbent <- 1 - vot$dopenseat

# capital municipalities
sel <- which(vot$ife  %in%  c( 1001,
                               2002,
                               3003,
                               4001,
                               5030,
                               6001,
                               7102,
                               8019,
#                              90,  
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

# single-term states after reform
vot$dhgover <- as.numeric(vot$edon==13 | vot$edon==30)
# pre-reform dummy
vot$dpreref  <- as.numeric(vot$yr< 2018)
vot$dpostref <- as.numeric(vot$yr>=2018)

# left vote is prd pre-2015, morena in 2015 and post
vot <- within(vot, {
    left = prd
    left[yr>=2015] = morena[yr>=2015]
    res.left = res.prd
    res.left[yr>=2015] = res.morena[yr>=2015]
})





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


