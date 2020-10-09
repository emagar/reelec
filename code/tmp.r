Necesito conservar win.prior en inc/vot para que lo herede tmp ---- me permitirá recodificar win para left...


#inc.dupli <- inc # duplicate for debug
#inc <- inc.dupli # restore
# lag win
library(DataCombine) # easy lags with slide
inc <- inc[order(inc$ife, inc$emm),] # verify sorted before lags
inc <- slide(inc, Var = "win", NewVar = "win.after", GroupVar = "ife", slideBy = +1) # lead by one period
# win.left, win.after.left, race.after.left (overestimates reelection cases, just like race.after misses some where perredista went to morena) 
inc$win.left <- inc$win
inc$win.after.left <- inc$win.after
inc$race.after.left <- inc$race.after
# recode left cases only, remaining prds were take care in excel and appear in race.after
sel1 <- grep("left", inc$win.left)
sel2 <- grep("left", inc$win.after.left)
sel <- intersect(sel1, sel2) # both
tmp <- inc$race.after.left[sel] # subset for manipulation
tmp <- sub("lost","won",tmp)
table(tmp) # ojo: retains Beaten --- ie cases where perredista incumbent lost to morena
inc$race.after.left[sel] <- tmp # return post manipulation
# incumbent in ballot, incumbent won
inc$dincran.after <- 0
sel <- which(inc$race.after=="Beaten" | inc$race.after=="Reelected")
inc$dincran.after[sel] <- 1
inc$dincwon.after <- 0
sel <- which(inc$race.after=="Reelected")
inc$dincwon.after[sel] <- 1
# party won
inc$dinptywon.after <- 0
sel <- grep("p-won|Reelected",inc$race.after)
inc$dinptywon.after[sel] <- 1
inc$dinptywon.after.left <- 0
sel <- grep("p-won",inc$race.after.left)
inc$dinptywon.after.left[sel] <- 1
# lag to get current versions
inc <- inc[order(inc$ife, inc$emm),] # verify sorted before lags
inc <- slide(inc, Var = "race.after", NewVar = "race.current", GroupVar = "ife", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "race.after.left", NewVar = "race.current.left", GroupVar = "ife", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "dincran.after", NewVar = "dincran.current", GroupVar = "ife", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "dincwon.after", NewVar = "dincwon.current", GroupVar = "ife", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "dinptywon.after", NewVar = "dinptywon.current", GroupVar = "ife", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "dinptywon.after.left", NewVar = "dinptywon.current.left", GroupVar = "ife", slideBy = -1) # lag by one period
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
inc$race.prior <- NULL # it's not prior, its current
#table(inc$win.prior, inc$win.current)
inc$win.prior <- NULL # it is prior, but not needed
# inc$dptywon.current is original inc$dpwon.prior with less info, update
sel <- which(inc$dpwon.prior==1 & inc$dinptywon.current==0) # cases where win.long has the returning party
inc$dinptywon.current[sel] <- 1                             # cases where win.long has the returning party
sel <- which(inc$dpwon.prior==0 & inc$dinptywon.current==1) # cases where incumbent reelected with other pty
inc$dinptywon.current[sel] <- 0                             # cases where incumbent reelected with other pty
sel <- which(inc$dpwon.prior==0 & is.na(inc$dinptywon.current)==TRUE) # new municipalities
inc$dinptywon.current[sel] <- 0                                       # new municipalities
sel <- which(inc$dpwon.prior==1 & is.na(inc$dinptywon.current)==TRUE) # new municipalities
inc$dinptywon.current[sel] <- 1                                       # new municipalities
table(inc$dinptywon.current, inc$dpwon.prior, useNA = "always")
inc$dpwon.prior <- NULL # drop redundant
# remaining NAs are all start-of-series in early 1990s
sel <- which(is.na(inc$dinptywon.current)==TRUE) # 
table(sub("^[a-z]+-([0-9]{2})[.0-9]+$", "\\1", inc$emm[sel])) # cycle they occur in


colnames(inc)

#
#drop all after versions, unneeded for analysis of current period
inc$race.after         <- NULL 
inc$race.after.left    <- NULL
inc$dinptywon.after.left <- NULL
inc$dincran.after      <- NULL
inc$dincwon.after      <- NULL
inc$dinptywon.after      <- NULL
inc$win.after          <- NULL
inc$win.after.left     <- NULL
#
# sort columns
sel <- c("emm","ord","mun","yr","dextra","dy","mo","edon","munn","ife", "inegi", "incumbent","mg","pty2nd","runnerup", "win.long", "returning.p", "win.current", "win.current.left", "race.current", "race.current.left", "dptywon.current", "dptywon.current.left", "dincran.current", "dincwon.current", "dptywon.current")
inc <- inc[,sel]




# sort columns
c("emm", "ord","mun","yr","dextra","dy","mo","edon","munn", "ife","inegi",
  "win","incumbent","race.after",
  "runnerup","pty2nd","mg","note", "drepe","drepg","who","check",
  "win.long",
  "race.prior","win.prior","dpwon.prior",
  "dpwon", "returning.p",
  "win.left",
  "race.after.left","dincran.after","dincwon.after","dptywon.after","dptywon.after.left",
  "race.prior.left","dincwon.prior", "dincran.prior","dptywon.prior","dptywon.prior.left")


colnames(inc)
table(inc$dptywon.after, inc$dptywon.after.left)



NEED TO UNLAG AFTERWARDS...

# unlag
library(DataCombine) # easy lags with slide
inc <- inc[order(inc$ife, inc$emm),] # verify sorted before lags
inc <- slide(inc, Var = "dpwon.prior", NewVar = "dpwon.manip", GroupVar = "ife", slideBy = +1) # lead by one period
table(inc$dpwon[sel])       # debug
table(inc$dpwon.manip[sel]) # debug
table(inc$dpwon[-sel])       # debug --- some of these also change!!!
table(inc$dpwon.manip[-sel]) # debug

# debug
options(width=199)
tmp <- inc[-sel,]
tmp[which(tmp$dpwon!=tmp$dpwon.manip),]
inc[grep("mic-[0-9]{2}[.]105", inc$emm),]

1. recode race after given left
2. compute dincran.after
3. compute dincwon.after
4. compute dptywon.after
5. lag 234 to prior
