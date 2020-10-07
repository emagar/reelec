Necesito conservar win.prior en inc/vot para que lo herede tmp ---- me permitirá recodificar win para left...


#inc.dupli <- inc # duplicate for debug
#inc <- inc.dupli # restore
# lag win
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
inc$dptywon.after <- 0
sel <- grep("p-won",inc$race.after)
inc$dptywon.after[sel] <- 1
inc$dptywon.after.left <- 0
sel <- grep("p-won",inc$race.after.left)
inc$dptywon.after.left[sel] <- 1
# lag to get current versions
inc <- inc[order(inc$ife, inc$emm),] # verify sorted before lags
inc <- slide(inc, Var = "race.after", NewVar = "race.current", GroupVar = "ife", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "race.after.left", NewVar = "race.current.left", GroupVar = "ife", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "dincran.after", NewVar = "dincran.current", GroupVar = "ife", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "dincwon.after", NewVar = "dincwon.current", GroupVar = "ife", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "dptywon.after", NewVar = "dptywon.current", GroupVar = "ife", slideBy = -1) # lag by one period
inc <- slide(inc, Var = "dptywon.after.left", NewVar = "dptywon.current.left", GroupVar = "ife", slideBy = -1) # lag by one period
# rename current vars
inc$win.current <- inc$win; inc$win <- NULL
inc$win.current.left <- inc$win.left; inc$win.left <- NULL
#drop all after versions, unneeded for analysis of current period
inc$race.after         <- NULL 
inc$race.after.left    <- NULL
inc$dptywon.after.left <- NULL
inc$dincran.after      <- NULL
inc$dincwon.after      <- NULL
inc$dptywon.after      <- NULL
inc$win.after          <- NULL
inc$win.after.left     <- NULL
# drop name-searching ancillary
inc$note <- NULL
inc$drepe <- NULL
inc$drepg <- NULL
inc$who <- NULL
inc$check <- NULL
# sort columns
c("emm","ord","mun","yr","dextra","dy","mo","edon","munn","ife", "inegi",
"incumbent","mg","pty2nd","runnerup", "win.long",

"race.prior","win.prior","dpwon.prior","dpwon","returning.p", # from 1st round of coding inc, redo/drop

"win.current",
"win.current.left",
"race.current", 
"race.current.left", 
"dptywon.current",
"dptywon.current.left", 
"dincran.current", 
"dincwon.current", 
"dptywon.current")

colnames(inc)
table(inc$race.prior)
inc$win.current




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
