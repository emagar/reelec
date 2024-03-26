## replicate lucardi rosas
##
## rename prd/morena as left. left is prd pre-2015, morena since 2018, or either in between
luro$win       <- sub("morena", "left", luro$win)
luro$part2nd   <- sub("morena", "left", luro$part2nd)
luro$win.prior <- sub("morena", "left", luro$win.prior)
luro$run.prior <- sub("morena", "left", luro$run.prior)
luro$labs.prior <- sub("morena", "left", luro$labs.prior)
ltmp <- luro[luro$yr < 2015,]
ltmp$win       <- sub("prd", "left", ltmp$win)
ltmp$part2nd   <- sub("prd", "left", ltmp$part2nd)
ltmp$win.prior <- sub("prd", "left", ltmp$win.prior)
ltmp$run.prior <- sub("prd", "left", ltmp$run.prior)
ltmp$labs.prior <- sub("prd", "left", ltmp$labs.prior)
ltmp -> luro[luro$yr < 2015,]
rm(ltmp)
## DVs
luro$dpanwin    <- 0; luro$dpanwin   [grep("pan"   , luro$win)] <- 1
luro$dpriwin    <- 0; luro$dpriwin   [grep("pri"   , luro$win)] <- 1
luro$dleftwin   <- 0; luro$dleftwin  [grep("left"  , luro$win)] <- 1
luro$dprdwin    <- 0; luro$dprdwin   [grep("prd"   , luro$win)] <- 1
luro$dpvemwin   <- 0; luro$dpvemwin  [grep("pvem"  , luro$win)] <- 1
luro$dptwin     <- 0; luro$dptwin    [grep("pt"    , luro$win)] <- 1
luro$dmcwin     <- 0; luro$dmcwin    [grep("mc"    , luro$win)] <- 1
luro <- within(luro, dothwin <- 1-dpanwin-dpriwin-dleftwin-dprdwin-dpvemwin-dptwin-dmcwin); luro$dothwin[luro$dothwin < 0] <- 0
## 
## pre-selectors (still need to filter margin)
luro$dselpan <- 0
tmp <- grep("pan", luro$win.prior)
luro$dselpan[tmp] <- 1
tmp <- grep("pan", luro$run.prior)
luro$dselpan[tmp] <- 1
##
luro$dselpri <- 0
tmp <- grep("pri", luro$win.prior)
luro$dselpri[tmp] <- 1
tmp <- grep("pri", luro$run.prior)
luro$dselpri[tmp] <- 1
##
luro$dselleft <- 0
tmp <- grep("left", luro$win.prior)
luro$dselleft[tmp] <- 1
tmp <- grep("left", luro$run.prior)
luro$dselleft[tmp] <- 1
##
luro$dselinc <- 0
????
##
## Re-compute margin according to party's 1st/runner-up status
luro$mgpan <- NA
tmp <- grep("pan",  luro$win.prior)
luro$mgpan[tmp] <-  luro$mg.prior[tmp]
tmp <- grep("pan",  luro$run.prior)
luro$mgpan[tmp] <- -luro$mg.prior[tmp]
luro$mgpan[luro$dselpan==0] <- NA
##
luro$mgpri <- NA
tmp <- grep("pri", luro$win.prior)
luro$mgpri[tmp] <- luro$mg.prior[tmp]
tmp <- grep("pri", luro$run.prior)
luro$mgpri[tmp] <- -luro$mg.prior[tmp]
luro$mgpri[luro$dselpri==0] <- NA
##
luro$mgleft <- NA
tmp <- grep("left", luro$win.prior)
luro$mgleft[tmp] <- luro$mg.prior[tmp]
tmp <- grep("left", luro$run.prior)
luro$mgleft[tmp] <- -luro$mg.prior[tmp]
luro$mgleft[luro$dselleft==0] <- NA
##
## left=prd|morena in 2015:17 generates no 1st/2nd left overlap
intersect(grep("left", luro$win), grep("left", luro$part2nd))
