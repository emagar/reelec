rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data/"
setwd(wd)

options(width = 145)

# load incumbent data since 1989
inc <- read.csv(paste(dd, "aymu1989-present.incumbents.csv", sep = ""), stringsAsFactors = FALSE)
## # fix extraordinaria yrs --- became redundant after switch to aymu1989-present.incumbents.csv from aymu1997-present.incumbents.csv
## sel <- which(inc$edon==5 & inc$yr==2010)
## inc$yr[sel] <- 2009
## sel <- which(inc$edon==8 & inc$yr==2002)
## inc$yr[sel] <- 2001
## sel <- which(inc$edon==13 & inc$yr==2009)
## inc$yr[sel] <- 2008
## sel <- which(inc$edon==13 & inc$yr==2012)
## inc$yr[sel] <- 2011
## sel <- which(inc$edon==14 & inc$yr==1998)
## inc$yr[sel] <- 1997
## sel <- which(inc$edon==14 & inc$yr==2004)
## inc$yr[sel] <- 2003
## sel <- which(inc$edon==14 & inc$yr==2007)
## inc$yr[sel] <- 2006
## sel <- which(inc$edon==16 & inc$yr==2008)
## inc$yr[sel] <- 2007
## sel <- which(inc$edon==16 & inc$yr==2012)
## inc$yr[sel] <- 2011
## sel <- which(inc$edon==20 & inc$yr==2005)
## inc$yr[sel] <- 2004
## sel <- which(inc$edon==20 & inc$yr==2017)
## inc$yr[sel] <- 2016
## sel <- which(inc$edon==21 & inc$yr==2005)
## inc$yr[sel] <- 2004
## sel <- which(inc$edon==21 & inc$yr==2014)
## inc$yr[sel] <- 2013
## sel <- which(inc$edon==23 & inc$yr==2009)
## inc$yr[sel] <- 2008
## sel <- which(inc$edon==26 & inc$yr==1998)
## inc$yr[sel] <- 1997
## sel <- which(inc$edon==27 & inc$yr==2016)
## inc$yr[sel] <- 2015
## sel <- which(inc$edon==29 & inc$yr==1999)
## inc$yr[sel] <- 1998
## sel <- which(inc$edon==29 & inc$yr==2002)
## inc$yr[sel] <- 2001
# extract election yrs
tmp <- inc[, c("edon","yr")]
tmp$tmp <- paste(tmp$edon, tmp$yr, sep = "-")
tmp$tmp2 <- duplicated(tmp$tmp)
tmp <- tmp[tmp$tmp2==FALSE,]
tmp$dinc <- 1 # identify original obs
tmp$tmp <- tmp$tmp2 <- NULL
el.yrs <- tmp # rename


# determine election years for all states --- inafed has yrIn instead of yr
calendar <- list(
    ags=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016),
    bc= c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016),
    bcs=c(     1990,          1993,          1996,          1999,          2002,          2005,          2008,          2011,               2015,          2018),
    cam=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    coa=c(     1990,          1993,          1996,          1999,          2002,          2005,               2009,               2013,               2017,2018),
    col=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    cps=c(          1991,               1995,          1998,          2001,          2004,          2007,          2010,     2012,          2015,          2018),
    cua=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018),
    df= c(                                                       2000,          2003,          2006,          2009,          2012,          2015,          2018),
    dgo=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016),
    gua=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    gue=c(1989,               1993,          1996,          1999,          2002,          2005,          2008,               2012,          2015,          2018),
    hgo=c(     1990,          1993,          1996,          1999,          2002,          2005,          2008,          2011,                    2016),
    jal=c(               1992,          1995,     1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    mex=c(     1990,          1993,          1996,               2000,          2003,          2006,          2009,          2012,          2015,          2018),
    mic=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,               2011,               2015,          2018),
    mor=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    nay=c(     1990,          1993,          1996,          1999,          2002,          2005,          2008,          2011,          2014,          2017),
    nl= c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    oax=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018),
    pue=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,                    2018),
    que=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    qui=c(     1990,          1993,          1996,          1999,          2002,          2005,          2008,     2010,          2013,          2016,     2018),
    san=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    sin=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018),
    son=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    tab=c(          1991,          1994,          1997,          2000,          2003,          2006,          2009,          2012,          2015,          2018),
    tam=c(1989,          1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018),
    tla=c(          1991,          1994,               1998,          2001,          2004,          2007,          2010,          2013,          2016),
    ver=c(          1991,          1994,          1997,          2000,               2004,          2007,          2010,          2013,                2017),
    yuc=c(     1990,          1993,     1995,          1998,          2001,          2004,          2007,          2010,     2012,          2015,          2018),
    zac=c(               1992,          1995,          1998,          2001,          2004,          2007,          2010,          2013,          2016,     2018)
)
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


## # extract oax municipios to add more years
## # NOT NEEDED AGAIN
## tmp <- inc[inc$edon==20, c("inegi")]
## tmp <- unique(tmp)
## tmp <- as.character(tmp)
## tmp <- sub("^20", "", tmp)
## tmp1 <- paste("oax-07", tmp, sep=".")
## tmp2 <- paste("oax-08", tmp, sep=".")
## tmp3 <- paste("oax-09", tmp, sep=".")
## tmp <- c(tmp1,tmp2,tmp3)
## # get elec to subset oax cases
## tmp2 <- read.csv(paste(dd, "aymu1997-present.coalAgg.csv", sep = ""), stringsAsFactors = FALSE)
## tmp2 <- read.csv(paste(dd, "aymu1977-present.csv", sep = ""), stringsAsFactors = FALSE)
## tmp2 <- tmp2[tmp2$edon==20,]
## sel <- which(tmp2$emm %in% tmp)
## tmp2 <- tmp2[sel,]
## write.csv(tmp2, file = paste(dd, "tmp.csv", sep = ""))


# clean names
inc$incumbent <- gsub("  ", " ", inc$incumbent)  # drop double spaces
inc$incumbent <- sub("^ | $", "", inc$incumbent) # drop trainling/heading spaces
#inc$incumbent <- gsub("[.]", "", inc$incumbent)  # drop periods
#inc$incumbent <- gsub("[(|)]", "", inc$incumbent)  # drop parentheses

# recode some win labels
table(inc$win)
sel <- which(inc$win %in% c("eduardoquirogajimenez", "oscardanielcarrion", "indep1", "luisreneruelasortega"))
inc$win[sel] <- "indep"
sel <- which(inc$win %in% c("via_radical"))
inc$win[sel] <- "vrad"


# recode term-limit to pty-won or -lost
table(inc$win)

# more cleaning
inc$drep <- ave(inc$incumbent, as.factor(inc$inegi), FUN=sum, na.rm=TRUE)

    
# load my name-searching function
source("../code/search_names.r")


# count number of words in names
inc$words <- gsub("[^ ]", "", inc$incumbent); inc$words <- nchar(inc$words) + 1
table(inc$words)
# add column for incumbents with prvious elected municipal office
inc$drep <- NA # will receive dummy = 1 if name repeated in other obs
# sorts by decreasing words to process mem-intensive cases 1st, then drop to speed loops
inc <- inc[order(-inc$words),]

## # load incumbents from inafed data <-- NO LONGER NEEDED, aymu1989-present.incumbents.csv already has them in
## inafed <- read.csv("../../municipiosInafed/alcaldes/alcaldes.csv", stringsAsFactors = FALSE)
## inafed <- inafed[, c("edon","inegi","yr","incumb")] # select columns
## #inafed$edon <- as.integer(inafed$inegi/1000)
## inafed$incumb <- gsub("  ", " ", inafed$incumb) # drop double spaces
## inafed$incumb <- sub("^ | $", "", inafed$incumb) # drop trainling/heading spaces
## inafed$incumb <- gsub("[.]", "", inafed$incumb)  # drop periods
## inafed$incumb <- gsub("[(|)]", "", inafed$incumb)  # drop parentheses
## head(inafed)
## #
## # merge inafed names
## inc <- merge(x = inc, y = inafed[, c("inegi","yr","incumb")], by = c("inegi","yr"), all = TRUE)
## # how many words in names
## inc$spc1 <- gsub(pattern = "[^ ]", replacement = "", inc$incumbent, perl = TRUE) # keep spaces only
## inc$spc2 <- gsub(pattern = "[^ ]", replacement = "", inc$incumb, perl = TRUE) # keep spaces only
## inc$spc1 <- sapply(inc$spc1, nchar) # count them
## inc$spc2 <- sapply(inc$spc2, nchar) # count them
## inc$spc1[is.na(inc$spc1)] <- 0
## inc$spc2[is.na(inc$spc2)] <- 0
## # will receive name difference info
## inc$ddif <- NA
## sel <- which(inc$spc1>1 & inc$spc2>1) # only those names with at least two words manipulated
## tmp2 <- mapply(search_names, inc[sel, c("incumb")], inc[sel, c("incumbent")])
## tmp3 <- rep(1, length(tmp2)); tmp3[tmp2==TRUE] <- 0
## inc$ddif[sel] <- tmp3
## # consolida nombres
## table(is.na(inc$incumbent))
## table(inc$incumbent=="")
## table(is.na(inc$incumb))
## sel <- which((inc$incumbent=="" | is.na(inc$incumbent)) & is.na(inc$incumb)==FALSE)
## inc$incumbent[sel] <- inc$incumb[sel]
## inc$spc1[sel] <- inc$spc2[sel]
## inc$incumb <- inc$spc2 <- NULL
## sel <- which(inc$yr==0)
## inc <- inc[-sel,]
## inc$race.after[inc$yr<2016] <- "Term-limited"
## # add missing mun info
## sel <- which(is.na(inc$edon)==TRUE)
## inc$edon[sel] <- as.integer(inc$inegi[sel]/1000)
## inc$munn[sel] <- inc$inegi[sel] - as.integer(inc$inegi[sel]/1000)*1000
## # smthg wrong, check words
## sel <- which(is.na(inc$words))
## table(inc$words[-sel] - inc$spc1[-sel])


# subset to explore name-searching performance
sel <- which(inc$edon==14)
inc.jal <- inc[sel,]
inc.jal[1,]

# keep full version of inc in order to work w/o NAs (plug manipulation later)
inc.full <- inc # duplicate
inc <- inc.jal
sel.full <- which(inc$incumbent!="" & inc$words>1)
inc <- inc[sel.full,] # subset



# initialize for while
work <- which(is.na(inc$drep)==TRUE) # will be updated to FALSE after a hit recorded
ss <- inc[work,c("incumbent","drep")] # subset
ss$hit <- NA
tmp <- nrow(ss)
#i <- 1

while (tmp > 1){
    message(sprintf("%s loops, %s records left", i, tmp))
    ss$hit[-1] <- search_names(find_name = ss$incumbent[1], within_records = ss$incumbent[-1])
    sel <- which(ss$hit==TRUE)
    if (length(sel)>0) {
        ss$drep[1]   <- i # i allows finding where repeated name is
        ss$drep[sel] <- i
    } else {
        ss$drep[1] <- 0
    }
    # return to data
    inc$drep[work] <- ss$drep
    # update for next loop
    work <- which(is.na(inc$drep)==TRUE) # will be updated to FALSE after a hit recorded
    ss <- inc[work,c("incumbent","drep")] # subset
    ss$hit <- NA
    tmp <- nrow(ss)
    i <- i+1 # prepare for next hit
}

getwd()
save.image(file = "drep-cut1.RData")

rm(list = ls())
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data/"
setwd(wd)
load(file = "drep-cut1.RData")
ls()
options(width = 130)



table(inc$drep)
sel <- which(inc$drep==15)
inc[sel,c("edon","mun","incumbent")]
x


inafed$spcs <- gsub(pattern = "[^ ]", replacement = "", inafed$incumb, perl = TRUE) # keep spaces only
inafed$spcs <- sapply(inafed$spcs, nchar) # count them
3 sort by spcs, export to csv
4 manually input commas to separate name, patronyn, matronym

head(inafed$spcs)
head(inafed$incumb)

# fuzzy matching of names
N <- nrow(inafed)
tmp <- rep(0, N) #will receive dummy

hits <- agrep(pattern = inafed$incumb[1], x = inafed$incumb, ignore.case = TRUE)
n.hits <- length(hits)
names <- agrep(pattern = inafed$incumb[1], x = inafed$incumb, ignore.case = TRUE, value = TRUE)

apply(inafed$incumb[-1], function(x) = agrep(pattern = inafed$incumb[1], inafed$incumb[-1]))

agrep(pattern = "CUAUHTEMOC CALDERON GALVAN", x = "xxAUHTEMOC xxLDERON GALVAN", ignore.case = TRUE)
agrep("lasy", "1 lazy 2", max = list(sub = 0))
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2)
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2, value = TRUE)
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2, ignore.case = TRUE)

# determine/fix mismatches btw yrIn and yr
tmp <- inafed; tmp$tmp <- paste(tmp$edon, tmp$yrIn, sep = "-")
tmp <- inafed[duplicated(tmp$tmp)==FALSE,] # state-yrs in inafed 
tmp$inafed <- tmp$inegi <- NULL
tmp$yr <- tmp$yrIn # emulate column for merge
tmp$dinaf <- 1 # identify obs

el.yrs <- merge(x = el.yrs, y = tmp, by = c("edon","yr"), all = TRUE)
el.yrs <- el.yrs[order(el.yrs$edon, el.yrs$yr),]
el.yrs$dinaf[is.na(el.yrs$dinaf)] <- 0
el.yrs$dinc[is.na(el.yrs$dinc)] <- 0


#merge to incumb election yrs
cal$dcal <- 1
dim(el.yrs)
el.yrs <- merge(x = el.yrs, y = cal, by = c("edon","yr"), all = TRUE)
dim(el.yrs)
el.yrs$dinaf[is.na(el.yrs$dinaf)] <- 0
el.yrs$dinc[is.na(el.yrs$dinc)] <- 0
el.yrs$dcal[is.na(el.yrs$dcal)] <- 0
head(el.yrs)
el.yrs$where <- "none"
el.yrs$where[el.yrs$dcal==0 & el.yrs$dinc==0 & el.yrs$dinaf==1] <- "inaf-only" # 001
el.yrs$where[el.yrs$dcal==0 & el.yrs$dinc==1 & el.yrs$dinaf==0] <- "inc-only"  # 010
el.yrs$where[el.yrs$dcal==0 & el.yrs$dinc==1 & el.yrs$dinaf==1] <- "inc-inaf"  # 011
el.yrs$where[el.yrs$dcal==1 & el.yrs$dinc==0 & el.yrs$dinaf==0] <- "cal-only"  # 100
el.yrs$where[el.yrs$dcal==1 & el.yrs$dinc==0 & el.yrs$dinaf==1] <- "cal-inaf"  # 101
el.yrs$where[el.yrs$dcal==1 & el.yrs$dinc==1 & el.yrs$dinaf==0] <- "cal-inc"   # 110
el.yrs$where[el.yrs$dcal==1 & el.yrs$dinc==1 & el.yrs$dinaf==1] <- "all"       # 111

table(el.yrs$where)
table(el.yrs$where, el.yrs$edon)
table(el.yrs$where, el.yrs$yr)

i <- i+1
sel <- which(el.yrs$edon==i)
table(el.yrs$where[sel], el.yrs$yr[sel])
el.yrs[sel,]
calendar[[i]]


# keep only years missing
el.yrs <- el.yrs[el.yrs$dinc==0,]
el.yrs$where <- el.yrs$dinc <- el.yrs$dcal <- el.yrs$edo <- NULL
# add missing columns to rbind to inc
el.yrs$emm <- el.yrs$ord <- el.yrs$munn <- el.yrs$inegi <- el.yrs$ife <- el.yrs$mo <- el.yrs$dy <- el.yrs$mun <- el.yrs$win <- el.yrs$incumbent <- el.yrs$race.after <- el.yrs$nota <- el.yrs$suplente <- NA
el.yrs <- el.yrs[, c("emm", "ord", "edon", "munn", "inegi", "ife", "yr", "mo", "dy", "mun", "win", "incumbent", "race.after", "nota", "suplente")] # sort columns
# paste missing yrs to inc
inc <- rbind(inc, el.yrs)
inc <- inc[order(inc$edon, inc$yr),]


head(inc)
head(el.yrs)

x

dim(inafed)
inafed <- merge(x = inafed, y = cal, by = c("edon","yr"), all = TRUE)
dim(inafed)
x




head(inafed)
head(cal)

dim(inc)


inc <- merge(x = inc, y = inafed, by = c("inegi","yr"), all = TRUE)



edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")
edon <- 18
edo <- edos[edon]
#
inc$inafed <- NA # add empty column to receive incumbents
#



# load elec data for margin
mv <- read.csv(paste(dd, "aymu1997-present.coalAgg.csv", sep = ""), stringsAsFactors = FALSE)
sel <- grep("^v[0-9]+", colnames(mv))
v <- mv[,sel] # select vote columns only
mv$efec <- rowSums(v) # recompute efec --- should be redundant
mv$mg <- round((mv$v01 - mv$v02) / mv$efec, 3) # compute margin of victory
# prep margin for merge
mv <- mv[,c("emm","mg","lisnom","efec")]
# merge
inc <- merge(x = inc, y = mv, by = "emm", all.x = TRUE, all.y = FALSE)

head(inc)

#############################################################################################
## get lisnom from federal els (taken from naylum code)                                    ##
## ignores vote code bec aggregating multi-district munics w partial coals not straightfwd ##
#############################################################################################
### get seccion equivalencias to fill missing municipio names
pth <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv"
eq <- read.csv(pth, stringsAsFactors = FALSE)
eq <- eq[,c("edon","seccion","ife","inegi","mun")]
### 2006 president has lisnom 
pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/pre2006.csv"
tmp <- read.csv(pth, stringsAsFactors = FALSE)
tmp <- tmp[-which(tmp$seccion==0),] # drop voto extranjero
tmp$lisnom[is.na(tmp$lisnom)] <- 0 # make NAs zeroes because ave() somehow not ignoring them
tmp$lisnom <- ave(tmp$lisnom, as.factor(tmp$edon*10000+tmp$seccion), FUN=sum, na.rm=TRUE) # aggregate secciones
tmp <- tmp[duplicated(tmp$edon*10000+tmp$seccion)==FALSE,] # drop reduntant obs --- keep seccion structure to merge to 09
tmp$lisnom <- ave(tmp$lisnom, as.factor(tmp$munn), FUN=sum, na.rm=TRUE) # aggregate municipios
tmp <- tmp[duplicated(tmp$munn)==FALSE,] # drop reduntant obs --- keep seccion structure to merge to 09
tmp$lisnom.06 <- tmp$lisnom
ln <- tmp[, c("edon","munn","lisnom.06")]
#
# 2009 has mun only, merge to eq for all munn (slight mistakes likely due to remunicipalización)
pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/dip2009.csv"
tmp <- read.csv(pth, stringsAsFactors = FALSE)
tmp$lisnom[is.na(tmp$lisnom)] <- 0 # make NAs zeroes because ave() somehow not ignoring them
tmp$lisnom.09 <- tmp$lisnom # rename
tmp$mun <- tmp$munn # wrong name
tmp <- tmp[,c("edon","mun","seccion","lisnom.09")] # data has no munn
tmp$lisnom.09 <- ave(tmp$lisnom.09, as.factor(tmp$edon*10000+tmp$seccion), FUN=sum, na.rm=TRUE) # aggregate secciones
tmp <- tmp[duplicated(tmp$edon*10000+tmp$seccion)==FALSE,] # drop reduntant obs
tmp$lisnom.09 <- ave(tmp$lisnom.09, as.factor(paste(tmp$edon, tmp$mun, sep = ".")), FUN=sum, na.rm=TRUE) # aggregate municipios
tmp <- merge(x = tmp, y = eq, by = c("edon","seccion"), all.x = TRUE, all.y = FALSE) # merge to have munn
tmp <- tmp[duplicated(as.factor(paste(tmp$edon, tmp$mun.x, sep = ".")))==FALSE,]
tmp$munn <- tmp$ife
tmp$mun <- tmp$mun.y
tmp <- tmp[,c("edon","munn","inegi","mun","lisnom.09")]
#
ln <- merge(x = ln, y = tmp, by = c("edon","munn"), all = TRUE)
#
# 2012 presid
pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/pre2012.csv"
tmp <- read.csv(pth, stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","mun","lisnom")]
tmp <- tmp[-which(tmp$seccion==0),] # drop voto extranjero
tmp[tmp=="-"] <- 0 # remove "-"
# agrega secciones
tmp$lisnom   <- ave(tmp$lisnom,   as.factor(tmp$edon*10000+tmp$seccion), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edon*10000+tmp$seccion)==FALSE,] # drop reduntant obs
tmp$lisnom.12 <- tmp$lisnom; tmp$lisnom <- NULL
# add munn
tmp <- merge(x = tmp, y = eq, by = c("edon","seccion"), all.x = TRUE, all.y = FALSE) 
tmp$munn <- tmp$ife; tmp$ife <- NULL # rename
# aggregate municipios
tmp$lisnom.12   <- ave(tmp$lisnom.12,   as.factor(paste(tmp$edon, tmp$mun.x)), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(as.factor(paste(tmp$edon, tmp$mun.x)))==FALSE,] # drop reduntant obs
tmp$mun <- tmp$mun.y; tmp$mun.x <- tmp$mun.y <- NULL
tmp <- tmp[,c("edon","munn","inegi","mun","lisnom.12")]
#
ln <- merge(x = ln, y = tmp, by = c("edon","munn"), all = TRUE)
# plug mun.y when mun.x is missing
sel <- which(is.na(ln$mun.x))
ln$mun.x[sel] <- ln$mun.y[sel]
sel <- which(is.na(ln$inegi.x))
ln$inegi.x[sel] <- ln$inegi.y[sel]
ln$mun <- ln$mun.x; ln$mun.x <- ln$mun.y <- NULL
ln$inegi <- ln$inegi.x; ln$inegi.x <- ln$inegi.y <- NULL
#
# 2015 dip fed
pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/dip2015.csv"
tmp <- read.csv(pth, stringsAsFactors = FALSE)
#tmp <- tmp[-which(tmp$seccion==0),] # drop voto extranjero
tmp$ord <- tmp$OBSERVACIONES <- tmp$ID_CASILLA <- tmp$TIPO_CASILLA <- tmp$EXT_CONTIGUA <- tmp$nr <- tmp$nul <- tmp$tot <- NULL # clean
tmp$lisnom[is.na(tmp$lisnom)] <- 0
tmp$lisnom.15 <- tmp$lisnom
tmp <- tmp[,c("edon","seccion","lisnom.15")]
# agrega secciones
tmp$lisnom.15   <- ave(tmp$lisnom.15,   as.factor(paste(tmp$edon, tmp$seccion, sep = ".")), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(as.factor(paste(tmp$edon, tmp$seccion, sep = ".")))==FALSE,] # drop reduntant obs
tmp <- merge(x = tmp, y = eq, by = c("edon","seccion"), all.x = TRUE, all.y = FALSE) # add munn
tmp$munn <- tmp$ife; tmp$ife <- NULL # rename
tmp$lisnom.15   <- ave(tmp$lisnom.15,   as.factor(tmp$munn), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$munn)==FALSE,] # drop reduntant obs
tmp <- tmp[,c("edon","munn","mun","lisnom.15")]
#
ln <- merge(x = ln, y = tmp, by = c("edon","munn"), all = TRUE)
# plug mun.y when mun.x is missing
sel <- which(is.na(ln$mun.x))
ln$mun.x[sel] <- ln$mun.y[sel]
ln$mun <- ln$mun.x; ln$mun.x <- ln$mun.y <- NULL
#
# 2018 presid
pth <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/pre2018.csv"
tmp <- read.csv(pth, stringsAsFactors = FALSE)
tmp <- tmp[-which(tmp$seccion==0),] # drop voto extranjero
tmp[tmp=="-"] <- 0 # remove "-"
tmp <- tmp[, c("edon","seccion","lisnom")]
# agrega secciones
tmp$lisnom   <- ave(tmp$lisnom,   as.factor(tmp$edon*10000+tmp$seccion), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edon*10000+tmp$seccion)==FALSE,] # drop reduntant obs
tmp$lisnom.18 <- tmp$lisnom; tmp$lisnom <- NULL
# add munn
tmp <- merge(x = tmp, y = eq, by = c("edon","seccion"), all.x = TRUE, all.y = FALSE) 
tmp$munn <- tmp$ife; tmp$ife <- NULL # rename
sel <- which(is.na(tmp$munn)) # reseccionamiento, drop handful of secciones not in equiv secciones
tmp <- tmp[-sel,]
# aggregate municipios
tmp$lisnom.18   <- ave(tmp$lisnom.18,   as.factor(tmp$munn), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$munn)==FALSE,] # drop reduntant obs
tmp <- tmp[,c("edon","munn","mun","lisnom.18")]
#
ln <- merge(x = ln, y = tmp, by = c("edon","munn"), all = TRUE)
ln$mun <- ln$mun.x; ln$mun.x <- ln$mun.y <- NULL
#
# sort
ln <- ln[order(ln$edon, ln$munn), c("edon","munn","inegi","mun","lisnom.06","lisnom.09","lisnom.12","lisnom.15","lisnom.18")]
rm(eq,tmp)

# will need to project lisnom from fed elecs (cf naylum), too many missings here
head(ln)
with(inc, table(is.na(mg)))
with(inc, table(is.na(lisnom)))
ls()

# adds object cen.yr (and mpv, unneeded) with yearly census projections
load(file="/home/eric/Desktop/naylum/data/electoral/nay2002-on.RData") 
rm(vpm)


unique(sub("[.][0-9]{2}", "", colnames(cen.yr))) # names omitting yrs
cen.yr$lisnom.12


# lag margin (measure of candidate quality)
library(DataCombine) # easy lags
inc$tmp <- sub("([a-z]+)[-][0-9]{2}([.][0-9]{3})", "\\1\\2", inc$emm) # drop elec cycle from emm
inc <- slide(data = inc, TimeVar = "yr", GroupVar = "tmp", Var = "mg", NewVar = "mg.lag",    slideBy = -1)
inc <- inc[order(inc$ord),] # sort
inc$tmp <- inc$suplente <- NULL

head(inc)

