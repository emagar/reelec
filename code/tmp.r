
THIS IS WHAT DATA IN aymu.incumbents LOOKS LIKE
case after0 inc1 after1 inc2 after2
a    term-l mr X reran  ms Y out-l
b    out-w  mr Z reelec mr Z term-l
c    out-l  ms V out-l  mr W reran

THIS IS HOW IT LOOKS AFTER LAGGING
case prior1 inc1 prior2 inc2 ...... dinc1 dinc2
a    reran  mr X out    ms Y            1     0
b    reelec mr Z term-l mr Z            1     0
c    out    ms V reran  mr W            0     1

----------------

THIS IS WHAT DATA IN LOOKS LIKE | AFTER LAG
case ...... inc  after            case  prior  inc   ......
a              ? term-l           a         NA     ? 
a           mr X reran            a     term-l  mr X 
a           ms Y out-l            a     reran   ms Y 
b              ? out-w            b         NA     ? 
b           mr Z reelec           b     out-w   mr Z 
b           mr Z term-l           b     reelec  mr Z 
c              ? out-l            c         NA     ? 
c           ms V out-l            c     out-l   ms V 
c           mr W reran            c     out-l   mr W 



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
#sel <- which(inc$emm=="bc-19.006"); # san quintin might be ready for 2024
#inc$race.prior[sel] <- "new mun"; inc$win.long.prior[sel] <- "morena" ; inc$win.prior[sel] <- "new mun morena"
sel <- which(inc$emm=="bcs-08.009");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cam-08.009");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cam-10.010");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cam-11.011");
inc$race.prior[sel] <- "new mun"; inc$win.prior[sel] <- "new mun pri"
sel <- which(inc$emm=="cam-18.xxx"); # ojo: need codigo inegi
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

### get voting histories
