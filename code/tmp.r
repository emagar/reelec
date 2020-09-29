###################################
## function to estimate ols regs ##
###################################
library(DataCombine) # easy lags with slide
#
form <- "res.pty ~ vot.lag  + dptyinc + dothinc + dptyopen - dconcgob + dsamegov + ptot + wmeanalt*wsdalt + dpostref - dcapital - as.factor(edon)"
#
estim.mod <- function(pty = "pan"){
    # duplicate vot for analysis
    tmp <- vot
    # retain year with vhat histories only
    sel <- which(tmp$yr>2004)
    tmp <- tmp[sel,]
    #
    if (pty == "pan"){
        tmp$vot <- tmp$pan
        tmp$res.pty <- tmp$res.pan
        sel <- grep("pan", tmp$win); tmp$dpty <- 0; tmp$dpty[sel] <- 1
    }
    if (pty == "pri"){
        tmp$vot <- tmp$pri
        tmp$res.pty <- tmp$res.pri
        sel <- grep("pri", tmp$win); tmp$dpty <- 0; tmp$dpty[sel] <- 1
    }
    if (pty == "left"){
        tmp$vot <- tmp$left
        tmp$res.pty <- tmp$res.left
        sel <- grep("prd", tmp$win); tmp$dpty <- 0; tmp$dpty[sel] <- 1
        tmp$dpty[tmp$yr>=2015] <- 0 # prd before 2015
        sel <- grep("morena", tmp$win); tmp$dtmp <- 0; tmp$dtmp[sel] <- 1
        tmp$dpty[tmp$yr>=2015] <- tmp$dtmp[tmp$yr>=2015]; tmp$dtmp <- NULL # morena since 2015
    }
    #
    # incumbent x pty dummies (complement is open seat)
    tmp$dptyinc  <-      tmp$dpty  * (1 - tmp$dopenseat)
    tmp$dothinc  <- (1 - tmp$dpty) * (1 - tmp$dopenseat)
    tmp$dptyopen <-      tmp$dpty  *      tmp$dopenseat
    tmp$dothopen <- (1-  tmp$dpty) *      tmp$dopenseat # drop to avoid dummy trap
    #
    # manipulate prd/morena govpty for left
    sel <- which( (tmp$govpty.lag=="prd" & tmp$yr<=2015) | (tmp$govpty.lag=="morena" & tmp$yr>2015) )
    tmp$govpty.lag[sel] <- "left"
    # own party governor dummy
    tmp$dsamegov <- 0
    tmp$dsamegov[tmp$govpty.lag == pty] <- 1
    #
    # lag votes
    tmp <- tmp[order(tmp$emm),] # check sorted for lags
    tmp$cycle <- as.numeric(sub("^.+-([0-9]{2})[.][0-9]+", "\\1", tmp$emm))
    #tmp[1,]
    tmp <- slide(data = tmp, TimeVar = "cycle", GroupVar = "ife", Var = "vot", NewVar = "vot.lag", slideBy = -1) # lag by one period
    #
    tmp.mod <- lm(formula = form, data = tmp, subset = (dhgover==0 & yr>=2004))
    return(tmp.mod)
}


pan.mod <- estim.mod(pty = "pan")
summary(pan.mod)
pri.mod <- estim.mod(pty = "pri")
summary(pri.mod)
left.mod <- estim.mod(pty = "left")
summary(left.mod)



## # drop hgo ver
## tmp <- tmp[tmp$dhgover==0,] 

plot(tmp$alpha.pan, tmp$pan.lag, pch=20, cex = .05)
lines(formula = pan.lag ~ alpha.pan, data = tmp)
x
# model eric  x
colnames(tmp)
tmp.mod <- lm(formula = res.pan ~ alpha.pan + dpaninc + dothinc + dpanopen, data = tmp)
tmp.mod <- lm(formula = pan.lag ~ alpha.pan, data = tmp)
tmp.mod <- lm(formula = res.pan ~ pan.lag   + dpaninc + dothinc + dpanopen - dconcgob + dsamegov + ptot + wmeanalt*wsdalt + dpostref - dcapital - as.factor(edon), data = tmp, subset = (dhgover==0 & yr>=2006))
nobs(tmp.mod)
