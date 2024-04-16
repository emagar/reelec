## models
tmpp <- c("dincballot * dinc", "dgov", "dpres", "vhat.", "popshincab", "wsdalt", "lats", "p5lish", "lumwpop20", "as.factor(trienio)"); tmp <- mylm(dv="pan", data=lnr, subset="yr>1999", predictors = tmpp); summary(tmp)
