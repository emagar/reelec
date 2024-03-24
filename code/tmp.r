#########################################
## SWR MODEL W POSTREFORM INTERACTIONS ##
#########################################
##
######################################
### EXTRA DATA PREP FOR JAGS MODEL ###
######################################
depvar <- tmp$dwin
N <- length(depvar)
X <- data.frame(
    dneg=tmp$dneg, dnegxincball=tmp$dnegxincball, dnegxmg=tmp$dnegxmg, dnegxmgxincball=tmp$dnegxmgxincball
  , dpos=tmp$dpos, dposxincball=tmp$dposxincball, dposxmg=tmp$dposxmg, dposxmgxincball=tmp$dposxmgxincball
    )
##
## labels to interpret parameters
var.labels <- colnames(X)
K <- length(var.labels)
X <- as.matrix(X)
### Data, initial values, and parameter vector for jags
dl.data <- list("N", "K", "depvar", "X")
dl.inits <- function (){
    list (
    beta=rnorm(K)
    ##beta=summary(fit2)$coefficients[,1] # use lm's estimates
    )
    }
dl.parameters <- c("beta")
#dm.parameters <- c("beta", "sigma", "depvar.hat")
## test ride
fit1jags <- jags (data=dl.data, inits=dl.inits, dl.parameters,
             model.file=logitModel, n.chains=3,
             n.iter=100, n.thin=10
             )
## estimate
fit1jags <- jags (data=dl.data, inits=dl.inits, dl.parameters,
                  model.file=logitModel, n.chains=3,
                  n.iter=50000, n.thin=100,
                  )
#
tmp.bak <- fit1jags
fit1jags <- update(fit1jags, 10000) # continue updating to produce 10000 new draws per chain
traceplot(fit1jags) # visually check posterior parameter convergence
#
fit1jags$var.labels <- var.labels # add object to interpret coefficients
summary(fit1jags$BUGSoutput$summary)

# sims bayesian
antilogit <- function(X){ exp(X) / (exp(X)+1) }
## pr(urgent)
coefs <- fit1jags$BUGSoutput$sims.matrix; coefs <- coefs[,-grep("deviance", colnames(fit1jags$BUGSoutput$sims.matrix))]
scenario <- c(
    1 ## dneg <- c(0,1)
  , 0 ## dpos <- c(0,1)
  , -.1 ## dnegxmg
  , 0 ## dposxmg
)
names(scenario) <- var.labels
names(scenario) <- c("dneg", "dpos", "dnegxmg", "dposxmg")
##
n <- nrow(coefs)
sc <- matrix(rep(scenario, n), nrow = n, byrow = TRUE)
sc <- as.data.frame(sc)
colnames(sc) <- c("dneg", "dpos", "dnegxmg", "dposxmg")
tail(sc)
## change dpos/dneg by alternating 0,1
sc$dpos <- rep ( 1:0, n/2)
sc$dneg <- 1 - sc$dpos
sc$dposxmg <- c(round(seq(from= .15, to=0, length.out = (n-1)), 6), 0)
sc$dnegxmg[2:n] <- -sc$dposxmg[1:(n-1)] # duplicate previous times minus 1
sc$dposxmg <- sc$dposxmg * sc$dpos # make zeroes
sc$dnegxmg <- sc$dnegxmg * sc$dneg # make zeroes
sc <- as.matrix(sc)
#
tmp <- fit1jags$BUGSoutput$summary[grep("beta", rownames(fit1jags$BUGSoutput$summary)),1] # coef point pred (mean posterior)
pointPred <- sc %*% diag(tmp) # right side achieves multiplication of matrix columns by vector
pointPred <- antilogit(rowSums(pointPred)) # will plug this in sc later
##
pred <- sc * coefs
pred <- antilogit(rowSums(pred)) # will plug this in sc later
#
sc <- as.data.frame(sc); colnames(sc) <- c("dneg", "dpos", "dnegxmg", "dposxmg")
sc$pred <- pred; rm(pred)
sc$pointPred <- pointPred; rm(pointPred)
head(sc)
##
## plot
##png("../plots/pan-luro97-23-mcmc.png")
plot(x = c(-.15,.15), y = c(0,1), type = "n", main = "PAN \nMCMC logit link 1997-2023", xlab = expression("Margin"[t]), ylab = expression("Pr(win)"[t+1]))
points(sc$dnegxmg[sc$dneg==1], sc$pred[sc$dneg==1], pch = 20, col = "gray")
points(sc$dposxmg[sc$dpos==1], sc$pred[sc$dpos==1], pch = 20, col = "gray")
abline(v=0)
segments(x0 = -.15, y0 = (  sc$pointPred[sc$dnegxmg==-.15]  ),
         x1=  0,    y1 = (  sc$pointPred[sc$dnegxmg==0 & sc$dneg==1]  ))
segments(x0 =  .15, y0 = (  sc$pointPred[sc$dposxmg== .15]  ),
         x1=  0,    y1 = (  sc$pointPred[sc$dposxmg==0 & sc$dpos==1]  ))
## ## legend
## legend("topright", legend = c("incumbent running","open seat"), lty = c(2,1))
##dev.off()
##

## rename/save party estimation
pan1jags <- fit1jags
