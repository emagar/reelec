wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/presentations/"
setwd(wd)

# data prep (summary of calendarios reelección)
firstElecs <- data.frame(
    yr=2014:2024,
    df=   c(0,0,0,0, 0,0,0,1,0,0,0),
    sen=  c(0,0,0,0, 0,0,0,0,0,0,1),
    dl=   c(0,0,0,1,24,3,0,4,0,0,0),
    ay=   c(0,0,0,0,22,3,0,3,0,0,3),
    dl2 = c(0,0,0,0, 6,2,0,1,0,0,0),
    dl3 = c(0,0,0,0, 1,0,0,0,0,0,0),
    dl4 = c(0,0,0,1,17,1,0,3,0,0,0)
    )

f <- firstElecs
#
f$cumdl <- cumsum(f$dl)
f$cumay <- cumsum(f$ay)
#
f$cumdl4 <- cumsum(f$dl4)
f$cumdl3 <- cumsum(f$dl3)
f$cumdl2 <- cumsum(f$dl2)
f$cumdl1 <- 32 - f$cumdl2 - f$cumdl3 - f$cumdl4
#
f$cumay2 <- f$cumay; f$cumay1 <- 32 - f$cumay2
#
f$sen <- f$df <- NULL

# plots
# yes/no dl ban
plot(x = f$yr, y = seq(0, 32, length.out = 11), type = "n",
     axes = FALSE,
     xlab = "",
     ylab = "Frequency (states)",
     main = "State assembly incumbents banned from ballot")
axis(1, at = f$yr)
# i <- 2018 # debug year
w <- .15  # half-column width
d <- 32 - f$cumdl # what to plot
for (i in 2014:2021){
    j <- i-2013  # needed
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(32,d[j],d[j],32),
            col = "green")
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(0,d[j],d[j],0),
            col = "red")
}

# yes/no ay ban
plot(x = f$yr, y = seq(0, 32, length.out = 11), type = "n",
     axes = FALSE,
     xlab = "",
     ylab = "Frequency (states)",
     main = "Municipal incumbents banned from ballot")
axis(1, at = f$yr)
axis(2, at = seq(0,32,4))
# i <- 2018 # debug year
w <- .15  # half-column width
d <- 32 - f$cumay # what to plot
for (i in 2014:2024){
    j <- i-2013  # needed
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(32,d[j],d[j],32),
            col = "green")
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(0,d[j],d[j],0),
            col = "red")
}
f[,c("yr","dl4","dl3","dl2")]

# yes/no dl possib
library(RColorBrewer)
greens <- brewer.pal(n = 4, name = "Greens")
#fl <- "../pics/dldummy.pdf"; pdf(file = fl, width = 7, height = 5)
plot(x = f$yr, y = seq(0, 32, length.out = 11), type = "n",
     axes = FALSE,
     xlab = "",
     ylab = "Frequency (states)",
     main = "State legilators can run again")
axis(1, at = f$yr)
axis(2, at = seq(0,32,4))
# i <- 2018 # debug year
w <- .15  # half-column width
d <- f$cumdl # what to plot
for (i in 2014:2021){
    j <- i-2013  # needed
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(32,d[j],d[j],32),
            col = greens[1])
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(0,d[j],d[j],0),
            col = greens[4])
}
legend(x = 2022, y = 18, legend = c("no","yes"), fill = c(greens[1],greens[4]))
#dev.off()

 

# n term limits dl
library(RColorBrewer)
greens <- brewer.pal(n = 4, name = "Greens")
#fl <- "../pics/dlnterm.pdf"; pdf(file = fl, width = 7, height = 5)
plot(x = 2014:2024, y = seq(0, 32, length.out = 11), type = "n",
     axes = FALSE,
     xlab = "",
     ylab = "Frequency (states)",
     main = "Term limits for state legislators")
axis(1, at = f$yr)
axis(2, at = seq(0,32,4))
# i <- 2018 # debug year
w <- .15  # half-column width
d2 <- f$cumdl2 # what to plot
d3 <- f$cumdl3 # what to plot
d4 <- f$cumdl4 # what to plot
for (i in 2014:2021){
    j <- i-2013  # needed
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(32,d4[j]+d3[j]+d2[j],d4[j]+d3[j]+d2[j],32),
            col = greens[1])
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(d4[j]+d3[j],d4[j]+d3[j]+d2[j],d4[j]+d3[j]+d2[j],d4[j]+d3[j]),
            col = greens[2])
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(d4[j],d4[j]+d3[j],d4[j]+d3[j],d4[j]),
            col = greens[3])
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(0,d4[j],d4[j],0),
            col = greens[4])
}
legend(x = 2022, y = 18, legend = c(1:3,"4 terms"), fill = greens)
#dev.off()


# n term limits ay
library(RColorBrewer)
greens <- brewer.pal(n = 4, name = "Greens")
#fl <- "../pics/aynterm.pdf"; pdf(file = fl, width = 7, height = 5)
plot(x = 2014:2024, y = seq(0, 32, length.out = 11), type = "n",
     axes = FALSE,
     xlab = "",
     ylab = "Frequency (states)",
     main = "Term limits for municipal officers")
axis(1, at = f$yr)
axis(2, at = seq(0,32,4))
# i <- 2018 # debug year
w <- .15  # half-column width
d2 <- f$cumay2 # what to plot
d3 <- rep(0,11) # what to plot
d4 <- rep(0,11) # what to plot
for (i in 2014:2024){
    j <- i-2013  # needed
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(32,d4[j]+d3[j]+d2[j],d4[j]+d3[j]+d2[j],32),
            col = greens[1])
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(d4[j]+d3[j],d4[j]+d3[j]+d2[j],d4[j]+d3[j]+d2[j],d4[j]+d3[j]),
            col = greens[2])
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(d4[j],d4[j]+d3[j],d4[j]+d3[j],d4[j]),
            col = greens[3])
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(0,d4[j],d4[j],0),
            col = greens[4])
}
#dev.off()

# adds number of legislators nationwide that can reelect every new year
dd.old <- dd
dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)
#
edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac", "dipfed", "sen")
#
nsmd <- data.frame(edo = edos)
nsmd$smds <- c(rep(0,32), 300, 96)
nsmd$yr <- c(rep(0,32), 2021, 2024)
#
nsmd$smds[9]  <- 33 # df
nsmd$smds[17] <- 12 # mor
nsmd$smds[29] <- 15 # tlax
#
for (i in c(1:8,10:16,18:28,30:32)){
    #i <- 1
    edo <- edos[i]
    tmp <- read.csv(
        paste("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/", edo, "Loc.csv", sep = "")
      , stringsAsFactors = FALSE)
    nsmd$smds[i] <- max(tmp$escenario1)
}
nsmd$yr <- c(2018, 2019, 2018, 2018, 2017, 2018, 2018, 2018, 2021, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2021, 2018, 2018, 2021, 2018, 2019, 2018, 2018, 2021, 2018, 2019, 2018, 2018, 2018, 2018, 2021, 2024)
#
n <- tapply(nsmd$smds, nsmd$yr, FUN = sum) 
n <- c(0,0,0,n[1:3],0,n[4],0,0,n[5])
f$nleg <- n

# adds number ayuntamientos
# pending --- need to figure out n regidores in each municipio
m <- read.csv("../../../../elecReturns/ancillary/mun.yrs.csv", stringsAsFactors = FALSE)
n <- tapply(m$ife, m$edon, FUN = length)
nsmd$nmun <- c(n,0,0)
##              1  2  3  4  5  6  7  8  9 10  1  2  3  4  5  6  7  8  9 20  1  2  3  4  5  6  7  8  9 30  1  2
nsmd$yray <- c(19,19,18,18,18,18,18,18,21,19,18,18,NA,18,18,18,18,24,18,18,21,18,18,18,18,18,18,18,24,24,18,18,NA,NA)+2000
# break oax into usos/not, using dipfed slot for usos
nsmd$nmun[nsmd$edo=="oax"] <- 570-sum(m$dusos)
nsmd$nmun[nsmd$edo=="dipfed"] <- sum(m$dusos)
# add to table
n <- tapply(nsmd$nmun, nsmd$yray, FUN = sum)
##   2014 15 16 17  18 19  20   21  22 23  24
n <- c(0, 0, 0, 0, n[1:2], 0, n[3], 0, 0, n[4])
f$nmun <- n
#
f$cumnleg <- cumsum(f$nleg)
f$cumnmun <- cumsum(f$nmun)
#
setwd(dd.old)


# n smd legislators who can reelect
N <- sum(f$nleg)
library(RColorBrewer)
greens <- brewer.pal(n = 4, name = "Greens")
#fl <- "../pics/legislatorsNationwide.pdf"; pdf(file = fl, width = 7, height = 5)
plot(x = 2014:2024, y = seq(0, N, length.out = 11), type = "n",
     axes = FALSE,
     xlab = "",
     ylab = "Frequency (legislators)",
     main = "Lawmakers nationwide (SMD) who can reelect")
axis(1, at = f$yr)
axis(2)
# i <- 2018 # debug year
w <- .15  # half-column width
d <- f$cumnleg # what to plot
axis(4, at = max(d))
#d3 <- rep(0,11) # what to plot
#d4 <- rep(0,11) # what to plot
for (i in 2014:2024){
    j <- i-2013  # needed
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(N,d[j],d[j],N),
            col = greens[1])
    ## polygon(x = c(i-w,i-w,i+w,i+w),
    ##         y = c(d4[j]+d3[j],d4[j]+d3[j]+d2[j],d4[j]+d3[j]+d2[j],d4[j]+d3[j]),
    ##         col = greens[2])
    ## polygon(x = c(i-w,i-w,i+w,i+w),
    ##         y = c(d4[j],d4[j]+d3[j],d4[j]+d3[j],d4[j]),
    ##         col = greens[3])
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(0,d[j],d[j],0),
            col = greens[4])
}
#dev.off()

# n municipios whose officers can reelect
N <- max(m$ord)
library(RColorBrewer)
greens <- brewer.pal(n = 4, name = "Greens")
##fl <- "../pics/ayuntamientos.pdf"; pdf(file = fl, width = 7, height = 5)
plot(x = 2014:2024, y = seq(0, N, length.out = 11), type = "n",
     axes = FALSE,
     xlab = "",
     ## ylab = "Frequency (municipalities)",
     ## main = "Municipalities whose officers can reelect")
     ylab = "Frecuencia (municipios)",
     main = "Ayuntamientos que pueden o no reelegirse")
axis(1, at = f$yr)
axis(2)
# i <- 2018 # debug year
w <- .15  # half-column width
d <- f$cumnmun # what to plot
axis(4, at = max(d))
#d3 <- rep(0,11) # what to plot
#d4 <- rep(0,11) # what to plot
for (i in 2014:2024){
    j <- i-2013  # needed
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(N,d[j],d[j],N),
            col = greens[1])
    ## polygon(x = c(i-w,i-w,i+w,i+w),
    ##         y = c(d4[j]+d3[j],d4[j]+d3[j]+d2[j],d4[j]+d3[j]+d2[j],d4[j]+d3[j]),
    ##         col = greens[2])
    ## polygon(x = c(i-w,i-w,i+w,i+w),
    ##         y = c(d4[j],d4[j]+d3[j],d4[j]+d3[j],d4[j]),
    ##         col = greens[3])
    polygon(x = c(i-w,i-w,i+w,i+w),
            y = c(0,d[j],d[j],0),
            col = greens[4])
}
for (i in 2018:2024){
polygon(x = c(i-w,i-w,i+w,i+w),
        y = c(N,N-sum(m$dusos),N-sum(m$dusos),N),
        col = "gray")
}
text(x = 2018,
     y = N-sum(m$dusos)/2,
     labels = "usos",
     srt=90)
text(x = 2018,
     y = 1740,
     labels = "no",
     srt=90)
text(x = 2018,
     y = 695,
     labels = "sí", col="ivory",
     srt=90)
##dev.off()

colnames(m)
