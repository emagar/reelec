###################################################################################################################
## invert ife and inegi locations to avoid confusion (inegi had some 0 codes in cps still in 2018) ##            ##
## See https://stackoverflow.com/questions/5724419/is-it-possible-to-swap-columns-around-in-a-data-frame-using-r ##
## answer by https://stackoverflow.com/users/13549245/kh%c3%b4ra-willis                                          ##
###################################################################################################################
swap <- function(DF, n, m){
    n <- if (class(n)=="character" & is.na(suppressWarnings(as.integer(n)))) which(colnames(DF)==n) else as.integer(n)
    m <- if (class(m)=="character" & is.na(suppressWarnings(as.integer(m)))) which(colnames(DF)==m) else as.integer(m)
    if (!(1<=n & n<=length(DF))) stop( "`n` represents invalid index!" )
    if (!(1<=m & m<=length(DF))) stop( "`m` represents invalid index!" )
    return (DF[ if (n==m) 1:length(DF) else c( (if (min(n,m)==1) c() else 1:(min(n,m)-1) ), (if (min(n,m)+1 == max(n,m)) (min(n,m)+1):(max(n,m)-1) else c( max(n,m), (min(n,m)+1):(max(n,m)-1), min(n,m))), (if (max(n,m)==length(DF)) c() else (max(n,m)+1):length(DF) ) ) ])
}
vot <- swap(vot,"ife","inegi")
rm(swap)

##################################
## reconstruct morena backwards ##
##################################
# add prd + morena up to 2017
colnames(vot)
sel <- which(vot$yr < 2018)
vot$morena[sel] <- vot$morena[sel] + vot$prd[sel]
vot$win[sel] <- "morena"

left # done elsewhere with dipfed prd + morena in 2015 but not in 2018, which seems right thing to do
res.morena # recalculate
race.current.morena # recalculate
dinptywon.current # recalculate
dincran.current   # recalculate
win.left # ?


-------------------



