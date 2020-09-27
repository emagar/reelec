# fix new municipios

# identify first federal election after municipio was created in a dataframe
# (parent and child need manipulation) 
tmp <- function(x){ # function to add rows
    x <- matrix(x, nrow=1)
    return(x)
    }
treat.yrs <- data.frame()
##
treat.yrs <- rbind(treat.yrs, tmp(c( 1001,1994,"AGUASCALIENTES","parent",1))) # need seccion-level 1991 to manipulate these
treat.yrs <- rbind(treat.yrs, tmp(c( 1010,1994,"","child", 1))) # need seccion-level 1991 to manipulate these
treat.yrs <- rbind(treat.yrs, tmp(c( 1011,1994,"","child", 1))) # need seccion-level 1991 to manipulate these
#
treat.yrs <- rbind(treat.yrs, tmp(c( 2004,2000,"parent","TIJUANA",2))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 2005,2000,"child","PLAYAS DE ROSARITO", 2))) # 
##
treat.yrs <- rbind(treat.yrs, tmp(c( 3003,1994,"","parent",3))) # need seccion-level 1991 to manipulate these
treat.yrs <- rbind(treat.yrs, tmp(c( 3005,1994,"","child", 3))) # need seccion-level 1991 to manipulate these
#
treat.yrs <- rbind(treat.yrs, tmp(c( 4006,1997,"parent","HOPELCHEN",4))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 4011,1997,"child","CALAKMUL", 4))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 4003,2000,"parent","CARMEN",5))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 4010,2000,"child","ESCARCEGA", 5))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7049,2003,"parent","LARRAINZAR",6))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7118,2003,"child","SANTIAGO EL PINAR", 6))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7082,2003,"parent","SIMOJOVEL",7))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7117,2003,"child","SAN ANDRES DURAZNAL", 7))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7008,2003,"parent","ANGEL ALBINO CORZO",8))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7116,2003,"child","MONTECRISTO DE GUERRERO", 8))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7108,2015,"parent","VILLA CORZO",9))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7122,2015,"child","EL PARRAL", 9))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7026,2003,"parent","CHENALHO",10))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7112,2003,"child","ALDAMA", 10))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7059,2003,"parent","OCOSINGO",11))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7113,2003,"child","BENEMERITO DE LAS AMERICAS", 11))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7115,2003,"child","MARQUES DE COMILLAS", 11))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7052,2003,"parent","LAS MARGARITAS",12))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7114,2003,"child","MARAVILLA TENEJAPA", 12))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7002,2015,"parent","ACALA",13))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7120,2015,"child","EMILIANO ZAPATA", 13))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7093,2018,"parent","TECPATAN",14))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7121,2018,"child","MEZCALAPA", 14))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7081,2018,"parent","SILTEPEC",15))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7123,2018,"child","CAPITAN LUIS ANGEL VIDAL", 15))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7071,2018,"parent","PUEBLO NUEVO SOLISTLAHUACAN",16))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7124,2018,"child","RINCON CHAMULA", 16))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(12073,1997,"parent","ZAPOTITLAN TABLAS",17))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(12076,1997,"child","ACATEPEC", 17))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(12013,2006,"parent","AZOYU",18))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(12023,2006,"parent","CUAJINICUALAPA",18))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(12080,2006,"child","MARQUELIA", 18))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(12044,2009,"parent","METLATONOC",19))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(12079,2009,"child","COCHOAPA EL GRANDE", 19))) # 
#
#treat.yrs <- rbind(treat.yrs, tmp(c(12013,2009,"parent-again",20))) # AZOYU no need to double, will be dealt with by hand
treat.yrs <- rbind(treat.yrs, tmp(c(12081,2009,"child","JUCHITAN", 20))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(12028,2009,"parent","CHILAPA DE ALVAREZ",21))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(12077,2009,"child","JOSE JOAQUIN DE HERRERA", 21))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(12042,2009,"parent","MALINALTEPEC",22))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(12053,2009,"parent","SAN LUIS ACATLAN",22))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(12078,2009,"child","ILIATENCO", 22))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(14008,2006,"parent","ARANDAS",23))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(14125,2006,"child","SAN IGNACIO CERRO GORDO", 23))) # 
##
treat.yrs <- rbind(treat.yrs, tmp(c(15026,1994,"","parent",24))) # need seccion-level 1991 to manipulate these
treat.yrs <- rbind(treat.yrs, tmp(c(15040,1994,"","parent",24))) # need seccion-level 1991 to manipulate these
treat.yrs <- rbind(treat.yrs, tmp(c(15122,1994,"","child", 24))) # need seccion-level 1991 to manipulate these
#
treat.yrs <- rbind(treat.yrs, tmp(c(15083,2003,"parent","TEJUPILCO",25))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(15123,2003,"child","LUVIANOS", 25))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(15075,2003,"parent","SAN FELIPE DEL PROGRESO",26))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(15124,2003,"child","SAN JOSE DEL RINCON", 26))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(15045,2006,"parent","JALTENCO",27))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(15125,2006,"child","TONATITLA", 27))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(23002,1997,"parent","COZUMEL",28))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(23008,1997,"child","SOLIDARIDAD", 28))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(23003,2009,"parent","FELIPE CARRILLO PUERTO",29))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(23009,2009,"child","TULUM", 29))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(23007,2015,"parent","OTHON P BLANCO",30))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(23010,2015,"child","BACALAR", 30))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(23001,2018,"parent","BENITO JUAREZ",31))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(23011,2018,"child","PUERTO MORELOS", 31))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(24037,1997,"parent","TAMAZUNCHALE",32))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(24058,1997,"child","MATLAPA", 32))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(24010,1997,"parent","CIUDAD DEL MAIZ",33))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(24057,1997,"child","EL NARANJO", 33))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(26063,1997,"parent","ETCHOJOA",34))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(26071,1997,"child","BENITO JUAREZ", 34))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(26061,1997,"parent","GUAYMAS",35))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(26072,1997,"child","SAN IGNACIO RIO MUERTO", 35))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29020,1997,"parent","SANCTORUM DE LAZARO CARDENAS",36))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29060,1997,"child","BENITO JUAREZ", 36))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29030,1997,"parent","TERRENATE",37))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29054,1997,"child","EMILIANO ZAPATA", 37))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29055,1997,"child","LARARO CARDENAS", 37))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29010,1997,"parent","CHIAUTEMPAN",38))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29051,1997,"child","SAN FRANCISCO TETLANOHCAN", 38))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29052,1997,"child","LA MAGDALENA TLALTELULCO", 38))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29032,1997,"parent","TETLATLAHUCA",39))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29053,1997,"child","SAN DAMIAN TEXOLOC", 39))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29056,1997,"child","SAN JERONIMO ZACUALPAN", 39))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29038,1997,"parent","TZOMPANTEPEC",40))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29050,1997,"child","SAN JOSE TEACALCO", 40))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29029,1997,"parent","TEPEYANCO",41))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29047,1997,"child","SAN JUAN HUACTZINCO", 41))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29049,1997,"child","SANTA ISABEL XILOXOXTLA", 41))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29044,1997,"parent","ZACATELCO",42))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29048,1997,"child","SANTA CATARINA AYOMETLA", 42))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29059,1997,"child","SAN LORENZO AXOCOMANITLA", 42))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29040,1997,"parent","XALTOCAN",43))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29057,1997,"child","SAN LUCAS TECOPILCO", 43))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29015,1997,"parent","IXTACUIXTLA DE MARIANO MATAMOROS",44))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29058,1997,"child","SANTA ANA NOPALUCAN", 44))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29023,1997,"parent","NATIVITAS",45))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29045,1997,"child","SANTA APOLONIA TEACALCO", 45))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29022,1997,"parent","ACUAMANALA DE MIGUEL HIDALGO",46))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29046,1997,"child","SANTA CRUZ QUILEHTLA", 46))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(30047,1997,"parent","COSAMALOAPAN DE CARPIO",47))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30208,1997,"child","CARLOS A. CARRILLO", 47))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(30105,1997,"parent","MECAYAPAN",48))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30150,1997,"parent","SOTEAPAN",48))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30210,1997,"child","TATAHUICAPAN DE JUAREZ", 48))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(30072,1997,"parent","HIDALGOTITLAN",49))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30093,1997,"parent","JESUS CARRANZA",49))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30109,1997,"parent","MINATITLAN",49))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30209,1997,"child","UXPANAPA", 49))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(30103,2006,"parent","MARTINEZ DE LA TORRE",50))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30211,2006,"child","SAN RAFAEL", 50))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(30131,2006,"parent","PLAYA VICENTE",51))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30212,2006,"child","SANTIAGO SOCHIAPA", 51))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(32017,2003,"parent","GUADALUPE",52))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(32057,2003,"child","TRANCOSO", 52))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(32047,2009,"parent","TEUL DE GONZALEZ ORTEGA",53))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(32058,2009,"child","SANTA MARIA DE LA PAZ", 53))) # 
colnames(treat.yrs) <- c("ife","yr.chg","childparent","mun","dyad")
# remove factors
treat.yrs <- as.matrix(treat.yrs)
treat.yrs <- data.frame(treat.yrs, stringsAsFactors = FALSE)
# make numeric
treat.yrs$ife <- as.numeric(treat.yrs$ife)
treat.yrs$yr.chg <- as.numeric(treat.yrs$yr.chg)
treat.yrs$dyad <- as.numeric(treat.yrs$dyad)


## NOT NEEDED FOR 2010 CENSUS
## ################################################
## ## chg 1997                                   ##
## ## ########                                   ##
## ## 2006 <- 1991manip 1994manip 1997 2000 2003 ##
## ## 2009 <- 1994manip 1997 2000 2003 2006      ##
## ## 2012 <- 1997 2000 2003 2006 2009           ##
## ## 2015 <- 2000 2003 2006 2009 2012           ##
## ## 2018 <- 2003 2006 2009 2012 2015           ##
## ################################################
## # parents
## sel <- which(treat.yrs$yr.chg==1997 & treat.yrs$childparent=="parent")
## target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
## if (length(sel)>0){
##     for (i in 1:length(sel)){
##         #i <- 44 # debug
##         sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
##         sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
##         #names(regs.2006$pan)[sel1]      # debug
##         #names(regs.2006manip$pan)[sel2] # debug
##         extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
##         regs.2006$pan [[sel1]] <- regs.2006manip$pan [[sel2]]
##         regs.2006$left[[sel1]] <- regs.2006manip$left[[sel2]]
##         regs.2006$oth [[sel1]] <- regs.2006manip$oth [[sel2]]
##         regs.2009$pan [[sel1]] <- regs.2009manip$pan [[sel2]]
##         regs.2009$left[[sel1]] <- regs.2009manip$left[[sel2]]
##         regs.2009$oth [[sel1]] <- regs.2009manip$oth [[sel2]]
##         #need to figure if mean.regsmanip should also be used---manips input skipped
##     }
## }
## # children
## sel <- which(treat.yrs$yr.chg==1997 & treat.yrs$childparent=="child")
## target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
## if (length(sel)>0){
##     for (i in 1:length(sel)){
##         #i <- 1 # debug
##         sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
##         sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
##         tmp <- extendCoal[[sel1]]     # duplicate for manipulation
##         sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
##         tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
##         tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
##         extendCoal[[sel1]] <- tmp     # return manipulated data
##         # no need to change regs forward in new muns
##         #need to figure if mean.regsmanip should also be used---manips input skipped
##     }
## }
#        
## NOT NEEDED FOR 2010 CENSUS
## ##############
## ## chg 2000 ##
## ##############
## ## 2006 <- 1991manip 1994manip 1997manip 2000 2003
## ## 2009 <- 1994manip 1997manip 2000 2003 2006
## ## 2012 <- 1997manip 2000 2003 2006 2009
## ## 2015 <- 2000 2003 2006 2009 2012
## ## 2018 <- 2003 2006 2009 2012 2015
## # parents
## sel <- which(treat.yrs$yr.chg==2000 & treat.yrs$childparent=="parent")
## target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
## if (length(sel)>0){
##     for (i in 1:length(sel)){
##         #i <- 1 # debug
##         sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
##         sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
##         #names(regs.2006$pan)[sel1]      # debug
##         #names(regs.2006manip$pan)[sel2] # debug
##         extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
##         regs.2006$pan [[sel1]] <- regs.2006manip$pan [[sel2]]
##         regs.2006$left[[sel1]] <- regs.2006manip$left[[sel2]]
##         regs.2006$oth [[sel1]] <- regs.2006manip$oth [[sel2]]
##         regs.2009$pan [[sel1]] <- regs.2009manip$pan [[sel2]]
##         regs.2009$left[[sel1]] <- regs.2009manip$left[[sel2]]
##         regs.2009$oth [[sel1]] <- regs.2009manip$oth [[sel2]]
##         regs.2012$pan [[sel1]] <- regs.2012manip$pan [[sel2]]
##         regs.2012$left[[sel1]] <- regs.2012manip$left[[sel2]]
##         regs.2012$oth [[sel1]] <- regs.2012manip$oth [[sel2]]
##         #need to figure if mean.regsmanip should also be used---manips input skipped
##     }
## }
## # children
## sel <- which(treat.yrs$yr.chg==2000 & treat.yrs$childparent=="child")
## target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
## if (length(sel)>0){
##     for (i in 1:length(sel)){
##         #i <- 1 # debug
##         sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
##         sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
##         tmp <- extendCoal[[sel1]]     # duplicate for manipulation
##         sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
##         tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
##         tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
##         tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
##         extendCoal[[sel1]] <- tmp     # return manipulated data
##         # no need to fwd change regs in new muns
##         #need to figure if mean.regsmanip should also be used---manips input skipped
##     }
## }
#
## NOT NEEDED FOR 2010 CENSUS
## ##############
## ## chg 2003 ##
## ##############
## ## 2006 <- 1991manip 1994manip 1997manip 2000manip 2003
## ## 2009 <- 1994manip 1997manip 2000manip 2003 2006
## ## 2012 <- 1997manip 2000manip 2003 2006 2009
## ## 2015 <- 2000manip 2003 2006 2009 2012
## ## 2018 <- 2003 2006 2009 2012 2015
## # parents
## sel <- which(treat.yrs$yr.chg==2003 & treat.yrs$childparent=="parent")
## target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
## if (length(sel)>0){
##     for (i in 1:length(sel)){
##         #i <- 1 # debug
##         sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
##         sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
##         #names(regs.2006$pan)[sel1]      # debug
##         #names(regs.2006manip$pan)[sel2] # debug
##         extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
##         regs.2006$pan [[sel1]] <- regs.2006manip$pan [[sel2]]
##         regs.2006$left[[sel1]] <- regs.2006manip$left[[sel2]]
##         regs.2006$oth [[sel1]] <- regs.2006manip$oth [[sel2]]
##         regs.2009$pan [[sel1]] <- regs.2009manip$pan [[sel2]]
##         regs.2009$left[[sel1]] <- regs.2009manip$left[[sel2]]
##         regs.2009$oth [[sel1]] <- regs.2009manip$oth [[sel2]]
##         regs.2012$pan [[sel1]] <- regs.2012manip$pan [[sel2]]
##         regs.2012$left[[sel1]] <- regs.2012manip$left[[sel2]]
##         regs.2012$oth [[sel1]] <- regs.2012manip$oth [[sel2]]
##         regs.2015$pan [[sel1]] <- regs.2015manip$pan [[sel2]]
##         regs.2015$left[[sel1]] <- regs.2015manip$left[[sel2]]
##         regs.2015$oth [[sel1]] <- regs.2015manip$oth [[sel2]]
##         #need to figure if mean.regsmanip should also be used---manips input skipped
##     }
## }
## # children
## sel <- which(treat.yrs$yr.chg==2003 & treat.yrs$childparent=="child")
## target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
## if (length(sel)>0){
##     for (i in 1:length(sel)){
##         #i <- 1 # debug
##         sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
##         sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
##         tmp <- extendCoal[[sel1]]     # duplicate for manipulation
##         sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
##         tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
##         tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
##         tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
##         tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
##         extendCoal[[sel1]] <- tmp     # return manipulated data
##         # no need to change regs fwd in new muns
##         #need to figure if mean.regsmanip should also be used---manips input skipped
##     }
## }
#
## NOT NEEDED FOR 2010 CENSUS
## ##############
## ## chg 2006 ##
## ##############
## ## 2006 <- 1991manip 1994manip 1997manip 2000manip 2003manip
## ## 2009 <- 1994manip 1997manip 2000manip 2003manip 2006
## ## 2012 <- 1997manip 2000manip 2003manip 2006 2009
## ## 2015 <- 2000manip 2003manip 2006 2009 2012
## ## 2018 <- 2003manip 2006 2009 2012 2015
## # parents
## sel <- which(treat.yrs$yr.chg==2006 & treat.yrs$childparent=="parent")
## target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
## if (length(sel)>0){
##     for (i in 1:length(sel)){
##         #i <- 1 # debug
##         sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
##         sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
##         #names(regs.2006$pan)[sel1]      # debug
##         #names(regs.2006manip$pan)[sel2] # debug
##         extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
##         regs.2006$pan [[sel1]] <- regs.2006manip$pan [[sel2]]
##         regs.2006$left[[sel1]] <- regs.2006manip$left[[sel2]]
##         regs.2006$oth [[sel1]] <- regs.2006manip$oth [[sel2]]
##         regs.2009$pan [[sel1]] <- regs.2009manip$pan [[sel2]]
##         regs.2009$left[[sel1]] <- regs.2009manip$left[[sel2]]
##         regs.2009$oth [[sel1]] <- regs.2009manip$oth [[sel2]]
##         regs.2012$pan [[sel1]] <- regs.2012manip$pan [[sel2]]
##         regs.2012$left[[sel1]] <- regs.2012manip$left[[sel2]]
##         regs.2012$oth [[sel1]] <- regs.2012manip$oth [[sel2]]
##         regs.2015$pan [[sel1]] <- regs.2015manip$pan [[sel2]]
##         regs.2015$left[[sel1]] <- regs.2015manip$left[[sel2]]
##         regs.2015$oth [[sel1]] <- regs.2015manip$oth [[sel2]]
##         regs.2018$pan [[sel1]] <- regs.2018manip$pan [[sel2]]
##         regs.2018$left[[sel1]] <- regs.2018manip$left[[sel2]]
##         regs.2018$oth [[sel1]] <- regs.2018manip$oth [[sel2]]
##         #need to figure if mean.regsmanip should also be used---manips input skipped
##     }
## }
## # children
## sel <- which(treat.yrs$yr.chg==2006 & treat.yrs$childparent=="child")
## target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
## if (length(sel)>0){
##     for (i in 1:length(sel)){
##         #i <- 1 # debug
##         sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
##         sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
##         tmp <- extendCoal[[sel1]]     # duplicate for manipulation
##         sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
##         tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
##         tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
##         tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
##         tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
##         tmp[tmp$yr==2003,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2003)        
##         extendCoal[[sel1]] <- tmp     # return manipulated data
##         # no need to change regs fwd in new muns
##         #need to figure if mean.regsmanip should also be used---manips input skipped
##     }
## }
#
## NOT NEEDED FOR 2010 CENSUS
## ##############
## ## chg 2009 ##
## ##############
## ## 2006 <- 1991 1994 1997 2000 2003
## ## 2009 <- 1994manip 1997manip 2000manip 2003manip 2006manip
## ## 2012 <- 1997manip 2000manip 2003manip 2006manip 2009
## ## 2015 <- 2000manip 2003manip 2006manip 2009 2012
## ## 2018 <- 2003manip 2006manip 2009 2012 2015
## # parents
## sel <- which(treat.yrs$yr.chg==2009 & treat.yrs$childparent=="parent")
## target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
## if (length(sel)>0){
##     for (i in 1:length(sel)){
##         #i <- 1 # debug
##         sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
##         sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
##         #names(regs.2006$pan)[sel1]      # debug
##         #names(regs.2006manip$pan)[sel2] # debug
##         extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
##         regs.2009$pan [[sel1]] <- regs.2009manip$pan [[sel2]]
##         regs.2009$left[[sel1]] <- regs.2009manip$left[[sel2]]
##         regs.2009$oth [[sel1]] <- regs.2009manip$oth [[sel2]]
##         regs.2012$pan [[sel1]] <- regs.2012manip$pan [[sel2]]
##         regs.2012$left[[sel1]] <- regs.2012manip$left[[sel2]]
##         regs.2012$oth [[sel1]] <- regs.2012manip$oth [[sel2]]
##         regs.2015$pan [[sel1]] <- regs.2015manip$pan [[sel2]]
##         regs.2015$left[[sel1]] <- regs.2015manip$left[[sel2]]
##         regs.2015$oth [[sel1]] <- regs.2015manip$oth [[sel2]]
##         regs.2018$pan [[sel1]] <- regs.2018manip$pan [[sel2]]
##         regs.2018$left[[sel1]] <- regs.2018manip$left[[sel2]]
##         regs.2018$oth [[sel1]] <- regs.2018manip$oth [[sel2]]
##         #need to figure if mean.regsmanip should also be used---manips input skipped
##     }
## }
## # children
## sel <- which(treat.yrs$yr.chg==2009 & treat.yrs$childparent=="child")
## target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
## if (length(sel)>0){
##     for (i in 1:length(sel)){
##         #i <- 1 # debug
##         sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
##         sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
##         tmp <- extendCoal[[sel1]]     # duplicate for manipulation
##         sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
##         tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
##         tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
##         tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
##         tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
##         tmp[tmp$yr==2003,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2003)        
##         tmp[tmp$yr==2006,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2006)        
##         extendCoal[[sel1]] <- tmp     # return manipulated data
##         # no need to change regs fwd in new muns
##         #need to figure if mean.regsmanip should also be used---manips input skipped
##     }
## }
#
##############
## chg 2012 ##
##############
## 2006 <- 1991 1994 1997 2000 2003
## 2009 <- 1994 1997 2000 2003 2006
## 2012 <- 1997manip 2000manip 2003manip 2006manip 2009manip
## 2015 <- 2000manip 2003manip 2006manip 2009manip 2012
## 2018 <- 2003manip 2006manip 2009manip 2012 2015
# parents
sel <- which(treat.yrs$yr.chg==2012 & treat.yrs$childparent=="parent")
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        #names(regs.2006$pan)[sel1]      # debug
        #names(regs.2006manip$pan)[sel2] # debug
        extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
        regs.2012$pan [[sel1]] <- regs.2012manip$pan [[sel2]]
        regs.2012$left[[sel1]] <- regs.2012manip$left[[sel2]]
        regs.2012$oth [[sel1]] <- regs.2012manip$oth [[sel2]]
        regs.2015$pan [[sel1]] <- regs.2015manip$pan [[sel2]]
        regs.2015$left[[sel1]] <- regs.2015manip$left[[sel2]]
        regs.2015$oth [[sel1]] <- regs.2015manip$oth [[sel2]]
        regs.2018$pan [[sel1]] <- regs.2018manip$pan [[sel2]]
        regs.2018$left[[sel1]] <- regs.2018manip$left[[sel2]]
        regs.2018$oth [[sel1]] <- regs.2018manip$oth [[sel2]]
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}
# children
sel <- which(treat.yrs$yr.chg==2012 & treat.yrs$childparent=="child")
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        tmp <- extendCoal[[sel1]]     # duplicate for manipulation
        sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
        tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
        tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
        tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
        tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
        tmp[tmp$yr==2003,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2003)        
        tmp[tmp$yr==2006,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2006)        
        tmp[tmp$yr==2009,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2009)        
        extendCoal[[sel1]] <- tmp     # return manipulated data
        # no need to change regs fwd in new muns
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}
#
###############################################################
## chg 2015                                                  ##
## ########                                                  ##
## 2006 <- 1991 1994 1997 2000 2003                          ##
## 2009 <- 1994 1997 2000 2003 2006                          ##
## 2012 <- 1997 2000 2003 2006 2009                          ##
## 2015 <- 2000manip 2003manip 2006manip 2009manip 2012manip ##
## 2018 <- 2003manip 2006manip 2009manip 2012manip 2015      ##
###############################################################
# parents
sel <- which(treat.yrs$yr.chg==2015 & treat.yrs$childparent=="parent")
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel <- which(censo.sec$ife!=censo.sec$mun2015)
        censo.sec[sel[1],]
        
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        #names(regs.2006$pan)[sel1]      # debug
        #names(regs.2006manip$pan)[sel2] # debug
        extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
        regs.2015$pan [[sel1]] <- regs.2015manip$pan [[sel2]]
        regs.2015$left[[sel1]] <- regs.2015manip$left[[sel2]]
        regs.2015$oth [[sel1]] <- regs.2015manip$oth [[sel2]]
        regs.2018$pan [[sel1]] <- regs.2018manip$pan [[sel2]]
        regs.2018$left[[sel1]] <- regs.2018manip$left[[sel2]]
        regs.2018$oth [[sel1]] <- regs.2018manip$oth [[sel2]]
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}
# children
sel <- which(treat.yrs$yr.chg==2015 & treat.yrs$childparent=="child")
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        tmp <- extendCoal[[sel1]]     # duplicate for manipulation
        sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
        tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
        tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
        tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
        tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
        tmp[tmp$yr==2003,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2003)        
        tmp[tmp$yr==2006,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2006)        
        tmp[tmp$yr==2009,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2009)        
        tmp[tmp$yr==2012,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2012)        
        extendCoal[[sel1]] <- tmp     # return manipulated data
        # no need to change regs fwd in new muns
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}
#
###############################################################
## chg 2018                                                  ##
## ########                                                  ##
## 2006 <- 1991 1994 1997 2000 2003                          ##
## 2009 <- 1994 1997 2000 2003 2006                          ##
## 2012 <- 1997 2000 2003 2006 2009                          ##
## 2015 <- 2000 2003 2006 2009 2012                          ##
## 2018 <- 2003manip 2006manip 2009manip 2012manip 2015manip ##
###############################################################
# parents
sel <- which(treat.yrs$yr.chg==2018 & treat.yrs$childparent=="parent")
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        #names(regs.2006$pan)[sel1]      # debug
        #names(regs.2006manip$pan)[sel2] # debug
        extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
        regs.2018$pan [[sel1]] <- regs.2018manip$pan [[sel2]]
        regs.2018$left[[sel1]] <- regs.2018manip$left[[sel2]]
        regs.2018$oth [[sel1]] <- regs.2018manip$oth [[sel2]]
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}
# children
sel <- which(treat.yrs$yr.chg==2018 & treat.yrs$childparent=="child")
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        tmp <- extendCoal[[sel1]]     # duplicate for manipulation
        sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
        tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
        tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
        tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
        tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
        tmp[tmp$yr==2003,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2003)        
        tmp[tmp$yr==2006,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2006)        
        tmp[tmp$yr==2009,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2009)        
        tmp[tmp$yr==2012,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2012)        
        tmp[tmp$yr==2015,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2015)        
        extendCoal[[sel1]] <- tmp     # return manipulated data
        # no need to change regs fwd in new muns
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}

################################
## FIX TWICE SPLIT MUNICIPIOS ##
################################
#
# read estimates (produced externally with script code/script-to-fix-twice-split-muns.r)
load(file = paste(wd, "data/regs-to-fix-twice-split-muns.RData", sep = "/"))
## # BLOCK MANIPULATED ABOVE
## ###############################################################
## ## chg 2006                                                  ##
## ## ########                                                  ##
## ## 2006 <- 1991manip 1994manip 1997manip 2000manip 2003manip ##
## ###############################################################
## target.ife <- 12013 # azoyu parent
## #for (i in 1:length(sel)){
##     i <- 1 # debug
##     sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
##     sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
##     #names(regs.2006$pan)[sel1]      # debug
##     #names(regs.2006manip$pan)[sel2] # debug
##     extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
##     regs.2006$pan [[sel1]] <- regs.2006manip$pan [[sel2]]
##     regs.2006$left[[sel1]] <- regs.2006manip$left[[sel2]]
##     regs.2006$oth [[sel1]] <- regs.2006manip$oth [[sel2]]
##     #need to figure if mean.regsmanip should also be used---manips input skipped
## #}
## target.ife <- 12080 # child
## #for (i in 1:length(sel)){
##     i <- 1 # debug
##     sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
##     sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
##     tmp <- extendCoal[[sel1]]     # duplicate for manipulation
##     sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
##     tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
##     tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
##     tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
##     tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
##     tmp[tmp$yr==2003,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2003)        
##     extendCoal[[sel1]] <- tmp     # return manipulated data
##     # no need to change regs fwd in new muns
##     #need to figure if mean.regsmanip should also be used---manips input skipped
## #}
#
####################################################################
## chg 2009                                                       ##
## ########                                                       ##
## 2009 <- 1994manip2 1997manip2 2000manip2 2003manip2 2006manip2 ##
## 2012 <- 1997manip2 2000manip2 2003manip2 2006manip2 2009       ##
## 2015 <- 2000manip2 2003manip2 2006manip2 2009 2012             ##
## 2018 <- 2003manip2 2006manip2 2009 2012 2015                   ##
####################################################################
target.ife <- 12013 # azoyu parent
i <- 1 # debug
sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
extendCoal[[sel1]] <- extendCoalmanip2[[1]]
regs.2009$pan [[sel1]] <- regs.2009manip2$pan 
regs.2009$left[[sel1]] <- regs.2009manip2$left
regs.2009$oth [[sel1]] <- regs.2009manip2$oth 
regs.2012$pan [[sel1]] <- regs.2012manip2$pan 
regs.2012$left[[sel1]] <- regs.2012manip2$left
regs.2012$oth [[sel1]] <- regs.2012manip2$oth 
regs.2015$pan [[sel1]] <- regs.2015manip2$pan 
regs.2015$left[[sel1]] <- regs.2015manip2$left
regs.2015$oth [[sel1]] <- regs.2015manip2$oth 
regs.2018$pan [[sel1]] <- regs.2018manip2$pan 
regs.2018$left[[sel1]] <- regs.2018manip2$left
regs.2018$oth [[sel1]] <- regs.2018manip2$oth 
#need to figure if mean.regsmanip should also be used---manips input skipped
target.ife <- 12081 # child
#for (i in 1:length(sel)){
    i <- 1 # debug
    sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
    sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
    tmp <- extendCoal[[sel1]]     # duplicate for manipulation
    sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
    tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
    tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
    tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
    tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
    tmp[tmp$yr==2003,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2003)        
    tmp[tmp$yr==2006,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2006)        
    extendCoal[[sel1]] <- tmp     # return manipulated data
    # no need to change regs fwd in new muns
    #need to figure if mean.regsmanip should also be used---manips input skipped
#}
#
# clean
rm(v91manip, v94manip, v97manip, v00manip, v03manip, v06manip, v09manip, v12manip, v15manip, v18manip,
   regs.2006manip, regs.2009manip, regs.2012manip, regs.2015manip, regs.2018manip, mean.regsmanip,
   regs.2006manip2, regs.2009manip2, regs.2012manip2, regs.2015manip2, regs.2018manip2, mean.regsmanip2,
   extendCoalmanip, extendCoalmanip2)
rm(v91,v94,v97,v00,v03,v06,v09,v12,v15,v18)
rm(pan,pri,left,oth,efec)
rm(sel,sel1,sel2,sel.to,sel.c,target.ife,i,tmp)
