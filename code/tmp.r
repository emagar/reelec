d <- read.table(file = "clipboard", sep = "\t", header=TRUE)
head(d)
d$inegi <- ife2inegi(d$ife)
colnames(d)
sel.c <- which(colnames(d) %in% c("pan", "pri", "prd", "pvem", "pt", "mc", "morena", "pna", "pan.pri.prd.pna", "pan.pri.prd", "pan.pri.pna", "pan.prd.pna", "pri.prd.pna", "pan.pri", "pan.prd", "pan.naem", "pri.prd", "pri.naem", "prd.naem", "pvem.pt.morena", "pvem.pt", "pvem.morena", "pt.morena", "pan.pri.prd.naem.cc", "pvem.pt.morena.cc", "LUIS_FERNANDO_AMBROCIO_FLORES", "JORGE_MARTINEZ_SANTIAGO", "BEATRIZ_CHAVARRIA_COBOS", "ARMANDO_TRUJILLO_VALDIN", "DANIEL_JUAREZ_JUAREZ", "JOSE_LUIS_OROZPE_LOPEZ", "JUAN_ALVARADO_SOLIS", "XOCHITL_AMERICA_VARILLER_RAMIREZ", "MONICA_COREY_MORALES_TRUJILLO", "nr", "nul", "tot", "lisnom"))

str(d)#[,sel.c])
for (i in sel.c){
    d[,i] <- as.numeric(d[,i])
}
d2 <- d
d2 -> d

for (i in sel.c){
    d[,i] <- ave(d[,i], as.factor(d$inegi), FUN=function(x) sum(x, na.rm=TRUE))
}

d$pan             <- ave(d$pan             , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pri             <- ave(d$pri             , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$prd             <- ave(d$prd             , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pvem            <- ave(d$pvem            , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt              <- ave(d$pt              , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$mc              <- ave(d$mc              , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$psi             <- ave(d$psi             , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$morena          <- ave(d$morena          , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pna             <- ave(d$pna             , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$fxm             <- ave(d$fxm             , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$indep1           <- ave(d$indep1         , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$indep2           <- ave(d$indep2         , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pan.pri.prd.psi        <- ave(d$pan.pri.prd.psi        , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pan.pri.prd            <- ave(d$pan.pri.prd            , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pan.pri.psi            <- ave(d$pan.pri.psi            , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pan.prd.psi            <- ave(d$pan.prd.psi            , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pri.prd.psi            <- ave(d$pri.prd.psi            , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pan.pri                <- ave(d$pan.pri                , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pan.prd                <- ave(d$pan.prd                , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pan.psi                <- ave(d$pan.psi                , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pri.prd                <- ave(d$pri.prd                , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pri.psi                <- ave(d$pri.psi                , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$prd.psi                <- ave(d$prd.psi                , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.pvem.morena.pna.fxm <- ave(d$pt.pvem.morena.pna.fxm , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.pvem.morena.pna     <- ave(d$pt.pvem.morena.pna     , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.pvem.morena.fxm     <- ave(d$pt.pvem.morena.fxm     , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.pvem.pna.fxm        <- ave(d$pt.pvem.pna.fxm        , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.morena.pna.fxm      <- ave(d$pt.morena.pna.fxm      , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pvem.morena.pna.fxm    <- ave(d$pvem.morena.pna.fxm    , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.pvem.morena         <- ave(d$pt.pvem.morena         , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.pvem.pna            <- ave(d$pt.pvem.pna            , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.pvem.fxm            <- ave(d$pt.pvem.fxm            , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.morena.pna          <- ave(d$pt.morena.pna          , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.morena.fxm          <- ave(d$pt.morena.fxm          , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.pna.fxm             <- ave(d$pt.pna.fxm             , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pvem.morena.pna        <- ave(d$pvem.morena.pna        , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pvem.morena.fxm        <- ave(d$pvem.morena.fxm        , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pvem.pna.fxm           <- ave(d$pvem.pna.fxm           , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$morena.pna.fxm         <- ave(d$morena.pna.fxm         , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.pvem                <- ave(d$pt.pvem                , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.morena              <- ave(d$pt.morena              , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.pna                 <- ave(d$pt.pna                 , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.fxm                 <- ave(d$pt.fxm                 , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pvem.morena            <- ave(d$pvem.morena            , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pvem.pna               <- ave(d$pvem.pna               , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pvem.fxm               <- ave(d$pvem.fxm               , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$morena.pna             <- ave(d$morena.pna             , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$morena.fxm             <- ave(d$morena.fxm             , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$pna.fxm                <- ave(d$pna.fxm                , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$nr              <- ave(d$nr              , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$nul             <- ave(d$nul             , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$tot             <- ave(d$tot             , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d$lisnom          <- ave(d$lisnom          , as.factor(d$mun), FUN=function(x) sum(x, na.rm=TRUE))
d <- d[duplicated(d$inegi)==FALSE,]
dim(d)
setwd("~/Downloads")
write.csv(d, file = "~/Downloads/tmp.csv", row.names=FALSE)


Parece que, finalmente, quedó funcional el desagüe de la tarja de mi departamento. Fue por lo buenos oficios de Valentina. Narro sucesos para la bitácora. 

(1) El miércoles Claclis nos mandó a Bruce y su gente. Les pedí que quitaran el triturador. Tras varias visitas/intervenciones en 2023 de Isaí, que hizo la instalación del drenaje, yo sospechaba al triturador por quedar muy bajo en una tina tan profunda. 

(2) Lejos de resolver el problema, con Bruce se tapó por completo la tarja. Diagnosticó la necesidad de reconectar el desagüe por el plafón del departamento 161.

(3) Ayer Vala tuvo que pegar de gritos porque ni Claclá, ni Ibeli, ni Viento se responsabilizan del problema. Primero a Ibeli, para que trabajara desde el 161. Luego a David Cova, para que autorizara entrar al plafón 161. Luego a Oscar, para lo mismo. 

(4) Del 161 salió una tubería llena de arena, que cambió la gente de Cova. Cova e Isaí culpan de esto es Ibeli. Ibeli se dice chivo expiatorio: él coló la cocina al final.

(5) Ardió Troya y ahora todo el mundo está molesto. Responsabilidad de Claclá en todo esto? 

(6) Importa separar responsabilidades para argumentar cuando Cova pase la cuenta (y Bruce e Ibeli). 


