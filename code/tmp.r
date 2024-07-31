d <- read.table(file = "clipboard", sep = "\t", header=TRUE)
head(d)
colnames(d)
d2 <- d
d$pan             <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$pri             <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$prd             <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$pvem            <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$pt              <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$mc              <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$morena          <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$indep           <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$pan.pri.prd     <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$pan.pri         <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$pan.prd         <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$pri.prd         <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$morena.pt.pvem  <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$morena.pt       <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$morena.pvem     <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$pt.pvem         <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$nr              <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$nul             <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$tot             <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d$lisnom          <- ave(d$ , as.factor(d$munn), FUN=function(x) sum(x, na.rm=TRUE))
d <- d[duplicated(d$munn)==FALSE,]
dim(d)
setwd("~/Downloads")
write.csv(d, file = "tmp.csv", row.names=FALSE)

Parece que, finalmente, quedó funcional el desagüe de la tarja de mi departamento. Fue por lo buenos oficios de Valentina. Narro sucesos para la bitácora. 

(1) El miércoles Claclis nos mandó a Bruce y su gente. Les pedí que quitaran el triturador. Tras varias visitas/intervenciones en 2023 de Isaí, que hizo la instalación del drenaje, yo sospechaba al triturador por quedar muy bajo en una tina tan profunda. 

(2) Lejos de resolver el problema, con Bruce se tapó por completo la tarja. Diagnosticó la necesidad de reconectar el desagüe por el plafón del departamento 161.

(3) Ayer Vala tuvo que pegar de gritos porque ni Claclá, ni Ibeli, ni Viento se responsabilizan del problema. Primero a Ibeli, para que trabajara desde el 161. Luego a David Cova, para que autorizara entrar al plafón 161. Luego a Oscar, para lo mismo. 

(4) Del 161 salió una tubería llena de arena, que cambió la gente de Cova. Cova e Isaí culpan de esto es Ibeli. Ibeli se dice chivo expiatorio: él coló la cocina al final.

(5) Ardió Troya y ahora todo el mundo está molesto. Responsabilidad de Claclá en todo esto? 

(6) Importa separar responsabilidades para argumentar cuando Cova pase la cuenta (y Bruce e Ibeli). 


