# plug into vot
vot$govpty.lag <- NA
for (e in 1:32){ # loop across states
    message(sprintf("loop %s", e))
    #e <- 17
    sel.e <- which(vot$edon==e) # state e's indices in vot
    vot.e <- vot[sel.e,]        # subset state e in vot
    for (y in 1994:2023){ # loop over years up to 2023
        #y <- 2006
        sel.y <- which(gob$edon==e & gob$yr==y) # index for year y
        sel.ey <- which(vot.e$yr==y) # subset state's obs in year y
        if (length(sel.ey)>0){
            vot.e$govpty.lag[sel.ey] <- gob$govpty.lag[sel.y]
        }
    }
    vot[sel.e,] <- vot.e # return manipulated subset to vot
}
        
vot[10000,]        



tmp.dir <- paste("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/secciones/censo2010", edos[i], sep = "/")
tmp.file <- grep("secciones.+dbf", dir(tmp.dir))
tmp.file <- dir(tmp.dir)[grep("secciones.+csv", dir(tmp.dir))]
tmp <- read.dbf(paste(tmp.dir, tmp.file, sep = "/"), as.is = TRUE)
write.csv(tmp, paste(tmp.dir, "secciones_18.csv", sep = "/"))
