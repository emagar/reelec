######################################################################################################
## A function to match names in any order                                                           ##
##                                                                                                  ##
## adapted 30-5-2019 from                                                                           ##
## https://stackoverflow.com/questions/22894265/how-to-perform-approximate-fuzzy-name-matching-in-r ##
## by Eric Magar                                                                                    ##
######################################################################################################

search_names <- function(find_name = NA,                    # TARGET, NAME TO LOOK FOR
                         within_records = NA,               # VECTOR OF NAMES WHERE TARGET WILL BE SEARCHED
                         include_records_in_output = FALSE, # WHETHER 
                         report_hit = FALSE){               # WHETHER 
    require(gtools);
    names <- within_records;
    search_for <- find_name;
    #names <- tmp[-1]; search_for <- tmp[1] # debug
    #
    ## # debug
    ## # generate name records
    ## names <- c(
    ##     "John Smith", 
    ##     "Robert Allen Zimmerman (Bob Dylan)",
    ##     "Bob Dylan",
    ##     "Valentina Margarita Isabel Riquelme Molina",
    ##     "Valentina Riquelme Molina",
    ##     "Smith J",
    ##     "Smith John",
    ##     "John S",
    ##     "John Sally",
    ##     "A B C",
    ##     "B C"
    ## );
    ## # suppose we're looking for this name
    ## search_for <- "Valentina Margarita Isabel Riquelme";
    #
    # split names and get all ways to write each name into a list of same length
    split_names <- lapply(
        X = gsub("[(|)]", "", names), # drop parentheses, if any
        FUN = function(x){
            #print(x);
            # split by a space
            c_split = unlist(x = strsplit(x = x, split = " "));
            # get all permutations of c_split to compensate for order
            n <- r <- length(c_split);
            c_splits <- list(permutations(n=n, r=r, v=c_split, set = FALSE, repeats.allowed = FALSE));
            # return c_splits
            c_splits;
        }
    )
    # 
    # split it by " " and then find all ways to write search name
    search_for_split <- unlist(x = strsplit(x = search_for, split = " "));
    # permutations over search_for_split seem redundant
    #
    # initialise a vector containing if search_for was matched in names
    match_statuses <- c();
    #
    # for each name that's been split
    for (i in 1:length(names)){
        #i <- 5 # debug
        # the match status for the current name
        match_status <- FALSE;
        #
        # the current split name
        c_split_name <- as.data.frame(split_names[[i]]);
        #
        # for each element in c_split_name
        for (j in 1:nrow(c_split_name)){
            #j <- 1 # debug
            # the current permutation of current split name
            c_c_split_name <- as.matrix(c_split_name[j,]);
            #
            # will receive hits in name's words, one by one, in sequence
            hits <- rep(0, 20) # no name should have more than 20 words... else what?
            #
            # for each element in search_for_split
            for (k in 1:length(search_for_split)){
                #k <- 2 # debug
                # the current permutation of name
                c_search_for_split <- search_for_split[[k]];
                #
                # L first hits will receive hit counts
                L <- min(ncol(c_c_split_name), length(search_for_split));
                #
                # will match as many elements as the shortest of current pair of names has  
                for (l in 1:L){
                    #l <- 1 # debug
                    # if there's a match, the length of grep is greater than zero
                    if (
                        # is c_search_for_split in c_c_split_name's lth element
                        length(
                            grep(
                                pattern = c_search_for_split,
                                x = as.character(c_c_split_name[l]),
                                ignore.case = TRUE
                            )
                        ) > 0 ||
                        # or, is c_c_split_name's lth element in c_search_for_split
                        length(
                            grep(
                                pattern = c_c_split_name[l],
                                x = c_search_for_split,
                                ignore.case = TRUE
                            )
                        ) > 0
                        #
                    # if this is the case, record a hit    
                    ){
                        hits[l] <- 1;
                    } else {
                    # otherwise, don't update hit
                    }
                }
            }
            # take L first elements
            hits <- hits[1:L]
            #
            # if hits vector has all ones for this permutation, update match status to TRUE
            if (
                sum(hits)/length(hits)==1 # <- can/should be made more flexible (agrep, or sum/length<1)
            ){
                match_status <- TRUE;
                if (report_hit==TRUE){
                    message(sprintf("found %s in", search_for)); # report the hit on screen
                    print(c_c_split_name)
                        }
            } else {
            # otherwise, don't update match status
            }
        }
        #
        # append match_status to the match_statuses list
        match_statuses <- c(match_statuses, match_status);
    }
    #
    # output
    if (
        include_records_in_output==TRUE
    ){
        return(cbind(names, match_statuses))
    } else {
        return(match_statuses)
    }
}


#search_names(find_name = "B A C", within_records = names, include_records_in_output = TRUE)

# try it
## search_names(find_name = "J Smith",
##              within_records = c(
##                  "John Smith", 
##                  "Robert Allen Zimmerman (Bob Dylan)",
##                  "Valentina Margarita Isabel Riquelme Molina",
##                  "Smith J",
##                  "Smith John",
##                  "John S",
##                  "John Sally"),
##              include_records_in_output = TRUE,
##              report_hit = TRUE
##              )
 
Falta
1. para eficientar:
            a) seleccionar sólo subconjunto de nombres de estado(s) de interés
            b) hacer permutación de nombres, vectorizar
            c) data frame con el vector b, el emm o inegi del municipio, una columna de hit completo, otra de N palabras,
               N caracteres, N palabras completas, N palabras fuzzy, N caracteres comunes, y una columna excluir = 1 para NAs 0 demás
            d) buscar nombre más largo primero, excluirlo = 1, si hit se excluye también ése
            e) loop sobre subconjunto a menos exlcuidos
1. fuzzy matching agrep
2. hit 2 de 3, 3 de 4, etc


## ##########################################################################################################
## ## This version (a) seems tho show that permutations over search_for (carried) are indeed redundant and ##
## ##              (b) by adding one more loop, becomes much much slower seaching for lon names            ##
## ##########################################################################################################
## search_names2 <- function(find_name = NA,
##                           within_records = NA,
##                           include_records_in_output = FALSE){
##     require(gtools);
##     names <- within_records;
##     search_for <- find_name;
##     #names <- tmp[-1]; search_for <- tmp[1] # debug
##     #
##     # debug
##     # generate name records
##     ## names <- c(
##     ##     "John Smith", 
##     ##     "Robert Allen Zimmerman (Bob Dylan)",
##     ##     "Bob Dylan",
##     ##     "Valentina Margarita Isabel Riquelme Molina",
##     ##     "Valentina Riquelme Molina",
##     ##     "Smith J",
##     ##     "Smith John",
##     ##     "John S",
##     ##     "John Sally"
##     ## );
##     # suppose we're looking for this name
##     #search_for <- "Bob Dylan";
##     #
##     # split names and get all ways to write each name into a list of same length
##     split_names <- lapply(
##         X = gsub("[(|)]", "", names), # drop parentheses, if any
##         FUN = function(x){
##             #print(x);
##             # split by a space
##             c_split = unlist(x = strsplit(x = x, split = " "));
##             # get all permutations of c_split to compensate for order
##             n <- r <- length(c_split);
##             c_splits <- list(permutations(n=n, r=r, v=c_split, set = FALSE, repeats.allowed = FALSE));
##             # return c_splits
##             c_splits;
##         }
##     )
##     # 
##     # split it by " " and then find all ways to write search name
##     search_for_split <- unlist(x = strsplit(x = search_for, split = " "));
##     n <- r <- length(search_for_split);
##     search_for_split <- list(permutations(n=n, r=r, v=search_for_split, set = FALSE, repeats.allowed = FALSE));
##     #
##     # initialise a vector containing if search_for was matched in names
##     match_statuses <- c();
##     #
##     # for each name that's been split
##     for (i in 1:length(names)){
##         #i <- 2 # debug
##         # the match status for the current name
##         match_status <- FALSE;
##         #
##         # the current split name
##         c_split_name <- as.data.frame(split_names[[i]]);
##         #
##         # for each element in c_split_name
##         for (j in 1:nrow(c_split_name)){
##             #j <- 16 # debug
##             # the current permutation of current split name
##             c_c_split_name <- as.matrix(c_split_name[j,]);
##             #
##             # will receive hits in name's words, one by one, in sequence
##             hits <- rep(0, 20) # length 20 should always be above max number of words in names
##             #
##             # for each element in search_for_split
##             for (m in 1:nrow(search_for_split[[1]])){
##                 #m <- 1 # debug
##                 # the current permutation of name
##                 c_search_for_split <- search_for_split[[1]][m,];
##                 # for each element in search_for_split
##                 for (k in 1:ncol(search_for_split[[1]])){
##                     #k <- 2 # debug
##                     # the current element of the current permutation of name
##                     c_c_search_for_split <- c_search_for_split[[k]];
##                     #
##                     # L first hits will receive hit counts
##                     L <- min(ncol(c_c_split_name), ncol(search_for_split[[1]]));
##                     #
##                     # will match as many elements as the shortest of current pair of names has  
##                     for (l in 1:L){
##                         #l <- 1 # debug
##                         # if there's a match, the length of grep is greater than zero
##                         if (
##                             # is c_search_for_split in c_c_split_name's lth element
##                             length(
##                                 grep(
##                                     pattern = c_c_search_for_split,
##                                     x = as.character(c_c_split_name[l])
##                                 )
##                             ) > 0 ||
##                             # or, is c_c_split_name's lth element in c_search_for_split
##                             length(
##                                 grep(
##                                     pattern = as.character(c_c_split_name[l]),
##                                     x = c_c_search_for_split
##                                 )
##                             ) > 0
##                             #
##                         # if this is the case, record a hit    
##                         ){
##                             hits[l] <- 1;
##                         } else {
##                         # otherwise, don't update hit
##                         }
##                     }
##                 }
##             }
##             # take L first elements
##             hits <- hits[1:L]
##             #
##             # if hits vector has all ones for this permutation, update match status to TRUE
##             if (
##                 sum(hits)/length(hits)==1 # <- can/should be made more flexible (agrep, or sum/length<1)
##             ){
##                 match_status <- TRUE;
##             } else {
##             # otherwise, don't update match status
##             }
##         }
##         #
##         # append match_status to the match_statuses list
##         match_statuses <- c(match_statuses, match_status);
##     }
##     #
##     # output
##     if (
##         include_records_in_output==TRUE
##     ){
##         return(cbind(names, match_statuses))
##     } else {
##         return(match_statuses)
##     }
## }

## # permutes o search_for----should be more efficient but in fact is not... and misses hits
## search_names3 <- function(find_name = NA,
##                           within_records = NA,
##                           include_records_in_output = FALSE){
##     require(gtools);
##     names <- within_records;
##     search_for <- find_name;
##     #names <- tmp[-1]; search_for <- tmp[1] # debug
##     #
##     # debug
##     # generate name records
##     ## names <- c(
##     ##     "John Smith", 
##     ##     "Robert Allen Zimmerman (Bob Dylan)",
##     ##     "Bob Dylan",
##     ##     "Valentina Margarita Isabel Riquelme Molina",
##     ##     "Valentina Riquelme Molina",
##     ##     "Smith J",
##     ##     "Smith John",
##     ##     "John S",
##     ##     "John Sally",
##     ##     "A B C",
##     ##     "B C"
##     ## );
##     # suppose we're looking for this name
##     #search_for <- "Bob Dylan";
##     #
##     # split names and get all ways to write each name into a list of same length
##     split_names <- lapply(
##         X = gsub("[(|)]", "", names), # drop parentheses, if any
##         FUN = function(x){
##             #print(x);
##             # split by a space
##             c_split = unlist(x = strsplit(x = x, split = " "));
##             ## # get all permutations of c_split to compensate for order
##             ## n <- r <- length(c_split);
##             ## c_splits <- list(permutations(n=n, r=r, v=c_split, set = FALSE, repeats.allowed = FALSE));
##             # return c_splits
##             c_split;
##         }
##     )
##     # 
##     # split it by " " and then find all ways to write search name
##     search_for_split <- unlist(x = strsplit(x = search_for, split = " "));
##     # determine longest name (in words)
##     L <- max(sapply(X = gsub("[^ ]", "", names), FUN = function(x) nchar(x)+1));
##     # then add @s to search_for until length equals L
##     if (L - length(search_for_split) > 0){
##         tmp <- rep("@", L - length(search_for_split));
##         search_for_split <- c(search_for_split, tmp)
##     }
##     n <- r <- length(search_for_split);
##     search_for_split <- list(permutations(n=n, r=r, v=search_for_split, set = FALSE, repeats.allowed = FALSE));
##     #
##     # initialize a vector containing if search_for was matched in names
##     match_statuses <- c();
##     #
##     # for each name that's been split
##     for (i in 1:length(names)){
##         #i <- 2 # debug
##         # the match status for the current name
##         match_status <- FALSE;
##         #
##         # the current split name
##         c_split_name <- split_names[[i]];
##         #
## #        # for each element in c_split_name
## #        for (j in 1:nrow(c_split_name)){
## #            #j <- 16 # debug
## #            # the current permutation of current split name
## #            c_c_split_name <- as.matrix(c_split_name[j,]);
##             #
##             # will receive hits in name's words, one by one, in sequence
##             hits <- rep(0, 20) # length 20 should always be above max number of words in names
##             #
##             # for each element in search_for_split
##             for (m in 1:nrow(search_for_split[[1]])){
##                 #m <- 1 # debug
##                 # the current permutation of name
##                 c_search_for_split <- search_for_split[[1]][m,];
##                 # for each element in search_for_split
##                 for (k in 1:ncol(search_for_split[[1]])){
##                     #k <- 2 # debug
##                     # the current element of the current permutation of name
##                     c_c_search_for_split <- c_search_for_split[[k]];
##                     #
##                     # L first hits will receive hit counts
##                     L <- min(length(c_split_name), ncol(search_for_split[[1]]));
##                     #
##                     # will match as many elements as the shortest of current pair of names has  
##                     for (l in 1:L){
##                         #l <- 1 # debug
##                         # if there's a match, the length of grep is greater than zero
##                         if (
##                             # is c_search_for_split in c_c_split_name's lth element
##                             length(
##                                 grep(
##                                     pattern = c_c_search_for_split,
##                                     x = as.character(c_split_name[l]),
##                                     ignore.case = TRUE
##                                 )
##                             ) > 0 ||
##                             # or, is c_c_split_name's lth element in c_search_for_split
##                             length(
##                                 grep(
##                                     pattern = as.character(c_split_name[l]),
##                                     x = c_c_search_for_split,
##                                     ignore.case = TRUE
##                                 )
##                             ) > 0
##                             #
##                         # if this is the case, record a hit    
##                         ){
##                             hits[l] <- 1;
##                         } else {
##                         # otherwise, don't update hit
##                         }
##                     }
##                 }
##             }
##             # take L first elements
##             hits <- hits[1:L]
##             #
##             # if hits vector has all ones for this permutation, update match status to TRUE
##             if (
##                 sum(hits)/length(hits)==1 # <- can/should be made more flexible (agrep, or sum/length<1)
##             ){
##                 match_status <- TRUE;
##             } else {
##             # otherwise, don't update match status
##             }
## #        }
##         #
##         # append match_status to the match_statuses list
##         match_statuses <- c(match_statuses, match_status);
##     }
##     #
##     # output
##     if (
##         include_records_in_output==TRUE
##     ){
##         return(cbind(names, match_statuses))
##     } else {
##         return(match_statuses)
##     }
## }

## head(inc.jal)

## start <- Sys.time()
## search_names1(find_name = inc.jal$incumbent[1], within_records = inc.jal$incumbent, include_records_in_output = TRUE)
## end <- Sys.time()
## end-start

## search_names3(find_name = "Valentina Riquelme Molina", within_records = "Valentina Margarita Isabel Riquelme Molina", include_records_in_output = TRUE)

## search_names3(find_name = "Valentina Margarita Isabel Riquelme Molina", within_records = "Valentina Riquelme Molina", include_records_in_output = TRUE)
