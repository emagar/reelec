######################################################################################################
## A function to match strings with names and last names in any order                               ##
##                                                                                                  ##
## adapted 30-5-2019 from                                                                           ##
## https://stackoverflow.com/questions/22894265/how-to-perform-approximate-fuzzy-name-matching-in-r ##
## by Eric Magar                                                                                    ##
######################################################################################################

search_names <- function(#find_name = NA,                    # TARGET, NAME TO LOOK FOR
                         within_records = NA,               # VECTOR OF NAMES WHERE TARGET WILL BE SEARCHED
                         ids = NA,                          # VECTOR OF IDs FOR RECORDS
                         method = NA){                      # whether search will be exact, grep, or fuzzy
    require(gtools);
    require(plyr);
    '%!in%' <- function(x,y)!('%in%'(x,y)) # not in function
    if (method %!in% c("exact","grep","fuzzy")){
        print("Set method to exact, grep, or fuzzy");
        break;
    }
    names <- within_records;
    # DROP #search_for <- find_name;
    #names <- tmp[-1]; # DROP #search_for <- tmp[1] # debug
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
    ## ids <- 1:11
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
    # above returns a list of lists (each with a single matrix)
    # we want a list of matrices
    split_names <- sapply(split_names,c)
    # 
    # change vectors with dataframes within list
    split_names <- sapply(split_names, as.data.frame) # list of data frames
    nperm <- sapply(split_names, nrow)     # how many permutations per name
    split_names <- rbind.fill(split_names) # makes single dataframe with NAs when empty
    # how many word in each names and split_name
       tmp <- function(x){
           tmp1 <- length(x[is.na(x)==FALSE])
           return(tmp1)
       }
    s.words <- apply(split_names, 1, tmp) # words in each split name permutation, denominator for share hits
    N <- length(names) # for use in loop
    S <- nrow(split_names) # for use in loop
    # init square matrix containing how many times c_split_name was matched in each split_names
    n.hits  <- matrix(NA, nrow = S, ncol = S)  # total
    #
    # DROP #split_names2 <- split_names; s.words2 <- s.words # duplicate (originals will be pruned at each loop)
    for (s in 1:S){
        #s <- 1 # debug
        message(sprintf("loop %s of %s", s, S))
        # the current split name
        c_split_name <- split_names[s,];
        c_s.words <- s.words[s];
        # init rectangular matrix for c_split_name matches=1 in split_names
        hits <- matrix(0, nrow = nrow(split_names), ncol = ncol(split_names))
        #
        # for each element in c_split_name
        for (j in 1:c_s.words){
            #j <- 1 # debug
            # the current permutation of current split name
            c_word <- as.character(as.matrix(c_split_name[j])); # drops factor
            if (method=="exact"){
                # record exact hits
                hits[which(split_names[,j]==c_word)] <- 1
            }
            if (method=="grep"){
                ## # alternative: use regex (search matched initial)
                if (nchar(c_word)==1) c_word <- paste("^", c_word, sep = "") # if initial only, search 1st character only
                hits[grep(pattern=c_word, x=split_names[,j])] <- 1
            }
            if (method=="fuzzy"){
                print("Sorry, method fuzzy under construction ):")
            ## # alternative: use fuzzy search
            ## hits[agrep(pattern=c_word, x=split_names[,j])] # alternative: use fuzzy search
            }
        }
        #
        # count total hits in each row of split_names and plug into n.hits
        n.hits[,s] <- rowSums(hits) # fill sth col (hits of name s in whole column)
        #
    }
    #
    n.words <- unlist(lapply(X = strsplit(names, split=" "), length))
    #
    sel.ids <- rep(ids, nperm)
    tmp1 <- n.hits * (1-diag(S)) # n.hits with diagonal set to zero (to exclude autohits from max)
    tmp2 <- diag(N) * n.words  # will receive hits when all permutations of a name are consolidated
    # subset n.hits to find maximum
    for (r in 1:N){ # loop over names' rows
        #r <- 1 # debug
        sel.r <- which(sel.ids==ids[r])
        ss.r <- tmp1[sel.r,] # subset a row
        for (c in setdiff(1:N, r)){ # loop over names' columns (except diagonal items)
            #c <- 1 # debug
            sel.c <- which(sel.ids==ids[c])
            ss.rc <- ss.r[,sel.c] # subset
            maxi <- max(ss.rc)
            tmp2[r,c] <- maxi
        }
    }
    n.hits <- tmp2 # rename consolidated hits
    sh.hits <- round(n.hits / n.words, 1)
    #
    ## # debug
    ## s <- s+1
    ## n.hits[s,] 
    ## x
    #
    # output
    return(list(n.hits=n.hits,
                sh.hits=sh.hits,
                #nperm=nperm,
                #s.words=s.words,
                n.words=n.words,
                names=names,
                ids=ids)
           )
}


## try it
## tmp <- search_names(#find_name = "J Smith",
##              within_records = c(
##                  "John Smith", 
##                  "Robert Allen Zimmerman (Bob Dylan)",
##                  "Valentina Margarita Isabel Riquelme Molina",
##                  "Smith J",
##                  "Smith John",
##                  "John S",
##                  "John Sally"),
##              ids = 1:7,
##              method = "exact"
##              )
## summary(tmp)
## tmp$sh.hits
## x

## Falta
## 1. fuzzy matching agrep
## 2. hit 2 de 3, 3 de 4, etc


## #########################################################################################################
## ## This version (a) seems to show that permutations over search_for (carried) are indeed redundant and ##
## ##              (b) by adding one more loop, becomes much much slower seaching for long names          ##
## #########################################################################################################
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
