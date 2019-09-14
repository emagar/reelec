######################################################################################################
## A function to match strings with names and last names in any order                               ##
##                                                                                                  ##
## adapted 30-5-2019 from                                                                           ##
## https://stackoverflow.com/questions/22894265/how-to-perform-approximate-fuzzy-name-matching-in-r ##
## by Eric Magar                                                                                    ##
######################################################################################################

search.names <- function(within.records = NA,               # VECTOR OF NAMES WHERE TARGET WILL BE SEARCHED
                         ids = NA,                          # VECTOR OF IDs FOR RECORDS
                         method = NA){                      # whether search will be exact, grep, or fuzzy
    require(gtools);
    require(plyr);
    '%!in%' <- function(x,y)!('%in%'(x,y)) # not in function
    if (method %!in% c("exact","grep","fuzzy")){
        print("Set method to exact, grep, or fuzzy");
        break;
    }
    names <- within.records;
    ## # debug
    ## # generate name records
    ## names <- c(
    ##     "John Smith", 
    ##     "Robert Allen Zimmerman (Bob Dylan)",
    ##     "Bob Dylan",
    ##     "Valentina Margarita Isabel Riquelme Molina",
    ##     "Valentina Riquelme Molina",
    ##     "J Smith",
    ##     "Smith John",
    ##     "John S",
    ##     "John Sally",
    ##     "Riquelme Molina Vale",
    ##     "V R"
    ## );
    ## ids <- 1:11
    #
    # split names and get all ways to write each name into a list of same length
    names.perm <- lapply(
        X = gsub("[(),]", "", names), # drop parentheses, commas if any
        FUN = function(x){
            # split by a space
            c.split = unlist(x = strsplit(x = x, split = " "));
            # get all permutations of c.split to compensate for order
            n <- r <- length(c.split);
            c.splits <- list(permutations(n=n, r=r, v=c.split, set = FALSE, repeats.allowed = FALSE));
            # return c.splits
            c.splits;
        }
    )
    # above returns a list of lists (each with a single matrix)
    # we want a list of matrices
    names.perm <- sapply(names.perm,c)
    # 
    # change matrices with dataframes within list
    names.perm <- sapply(names.perm, as.data.frame) # list of data frames
    n.perm <- sapply(names.perm, nrow)     # how many permutations per name
    ids.perms <- rep(ids, n.perm) # identify permutations from same name
    names.perm <- rbind.fill(names.perm) # makes single dataframe with NAs when empty
    #
    # un-permuted version to search over
    split.names <- lapply(
        X = gsub("[(),]", "", names), # drop parentheses, commas if any
        FUN = function(x){
            #print(x);
            # split by a space
            c.split = unlist(x = strsplit(x = x, split = " "));
            # return c.split
            c.split;
        }
    )
    split.names <- sapply(split.names, as.matrix)
    split.names <- sapply(split.names, t)
    split.names <- sapply(split.names, as.data.frame)
    split.names <- do.call(rbind.fill, split.names)   # in dataframe
    #
    # how many word in each names and split.name
       tmp <- function(x){
           tmp1 <- length(x[is.na(x)==FALSE])
           return(tmp1)
       }
    #
    n.words <- apply(split.names, 1, tmp) # words in each split name, denominator for share hits
    N <- length(names) # for use in loop
    #P <- nrow(names.perm) # for use in loop
    # init square matrix containing how many times c.split.name was matched in each split.names
    n.hits  <- matrix(NA, nrow = N, ncol = N)  # will receive how many times each split name is matched
    #
    for (n in 1:N){ # loop over names
        #n <- 727 # debug
        message(sprintf("loop %s of %s (id = %s)", n, N, ids[n]))
        # skip missing names 
        if (is.na(names[n])==TRUE) next
        # the current name's permutations
        sel <- which(ids.perms==ids[n])
        c.name.perm <- names.perm[sel,]
        P <- length(sel); # how many current permutations
        W <- n.words[n]; # current number of words (columns) to explore
        p.hits  <- matrix(NA, nrow = N, ncol = P) # will receive how many times each split name is matched by c.permutation
        # the current split name
        for (p in 1:P){
            #p <- 1 # debug
            c.split.name <- c.name.perm[p,];
            # init rectangular matrix for c.split.name matches=1 in split.names
            hits <- matrix(0, nrow = N, ncol = ncol(split.names))
            #
            # for each element in c.split.name
            for (w in 1:W){
                #w <- 1 # debug
                # the current permutation of current split name
                c.word <- as.character(as.matrix(c.split.name[w])); # drops factor
                if (method=="exact"){
                    # record exact hits
                    hits[which(split.names[,w]==c.word),w] <- 1
                }
                if (method=="grep"){
                    ## # alternative: use regex (search matched initial)
                    if (nchar(c.word)==1) c.word <- paste("^", c.word, sep = "") # if initial only, search 1st character only
                    hits[grep(pattern=c.word, x=split.names[,w]),w] <- 1
                }
                if (method=="fuzzy"){
                    ## # alternative: use fuzzy search
                    if (nchar(c.word)==1) c.word <- paste("^", c.word, sep = "") # if initial only, search 1st character only
                    hits[agrep(pattern=c.word, x=split.names[,w])] # alternative: use fuzzy search
                }
            }
            #
            # count total hits or permutation in each row of split.names and plug into n.hits
            p.hits[,p] <- rowSums(hits) # fill sth col (hits of name's permutation in whole column)
        }
        #
        # count total hits or permutation in each row of split.names and plug into n.hits
        n.hits[,n] <- apply(p.hits, 1, max) # fill nth col (hits of name s in whole column)
    }
    #   
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
                #n.perm=n.perm,
                n.words=n.words,
                names=names,
                ids=ids)
           )
}


## #try it
## tmp <- search.names(#find_name = "J Smith",
##              within.records = c(
##                  "John Smith", 
##                  "Robert Allen Zimmerman (Bob Dylan)",
##                  "Smith J",
##                  "Smith John",
##                  "John S",
##                  "John Sally"),
##              ids = 1:6,
##              method = "grep"
##              )
## summary(tmp)
## tmp$n.hits
## tmp$sh.hits
## x




## head(inc.jal)

## start <- Sys.time()
## search.names1(find_name = inc.jal$incumbent[1], within.records = inc.jal$incumbent, include_records_in_output = TRUE)
## end <- Sys.time()
## end-start

## search.names3(find_name = "Valentina Riquelme Molina", within.records = "Valentina Margarita Isabel Riquelme Molina", include_records_in_output = TRUE)

## search.names3(find_name = "Valentina Margarita Isabel Riquelme Molina", within.records = "Valentina Riquelme Molina", include_records_in_output = TRUE)
