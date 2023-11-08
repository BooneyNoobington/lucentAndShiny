### --- Custom Numbering -------------------------------------------------------

# Vectorize system configuraiton.
VectorizeNumberingSpec <- function(numbering.lst) {

    for (i in 1:length(numbering.lst)) {
        numbering.lst[i] <- strsplit(numbering.lst[[i]], " ")
    }

    return(numbering.lst)
}



# Get the currently "highest" identifier.
GetMaxIdentifier <- function(db.conn, numbering.lst) {

    # To predict the next collection identifier,
    # one needs to know the recent one.
    recent_ident.v <- pool::dbGetQuery(
        db.conn
      , paste(readLines("./SQL/QUERY_MAX_COLLECTION_NAME.SQL"), collapse = " ")
    )$name

    # Query is unsuccesful. Probably first ever colleciton.
    if (length(recent_ident.v) == 0) return(-1)

    # Return the selected identifier.
    # Transform it to a vector first.
    return(strsplit(recent_ident.v, "·")[[1]])
}



# A bit more tricky. Get the next identifier.
GetNextIdentifier <- function(recent_ident.chr, numbering.lst) {

    # What happens if there isn't an identifier yet?
    # Pick the first possible one from numbering.lst.
    if(!is.character(recent_ident.chr)) {
        return(as.vector(sapply(numbering.lst,"[[",1)))
    }

    # How many sections does the safes.list have?
    n <- length(numbering.lst)

    # Sanity check if recent_ident.chr is usable.
    if(! length(recent_ident.chr) == n) {
        stop("Recent identifier has different length than numbering list.")
    }

    # Where is the current safe "located"? At which list indices?
    recent_ident.pos <- numeric(0)

    for (i in 1:n) recent_ident.pos <- c(recent_ident.pos,
           match(recent_ident.chr[i], numbering.lst[[i]]))

    # Create a matrix that contains all possible next safe positions.
    possible.next.mat <- t(  # Transpose the matrix.
      matrix(rep(recent_ident.pos, n), nrow = n) + diag(n)
    )[c(n:1),]  # And flip it upside down.

    # Set the lower triangle of this matrix to 1
    # in order to reset the counter.
    possible.next.mat[lower.tri(possible.next.mat)[,n:1]] <- 1

    # Loop over all rows in "possible.next.mat".
    for (i in 1:n) {

        # Build a vector for the possible next …
        possible.next.v <- character(0)
        # … by looping over all sections in numbering.lst and the corresponding
        # index "j".
        for (j in 1:n) possible.next.v <- c(possible.next.v,
                 numbering.lst[[j]][possible.next.mat[i,j]])
        
        # Return the first possibility that doesn't contain an "NA".
        if(!any(is.na(possible.next.v))) return(possible.next.v)
    }

    # Or return -1 if nothing is found.
    return(-1)
}



### --- Input Handling ---------------------------------------------------------

# Interpret text in parenthesis as separate information.
HandleParanthesisInput <- function(
    s.chr
  , field1 = "name"
  , field2 = "description"
  , para.bol= as.logical(getOption("paraIsDesc", default = FALSE))
)
{
  
    s.lst <- list()
    
    # If paranthesis shouldn't be interpreted as description, just
    # return the string and an empty description.
    if (!para.bol) {
        s.lst[[field1]] <- s.chr
        s.lst[[field2]] <- NA
    } else {  # Otherwise split.
      
        warning("Text in paranthesis detected. Interpreting as description.")
      
        s.v <- unlist(strsplit(s.chr, " (", fixed = TRUE))
        s.v <- gsub("\\)", "", s.v)
        s.v <- trimws(s.v)
        
        s.lst[[field1]] <- s.v[1]
        s.lst[[field2]] <- s.v[2]
    }
    
    
    return(s.lst)
}
