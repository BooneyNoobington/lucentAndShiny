### --- Custom Numbering -------------------------------------------------------

# Get the next identifier.
getNextIdentifier <- function(
        tab = "collection"
      , name_col = "name"
      , id_col = glue::glue("id_{tab}")
      , sep = "-"
)
{

    # To predict the next collection identifier,
    # one needs to know the recent one.
    DBI::dbGetQuery(
          db.conn
        , glue::glue_sql(
              .con = db.conn
            , "
              SELECT        {`name_col`}
              FROM          {`tab`}
              WHERE         {`id_col`} = (
                    SELECT MAX({`id_col`}) FROM {`tab`}
              )
              "
          )
    ) %>%
        dplyr::pull(name_col) -> name.max.chr

    # What happens if there isn't an identifier yet?
    # Pick the first possible one from sys.cnf$enumeration.
    if(identical(name.max.chr, character(0))) {
        return(as.vector(sapply(sys.cnf$enumeration,"[[",1)))
    }
    
    # Turn the character into a vector
    name.max.chr.v <- strsplit(x = name.max.chr, split = sep)[[1]]

    # Keys in the numbering scheme
    n <- length(sys.cnf$enumeration)
    
    print(n)
    print(name.max.chr.v)
    
    # Sanity check if name.max.chr is usable.
    if(length(name.max.chr.v) != n) {
        stop("Recent identifier has different length than numbering list.")
    }

    # Where is the current safe "located"? At which list indices?
    recent_ident.pos <- numeric(0)

    for (i in 1:n) recent_ident.pos <- c(
        recent_ident.pos
      , match(name.max.chr.v[i], sys.cnf$enumeration[[i]])
    )

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
        # … by looping over all sections in sys.cnf$enumeration and the corresponding
        # index "j".
        for (j in 1:n) possible.next.v <- c(possible.next.v,
                 sys.cnf$enumeration[[j]][possible.next.mat[i,j]])
        
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
