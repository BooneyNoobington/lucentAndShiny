### ----------------------------------------------------------------------------
### --- Create Safe Identifiers ------------------------------------------------
### --- They are readable, and avoid number swaps ------------------------------
### ----------------------------------------------------------------------------



# Check wether a number is a "swapper".
# E.g. 110 can be swapped to 101.
CheckSwapper <- function(x) {
  
    library(combinat)
  
    # Create a list of all possible combinations.
    x.perms <- unique(combinat::permn(unlist(strsplit(as.character(x), ""))))
    
    # Turn them back into numbers.
    x.nums <- lapply(x.perms, function(v) as.numeric(paste(v, collapse = "")))
    
    # Digits cannot be rearranged to create a smaller number than x.
    if (x <= min(unlist(x.nums))) {
        return(FALSE)
    # Digits can indeed be rearranged into a smaller number.
    } else {
        return(TRUE)
    }
}



# Check if a digit appears more than once in a number.
CheckReps <- function(x) {
    x.chr.v <- strsplit(as.character(x), "")[[1]]

    # Check if there are duplicated characters.
    any(duplicated(x.chr.v))    
}



# Get safe numbers.
GetSafes <- function(n, start.int = 1, debug = FALSE) {
  
  i <- start.int  # Start with 1 by default.
  safe.nums <- numeric(0)  # Initialize numeric vector.
  
  # Repeat until break condition.
  while (TRUE) {
      if(debug) print(paste("Checking:", i, "Swapper:", CheckSwapper(i), "Repeatung", CheckReps(i)))
    
      # If "i" passed both tests, add it to the safe numbers.
      if (!CheckSwapper(i) & !CheckReps(i)) safe.nums <- c(safe.nums, i)
      
      # As soon as "safe.nums" contains enough elements, break.
      if (length(safe.nums) == n) break
    
      if (i == 99999) break
      
      # Increase counter.
      i <- i + 1
  }
  
  
  return(safe.nums)
}


safes.v <- GetSafes(400)


# Create a list later to permute.
sections.lst <- list(
    letters = LETTERS
  , numbers1 = safes.v
  , colours = c("Red", "Blue", "Green", "Gold", "Gray", "Black", "White")
  , numbers2 = safes.v
  , phonetic = c("Alpha", "Bravo", "Charlie", "Delta", "Echo", "Foxtrot", 
                 "Golf", "Hotel", "India", "Juliett", "Kilo", "Lima", "Mike", 
                 "November", "Oscar", "Papa", "Quebec", "Romeo", "Sierra", 
                 "Tango", "Uniform", "Victor", "Whiskey", "X-ray", "Yankee",
                 "Zulu")
  , numbers3 = safes.v
)



# Create a function "counts" up one given a sections.lst as above.
GetNextSafe <- function(cur.safe, safes.lst) {
    
    # How many sections does the safes.list have?
    n <- length(safes.lst)
  
    # Sanity check if cur.safe is usable.
    if(! length(cur.safe) == n) return -1
    
    # Where is the current safe "located"? At which list indices?
    cur.safe.pos <- numeric(0)
      
    for (i in 1:n) cur.safe.pos <- c(cur.safe.pos,
            match(cur.safe[i], sections.lst[[i]]))
  
      
    # Create a matrix that contains all possible next safe positions.
    possible.next.mat <- t(  # Transpose the matrix.
      matrix(rep(cur.safe.pos, n), nrow = n) + diag(n)
    )[c(n:1),]  # And flip it upside down.
    
    # Set the lower triangle of this matrix to 1
    # in order to reset the counter.
    possible.next.mat[lower.tri(possible.next.mat)[,n:1]] <- 1
    
    
    # Loop over all rows in "possible.next.mat".
    for (i in 1:n) {
      
        # Build a vector for the possible next …
        possible.next.v <- character(0)
        # … by looping over all sections in safes.lst and the corresponding
        # index "j".
        for (j in 1:n) possible.next.v <- c(possible.next.v,
                     safes.lst[[j]][possible.next.mat[i,j]])
        
        # Return the first possibility that doesn't contain an "NA".
        if(! any(is.na(possible.next.v))) return(possible.next.v)
    }
    
    # Or return -1 if nothing is found.
    return(-1)
}

last_safe.v <- c("A", 1, "Blue", 1, "Alpha", 1)
GetNextSafe(last_safe.v, sections.lst)


# Create a combinations data.frame.
combinations.df <- expand.grid(
    phonetic = sections.lst$phonetic     # Start with letters.
  , numbers1 = sections.lst$numbers      # First row of numbers.
  , numbers2 = sections.lst$numbers      # Then colours.
  , colours  = sections.lst$colours      # Second row of numbers
)



# Write the names to the database.
mariadb.con <- pool::dbPool(
    RMariaDB::MariaDB()
  , dbname = "lucent"
  , unix.sock = "/home/grindel/Entwicklung/lucentLIMS/mariadb/tmp/mariadb.sock"
  , username = "grindel"
)


# Write them to the database.
for (i in 1:nrow(combinations.df)) {
    pool::dbExecute(
        mariadb.con
      , sprintf(
            "INSERT INTO safe_number (unique_string) VALUES ('%s')"
          , paste(
                combinations.df$phonetic[i]
              , combinations.df$numbers1[i]
              , combinations.df$colours[i]
              , combinations.df$numbers2[i]
              , sep = "·"
            )
        )
    )
}

safes <- GetSafes(400)

test.df <- data.frame(n = 1:999999)

test.df$is.safe <- rep(0, 999999)

test.df$is.safe[test.df[safes,1]] <- 1

test.df$sum <- rep(0, 999999)

for (i in 1:999999) {
    test.df$sum[i] <- sum(test.df$is.safe[1:i])
}
