### Custom Numbering ###############################################################################


## Get the current maximum identifier ##############################################################
getMaxCustomEnum <- function(
        db_conn,
        tab = "collection",
        name_col = "name",
        id_col = glue::glue("id_{tab}")
)
{
    DBI::dbGetQuery(
          db_conn
        , glue::glue_sql(
              .con = db_conn,
              "
              SELECT        {`name_col`}
              FROM          {`tab`}
              WHERE         {`id_col`} = (
                    SELECT MAX({`id_col`}) FROM {`tab`}
              )
              "
        )
    ) %>%
        dplyr::pull(name_col)  # Will return character(0) if nothing is found
}


## Get the next identifier #########################################################################
getNextCustomEnum <- function(db_conn, enum_list, current_max, sep = "-") {

    # What happens if there isn't an identifier yet?
    # Pick the first possible one from enum_list.
    if(identical(current_max, character(0))) {
        message("Maximum enumerator was character(0). This is okay, if there is no record in the table to be enumerated yet.")
        
        candidate <- sapply(enum_list, "[[",1)

        return(paste(candidate, collapse = sep))
    }
    
    # Turn the character into a vector
    current_max.v <- strsplit(x = current_max, split = sep)[[1]]

    # Keys in the numbering scheme
    n <- length(enum_list)
    
    # Sanity check if current_max is usable.
    if(length(current_max.v) != n) {
        stop("Recent identifier has different length than numbering list.")
    }

    # Where is the current safe "located"? At which list indices?
    recent_ident.pos <- numeric(0)

    for (i in 1:n) recent_ident.pos <- c(
        recent_ident.pos
      , match(current_max.v[i], enum_list[[i]])
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
        candidate <- character(0)
        # … by looping over all sections in enum_list and the corresponding
        # index "j".
        for (j in 1:n) candidate <- c(candidate,
                 enum_list[[j]][possible.next.mat[i,j]])
        
        # Return the first possibility that doesn't contain an "NA".
        if(!any(is.na(candidate))) return(paste(candidate, collapse = sep))
    }

    # Or return -1 if nothing is found.
    return(-1)
}


## Turn a number into an encoded representation ####################################################
encodeNumber <- function(x, enum_rules) {
    # Sanity check for numbers which are too big to be encoded
    # (Bigger than 999 * biggest enumeration rule)
    max_power <- max(unname(unlist(enum_rules)))
    max_encodable <- max_power * 10 - 1
    
    if (x > max_encodable) {
        warning(
            glue::glue("{x} is bigger than the biggest encodable number ({max_encodable}) with the current encoding rules.")
        )
        return(glue::glue("¯\\_(ツ)_/¯"))
    }
    
    # Break x down to its components and pair those with the keys of the enumeration list
    data.frame(
        power_encoding = rev(names(enum_rules)),  # Reverse order of keys
        digits = strsplit(
            x = formatC(
                x, digits = nchar(format(max_encodable, scientific = FALSE)) - 1, flag = "0"
            ),
            split = ""
        )[[1]]
    ) %>%
        dplyr::filter(digits != 0) %>%
        apply(MARGIN = 1, FUN = function(row) paste(row, collapse = " ")) %>%
        paste(collapse = " ")
}


## Decode such an encoded number into a regular number again #######################################
decodeNumber <- function(input_string, enum_rules) {
    # Split the input string into individual elements
    elements <- str_split(input_string, " ")[[1]]
    
    # Replace the non-integer parts with the powers of ten from the list
    expression_parts <- purrr::map2_chr(
        elements[seq(1, length(elements), 2)], elements[seq(2, length(elements), 2)],
        ~ paste0("10^", log10(enum_rules[[.x]]), " * ", .y)
    )
    
    # Join the parts together to form the final expression
    final_expression <- paste(expression_parts, collapse = " + ")
    
    return(eval(parse(text = final_expression)))
}


## Encode number to a mixed radix numbering scheme #################################################
# Get the bases for each segment (number of possible values)
bases <- sapply(segments, length)

# Compute multipliers for each segment
multipliers <- sapply(seq_along(bases), function(i) {
    if (i < length(bases)) {
        prod(bases[(i + 1):length(bases)])
    } else {
        1
    }
})
names(multipliers) <- names(segments)

# Create a function to encode the sample ID
encode_sample_id <- function(sample_id, segments, bases, multipliers) {
    # Adjust for zero-based indexing
    remaining <- sample_id - 1
    
    # Initialize a list to store segment indices
    segment_indices <- list()
    
    # Compute the index for each segment
    for (i in seq_along(segments)) {
        base <- bases[[i]]
        multiplier <- multipliers[[i]]
        
        index <- remaining %/% multiplier
        remaining <- remaining %% multiplier
        
        # Adjust index to 1-based indexing
        segment_indices[[i]] <- index + 1
    }
    
    # Map indices to actual segment values
    encoded_segments <- mapply(function(segment, index) {
        segment[[index]]
    }, segments, segment_indices, SIMPLIFY = FALSE)
    
    return(encoded_segments)
}


## Encode with base 10 substitution ################################################################

# Substitute every odd power of ten by a character (word or letter)
# Encoding function
encodeBase10Sub <- function(x, segments) {
    
    # Ensure all segments have exactly 10 elements
    segment_lengths <- sapply(segments, length)
    if (!all(segment_lengths == 10)) {
        stop("All segments must have exactly 10 elements.")
    }
    
    base <- 10
    
    k <- length(segments)
    
    segment_indices <- integer(k)
    
    for (i in k:1) {
        segment_indices[i] <- x %% base
        x <- x %/% base
    }
    
    segment_names <- names(segments)
    
    encoded_segments <- mapply(
        function(segment_name, index) {
            segment <- segments[[segment_name]]
            segment[[index + 1]]
        },
        segment_names,
        segment_indices,
        SIMPLIFY = FALSE
    )
    

    return(paste(encoded_segments, collapse = "-"))
}