### --- Various Helper Function --------------------------------------------------------------------



## -- Logging --------------------------------------------------------------------------------------

# Write a log into a file.
AddLogLine <- function(log.path, event.chr, desc.chr, t.format = "%Y.%m.%d %T") {
    # Get the current time.
    c.time.chr <- format(Sys.time(), format = t.format)
    # Get the current user.
    u.chr <- Sys.getenv("LOGNAME")
    # Compose the log line.
    log_line.v <- data.frame(
        date = c.time.chr
      , user = unix_user.chr
      , event = event.chr
      , description = desc.chr
    )

    write.table(
        log_line.v
      , log.path
      , sep = " · "
      , append = TRUE
      , quote = FALSE
      , row.names = FALSE
      , headers = FALSE
    )
}
