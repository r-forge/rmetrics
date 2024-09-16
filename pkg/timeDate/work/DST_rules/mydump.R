
mydump_single_file <- function(charvec, sysfile = "sysdata.rda",
                               funfile = "timeDate-DaylightSavingTime.R", compress = "xz") {
    cat("", file = funfile) # TODO: put a preamble
    for(s in charvec){
        f <- match.fun(s)
        df <- f()
        nam <- paste0("df", s)
        assign(nam, df)
        ## print(tail(df))

        ## create corresponding function
        text <- paste0('\n"', s, '"', " <- ", "function() {\n",
                       "    ", nam, "\n", "}")
        cat(text, "\n", file = funfile, append = TRUE)
    }
    charvec <- c(charvec, "nyse_early_closings", "nyse_special_closings")
    save(list = charvec, file = sysfile, compress = compress)
    NULL
}
