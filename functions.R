`.unflatten`<- function(x, sep=sep) {
    if (!length(x)) {
        return(list())
    }
    else if (x[[1]]==sep) {
        return(list(.unflatten(x[[2:length(x)]])))
    }
}

`partition`<- function(s, sep='|') {
    p<- strsplit(s, split=sep, fixed=TRUE)[[1]]

    if (!length(p[[1]]))
        return list("", "", "")

    # could be e.g. "AB" or "AB|" 
    if (length(p) == 1) {
        if (substring(p, nchar(p))==sep)
            return(list(p, sep, ""))
        return(list(p, "", ""))
    }

    list(p, sep, p[[2:length(p)]])
}
