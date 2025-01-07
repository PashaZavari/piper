#' @title check_empty
#' @description checks an object to determine if it is "empty".
#' @param data an object that you want to check for "emptiness"
#' @return logical TRUE/FALSE
#' @export check_empty
check_empty <- function(data) {
    if (is.null(data) || identical(data, list())) return(TRUE)

    cls <- attr(data, "class")
    if (is.null(cls)) cls <- typeof(data)

    switch(cls[1],
        tbl_df = ,
        tbl = nrow(data) == 0 || all(vapply(data, function(x) all(is.na(x)), logical(1))),
        data.frame = all(sapply(data, function(x) all(is.na(x)))),
        matrix = !prod(dim(data)) || all(is.na(data)),
        list = !length(unlist(data, recursive = TRUE, use.names = FALSE)),
        !length(data) || all(is.na(data))
    )
}