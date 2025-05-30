#' @title attach_globals
#' @description Export arguments to global namespace
#' @param ... a list of functional arguments
#' @param .data an optional object to map from
#' @param .f an optional transform function to apply to mapping
#' @param .env a target export environment, DEFAULT parent.frame()
#' @export attach_globals
attach_globals <- function(..., .data, .f, .env = parent.frame()) {
    if (!missing(.data) && !CHECK_EMPTY(.data)) {
        .args <- lapply(as.list(substitute(alist(...)))[-1], function(.x) pluck(.data, deparse(.x)))
        if (!missing(.f)) {
            .args <- lapply(.args, function(.x) .f(.x))
        }
    } else {
        .args <- list(...)
    }

    list2env(.args, envir = .env)
}