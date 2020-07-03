#' @title Covid19 version
#'
#' @rdname get_Covid19_version
#'
#' @description Returns the version of the installed Covid19 package and app.
#'
#' @return The version of Covid19 as character string.
#'
#' @export
get_Covid19_version <- function() {
  as.character(utils::packageVersion("Covid19Mirai"))
}
