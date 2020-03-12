# Note that the documentation link is not going to work because the pipe
# operator ultimately resides in `magrittr` and is only re-exported in `dplyr`.
#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' Variants of standard operators
#' @rdname negations
#' @description Negation of [`%in%`] from R's `base` library.
#' @usage 'See the help for match in base R.'
#' @param ... Arguments to [`%in%`].
#' @export
#' @md
'%notin%' <- Negate('%in%')
