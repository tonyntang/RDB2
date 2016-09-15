#' @title C-style (x > b) ? a : b statement
#' @examples (4 > 3) %?% ..("Nice", "Dang")
#' @name utils
#' @export
`%?%` <- function(x, y, env = parent.frame())
{
  if(x) eval(y[[1]], envir = env)
  else eval(y[[2]], envir = env)
}

#' @param ... any objects
#' @rdname utils
#' @export
`..` <- function(...)
{
  eval(substitute(alist(...)))
}

#' @export
#' @keywords internal
setGeneric("trimws",
           def = function(x, ...) {standardGeneric("trimws")})

#' @keywords internal
setOldClass("data.frame")

#' Remove whitespaces of character strings from a data.frame
#' @param x a data.frame with character vectors or a character
#' @param stringAsFactors a logical
#' @note Each column of the data.frame must be a character vectior. This function will convert it to character if it is not.
#' @name trimws
#' @export
setMethod("trimws",
          "data.frame",
          function(x, stringsAsFactors = FALSE, ...)
          {
            if(!stringsAsFactors)
            {
              ans <- as.data.frame(sapply(x,
                                          function(x)
                                          {
                                            if (is.factor(x) | is.character(x)) x <- base::trimws(as.character(x))
                                            x
                                          },
                                          simplify = FALSE),
                                   stringsAsFactors = FALSE)
            }else
            {
              ans <- as.data.frame(sapply(x,
                                          function(x)
                                          {
                                            if (is.factor(x)) levels(x) <- base::trimws(levels(x))
                                            else if (is.character(x)) x <- base::trimws(x)
                                            x
                                          },
                                          simplify = FALSE),
                                   stringsAsFactors = TRUE)
            }
            ans
          })

#' @export
#' @rdname trimws
setMethod("trimws",
          "character",
          function(x, which = c("both", "left", "right"), ...)
          {
            base::trimws(x, which = which)
          })
