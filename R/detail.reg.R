#' Retrieve detail results of regression models
#'


#' @param x A reg or reg_y object
#' @export
#' @seealso \code{\link{reg}}
#' @examples
#' data(diabetes)
#' head(diabetes)
#'
#' reg_glm<-reg(data = diabetes, y = 5, factor = c(1, 3, 4), model = 'glm')
#' detail(reg_glm)


detail.reg <- function(x) {
    if (class(x) != "reg") {
        stop("x should be a `reg` object.", call. = FALSE)
    }
    result <- x$detail
    if (length(result)==1)  stop(paste0("`detail_show` should be used when create ",substitute(x),"."), call. = FALSE)
    return(result)
}


#'@describeIn detail.reg detail method for 'reg_y' class
detail.reg <- function(x) {
if (class(x) != "reg_y") {
  stop("x should be a `reg_y` object.", call. = FALSE)
}
result <- x$detail
if (length(result)==1)  stop(paste0("`detail_show` should be used when create ",substitute(x),"."), call. = FALSE)
return(result)
}
