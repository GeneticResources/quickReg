#' Generic function for class 'reg' or 'reg_y'
#'
#' Generic function to extracte every regression model result.
#' @param x A reg or reg_y object
#' @export
#' @seealso \code{\link{detail.reg}}, \code{\link{reg}}
#' @examples
#' data(diabetes)
#' head(diabetes)
#' reg_coxph<-reg(data = diabetes, x = c(1,4, 6), y = 5, time = 2, factor = c(1, 4), model = 'coxph')
#' detail(reg_coxph)

detail <- function(x) UseMethod("detail")



