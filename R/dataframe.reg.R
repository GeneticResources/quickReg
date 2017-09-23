#' Retrieve a data.frame from regression models
#'
#' Return a concentrated result of regression models.

#' @param x A reg or reg_y object
#' @param save A logical, whether to save the concentrated result
#' @param file A character string naming a file, see \code{\link{write.table}} for more details, the default filepath is current working directory and the filename is the current time.
#' @param sep,row.names,\dots See \code{\link{write.table}} for more details
#' @importFrom utils write.table
#' @export
#' @seealso \code{\link{reg}}
#' @examples
#' reg_glm<-reg(data = diabetes, x=c(1:4),y = 5,
#' factor = c(1, 3, 4), model = 'glm')
#'
#' dataframe(reg_glm)
#' # dataframe(reg_glm, save = TRUE)

dataframe.reg <- function(x, save = FALSE, file = NULL, sep = ",", row.names = FALSE,
    ...) {
    if (class(x) != "reg") {
        stop("x should be a `reg` object.", call. = FALSE)
    }
    if (is.null(file)) {
        time <- format(Sys.time(), "%Y_%m_%d_%H_%M")
        file_new <- paste0(substitute(x),"_", time, ".csv")
    }
    result <- x$dataframe

    if (save) {
      if (!is.null(file))  {
        write.table(result, file = file, sep = sep, row.names = row.names,
                    ...)
        } else write.table(result, file = file_new, sep = sep, row.names = row.names,
                           ...)

    }
    return(result)
}


#'@describeIn dataframe.reg dataframe method for 'reg_y' class
dataframe.reg_y <- function(x, save = FALSE, file = NULL, sep = ",", row.names = FALSE,
                          ...) {
  if (class(x) != "reg_y") {
    stop("x should be a `reg_y` object.", call. = FALSE)
  }
  if (is.null(file)) {
    time <- format(Sys.time(), "%Y_%m_%d_%H_%M")
    file_new <- paste0(substitute(x),"_", time, ".csv")
  }
  result <- x$dataframe

  if (save) {
    if (!is.null(file))  {
      write.table(result, file = file, sep = sep, row.names = row.names,
                  ...)
    } else write.table(result, file = file_new, sep = sep, row.names = row.names,
                       ...)

  }
  return(result)
}
