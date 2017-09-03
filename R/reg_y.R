#' Build regression models with more than one dependent varibale
#'
#' Build general linear model, generalized linear model, cox regression model,etc.

#' @param data A data.frame
#' @param x Integer column indices or names of the variables to be included in univariate analysis, the default columns are all the variables except `y` and `time` and `cov`.
#' @param y Integer column indices or name of dependent variable
#' @param cov Integer column indices or name of covariate variables
#' @param factor Integer column indices or names of variables to be treated as factor
#' @param model regression model, see \code{\link{lm}}, \code{\link{glm}}, \code{\link[survival]{coxph}} for more details
#' @param time Integer column indices  or name of survival time, used in cox regression, see \code{\link[survival]{coxph}} for more details
# @param \dots Further arguments passed to regression model
#' @param  cov_show A logical, whether to create covariates result, default FALSE
#' @param  detail_show A logical, whether to create each regression result, default FALSE
#' @return The return result is a list including two componets, the first part is a detailed anaysis result, the second part is a concentrated result in a  data.frame
#' @importFrom stats binomial confint glm lm
#' @export
#' @examples
#' reg_glm<-reg(data = diabetes, x = c(1:4, 6), y = 5, factor = c(1, 3, 4), model = 'glm')
#' ##  subset result like a list
#' reg_glm$detail
#' reg_glm$dataframe
#' reg_glm[2]
#' reg_glm$detail[2:4]
#' ##  other methods
#' fit<-reg(data = diabetes, x = c(1, 3:6), y = "age", factor = c(1, 3, 4), model = 'lm')
#' fit<-reg(data = diabetes, x = c( "sex","education","BMI"), y = "diabetes",
#' time ="age", factor = c("sex","smoking","education"), model = 'coxph')


reg_y <- function(data = NULL, x = NULL, y = NULL,cov=NULL, factor = NULL, model = NULL,
                time = NULL, cov_show=FALSE,detail_show=FALSE) {

  if(!is.data.frame(data)) {
    tryCatch( {
      data<-as.data.frame(data,stringsAsFactors = FALSE)
    }, error = function(e) stop("`data` is not a data.frame.", call. = FALSE)
    )
  }
  if (length(y) == 1)
    stop("Only One dependent varibale provided, use reg instead!", call. = FALSE)

  if (!is.character(x)) x<-names(data)[x]
  if (!is.character(y)) y<-names(data)[y]
  if (!is.character(cov)) cov<-names(data)[cov]
  if (!is.character(factor)) factor<-names(data)[factor]
  if (!is.character(time)) time<-names(data)[time]

  if (length(x)==0)
    x = setdiff(names(data), c(y,cov,time))

  if (any(y %in% x) || any(y %in% cov)) {
    warning(paste0("Dependent varibale indice: `", y, "` is also in `x` or `cov`, will be removed from them, please check.\n"),
            call. = FALSE)
    x = setdiff(x, c(y, time))
    cov = setdiff(cov, c(y, time))
  }

  if (length(intersect(x, cov)!=0)) {
    warning(paste0("`x` varibale indice: `", intersect(x, cov), "` is also in `cov`, will be removed from `cov`, please check.\n"),
            call. = FALSE)
    cov = setdiff(cov,x)
  }


  if (isTRUE(detail_show)) {
    result_detail_y <- result_dataframe_y <- vector(mode = "list", length = length(y))
  } else {
    result_dataframe_y <- vector(mode = "list", length = length(y))
    result_detail_y<-NULL
  }


  split_line <- paste0(rep.int("#",100),collapse = "")

  for (i in seq_along(y)) {
    group_y <- y[i]
    fit_y<-reg(data = data, x = x, y = group_y,cov=cov, factor = factor, model = model,
            time = time, cov_show=cov_show,detail_show=detail_show)

    if (isTRUE(detail_show)) {
      result_detail_y[[i]] <- list(split_line = split_line, group_x = fit_y$detail)
    }
    result_dataframe_y[[i]]<-as.data.frame(cbind(group_y, fit_y$dataframe),stringsAsFactors = FALSE)
  }

  if (isTRUE(detail_show)) {
    names(result_detail_y)<-y
  }


  result_dataframe_y<-as.data.frame(do.call(rbind,result_dataframe_y), stringsAsFactors = FALSE)


  result <- list(detail = result_detail_y, dataframe = result_dataframe_y)
  class(result) <- "reg_y"
  return(invisible(result))
}


