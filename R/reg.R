#' Build regression models only one dependent varibale
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


reg <- function(data = NULL, x = NULL, y = NULL,cov=NULL, factor = NULL, model = NULL,
    time = NULL, cov_show=FALSE,detail_show=FALSE) {
  if(!is.data.frame(data)) {
    tryCatch( {
      data<-as.data.frame(data,stringsAsFactors = FALSE)
    }, error = function(e) stop("`data` is not a data.frame.", call. = FALSE)
    )
  }
  if (is.null(y) || length(y) != 1)
    stop("One dependent varibale should be provided or use reg_y for more than one dependent varibales!", call. = FALSE)
  if (!is.character(x)) x<-names(data)[x]
  if (!is.character(y)) y<-names(data)[y]
  if (!is.character(cov)) cov<-names(data)[cov]
  if (!is.character(factor)) factor<-names(data)[factor]
  if (!is.character(time)) time<-names(data)[time]

  if (length(x)==0)
        x = setdiff(names(data), c(y,cov,time))
  if (y %in% x || y %in% cov) {
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

    if (is.null(factor)) {
      data <- data
    } else  data[, factor] <- lapply(data[, factor], factor)

    if (!(model %in% c("lm", "glm", "coxph")))
        stop("model should be one of `lm`, `glm` or `coxph`.", call. = FALSE)
    # arg <- list(...)
    # if (any(c("data", "x", "y", "model") %in% names(arg)))
    #   warning("Please check the arguments: data, x, y, model!", call. = FALSE)

    if (isTRUE(detail_show)) {
              result_detail <- result_dataframe <- vector(mode = "list", length = length(x))
    } else {
              result_dataframe <- vector(mode = "list", length = length(x))
              result_detail<-NULL
            }



    split_line <- paste0(rep.int("=",80),collapse = "")


    for (i in seq_along(x)) {
        group_x <- x[i]
        dd<-data[,c(y,group_x,cov,time)]

        var_n<-sum(complete.cases(dd))

        var_formula<-as.formula(paste0(y,"~",paste(c(group_x,cov),sep="",collapse = "+")))

        if (model == "lm") {
          tryCatch({
            fit <- lm(var_formula,data=dd)
            coef <- cbind(fit$coef, suppressMessages(confint(fit)))
            one <- cbind(group_x, summary(fit)$coefficients, coef,var_n)
             one<-as.data.frame(one,stringsAsFactors = FALSE)
             if (!isTRUE(cov_show)) {
              var_rowname<-rownames(one)
              one<-one[grep(pattern=paste0("^",group_x),var_rowname),]
            }
          },error = function(err) {
             warning(paste0("Column: `", group_x, "` cannot fit regression model, please check.\n"),
            call. = FALSE)
            fit<<-NA
            one <<- c(group_x, rep(NA,8))
            },finally = {result_dataframe[[i]] <- one})
            if (isTRUE(detail_show)) {
              result_detail[[i]] <- list(split_line = split_line, summary = summary(fit))
            }

        } else if (model == "glm") {
            # if ("family" %in% names(arg)) {
            #   fit <- glm(var_formula,data=dd)
            # } else {
            #     fit <- glm(var_formula,data=dd, family = binomial(link = "logit"))
            #     }
          tryCatch({
            fit <- glm(var_formula,data=dd, family = binomial(link = "logit"))
            coef <- cbind(fit$coef, suppressMessages(confint(fit)))
            or = exp(coef)
            one <- cbind(group_x, summary(fit)$coefficients, or,var_n)
            one<-as.data.frame(one,stringsAsFactors = FALSE)
            if (!isTRUE(cov_show)) {
              var_rowname<-row.names(one)
              one<-one[grep(pattern=paste0("^",group_x),var_rowname),]
            }
          },error = function(err) {
             warning(paste0("Column: `", group_x, "` cannot fit regression model, please check.\n"),
            call. = FALSE)
            fit<<-NA
            one <<- c(group_x, rep(NA,8))
            },finally = {result_dataframe[[i]] <- one})
            if (isTRUE(detail_show)) {
              result_detail[[i]] <- list(split_line = split_line, summary = summary(fit),
                                               `OR(95%CI)` = or)
            }

        } else if (model == "coxph") {
          var_formula<-as.formula(paste0("survival::Surv(time = ",time,", event =",y,")","~",paste(c(group_x,cov),sep="",collapse = "+")))
          tryCatch({
            fit <- survival::coxph(var_formula,data=dd)
          one <- cbind(group_x, summary(fit)$coefficients, exp(confint(fit)),var_n)
          one<-as.data.frame(one,stringsAsFactors = FALSE)
          if (!isTRUE(cov_show)) {
            var_rowname<-rownames(one)
            one<-one[grep(pattern=paste0("^",group_x),var_rowname),]
          }
          },error = function(err) {
             warning(paste0("Column: `", group_x, "` cannot fit regression model, please check.\n"),
            call. = FALSE)
            fit<<-NA
            one <<- c(group_x, rep(NA,8))
            },finally = {result_dataframe[[i]] <- one})

          if (isTRUE(detail_show)) {
            result_detail[[i]] <- list(split_line = split_line, summary = summary(fit))
          }

        }

    }

    if (isTRUE(detail_show)) {
           names(result_detail)<-x
    }

    result_dataframe<-as.data.frame(do.call(rbind,result_dataframe), stringsAsFactors = FALSE)

    result_dataframe <-  cbind(term= row.names(result_dataframe), result_dataframe)
    row.names(result_dataframe) <- NULL
    result_dataframe <- as.data.frame(result_dataframe, stringsAsFactors = FALSE)
    result_dataframe <- result_dataframe[!grepl("(Intercept)",result_dataframe$term),]

    result_dataframe[,c(3:NCOL(result_dataframe))] <- lapply(result_dataframe[,c(3:NCOL(result_dataframe))], as.numeric)

    if (model == "lm") {
        names(result_dataframe) <- c("term","group_x", "estimate", "std.error", "statistic",
            "p.value", "coef", "coef.low", "coef.high","N")

    } else if (model == "glm") {
        names(result_dataframe) <- c("term","group_x", "estimate", "std.error", "statistic",
            "p.value", "OR", "OR.low", "OR.high","N")
    } else if (model == "coxph") {
        names(result_dataframe) <- c("term","group_x", "estimate", "HR", "std.error",
            "statistic", "p.value", "HR.low", "HR.high","N")
        result_dataframe <- result_dataframe[, c("term","group_x", "estimate", "std.error",
            "statistic", "p.value", "HR", "HR.low", "HR.high","N")]
    }

    result_detail[["call"]] <- match.call()
    result <- list(detail = result_detail, dataframe = result_dataframe)
    class(result) <- "reg"
    return(invisible(result))
}


