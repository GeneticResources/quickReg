#' Display a table used in paper
#'
#' Dispaly count, frequency or mean, standard deviation and test of normality, etc.

#' @param data A data.frame
#' @param var Column indices or names of the variables in the dataset to display, the default columns are all the variables
#' @param group Column indices or names of the subgroup variables.
#' @param mean_or_median A character to specify mean or median to used for continuous variables, either "mean" or "median". The default is "mean"
#' @param useNA Whether to include NA values in the table, see \code{\link{table}} for more details
#' @param discrete_limit A numeric defining the minimal of unique value to display the variable as count and frequency
#' @param exclude_discrete Logical, whether to exclude discrete variables with more unique values specified by discrete_limit
#' @param save_to_file  A character, containing file name or path
#' @param normtest  A character indicating test of normality, the default method is \code{\link{shapiro.test}} when sample size no more than 5000, otherwise \code{\link[nortest]{lillie.test}}{Kolmogorov-Smirnov} is used, see package \strong{nortest} for more methods.Use 'shapiro.test', 'lillie.test', 'ad.test', etc to specify methods.
#' @param \dots additional arguments
#' @import nortest
#' @export
#' @seealso \code{\link{display.reg}},  \code{\link{display}}
#' @examples
#' data(diabetes)
#' head(diabetes)
#' display_table(diabetes, var=c(1:4,6:10),group="diabetes")



display_table <- function(data = NULL, var = NULL,group=NULL, mean_or_median="mean",addNA = TRUE, discrete_limit = 10, exclude_discrete=TRUE, save_to_file=NULL,normtest = NULL) {

   if(!is.data.frame(data)) {
    tryCatch( {
      data<-as.data.frame(data,stringsAsFactors = FALSE)
    }, error = function(e) stop("`data` is not a data.frame.", call. = FALSE)
    )
  }

    if (is.null(var))
      var = seq_along(1:NCOL(data))
    if (!is.character(var)) var<-names(data)[var]
    if(length(group)!=1) stop("`group` is not one length.", call. = FALSE)
    if (!is.character(group)) group<-names(data)[group]
    data<-filter(data,!is.na(!!sym(group)))

    if (any(group %in% var)) {
      warning(paste0("group varibale indice: `", group, "` is also in `var`, will be removed from them, please check.\n"),
              call. = FALSE)
      var = setdiff(var, group)
    }

    if (is.null(normtest)) {
      normtest <- ifelse(NROW(data) <= 5000, "shapiro.test", "lillie.test")
      }

    result <- vector(mode = "list", length = length(var))
    for (i in seq_along(var)) {
      print(i)
      var_i<- var[i]
      if(is_discrete(var_i)) {
        result[[i]]<-discrete_test(var_i)
      } else result[[i]]<-continuous_table(var_i)

    }
    result<-as.data.frame(do.call(rbind,result), stringsAsFactors = FALSE)
    row.names(result)<-NULL
    result_name<-as.list(table(data[,group]))
    names(result)[4:(3+length(result_name))]<-paste0(group," = ",names(result_name),"\n (N=",result_name,")")

    if (!addNA) {
      result<-result[result$level!="NA",]
    }

    if(!is.null(save_to_file)) {
      write.table(result, file = save_to_file, sep = ",", row.names = FALSE)
    }

    return(result)

}






## functions used in display


format_sprint<-function(x) {
  x<-ifelse(abs(x)<0.005,sprintf(fmt="%.2E",x),sprintf(fmt="%.2f",x))
  x
}


## using appropriate method, table or mean

is_discrete<-function(x) {
  x_value<-data[,x]
  if((length(unique(x_value)) >= discrete_limit & is.numeric(x_value))) {
    return(FALSE)
  } else if (length(unique(x_value)) >= discrete_limit&&exclude_discrete) {
    warning=paste0("`",x,"`"," is verbose. To display it using `exclude_discrete = FALSE` ")
    return(NA)
  } else return(TRUE)
}


### for continuous variables
continuous_stat<-function (x,mean_or_median_=mean_or_median) {

  result_1<-data %>%
    group_by(!!sym(group)) %>%
    summarise(mean=mean(!!sym(x),na.rm=TRUE),sd=sd(!!sym(x),na.rm=TRUE),
              median=median(!!sym(x),na.rm = TRUE),Q1=quantile(!!sym(x),probs=c(0.25),na.rm = TRUE),
              Q3=quantile(!!sym(x),probs=c(0.75),na.rm = TRUE),NA_=sum(is.na(!!sym(x)))) %>%
    ungroup()
  result_1[,2:6]<-lapply(result_1[,2:6],format_sprint)
  result_1$mean_sd<-paste0(result_1$mean," ± ",result_1$sd)
  result_1$median_IQR<-paste0(result_1$median," (",result_1$Q1,", ",result_1$Q3,")")
  result_1<- as.data.frame(t(result_1),stringsAsFactors = FALSE)
  mean_sd_1<-result_1[c("mean_sd","NA_"),]
  median_IQR_1<-result_1[c("median_IQR","NA_"),]


  result_2<-data %>%
    summarise(mean=mean(!!sym(x),na.rm=TRUE),sd=sd(!!sym(x),na.rm=TRUE),
              median=median(!!sym(x),na.rm = TRUE),Q1=quantile(!!sym(x),probs=c(0.25),na.rm = TRUE),
              Q3=quantile(!!sym(x),probs=c(0.75),na.rm = TRUE),NA_=sum(is.na(!!sym(x)))) %>%
    ungroup()

  result_2[,1:5]<-lapply(result_2[,1:5],format_sprint)
  result_2$mean_sd<-paste0(result_2$mean," ± ",result_2$sd)
  result_2$median_IQR<-paste0(result_2$median," (",result_2$Q1,", ",result_2$Q3,")")
  result_2<- as.data.frame(t(result_2),stringsAsFactors = FALSE)
  mean_sd_2<-result_2[c("mean_sd","NA_"),]
  median_IQR_2<-result_2[c("median_IQR","NA_"),]
  var_NCOl<-length(unique(data[,group]))
  mean_sd<-cbind(c("mean ± sd","NA"),mean_sd_2,mean_sd_1)
  names(mean_sd)<-c("level",paste0("V",1:(var_NCOl+1)))
  median_IQR<-cbind(c("median (Q1,Q3)","NA"),median_IQR_2,median_IQR_1)
  names(median_IQR)<-c("level",paste0("V",1:(var_NCOl+1)))

  if(mean_or_median_=="mean") {
    return(mean_sd)
  } else if (mean_or_median_=="median") {
    return(median_IQR)
  } else  stop("Mean or Meidan for continuous variables. ", call. = FALSE)

}



continuous_normality<-function(x) {
  if (is.null(normtest)) {
    normtest <- ifelse(length(na.omit(data[,x])) <= 10000, "shapiro.test", "lillie.test")
  }
  p.normality<-data %>%
    group_by(!!sym(group)) %>%
    #select_at(vars(x)) %>%
    do(data.frame(p.normality=do.call(normtest, list(.$UQ(x)))$p.value)) %>%
    ungroup()
  is.normality<-all(p.normality$p.normality>0.05)
  p.normality<-paste(format_sprint(p.normality$p.normality),collapse ="; ")
  return(list(p.normality=p.normality,is.normality=is.normality))
}

continuous_test<-function(x) {
  var_formula<-as.formula(paste0(x,"~",group,sep="",collapse = "+"))
  fit<-anova(lm(var_formula,data = data))
  p.anova<-format_sprint(fit[1,5])

  fit<-kruskal.test(var_formula,data = data)
  p.rank<-format_sprint(fit$p.value)
  return(list(p.anova=p.anova,p.rank=p.rank))
}

continuous_table<-function(x) {
  result<-continuous_stat(x)
  row_fill<-rep("",NROW(result)-1)
  result<-cbind(variable=c(x,row_fill),result,P.value1=c(continuous_test(x)$p.anova,row_fill),P.value2=c(continuous_test(x)$p.rank,row_fill),normality=c(continuous_normality(x)$p.normality,row_fill),stringsAsFactors=FALSE)
  return(result)
}


### for discrete variables
discrete_test<-function (x) {
  var_tab<-table(data[,x],data[,group])
  fit<-suppressWarnings(chisq.test(var_tab))
  p.chisq<-format_sprint(fit$p.value)
  if(any(fit$expected<5)) {
    p.fisher<-fisher.test(tab.test)$p.value
    p.fisher<-format_sprint(p.fisher)
  } else p.fisher<-NA
  var_prop=round(100*prop.table(var_tab,margin=table_margin),2)
  var_tab=cbind(table(data[,x]),var_tab)
  var_NCOl<-NCOL(var_tab)

  var_prop=cbind(round(100*prop.table(table(data[,x])),2),var_prop)
  var_tab=as.data.frame(matrix(paste(var_tab," (",var_prop,"%)",sep=""),ncol =var_NCOl),stringsAsFactors = FALSE)

  if(addNA & any(is.na(data[,x]))) {
    var_value<-as.character(data[,x])
    var_value<-ifelse(is.na(var_value),"NA",var_value)
    table(var_value)
    var_tab_NA<-table(var_value,data[,group])
    var_tab_NA=cbind(table(var_value),var_tab_NA)
    var_tab_NA<-as.data.frame(var_tab_NA,stringsAsFactors = FALSE)
    names(var_tab_NA)<-names(var_tab)
    var_tab_NA$row_name<-row.names(var_tab_NA)
    var_tab_NA<-var_tab_NA[var_tab_NA$row_name=="NA",1:var_NCOl]
    var_tab<-rbind(var_tab,var_tab_NA)
  }
  var_tab<-cbind(level=row.names(var_tab),var_tab,stringsAsFactors=FALSE)
  row_fill<-rep("",NROW(var_tab)-1)
  result<-cbind(variable=c(x,row_fill),var_tab,P.value1=c(p.chisq,row_fill),P.value2=c(p.fisher,row_fill),normality="",stringsAsFactors=FALSE)
  return(result)

}
