##' @importFrom utils head
##' @method head venn
##' @export
head.venn<-function(x,n=6L,...){
  head(x@result,n,...)
}
##' @importFrom utils tail
##' @method tail venn
##' @export
tail.venn<-function(x,n=6L,...){
  tail(x@result,n,...)
}
##' @method dim venn
##' @export
dim.venn <- function(x) {
  dim(x@result)
}
##' @method [ venn
##' @export
`[.venn` <- function(x, i, j) {
  x@result[i,j]
}
##' @method $ venn
##' @export
`$.venn` <-  function(x, name) {
  x@result[, name]
}
##' @method result venn
##' @export
result.venn<-function(x){
  as.data.frame(x@result)
}
##' @method detail venn
##' @export
detail.venn<-function(x){
  x@detail
}
##' @importFrom magrittr %>%
##' @importFrom dplyr select
##' @importFrom dplyr everything
.add_colnames<-function(x){
  if(!"rown"%in%colnames(x)){
    x$rown<-rownames(x)
  }
  x%>%select(rown,everything())
}

.pasten<-function(x,name,sep="_"){
  colnames(x)<-paste(name,colnames(x),sep=sep)
  return(x)
}
.setrownames<-function(x,gind){
  rownames(x)=as.character(x[,gind])
  return(x[,-gind,drop=F])
}
##' @method rowname join
##' @title join by rownames
##' @rdname rowjoin-methods
##' @importFrom dplyr full_join
##' @importFrom dplyr left_join
##' @importFrom dplyr right_join
##' @importFrom dplyr anti_join
##' @importFrom dplyr semi_join
##' @param x data.frame x
##' @param y data.frame y
##' @param fun join type
##' @export
setMethod("rowjoin",signature(x="data.frame",y="data.frame"),function(x,y,fun="full_join"){
  x<-.add_colnames(x)
  y<-.add_colnames(y)
  f=match.fun(fun)
  return(f(x,y,by=c("rown"="rown")))
})


