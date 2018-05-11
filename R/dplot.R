##' @name dplot
##' @rdname dplot-methods
##' @method dplot venn
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes
##' @importFrom ggplot2 geom_bar
##' @importFrom ggplot2 element_text
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 theme_light
##' @importFrom ggplot2 geom_text
##' @param object venn object
##' @export
##' @examples
##' \dontrun{
##' A <- sample(1:100, 40, replace = FALSE);
##' B <- sample(1:100, 60, replace = FALSE);
##' C <- sample(1:100, 40, replace = FALSE);
##' res<-venndetail(list(A=A,B=B,C=C),plot=TRUE)
##' dplot(res)
##' }
setMethod("dplot",signature = (object="venn"),function(object,...){
  df<-data.frame(Group=names(object@detail),Detail=object@detail)
  p<-ggplot(df,aes(Group,Detail,fill=Group))+geom_bar(stat="identity")+theme_light(base_size = 12)+theme(axis.text.x=element_text(angle=90))+
  ggplot2::geom_text(aes(label=Detail),vjust=-0.3,size=5)
  p
})
##' @name get
##' @rdname get-methods
##' @importFrom dplyr filter
##' @param object venn object
##' @param group group you want extract
##' @export
##' @examples
##' \dontrun{
##' A <- sample(1:100, 40, replace = FALSE);
##' B <- sample(1:100, 60, replace = FALSE);
##' C <- sample(1:100, 40, replace = FALSE);
##' res<-venndetail(list(A=A,B=B,C=C),plot=TRUE)
##' get(res,"A")
##' }
setMethod("get",signature = (object="venn"),function(object,group,...){
  dd<-object@result
  lhs<-dd%>%filter(Group%in%group)
  head(lhs)
  return(lhs)
})
