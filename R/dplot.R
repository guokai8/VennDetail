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
##' @importFrom ggplot2 ylim
##' @param object venn object
##' @export
##' @examples
##' \dontrun{
##' A <- sample(1:100, 40, replace = FALSE);
##' B <- sample(1:100, 60, replace = FALSE);
##' C <- sample(1:100, 40, replace = FALSE);
##' res<-venndetail(list(A=A,B=B,C=C),plot=TRUE)
##' dplot(res,order=T,textsize=3)
##' }
setMethod("dplot",signature = (object="venn"),function(object,order=FALSE,textsize=5,...){
  df<-data.frame(Group=names(object@detail),Detail=object@detail)
  color=setcolor(length(object@detail))
  names(color)=names(object@detail)
  if(order==TRUE){
  df$Group<-factor(df$Group, levels = df$Group[order(df$Detail)])
  }
  p<-ggplot(df,aes(Group,Detail,fill=Group))+geom_bar(stat="identity")+scale_fill_manual(values=color)+theme_light(base_size = 12)+theme(axis.text.x=element_text(angle=90))+
  ggplot2::geom_text(aes(label=Detail),vjust=-0.3,size=textsize)+ggplot2::ylim(0,max(df$Detail)+1)
  p
})
##' @name get
##' @rdname get-methods
##' @importFrom dplyr filter
##' @importFrom magrittr %>%
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
##' @name show
##' @title show detail of venn object
##' @rdname show-methods
##' @param object venn object
##' @export
setMethod("show",signature = (object="venn"),function(object){
  cat("=== Here is the detail of Venndiagram===\n");
  cat("Total results: ",nrow(object@result),"x",ncol(object@result),"\n")
  cat("Total sets is:",length(unique(object@result$Group)),"\n")
  print(object@result[1:6,],quote=FALSE)
  cat("... with",nrow(object@result)-6,"more rows\n")
})
