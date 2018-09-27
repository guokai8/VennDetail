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
##' @method merge venn object
##' @title merge two or more venn object by group name
##' @rdname merge-method
##' @importFrom purrr flatten
##' @param object list of venn object
##' @param ingore.case ingore case of group name
##' @param useupper use uppercase for all group name
##' @export
##' @author Kai Guo
setMethod("merge",signature = (object="list"),function(object,ingore.case=F,useupper=T){
  input=lapply(object, function(x)slot(x,"input"))
  input=flatten(input)
  nam=names(input)
  if(ingore.case==TRUE){
    if(useupper==TRUE){
      nam=toupper(nam)
      names(input)<-nam
    }else{
      nam=tolower(nam)
      names(input)<-nam
    }
  }
  input=input[unique(nam)]
  ven=venndetail(input,plot=F)
  return(ven)
})
##'
setAs(from = "data.frame",to="venn",def=function(from){
  Group=from$Group
  Detail=from$Detail
  GroupNames=vector()
  raw=vector()
  input=data.frame()
  sep=character()
  detail=as.vector(table(from$Group))
  names(detail)=names(table(Group))
  result=data.frame(Group,Detail)
  new("venn",
      input=input,
      raw=raw,
      sep=sep,
      GroupNames=GroupNames,
      result=result,
      detail=detail)
})
##'
setAs(from = "list",to="venn",def=function(from){
  Group=from$Group
  Detail=from$Detail
  GroupNames=from$GroupName
  raw=from$raw
  input=data.frame()
  sep=character()
  detail=as.vector(table(Group))
  names(detail)=names(table(Group))
  result=data.frame(Group,Detail)
  new("venn",
      input=input,
      raw=raw,
      sep=sep,
      GroupNames=GroupNames,
      result=result,
      detail=detail)
})

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
##' @author Kai Guo
setMethod("rowjoin",signature(x="data.frame",y="data.frame"),function(x,y,fun="full_join"){
  x<-.add_colnames(x)
  y<-.add_colnames(y)
  f=match.fun(fun)
  return(f(x,y,by=c("rown"="rown")))
})
##' @name set colors
##' @title set color with given a vector
##' @param x length of color
##' @return color palette
##' @export
##' @author Kai Guo
setcolor<-function(x){
  mycolor=c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C",
            "#1B9E77","brown","#7570B3","#E7298A","#7FC97F","#A6761D",
            "#BEAED4","#FDC086","#FFFF99","chartreuse1","cyan3","purple",
            "pink4","cyan","royalblue","violet","springgreen2","gold3","darkseagreen4",
           "#E5D8BD",
            "#00AFBB","#FC4E07","#9999FF","#FF9326",
            "#984EA3","#F781BF","#B3B3B3",
            "#CCCCCC","#666666","#01665E","#542788")
  if(x<length(mycolor)){
    res<-mycolor[1:x]
  }else{
    res<-rep(mycolor,2)[1:x]
  }
  return(res)
}
