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
##' @title Extract the result from venn object
##' @description Result will return output in a table format including the contents of the groups included in the venndetail object
##' @rdname result
##' @examples
##' A <- sample(1:100, 40, replace = FALSE);
##' B <- sample(1:100, 60, replace = FALSE);
##' C <- sample(1:100, 40, replace = FALSE);
##' res<-venndetail(list(A=A,B=B,C=C),plot=FALSE)
##' result<-result(res)
##' @export
result.venn<-function(object){
  as.data.frame(object@result)
}
##' @method detail venn
##' @title The detail function provides a way to display the amount of members in each group
##' @description  The objective of this function is to allow insight into the amount of overlap between groups identified by
##' venndetail without the need to print a diagram. The groups displayed will include all comparisons such as
##' shared, pairwise, and unique.
##' @rdname detail
##' @examples
##' A <- sample(1:100, 40, replace = FALSE);
##' B <- sample(1:100, 60, replace = FALSE);
##' C <- sample(1:100, 40, replace = FALSE);
##' res<-venndetail(list(A=A,B=B,C=C),plot=FALSE)
##' detail(res)
##' @export
detail.venn<-function(object){
  object@detail
}
##' @method plot venn
##' @title Plot venn object
##' @description Plot allows users to graphically display the groups and overlap between groups
##' in their venn class object through a variety of graph types such as a bar plot, traditional venn,
##' or venn pie chart.
##' @rdname plot
##' @return different type of graphics based on user chose
##' @importFrom VennDiagram venn.diagram
##' @importFrom UpSetR upset
##' @param type use venn,vennpie or upset (default: venn)
##' @param filename output filename
##' @param col color for the circle
##' @param sep separation character used for the group and numbers
##' @param mycol color for VennDiagram graphic
##' @param cat.cex font size
##' @param alpha transparency value
##' @param cat.fontface font type
##' @param abbr abbreviate name or not (default: FALSE)
##' @param minlength set minmal length for names
##' @param text.scale vector of text size for upset (ylab,yaxis,xlab,group name,xaxis,insection)
##' @param abbr.method abbreviation method(default: both side)
##' @param sets.x.label x-axis label (upset)
##' @param mainbar.y.label y-axis label (upset)
##' @inheritParams UpSetR::upset
##' @examples
##' A<-sample(1:100,40,replace = FALSE);
##' B<-sample(1:100,60,replace = FALSE);
##' C<-sample(1:100,40,replace = FALSE);
##' res<-venndetail(list(A=A,B=B,C=C),plot=FALSE)
##' plot(res,type="venn")
##' @export
##' @author Kai Guo
plot.venn<-function(x,type="venn",col="black",sep="_",mycol=c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
                    cat.cex=1.5,alpha=0.5,cex=2,cat.fontface="bold",margin=0.05,
                    text.scale=c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5),filename=NULL,piecolor=NULL,revcolor="lightgrey",any=NULL,show.number=TRUE,show.x=TRUE,
                    log=FALSE,base=NULL,percentage=FALSE,sets.x.label = "Set Size",mainbar.y.label = "Intersection Size",nintersects = 40,...){
  result<-x
  x<-x@input
  if(type=="venn"&&length(x)<=5){
    #require(VennDiagram)
    n=length(x)
    p<-venn.diagram(x,filename = filename,
                    col = col,
                    fill = mycol[1:n],
                    alpha = alpha,
                    cex=cex,
                    cat.col = mycol[1:n],
                    cat.cex = cat.cex,
                    cat.fontface = cat.fontface,
                    #cat.pos=cat.pos,
                    #cat.dist=cat.dist,
                    margin = margin)
    grid.draw(p)
    rfile=list.files(pattern="*.log")
    file.remove(rfile)
  }
  if(type=="vennpie"){
    print(vennpie(result,sep=sep,color=piecolor,
                  revcolor=revcolor,any=any,show.number=show.number,show.x=show.x,
                  log=log,base=base,percentage=percentage))
  }
  if(type=="upset"){
    if(length(x)<=5){
      upset(fromList(x), nsets = length(x),sets.x.label = sets.x.label,mainbar.y.label = mainbar.y.label,nintersects = nintersects,point.size=5,sets.bar.color=mycol[seq_along(x)],text.scale = text.scale)
    }else{
      upset(fromList(x), nsets = length(x),sets.x.label = sets.x.label,mainbar.y.label = mainbar.y.label,nintersects = nintersects,point.size=5,sets.bar.color=setcolor[length(x)],text.scale = text.scale)
    }
  }

}
##' @importFrom magrittr %>%
##' @importFrom dplyr select
##' @importFrom dplyr everything
.add_colnames<-function(x){
  if(sum(grepl("RowNxyz",colnames(x)))==0){
    x$RowNxyz<-rownames(x)
  }else{
    colnames(x)[which(grepl("RowNxyz",colnames(x)))]<-"RowNxyz"
  }
  x%>%select(RowNxyz,everything())
}

.pasten<-function(x,name,sep="_"){
  colnames(x)<-paste(name,colnames(x),sep=sep)
  return(x)
}
.setrownames<-function(x,gin){
  #rownames(x)=as.character(x[,gind])
  if(is.character(gin)){
    ind=which(colnames(x)==gin)
  }else{
    ind=gin
  }
  colnames(x)[ind]<-"RowNxyz"
  #return(x[,-ind,drop=F])
  return(x)
}
##' @method merge venn
##' @title Merge two or more venndetail obejcts
##' @description Merge will combine multiple venn diagrams to allow comparison between multiple groups
##' @rdname merge
##' @importFrom purrr flatten
##' @param object list of venn objects
##' @param ignore.case ignore case of group name (default: FALSE)
##' @param useupper use uppercase for all group name (default: TRUE)
##' @param plot plot figure or not (default: FALSE)
##' @return venn object
##' @export
setMethod("merge",signature = (object="list"),function(object,ignore.case=FALSE,useupper=TRUE,plot=FALSE,...){
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
  ven=venndetail(input,plot=plot)
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
##' @title Join data.frame based on rownames
##' @description join two dataframes by rownames
##' @rdname rowjoin
##' @method rowname join
##' @importFrom dplyr full_join
##' @importFrom dplyr left_join
##' @importFrom dplyr right_join
##' @importFrom dplyr anti_join
##' @importFrom dplyr semi_join
##' @param x data.frame x
##' @param y data.frame y
##' @param fun different join format: left_join,full_join,right_join (default:full_join)
##' @return dataframe with join results
##' @author Kai Guo
setMethod("rowjoin",signature(x="data.frame",y="data.frame"),function(x,y,fun="full_join"){
  x<-.add_colnames(x)
  y<-.add_colnames(y)
  f=match.fun(fun)
  return(f(x,y,by=c("RowNxyz"="RowNxyz")))
})
##' @name setcolor
##' @title return colors with given a vector
##' @description Setcolor will provide a list of color vectors based on the number used as an input.
##' @param x amount of color
##' @return color vector
##' @examples
##' mycol<-setcolor(10)
##' mycol
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
    res<-c(mycolor,sample(colors(),x,replace=FALSE))
  }
  return(res)
}
