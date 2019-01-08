##' @name getFeature
##' @title get feature based on venn results
##' @rdname getFeature
##' @importFrom dplyr filter
##' @importFrom dplyr left_join
##' @importFrom dplyr full_join
##' @importFrom dplyr right_join
##' @importFrom tibble rownames_to_column
##' @importFrom magrittr %>%
##' @param object venn object
##' @param group group you want used
##' @param rlist list of detail dataframe with all information
##' @param userowname use rowname to join dataframe or not
##' @param gind extract column for each data.frame (if userowname=FALSE)
##' @param sep separate for new colnames
##' @export
##' @examples{
##' A <- sample(1:100, 40, replace = FALSE);
##' B <- sample(1:100, 60, replace = FALSE);
##' C <- sample(1:100, 40, replace = FALSE);
##' dA=data.frame(A=A,"FC"=rnorm(40))
##' dB=data.frame(B=B,"FC"=rnorm(60))
##' dC=data.frame(C=C,"FC"=rnorm(40))
##' res<-venndetail(list(A=A,B=B,C=C),plot=TRUE)
##' rhs<-getFeature(res,group="Shared",rlist=list(dA,dB,dC),userowname=FALSE,gind=rep(1,3))
##' }
setMethod("getFeature",signature = (object="venn"),function(object,group,rlist,userowname=TRUE,gind=NULL,sep="_",...){
  dd<-object@result
  if(missing(group)){
    group=unique(dd$Group)
  }
  lhs<-dd%>%filter(Group%in%group)
  lhs$Detail<-as.character(lhs$Detail)
  if(userowname==FALSE){
    if(is.null(gind)){
      gind=rep(1,length(rlist))
    }
    if(length(gind)<length(rlist)){
      stop("Please specify name for each column you want use!")
    }
    rlist=Map(function(x,y).setrownames(x,y),rlist,gind)
  }
  if(is.null(names(rlist))){
    names(rlist)<-paste("Group",1:length(rlist),sep="")
  }
  name<-names(rlist)
  rlist<-Map(function(x,y).pasten(x,y,sep=sep),rlist,name)
  rlist<-lapply(rlist, function(x).add_colnames(x))
  rr<-Reduce(function(x,y)rowjoin(x,y,fun="full_join"),rlist)
  rr$RowNxyz<-as.character(rr$RowNxyz)
  rhs<-left_join(lhs,rr,by=c("Detail"="RowNxyz"))
  return(rhs)
})
