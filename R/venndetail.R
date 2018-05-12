##' Extract shared and unique sets
##' @docType method
##' @name venndetail
##' @rdname venndetail-methods
##' @title venn detail information
##' @importFrom VennDiagram venn.diagram
##' @param x list of variables with group names
##' @param plot whether plot the venndiagram plot or not
##' @inheritParams VennDiagram::venn.diagram
##' @export
##' @author Kai Guo
##' @examples
##' \dontrun{
##' A <- sample(1:100, 40, replace = FALSE);
##' B <- sample(1:100, 60, replace = FALSE);
##' C <- sample(1:100, 40, replace = FALSE);
##' res<-venndetail(list(A=A,B=B,C=C),plot=TRUE)
##' }
venndetail<-function(x,plot=TRUE,filename=NULL,col="black",mycol=c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
                    cat.cex=1.5,alpha=0.5,cex=2,cat.fontface="bold",margin=0.05){
  if(is.null(names(x))){
    names(x)<-paste("Group",1:length(x))
  }
  GroupNames=names(x)
  if(length(x)==1){
    cat("Only one sets find!\n")
    return(NULL)
  }
  else if(length(x)==2){
    A=x[[1]]
    B=x[[2]]
    nAB=intersect(A,B)
    nA=setdiff(A,B)
    nB=setdiff(B,A)
    gname=rep(c("Shared",names(x)),times=c(length(nAB),length(nA),length(nB)))
    res<-data.frame(Group=gname,Detail=c(nAB,nA,nB))
    detail<-c(length(nAB),length(nA),length(nB))
    names(detail)<-c("Shared",names(x)[1],names(x)[2])
  }
  else if(length(x)==3){
    A=x[[1]]
    B=x[[2]]
    C=x[[3]]
    nABC=Reduce(intersect,x)
    nAB=setdiff(intersect(A,B),nABC)
    nAC=setdiff(intersect(A,C),nABC)
    nBC=setdiff(intersect(B,C),nABC)
    nA=Reduce(setdiff,list(A,B,C))
    nB=Reduce(setdiff,list(B,C,A))
    nC=Reduce(setdiff,list(C,A,B))
    ggname=c("Shared",paste(names(x)[1:2],sep="",collapse = "_"),
             paste(names(x)[c(1,3)],sep="",collapse = "_"),
             paste(names(x)[2:3],sep="",collapse = "_"),names(x))
    detail<-unlist(lapply(list(nABC,nAB,nAC,nBC,nA,nB,nC), function(x)length(x)))
    names(detail)<-ggname
    gname=rep(ggname,times=detail)
    res=data.frame(Group=gname,Detail=c(nABC,nAB,nAC,nBC,nA,nB,nC))
  }
  else if(length(x)==4){
    A=x[[1]]
    B=x[[2]]
    C=x[[3]]
    D=x[[4]]
    nABCD=Reduce(intersect,x)
    nABC=setdiff(Reduce(intersect,list(A,B,C)),nABCD)
    nABD=setdiff(Reduce(intersect,list(A,B,D)),nABCD)
    nACD=setdiff(Reduce(intersect,list(A,C,D)),nABCD)
    nBCD=setdiff(Reduce(intersect,list(B,C,D)),nABCD)
    nAB=Reduce(setdiff,list(intersect(A,B),nABC,nABD,nABCD))
    nAC=Reduce(setdiff,list(intersect(A,C),nABC,nACD,nABCD))
    nAD=Reduce(setdiff,list(intersect(A,D),nACD,nABD,nABCD))
    nBC=Reduce(setdiff,list(intersect(B,C),nABC,nBCD,nABCD))
    nBD=Reduce(setdiff,list(intersect(B,D),nABD,nBCD,nABCD))
    nCD=Reduce(setdiff,list(intersect(C,D),nBCD,nACD,nABCD))
    nA=Reduce(setdiff,list(A,B,C,D))
    nB=Reduce(setdiff,list(B,C,D,A))
    nC=Reduce(setdiff,list(C,D,A,B))
    nD=Reduce(setdiff,list(D,A,B,C))
    ggname=c("Shared",paste(names(x)[1:3],sep="",collapse = "_"),
             paste(names(x)[c(1,2,4)],sep="",collapse = "_"),
             paste(names(x)[c(1,3,4)],sep="",collapse = "_"),
             paste(names(x)[2:4],sep="",collapse = "_"),
             paste(names(x)[1:2],sep="",collapse = "_"),
             paste(names(x)[c(1,3)],sep="",collapse = "_"),
             paste(names(x)[c(1,4)],sep="",collapse = "_"),
             paste(names(x)[2:3],sep="",collapse = "_"),
             paste(names(x)[c(2,4)],sep="",collapse = "_"),
             paste(names(x)[3:4],sep="",collapse = "_"),
             names(x))
    detail<-unlist(lapply(list(nABCD,nABC,nACD,nABD,nBCD,nAB,nAC,nAD,nBC,nBD,nCD,nA,nB,nC,nD), function(x)length(x)))
    names(detail)<-ggname
    gname=rep(ggname,times=detail)
    res=data.frame(Group=gname,Detail=c(nABCD,nABC,nACD,nABD,nBCD,nAB,nAC,nAD,nBC,nBD,nCD,nA,nB,nC,nD))
  }
  else if(length(x)==5){
    A=x[[1]]
    B=x[[2]]
    C=x[[3]]
    D=x[[4]]
    E=x[[5]]
    nABCDE=Reduce(iintersect,x)
    nABCD=setdiff(Reduce(intersect,list(A,B,C,D)),nABCDE)
    nABCE=setdiff(Reduce(intersect,list(A,B,C,E)),nABCDE)
    nABDE=setdiff(Reduce(intersect,list(A,B,D,E)),nABCDE)
    nACDE=setdiff(Reduce(intersect,list(A,C,D,E)),nABCDE)
    nBCDE=setdiff(Reduce(intersect,list(B,C,D,E)),nABCDE)
    nA=Reduce(setdiff,list(A,B,C,D,E))
    nB=Reduce(setdiff,list(B,C,D,E,A))
    nC=Reduce(setdiff,list(C,D,E,A,B))
    nD=Reduce(setdiff,list(D,E,A,B,C))
    nE=Reduce(setdiff,list(E,A,B,C,D))
    ggname=c("Shared",paste(names(x)[1:4],sep="",collapse = "_"),
             paste(names(x)[c(1,2,3,5)],sep="",collapse = "_"),
             paste(names(x)[c(1,2,4,5)],sep="",collapse = "_"),
             paste(names(x)[1,3,4,5],sep="",collapse = "_"),
             paste(names(x)[2:5],sep="",collapse = "_"),
             names(x))
    detail<-unlist(lapply(list(nABCDE,nABCD,nABCE,nABDE,nACDE,nBCDE,nA,nB,nC,nD,nE), function(x)length(x)))
    gname=rep(ggname,times=detail)
    res=data.frame(Group=gname,Detail=c(nABCDE,nABCD,nABCE,nABDE,nACDE,nBCDE,nA,nB,nC,nD,nE))
  }
  result<-new("venn",
              GroupNames=GroupNames,
              result=res,
              detail=detail)
  if(plot==TRUE){
    require(VennDiagram)
    n=length(x)
    p<-venn.diagram(x,filename = filename,
                  col = col,
                  fill = mycol[1:n],
                  alpha = alpha,
                  cex=cex,
                  cat.col = mycol[1:n],
                  cat.cex = cat.cex,
                  cat.fontface = cat.fontface,
                  margin = margin)
   grid.draw(p)
  }
  return(result)
}
