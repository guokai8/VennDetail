##' Extract shared and unique sets
##' @name venndetail
##' @rdname venndetail-methods
##' @title venn detail information
##' @importFrom VennDiagram venn.diagram
##' @importFrom grid grid.draw
##' @param x list of variables with group names
##' @param plot whether plot the venndiagram plot or not
##' @param ven choose to use venn.diagram or not
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
venndetail<-function(x,plot=TRUE,filename=NULL,ven=TRUE,col="black",sep="_",mycol=c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
                    cat.cex=1.5,alpha=0.5,cex=2,cat.fontface="bold",margin=0.05,abbr=FALSE,minlength=3,abbr.method="both.sides",...){
  if(is.null(names(x))){
    names(x)<-paste("Group",1:length(x))
  }
  if(abbr==TRUE){
    names(x)=abbreviate(names(x),minlength = minlength,method = abbr.method)
  }
  GroupNames=names(x)
  raw=unlist(lapply(x,length))
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
    ggname=c("Shared",paste(names(x)[1:2],sep="",collapse = sep),
             paste(names(x)[c(1,3)],sep="",collapse = sep),
             paste(names(x)[2:3],sep="",collapse = sep),names(x))
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
    ggname=c("Shared",paste(names(x)[1:3],sep="",collapse = sep),
             paste(names(x)[c(1,2,4)],sep="",collapse = sep),
             paste(names(x)[c(1,3,4)],sep="",collapse = sep),
             paste(names(x)[2:4],sep="",collapse = sep),
             paste(names(x)[1:2],sep="",collapse = sep),
             paste(names(x)[c(1,3)],sep="",collapse = sep),
             paste(names(x)[c(1,4)],sep="",collapse = sep),
             paste(names(x)[2:3],sep="",collapse = sep),
             paste(names(x)[c(2,4)],sep="",collapse = sep),
             paste(names(x)[3:4],sep="",collapse = sep),
             names(x))
    detail<-unlist(lapply(list(nABCD,nABC,nABD,nACD,nBCD,nAB,nAC,nAD,nBC,nBD,nCD,nA,nB,nC,nD), function(x)length(x)))
    names(detail)<-ggname
    gname=rep(ggname,times=detail)
    res=data.frame(Group=gname,Detail=c(nABCD,nABC,nABD,nACD,nBCD,nAB,nAC,nAD,nBC,nBD,nCD,nA,nB,nC,nD))
  }
  else if(length(x)==5){
    A=x[[1]]
    B=x[[2]]
    C=x[[3]]
    D=x[[4]]
    E=x[[5]]
    nABCDE=Reduce(intersect,x)
    nABCD=setdiff(Reduce(intersect,list(A,B,C,D)),nABCDE)
    nABCE=setdiff(Reduce(intersect,list(A,B,C,E)),nABCDE)
    nABDE=setdiff(Reduce(intersect,list(A,B,D,E)),nABCDE)
    nACDE=setdiff(Reduce(intersect,list(A,C,D,E)),nABCDE)
    nBCDE=setdiff(Reduce(intersect,list(B,C,D,E)),nABCDE)
    nABC=Reduce(setdiff,list(Reduce(intersect,list(A,B,C)),nABCD,nABCE,nABCDE))
    nABD=Reduce(setdiff,list(Reduce(intersect,list(A,B,D)),nABCD,nABDE,nABCDE))
    nABE=Reduce(setdiff,list(Reduce(intersect,list(A,B,E)),nABDE,nABCE,nABCDE))
    nACD=Reduce(setdiff,list(Reduce(intersect,list(A,C,D)),nACDE,nABCD,nABCDE))
    nADE=Reduce(setdiff,list(Reduce(intersect,list(A,D,E)),nACDE,nABDE,nABCDE))
    nACE=Reduce(setdiff,list(Reduce(intersect,list(A,C,E)),nACDE,nABCE,nABCDE))
    nBCD=Reduce(setdiff,list(Reduce(intersect,list(B,C,D)),nABCD,nBCDE,nABCDE))
    nBCE=Reduce(setdiff,list(Reduce(intersect,list(B,C,E)),nABCE,nBCDE,nABCDE))
    nBDE=Reduce(setdiff,list(Reduce(intersect,list(B,D,E)),nBCDE,nABDE,nABCDE))
    nCDE=Reduce(setdiff,list(Reduce(intersect,list(C,D,E)),nACDE,nBCDE,nABCDE))
    nAB=setdiff(intersect(A,B),Reduce(union,list(C,D,E)))
    nAC=setdiff(intersect(A,C),Reduce(union,list(B,D,E)))
    nAD=setdiff(intersect(A,D),Reduce(union,list(B,C,E)))
    nAE=setdiff(intersect(A,E),Reduce(union,list(C,D,B)))
    nBC=setdiff(intersect(C,B),Reduce(union,list(A,D,E)))
    nBD=setdiff(intersect(D,B),Reduce(union,list(A,C,E)))
    nBE=setdiff(intersect(E,B),Reduce(union,list(A,D,C)))
    nCD=setdiff(intersect(C,D),Reduce(union,list(A,B,E)))
    nCE=setdiff(intersect(C,E),Reduce(union,list(A,B,D)))
    nDE=setdiff(intersect(E,D),Reduce(union,list(A,B,C)))
    nA=Reduce(setdiff,list(A,B,C,D,E))
    nB=Reduce(setdiff,list(B,C,D,E,A))
    nC=Reduce(setdiff,list(C,D,E,A,B))
    nD=Reduce(setdiff,list(D,E,A,B,C))
    nE=Reduce(setdiff,list(E,A,B,C,D))
    ggname=c("Shared",paste(names(x)[1:4],sep="",collapse = sep),
             paste(names(x)[c(1,2,3,5)],sep="",collapse = sep),
             paste(names(x)[c(1,2,4,5)],sep="",collapse = sep),
             paste(names(x)[c(1,3,4,5)],sep="",collapse = sep),
             paste(names(x)[2:5],sep="",collapse = sep),
             paste(names(x)[1:3],sep="",collapse = sep),
             paste(names(x)[c(1,2,4)],sep="",collapse = sep),
             paste(names(x)[c(1,2,5)],sep="",collapse = sep),
             paste(names(x)[c(1,3,4)],sep="",collapse = sep),
             paste(names(x)[c(1,4,5)],sep="",collapse = sep),
             paste(names(x)[c(1,3,5)],sep="",collapse = sep),
             paste(names(x)[c(2,3,4)],sep="",collapse = sep),
             paste(names(x)[c(2,3,5)],sep="",collapse = sep),
             paste(names(x)[c(2,4,5)],sep="",collapse = sep),
             paste(names(x)[3:5],sep="",collapse = sep),
             paste(names(x)[c(1,2)],sep="",collapse = sep),
             paste(names(x)[c(1,3)],sep="",collapse = sep),
             paste(names(x)[c(1,4)],sep="",collapse = sep),
             paste(names(x)[c(1,5)],sep="",collapse = sep),
             paste(names(x)[c(2,3)],sep="",collapse = sep),
             paste(names(x)[c(2,4)],sep="",collapse = sep),
             paste(names(x)[c(2,5)],sep="",collapse = sep),
             paste(names(x)[c(3,4)],sep="",collapse = sep),
             paste(names(x)[c(3,5)],sep="",collapse = sep),
             paste(names(x)[c(4,5)],sep="",collapse = sep),
             names(x))
    detail<-unlist(lapply(list(nABCDE,nABCD,nABCE,nABDE,nACDE,nBCDE,nABC,nABD,nABE,nACD,nADE,
                               nACE,nBCD,nBCE,nBDE,nCDE,nAB,nAC,nAD,nAE,nBC,nBD,nBE,nCD,nCE,
                               nDE,nA,nB,nC,nD,nE), function(x)length(x)))

    gname=rep(ggname,times=detail)
    names(detail)<-ggname
    res=data.frame(Group=gname,Detail=c(nABCDE,nABCD,nABCE,nABDE,nACDE,nBCDE,nABC,nABD,nABE,nACD,nADE,nACE,nBCD,nBCE,nBDE,nCDE,nAB,nAC,nAD,nAE,nBC,nBD,nBE,nCD,nCE,nDE,nA,nB,nC,nD,nE))
  }
  else if(length(x)==6){
    A=x[[1]]
    B=x[[2]]
    C=x[[3]]
    D=x[[4]]
    E=x[[5]]
    F=x[[6]]
    nABCDEF=Reduce(intersect,x)
    nABCDE=setdiff(Reduce(intersect,list(A,B,C,D,E)),nABCDEF)
    nABCDF=setdiff(Reduce(intersect,list(A,B,C,D,F)),nABCDEF)
    nABCEF=setdiff(Reduce(intersect,list(A,B,C,E,F)),nABCDEF)
    nABDEF=setdiff(Reduce(intersect,list(A,B,D,E,F)),nABCDEF)
    nACDEF=setdiff(Reduce(intersect,list(A,C,D,E,F)),nABCDEF)
    nBCDEF=setdiff(Reduce(intersect,list(B,C,D,E,F)),nABCDEF)
    #
    nAB=setdiff(intersect(A,B),Reduce(union,list(C,D,E,F)))
    nAC=setdiff(intersect(A,C),Reduce(union,list(D,E,F,B)))
    nAD=setdiff(intersect(A,D),Reduce(union,list(E,F,B,C)))
    nAE=setdiff(intersect(A,E),Reduce(union,list(B,C,D,F)))
    nAF=setdiff(intersect(A,F),Reduce(union,list(B,C,D,E)))
    #
    nBC=setdiff(intersect(B,C),Reduce(union,list(A,D,E,F)))
    nBD=setdiff(intersect(B,D),Reduce(union,list(A,C,E,F)))
    nBE=setdiff(intersect(B,E),Reduce(union,list(A,C,D,F)))
    nBF=setdiff(intersect(B,F),Reduce(union,list(A,C,D,E)))
    #
    nCD=setdiff(intersect(C,D),Reduce(union,list(A,B,E,F)))
    nCE=setdiff(intersect(C,E),Reduce(union,list(A,B,D,F)))
    nCF=setdiff(intersect(C,F),Reduce(union,list(A,B,D,E)))
    nDE=setdiff(intersect(D,E),Reduce(union,list(A,B,C,F)))
    nDF=setdiff(intersect(D,F),Reduce(union,list(A,B,C,E)))
    nEF=setdiff(intersect(E,F),Reduce(union,list(A,B,C,D)))
    #
    nABC=setdiff(Reduce(intersect,list(A,B,C)),Reduce(union,list(D,E,F)))
    nABD=setdiff(Reduce(intersect,list(A,B,D)),Reduce(union,list(C,E,F)))
    nABE=setdiff(Reduce(intersect,list(A,B,E)),Reduce(union,list(C,D,F)))
    nABF=setdiff(Reduce(intersect,list(A,B,F)),Reduce(union,list(C,D,E)))
    #
    nACD=setdiff(Reduce(intersect,list(A,C,D)),Reduce(union,list(B,E,F)))
    nACE=setdiff(Reduce(intersect,list(A,C,E)),Reduce(union,list(B,D,F)))
    nACF=setdiff(Reduce(intersect,list(A,C,F)),Reduce(union,list(B,D,E)))
    #
    nADE=setdiff(Reduce(intersect,list(A,D,E)),Reduce(union,list(B,C,F)))
    nADF=setdiff(Reduce(intersect,list(A,D,F)),Reduce(union,list(B,C,E)))
    nAEF=setdiff(Reduce(intersect,list(A,E,F)),Reduce(union,list(B,C,D)))
    #
    nBCD=setdiff(Reduce(intersect,list(B,C,D)),Reduce(union,list(A,E,F)))
    nBCE=setdiff(Reduce(intersect,list(B,C,E)),Reduce(union,list(A,D,F)))
    nBCF=setdiff(Reduce(intersect,list(B,C,F)),Reduce(union,list(A,D,E)))
    #
    nBDE=setdiff(Reduce(intersect,list(B,D,E)),Reduce(union,list(A,C,F)))
    nBDF=setdiff(Reduce(intersect,list(B,D,F)),Reduce(union,list(A,C,E)))
    nBEF=setdiff(Reduce(intersect,list(B,E,F)),Reduce(union,list(A,C,D)))
    #
    nCDE=setdiff(Reduce(intersect,list(C,D,E)),Reduce(union,list(A,B,F)))
    nCDF=setdiff(Reduce(intersect,list(C,D,F)),Reduce(union,list(A,B,E)))
    nCEF=setdiff(Reduce(intersect,list(C,E,F)),Reduce(union,list(A,B,D)))
    #
    nDEF=setdiff(Reduce(intersect,list(D,E,F)),Reduce(union,list(A,B,C)))
    #
    nABCD=setdiff(Reduce(intersect,list(A,B,C,D)),Reduce(union,list(E,F)))
    nABCE=setdiff(Reduce(intersect,list(A,B,C,E)),Reduce(union,list(D,F)))
    nABCF=setdiff(Reduce(intersect,list(A,B,C,F)),Reduce(union,list(D,E)))
    nABDE=setdiff(Reduce(intersect,list(A,B,D,E)),Reduce(union,list(C,F)))
    nABDF=setdiff(Reduce(intersect,list(A,B,D,F)),Reduce(union,list(C,E)))
    nABEF=setdiff(Reduce(intersect,list(A,B,E,F)),Reduce(union,list(C,D)))
    #
    nACDE=setdiff(Reduce(intersect,list(A,C,D,E)),Reduce(union,list(B,F)))
    nACDF=setdiff(Reduce(intersect,list(A,C,D,F)),Reduce(union,list(B,E)))
    nACEF=setdiff(Reduce(intersect,list(A,C,E,F)),Reduce(union,list(B,D)))
    #
    nADEF=setdiff(Reduce(intersect,list(A,D,E,F)),Reduce(union,list(B,C)))
    #
    nBCDE=setdiff(Reduce(intersect,list(B,C,D,E)),Reduce(union,list(A,F)))
    nBCDF=setdiff(Reduce(intersect,list(B,C,D,F)),Reduce(union,list(A,E)))
    nBDEF=setdiff(Reduce(intersect,list(B,D,E,F)),Reduce(union,list(A,C)))
    #
    nCDEF=setdiff(Reduce(intersect,list(C,D,E,F)),Reduce(union,list(A,B)))
    nA=Reduce(setdiff,list(A,B,C,D,E,F))
    nB=Reduce(setdiff,list(B,C,D,E,F,A))
    nC=Reduce(setdiff,list(C,D,E,F,A,B))
    nD=Reduce(setdiff,list(D,E,F,A,B,C))
    nE=Reduce(setdiff,list(E,F,A,B,C,D))
    nF=Reduce(setdiff,list(F,A,B,C,D,E))
    ggname=c("Shared",paste(names(x)[1:5],sep="",collapse = sep),
             paste(names(x)[c(1:4,6)],sep="",collapse = sep),
             paste(names(x)[c(1,2,3,5,6)],sep="",collapse = sep),
             paste(names(x)[c(1,2,4,5,6)],sep="",collapse = sep),
             paste(names(x)[c(1,3:6)],sep="",collapse = sep),
             paste(names(x)[2:6],sep="",collapse = sep),
             ##
             paste(names(x)[c(1:4)],sep="",collapse=sep),paste(names(x)[c(1:3,5)],sep="",collapse=sep),
             paste(names(x)[c(1,2,3,6)],sep="",collapse=sep),paste(names(x)[c(1,2,4,5)],sep="",collapse=sep),
             paste(names(x)[c(1,2,4,6)],sep="",collapse=sep),paste(names(x)[c(1,2,5,6)],sep="",collapse=sep),
             paste(names(x)[c(1,3,4,5)],sep="",collapse=sep),paste(names(x)[c(1,3,4,6)],sep="",collapse=sep),
             paste(names(x)[c(1,3,5,6)],sep="",collapse=sep),paste(names(x)[c(1,4,5,6)],sep="",collapse=sep),
             paste(names(x)[c(2,3,4,5)],sep="",collapse=sep),paste(names(x)[c(2,3,4,6)],sep="",collapse=sep),
             paste(names(x)[c(2,4,5,6)],sep="",collapse=sep),paste(names(x)[c(3,4,5,6)],sep="",collapse=sep),
             ##
             paste(names(x)[c(1,2,3)],sep="",collapse=sep),paste(names(x)[c(1,2,4)],sep="",collapse=sep),
             paste(names(x)[c(1,2,5)],sep="",collapse=sep),paste(names(x)[c(1,2,6)],sep="",collapse=sep),
             paste(names(x)[c(1,3,4)],sep="",collapse=sep),paste(names(x)[c(1,3,5)],sep="",collapse=sep),
             paste(names(x)[c(1,3,6)],sep="",collapse=sep),paste(names(x)[c(1,4,5)],sep="",collapse=sep),
             paste(names(x)[c(1,4,6)],sep="",collapse=sep),paste(names(x)[c(1,5,6)],sep="",collapse=sep),
             paste(names(x)[c(2,3,4)],sep="",collapse=sep),paste(names(x)[c(2,3,5)],sep="",collapse=sep),
             paste(names(x)[c(2,3,6)],sep="",collapse=sep),paste(names(x)[c(2,4,5)],sep="",collapse=sep),
             paste(names(x)[c(2,4,6)],sep="",collapse=sep),paste(names(x)[c(2,5,6)],sep="",collapse=sep),
             paste(names(x)[c(3,4,5)],sep="",collapse=sep),paste(names(x)[c(3,4,6)],sep="",collapse=sep),
             paste(names(x)[c(3,5,6)],sep="",collapse=sep),paste(names(x)[c(4,5,6)],sep="",collapse=sep),
             ##
             paste(names(x)[c(1,2)],sep="",collapse=sep),paste(names(x)[c(1,3)],sep="",collapse=sep),
             paste(names(x)[c(1,4)],sep="",collapse=sep),paste(names(x)[c(1,5)],sep="",collapse=sep),
             paste(names(x)[c(1,6)],sep="",collapse=sep),paste(names(x)[c(2,3)],sep="",collapse=sep),
             paste(names(x)[c(2,4)],sep="",collapse=sep),paste(names(x)[c(2,5)],sep="",collapse=sep),
             paste(names(x)[c(2,6)],sep="",collapse=sep),paste(names(x)[c(3,4)],sep="",collapse=sep),
             paste(names(x)[c(3,5)],sep="",collapse=sep),paste(names(x)[c(3,6)],sep="",collapse=sep),
             paste(names(x)[c(4,5)],sep="",collapse=sep),paste(names(x)[c(4,6)],sep="",collapse=sep),
             paste(names(x)[c(5,6)],sep="",collapse=sep),
             names(x))
    detail<-unlist(lapply(list(nABCDEF,nABCDE,nABCDF,nABCEF,nABDEF,nACDEF,nBCDEF,
                               nABCD,nABCE,nABCF,nABDE,nABDF,nABEF,nACDE,nACDF,nACEF,nADEF,nBCDE,nBCDF,nBDEF,nCDEF,
                               nABC,nABD,nABE,nABF,nACD,nACE,nACF,nADE,nADF,nAEF,nBCD,nBCE,nBCF,nBDE,nBDF,nBEF,nCDE,nCDF,nCEF,nDEF,
                               nAB,nAC,nAD,nAE,nAF,nBC,nBD,nBE,nBF,nCD,nCE,nCF,nDE,nDF,nEF,
                               nA,nB,nC,nD,nE,nF), function(x)length(x)))
    gname=rep(ggname,times=detail)
    names(detail)<-ggname
    res=data.frame(Group=gname,Detail=c(nABCDEF,nABCDE,nABCDF,nABCEF,nABDEF,nACDEF,nBCDEF,
                                        nABCD,nABCE,nABCF,nABDE,nABDF,nABEF,nACDE,nACDF,nACEF,nADEF,nBCDE,nBCDF,nBDEF,nCDEF,
                                        nABC,nABD,nABE,nABF,nACD,nACE,nACF,nADE,nADF,nAEF,nBCD,nBCE,nBCF,nBDE,nBDF,nBEF,nCDE,nCDF,nCEF,nDEF,
                                        nAB,nAC,nAD,nAE,nAF,nBC,nBD,nBE,nBF,nCD,nCE,nCF,nDE,nDF,nEF,
                                        nA,nB,nC,nD,nE,nF))
  }
  else if(length(x)==7){
    A=x[[1]]
    B=x[[2]]
    C=x[[3]]
    D=x[[4]]
    E=x[[5]]
    F=x[[6]]
    G=x[[7]]
    nABCDEFG=Reduce(intersect,x)
    nABCDEF=setdiff(Reduce(intersect,list(A,B,C,D,E,F)),nABCDEFG)
    nABCDEG=setdiff(Reduce(intersect,list(A,B,C,D,E,G)),nABCDEFG)
    nABCDFG=setdiff(Reduce(intersect,list(A,B,C,D,F,G)),nABCDEFG)
    nABCEFG=setdiff(Reduce(intersect,list(A,B,C,E,F,G)),nABCDEFG)
    nABDEFG=setdiff(Reduce(intersect,list(A,B,D,E,F,G)),nABCDEFG)
    nACDEFG=setdiff(Reduce(intersect,list(A,C,D,E,F,G)),nABCDEFG)
    nBCDEFG=setdiff(Reduce(intersect,list(A,B,C,D,F,G)),nABCDEFG)
    nA=Reduce(setdiff,list(A,B,C,D,E,F,G))
    nB=Reduce(setdiff,list(B,C,D,E,F,G,A))
    nC=Reduce(setdiff,list(C,D,E,F,G,A,B))
    nD=Reduce(setdiff,list(D,E,F,G,A,B,C))
    nE=Reduce(setdiff,list(E,F,G,A,B,C,D))
    nF=Reduce(setdiff,list(F,G,A,B,C,D,E))
    nG=Reduce(setdiff,list(G,A,B,C,D,E,F))
    ggname=c("Shared",paste(names(x)[1:6],sep="",collapse = sep),
             paste(names(x)[c(1:5,7)],sep="",collapse = sep),
             paste(names(x)[c(1:4,6,7)],sep="",collapse = sep),
             paste(names(x)[c(1:3,5:7)],sep="",collapse = sep),
             paste(names(x)[c(1,2,4:7)],sep="",collapse = sep),
             paste(names(x)[c(1,3:7)],sep="",collapse = sep),
             paste(names(x)[2:7],sep="",collapse = sep),
             names(x))
    detail<-unlist(lapply(list(nABCDEFG,nABCDEF,nABCDEG,nABCDFG,nABCEFG,nABDEFG,nACDEFG,nBCDEFG,nA,nB,nC,nD,nE,nF,nG),function(x)length(x)))
    gname=rep(ggname,times=detail)
    names(detail)<-ggname
    res=data.frame(Group=gname,Detail=c(nABCDEFG,nABCDEF,nABCDEG,nABCDFG,nABCEFG,nABDEFG,nACDEFG,nBCDEFG,nA,nB,nC,nD,nE,nF,nG))
  }
  else if(length(x)==8){
    A=x[[1]]
    B=x[[2]]
    C=x[[3]]
    D=x[[4]]
    E=x[[5]]
    F=x[[6]]
    G=x[[7]]
    H=x[[8]]
    nABCDEFGH=Reduce(intersect,x)
    nABCDEFG=setdiff(Reduce(intersect,list(A,B,C,D,E,F,G)),nABCDEFGH)
    nABCDEFH=setdiff(Reduce(intersect,list(A,B,C,D,E,F,H)),nABCDEFGH)
    nABCDEGH=setdiff(Reduce(intersect,list(A,B,C,D,E,G,H)),nABCDEFGH)
    nABCDFGH=setdiff(Reduce(intersect,list(A,B,C,D,F,G,H)),nABCDEFGH)
    nABCEFGH=setdiff(Reduce(intersect,list(A,B,C,E,F,G,H)),nABCDEFGH)
    nABDEFGH=setdiff(Reduce(intersect,list(A,B,D,E,F,G,H)),nABCDEFGH)
    nACDEFGH=setdiff(Reduce(intersect,list(A,C,D,E,F,G,H)),nABCDEFGH)
    nBCDEFGH=setdiff(Reduce(intersect,list(B,C,D,E,F,G,H)),nABCDEFGH)
    nA=Reduce(setdiff,list(A,B,C,D,E,F,G,H))
    nB=Reduce(setdiff,list(B,C,D,E,F,G,H,A))
    nC=Reduce(setdiff,list(C,D,E,F,G,H,A,B))
    nD=Reduce(setdiff,list(D,E,F,G,H,A,B,C))
    nE=Reduce(setdiff,list(E,F,G,H,A,B,C,D))
    nF=Reduce(setdiff,list(F,G,H,A,B,C,D,E))
    nG=Reduce(setdiff,list(G,H,A,B,C,D,E,F))
    nH=Reduce(setdiff,list(H,A,B,C,D,E,F,H))
    ggname=c("Shared",paste(names(x)[1:7],sep="",collapse = sep),
             paste(names(x)[c(1:6,8)],sep="",collapse = sep),
             paste(names(x)[c(1:5,7,8)],sep="",collapse = sep),
             paste(names(x)[c(1:4,6:8)],sep="",collapse = sep),
             paste(names(x)[c(1:3,5:8)],sep="",collapse = sep),
             paste(names(x)[c(1,2,4:8)],sep="",collapse = sep),
             paste(names(x)[c(1,3:8)],sep="",collapse = sep),
             paste(names(x)[2:8],sep="",collapse = sep),
             names(x))
    detail<-unlist(lapply(list(nABCDEFGH,nABCDEFG,nABCDEFH,nABCDEGH,nABCDFGH,nABCEFGH,nABDEFGH,nACDEFGH,nBCDEFGH,nA,nB,nC,nD,nE,nF,nG,nH),function(x)length(x)))
    gname=rep(ggname,times=detail)
    names(detail)<-ggname
    res=data.frame(Group=gname,Detail=c(nABCDEFGH,nABCDEFG,nABCDEFH,nABCDEGH,nABCDFGH,nABCEFGH,nABDEFGH,nACDEFGH,nBCDEFGH,nA,nB,nC,nD,nE,nF,nG,nH))
  }
  result<-new("venn",
              raw=raw,
              GroupNames=GroupNames,
              result=res,
              detail=detail)
  if(plot==TRUE){
    if(ven==TRUE&&length(x)<=5){
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
    }else{
   print(vennpie(result,sep=sep))
    }
  }
  return(result)
}





