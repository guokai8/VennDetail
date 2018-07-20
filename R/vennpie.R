##' @name vennpie
##' @title get feature based on venn results
##' @rdname vennpie-methods
##' @import ggplot2
##' @importFrom ggplot2 labs
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 geom_bar
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 guides
##' @importFrom ggplot2 theme_light
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 labs
##' @importFrom ggplot2 coord_polar
##' @importFrom ggplot2 scale_fill_manual
##' @importFrom ggplot2 scale_color_manual
##' @param object venn object
##' @param group set name you want display
##' @param color vector of color you want use for the group
##' @param revcolor backgroup color
##' @param show.number display the element number of the group or not
##' @param log use log transform or not
##' @param base log base
##' @param sep separate for new colnames
##' @param percentage display percentage format or not
##' @export
##' @examples
##' \dontrun{
##' A <- sample(1:100, 40, replace = FALSE);
##' B <- sample(1:100, 60, replace = FALSE);
##' C <- sample(1:100, 40, replace = FALSE);
##' dA=data.frame(A=A,"FC"=rnorm(40))
##' dB=data.frame(B=B,"FC"=rnorm(60))
##' dC=data.frame(C=C,"FC"=rnorm(40))
##' res<-venndetail(list(A=A,B=B,C=C),plot=TRUE)
##' vennpie(res)
##' }
setMethod("vennpie",signature = (object="venn"),function(object,group=NULL,color=NULL,
                                                         revcolor="lightgrey",any=NULL,show.number=TRUE,show.x=TRUE,
                                                         sep="_",log=FALSE,base=NULL,percentage=FALSE,...){
  #options(stringsAsFactors = F)
  detail<-object@detail
  gr=object@GroupNames
  raw=object@raw
  nr<-lengths(regmatches(names(detail), gregexpr(sep, names(detail))))+1
  names(nr)<-names(detail)
  nr[1]<-max(nr)+1
  leng<-length(gr)
  lend<-length(detail)
  tim<-unlist(lapply(strsplit(names(detail)[2:lend],split=sep,fixed = TRUE),length))
  val=rep(detail,times=c(leng,tim))
  gr=c(gr,unlist(strsplit(names(detail)[2:lend],split = sep,fixed = TRUE)))
  name=rep(names(detail),times=c(leng,tim))
  if(!is.null(color)){
    if(any(is.na(names(color)))){
      names(color)=names(detail)
    }
  }else{
      color=setcolor(length(detail))
      names(color)=names(detail)
  }
  if(!is.null(any)){
    ac=names(nr)[nr%in%any]
  }else{
    ac=""
  }
  if(!is.null(group)){
    color[setdiff(names(color),c(group,ac))]=revcolor
  }else{
    if(!is.null(any)){
      color[setdiff(names(color),ac)]=revcolor
    }else{
      color[setdiff(names(color),names(nr))]=revcolor
    }
  }
  if(sep==":"){
    delim=" "
  }else{
    delim=":"
  }
  #gcol<-color[gr]
  dd<-data.frame(name=name,group=gr,val=val)
  dd$sets<-paste(name,val,sep=delim)
  dd$group<-factor(dd$group, levels = names(raw))
  if(log==TRUE){
    if(!is.null(base)){
      dd$val<-round(log(dd$val,base=base),2)
    }else{
      dd$val<-round(log2(dd$val),2)
    }
  }
  if(show.number==TRUE){
   names(color)<-paste(names(color),detail,sep=delim)
    p<-ggplot(dd,aes(group,val,fill=sets,color=group))+theme_light(base_size = 15)
  }else{
    p<-ggplot(dd,aes(group,val,fill=name,color=group))+theme_light(base_size = 15)
  }
  gn=paste(names(raw),raw,sep=delim)
  gcol=rep("white",length(gn))
  ybreaks=as.integer(as.numeric(quantile(c(0,max(raw)))))
 # names(gcol)=gn
  if(percentage==TRUE){
    p<-p+geom_bar(stat="identity",position="fill")##color="white"
  }else{
    p<-p+geom_bar(stat="identity")+scale_y_continuous(breaks=ybreaks)#color="white"
  }
    p<-p+scale_fill_manual(values=color)+
      coord_polar("y", start=0)+labs(fill="")
    p<-p+scale_color_manual(breaks=names(raw),labels=gn,values=gcol)+labs(color="")+
      guides(fill=guide_legend(order=0),color=guide_legend(reverse=TRUE,order=1))
   # p<-p+scale_color_manual(values=rep("white",length(unique(dd$group))))+labs(color="")+
  #      guides(fill=guide_legend(order=0),color=guide_legend(reverse=TRUE,order=1))
    p<-p+.clean()
    if(show.x==FALSE){
      p<-p+theme(axis.text.x = element_blank())
    }
    p
    #return(dd)
})
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
.clean <- function(){
    theme(
    axis.title = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    #axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )
}
