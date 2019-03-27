##' @name vennpie Venn
##' @title Vennpie provides a pie-chart like visualization similar to
##' traditional Venn Diagram plots
##' @description Vennpie uses the venn object and to creates a figure in the
##' form of a venn pie diagram
##' rather than a traditional venn diagram. Users can highlight a specific
##' sections of the venn pie.
##' @rdname vennpie
##' @import ggplot2
##' @importFrom ggplot2 labs
##' @importFrom ggplot2 aes_string
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
##' @importFrom stats quantile
##' @param object Venn object
##' @param group Character vector giving the set users want to highlight.
##' @param color Character vector giving the colors of the sets.
##' @param revcolor Character giving the color for the non-selected sets.
##' @param show.number Boolean indicating whether to display the element numbers
##' of the sets or not (default: TRUE).
##' @param log Boolean indicating whether to transform the data in log scale .
##' @param base Base value for log transformation.
##' @param sep Character string used to separate the terms when concatenating
##' group names into new column names (colnames).
##' @param percentage Boolean indicating whether to display set percentages
##' (default: FALSE).
##' @param show.x Boolean indicating whether to show set labels outside the
##' circle (default: TRUE).
##' @param any Number to indicate selected sets,  such as 1 means any unique
##' sets,  2 means any sets shared by two groups.
##' @return vennpie figure
##' @export
##' @examples
##' A <- sample(1:100,  40,  replace = FALSE);
##' B <- sample(1:100,  60,  replace = FALSE);
##' C <- sample(1:100,  40,  replace = FALSE);
##' res <- venndetail(list(A = A, B = B, C = C))
##' vennpie(res)
setMethod("vennpie", signature = (object="Venn"), function(object, group=NULL,
                color=NULL, revcolor="lightgrey", any=NULL, show.number=TRUE,
                show.x=TRUE, sep="_", log=FALSE, base=NULL, percentage=FALSE){
    #options(stringsAsFactors = F)
    detail <- object@detail
    gr = object@GroupNames
    raw = object@raw
    nr <- lengths(regmatches(names(detail),  gregexpr(sep,  names(detail))))+1
    names(nr) <- names(detail)
    nr[1] <- max(nr)+1
    leng <- length(gr)
    lend <- length(detail)
    tim <- unlist(lapply(strsplit(names(detail)[2:lend], split=sep,
                        fixed = TRUE), length))
    val=rep(detail, times=c(leng, tim))
    gr=c(gr, unlist(strsplit(names(detail)[2:lend], split = sep, fixed = TRUE)))
    name=rep(names(detail), times=c(leng, tim))
    if(!is.null(color)){
        if(!any(is.na(names(color)))){
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
        color[setdiff(names(color), c(group, ac))]=revcolor
    }else{
        if(!is.null(any)){
            color[setdiff(names(color), ac)]=revcolor
        }else{
            color[setdiff(names(color), names(nr))]=revcolor
        }
    }
    if(sep==":"){
        delim=" "
    }else{
        delim=":"
    }
    #gcol <- color[gr]
    dd <- data.frame(name=name, group=gr, val=val)
    dd$sets <- paste(name, val, sep=delim)
    dd$group <- factor(dd$group,  levels = names(raw))
    if(log==TRUE){
        if(!is.null(base)){
            dd$val <- round(log(dd$val, base=base), 2)
        }else{
            dd$val <- round(log2(dd$val), 2)
        }
    }
    if(show.number==TRUE){
        names(color) <- paste(names(color), detail, sep=delim)
        p <- ggplot(dd, aes_(~group, ~val, fill=~sets, color=~group))+
            theme_light(base_size = 15)
    }else{
        p <- ggplot(dd, aes_(~group, ~val, fill=~name, color=~group))+
            theme_light(base_size = 15)
    }
    gn=paste(names(raw), raw, sep=delim)
    gcol=rep("white", length(gn))
    ybreaks=as.integer(as.numeric(quantile(c(0, max(raw)))))
    # names(gcol)=gn
    if(percentage==TRUE){
        p <- p+geom_bar(stat="identity", position="fill")##color="white"
    }else{
        p <- p+geom_bar(stat="identity")+scale_y_continuous(breaks=ybreaks)
    }
    p <- p+scale_fill_manual(values=color)+
        coord_polar("y",  start=0)+labs(fill="")
    p <- p+scale_color_manual(breaks=names(raw), labels=gn, values=gcol)+
        labs(color="")+
        guides(fill=guide_legend(order=0),
        color=guide_legend(reverse=TRUE, order=1))
    p <- p+.clean()
    if(show.x==FALSE){
        p <- p+theme(axis.text.x = element_blank())
    }
    p
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
