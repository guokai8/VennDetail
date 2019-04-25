##' @name vennpie
##' @title Vennpie provides a pie-chart like visualization similar to
##' traditional Venn Diagram plots
##' @description Vennpie uses the venn object and to creates a figure in the
##' form of a venn pie diagram
##' rather than a traditional venn diagram. Users can highlight a specific
##' sections of the venn pie.
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
##' @importFrom dplyr filter_
##' @importFrom dplyr select_
##' @importFrom stats quantile
##' @param object Venn object
##' @param subset Character vector giving the subset users want to highlight.
##' @param top number of subsets with largest to display (default: 31)
##' @param min The minimum number of input groups that a subset must belong to
##' e.g. min = 2 will only report those
##' subsets with elements shared by 2 or more input groups.
##' @param color Character vector giving the colors of the subsets.
##' @param revcolor Character giving the color for the non-selected subsets.
##' @param show.number Boolean indicating whether to display the element numbers
##' of the subsets or not (default: TRUE).
##' @param log Boolean indicating whether to transform the data in log scale .
##' @param base Base value for log transformation.
##' @param sep Character string used to separate the terms when concatenating
##' group names into new column names (colnames).
##' @param percentage Boolean indicating whether to display subset percentages
##' (default: FALSE).
##' @param show.x Boolean indicating whether to show subset labels outside the
##' circle (default: TRUE).
##' @param any Number to indicate selected subsets, such as 1 means any unique
##' subsets, 2 means any subsets shared by two groups.
##' @return vennpie figure
##' @export
##' @examples
##' A <- sample(1:100, 40, replace = FALSE)
##' B <- sample(1:100, 60, replace = FALSE)
##' C <- sample(1:100, 40, replace = FALSE)
##' res <- venndetail(list(A = A, B = B, C = C))
##' vennpie(res)
setMethod("vennpie", signature = (object="Venn"), function(object, subset = NULL,
                top = 31, min = 0,
                color = NULL, revcolor = "lightgrey", any = NULL,
                show.number = TRUE,show.x = TRUE, sep = "_", log = FALSE,
                base = NULL, percentage = FALSE){
    #options(stringsAsFactors = F)
    group=subset
    lhs <- result(object, wide = TRUE)
    lhs <- lhs%>%filter_(~SharedSets >= min)%>%select_(~Detail)
    lhs <- as.vector(lhs[, 1])
    rhs <- result(object)
    rhs <- rhs%>%filter_(~Detail %in% lhs)%>%select_(~Subset)
    rhs <- unique(as.vector(rhs[, 1]))
    det <- object@detail
    detail <- det[rhs]
   # detail["Other"] <- sum(det[setdiff(names(det), rhs)])
    if(length(det) >= top){
        ths <- names(sort(det,decreasing = TRUE))[seq_along(1:top)]
    }else{
        ths <- names(det)
    }
    detail <- detail[intersect(names(detail), ths)]
    #detail["Other"] <- sum(det[setdiff(names(det), names(detail))])
    gr <- object@GroupNames
    raw <- object@raw
    nr <- lengths(regmatches(names(detail), gregexpr(sep, names(detail))))+1
    names(nr) <- names(detail)
    if("Shared"%in%names(nr)){
        nr["Shared"] <- nr["Shared"] + 1
        leng <- length(gr)
        lend <- length(detail)
        tim <- unlist(lapply(strsplit(names(detail)[2:lend], split=sep,
                        fixed = TRUE), length))
        val <- rep(detail, times = c(leng, tim))
        gr <- c(gr, unlist(strsplit(names(detail)[2:lend], split = sep,
            fixed = TRUE)))
        name <- rep(names(detail), times = c(leng, tim))
    }else{
        #lend <- length(detail)
        tim <- unlist(lapply(strsplit(names(detail), split = sep,
                                    fixed = TRUE), length))
        val <- rep(detail, times = tim)
        gr <- unlist(strsplit(names(detail), split = sep,
                               fixed = TRUE))
        name <- rep(names(detail), times = tim)
    }
    if(!is.null(color)){
        if(!any(is.na(names(color)))){
            names(color) <- names(detail)
        }
    }else{
            color <- setcolor(length(detail))
            names(color) <- names(detail)
    }
    if(!is.null(any)){
        ac <- names(nr)[nr%in%any]
    }else{
        ac <- ""
    }
    if(!is.null(group)){
        color[setdiff(names(color), c(group, ac))] <- revcolor
    }else{
        if(!is.null(any)){
            color[setdiff(names(color), ac)] <- revcolor
        }else{
            color[setdiff(names(color), names(nr))] <- revcolor
        }
    }
    if(sep == ":"){
        delim <- " "
    }else{
        delim <- ":"
    }
    #gcol <- color[gr]
    dd <- data.frame(name = name, group = gr, val = val)
    dd$sets <- paste(name, val, sep = delim)
    dd$group <- factor(dd$group, levels = names(raw))
    if(isTRUE(log)){
        if(!is.null(base)){
            dd$val <- round(log(dd$val, base = base), 2)
        }else{
            dd$val <- round(log2(dd$val), 2)
        }
    }
    if(isTRUE(show.number)){
        names(color) <- paste(names(color), detail, sep = delim)
        p <- ggplot(dd, aes_(~group, ~val, fill =~ sets, color = ~group))+
            theme_light(base_size = 15)
    }else{
        p <- ggplot(dd, aes_(~group, ~val, fill = ~name, color = ~group))+
            theme_light(base_size = 15)
    }
    gn <- paste(names(raw), raw, sep = delim)
    gcol <- rep("white", length(gn))
    ybreaks <- as.integer(as.numeric(quantile(c(0, max(raw)))))
    # names(gcol)=gn
    if(isTRUE(percentage)){
        p <- p + geom_bar(stat="identity", position="fill", alpha = 0.8)
    }else{
        p <- p + geom_bar(stat = "identity", alpha = 0.8)+
            scale_y_continuous(breaks = ybreaks)
    }
    p <- p + scale_fill_manual(values = color)+
        coord_polar("y", start = 0)+labs(fill = "Visualization Subsets")
    p <- p + scale_color_manual(breaks = names(raw), labels = gn,
        values = gcol)+
        labs(color = "Input Groups")+
        guides(fill = guide_legend(order = 0),
        color = guide_legend(reverse = TRUE, order = 1))
    p <- p + .clean()
    if(isFALSE(show.number)){
        p <- p + theme(axis.text.x = element_blank())
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
