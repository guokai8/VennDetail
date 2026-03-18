##' @rdname dplot
setMethod("dplot", signature = (object="Venn"), function(object, order = FALSE,
                textsize = 5, color = NULL, theme = ggplot2::theme_light(),
                title = NULL, xlabel = NULL, ylabel = NULL){
    df <- data.frame(Group = names(object@detail), Detail = object@detail)
    if(is.null(color)){
        color <- setcolor(length(object@detail))
    }
    names(color) <- names(object@detail)
    if(isTRUE(order)){
        df$Group <- factor(df$Group, levels = df$Group[order(df$Detail)])
    }
    p <- ggplot(df, aes(x = .data$Group, y = .data$Detail, fill = .data$Group)) +
        geom_bar(stat = "identity") + scale_fill_manual(values = color) +
        theme + theme(axis.text.x =
        element_text(angle = 90)) + labs(fill = "Subset") +
        geom_text(aes(label = .data$Detail), vjust = -0.3, size = textsize) +
        ylim(0, max(df$Detail) + 1)
    if(!is.null(title)) p <- p + ggtitle(title)
    if(!is.null(xlabel)) p <- p + xlab(xlabel)
    if(!is.null(ylabel)) p <- p + ylab(ylabel)
    p
})
##' @rdname getSet
setMethod("getSet", signature = (object="Venn"), function(object, subset = NULL,
        min = 0, wide = FALSE){
    res <- result(object, wide = TRUE)
    dd <- object@result
    rhs <- res%>%filter(.data$SharedSets >= min)
    if(is.null(subset)){
        subset <- names(detail(object))
    }
    lhs <- dd%>%filter(.data$Subset %in% subset)
    if(isTRUE(wide)){
        out <- rhs%>%filter(.data$Detail %in% lhs$Detail)
    }else{
        out <- lhs%>%filter(.data$Detail %in% rhs$Detail)
    }
    return(out)
})
