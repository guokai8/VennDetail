##' @name dplot
##' @rdname dplot
##' @method dplot Venn
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 geom_bar
##' @importFrom ggplot2 element_text
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 theme_light
##' @importFrom ggplot2 geom_text
##' @importFrom ggplot2 ylim
##' @param object Venn object
##' @param order Boolean indicating whether to sort the bar (default: FALSE).
##' @param textsize Numeric vector giving the text size above the bar.
##' @examples
##' A <- sample(1:100, 40, replace = FALSE)
##' B <- sample(1:100, 60, replace = FALSE)
##' C <- sample(1:100, 40, replace = FALSE)
##' res <- venndetail(list(A = A, B = B, C = C))
##' dplot(res, order = TRUE, textsize = 3)
##' @export

setMethod("dplot", signature = (object="Venn"), function(object, order = FALSE,
                textsize = 5){
    df <- data.frame(Group = names(object@detail), Detail = object@detail)
    color <- setcolor(length(object@detail))
    names(color) <- names(object@detail)
    if(isTRUE(order)){
        df$Group <- factor(df$Group, levels = df$Group[order(df$Detail)])
    }
    p <- ggplot(df, aes_(~Group, ~Detail, fill = ~Group)) +
        geom_bar(stat = "identity") + scale_fill_manual(values = color) +
        theme_light(base_size = 12) + theme(axis.text.x =
        element_text(angle = 90)) + labs(fill = "Subset") +
        geom_text(aes_(label = ~Detail), vjust = -0.3, size = textsize) +
        ylim(0, max(df$Detail) + 1)
    p
})
##' @name getSet
##' @rdname getSet
##' @aliases getSet,Venn-method
##' @docType methods
##' @title getSet function provides a way to extract subsets
##' @description getSet function provides a way to extract subsets from
##' venndetail object
##' @importFrom dplyr filter_
##' @importFrom magrittr %>%
##' @param object Venn object
##' @param subset Character vector giving the subset names
##' @param min The minimum number of input groups that a subset must belong to
##' e.g. min = 2 will only report those
##' subsets with elements shared by 2 or more input groups.
##' @param wide Boolean indicating return wide format (default: FALSE).
##' @return Specific subset information
##' @export
##' @author Kai Guo
##' @examples
##' A <- sample(1:100, 40, replace = FALSE)
##' B <- sample(1:100, 60, replace = FALSE)
##' C <- sample(1:100, 40, replace = FALSE)
##' res <- venndetail(list(A = A, B = B, C = C))
##' getSet(res, "A")
setMethod("getSet", signature = (object="Venn"), function(object, subset = NULL,
        min = 0, wide = FALSE){
    res <- result(object, wide = TRUE)
    dd <- object@result
    rhs <- res%>%filter_(~SharedSets >= min)
    if(is.null(subset)){
        subset <- names(detail(object))
    }
    lhs <- dd%>%filter_(~Subset %in% subset)
    if(isTRUE(wide)){
        out <- rhs%>%filter_(~Detail %in% lhs$Detail)
    }else{
        out <- lhs%>%filter_(~Detail %in% rhs$Detail)
    }
    return(out)
})
##' @name show Venn
##' @aliases show,Venn-method
##' @title Show the summary of venn object
##' @description This function provides a summary of the venn object, including
##' a full results and subsets as well as an summary information.
##' @rdname show
##' @docType methods
##' @return summary information for the venn object
##' @param object venn object
##' @author Kai Guo
##' @examples
##' A <- sample(1:100, 40, replace = FALSE)
##' B <- sample(1:100, 60, replace = FALSE)
##' C <- sample(1:100, 40, replace = FALSE)
##' res <- venndetail(list(A = A, B = B, C = C))
##' show(res)
##'
setMethod("show", signature = (object="Venn"), function(object){
    cat("=== Here is the detail of Venndiagram ===\n")
    cat("Total results: ", nrow(object@result), "x", ncol(object@result), "\n")
    cat("Total sets is:", length(unique(object@result$Subset)), "\n")
    print(head(object@result), quote = FALSE)
    if(nrow(object@result) > 6){
    cat("... with", nrow(object@result) - 6, "more rows ...\n")
    }
})
