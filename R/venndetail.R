##' Extract shared and unique subsets
##' @name venndetail
##' @rdname venndetail
##' @title Extract shared and unique subsets
##' @description Extracts shared and unique elements from groups provided to
##' the function.This base function will create a formal class venn object and
##' can also graphically plot the amount of objects in each group. The plot will
##' be in the form of a traditional venn diagram as default.And users can also
##' use vennpie or upset methods to display the result.
##' @return venn object and figures
##' @importFrom VennDiagram venn.diagram
##' @importFrom grid grid.draw
##' @importFrom UpSetR upset
##' @importFrom UpSetR fromList
##' @importFrom methods new
##' @param x A list of variables with group names.
##' @param sep symbol character used when concatenating group names into subset
##' names (default: ‘_’).
##' @param abbr Boolean indicating whether to abbreviate subset names
##' (default: FALSE).
##' @param minlength Minmal length for the subset name.
##' @param abbr.method a character string specifying the method used.
##' Partial matches allowed. (default: both side).
##' @inheritParams VennDiagram::venn.diagram
##' @examples
##' A <- sample(1:100, 40, replace = FALSE)
##' B <- sample(1:100, 60, replace = FALSE)
##' C <- sample(1:100, 40, replace = FALSE)
##' res <- venndetail(list(A = A, B = B, C = C))
##' @export
##' @author Kai Guo
venndetail <- function(x,  sep="_",  abbr = FALSE,  minlength = 3,
                    abbr.method = "both side"){
    if(is.null(names(x))){
        names(x) <- paste("Group",  seq_along(x))
    }
    if(isTRUE(abbr)){
        names(x) <- abbreviate(names(x), minlength = minlength,
                method = abbr.method)
    }
    GroupNames <- names(x)
    raw <- unlist(lapply(x,  length))
    if(length(x) == 1){
        cat("Only one group find!\n")
        return(NULL)
    }
    out <- make.subset(x, sep = sep)
    #names(out)[1] <- "Shared"
    detail <- unlist(lapply(out, function(x) length(x)))
    res <- data.frame(Subset = rep(names(out), as.numeric(detail)),
            Detail = unlist(out))
    rownames(res) <- NULL
    ###create wide format result
    inp <- unique(unlist(x))
    wide <- data.frame(Detail = inp)
    wide <- cbind(wide, sapply(names(x),  function(y) inp %in% x[[y]]))
    wide[wide == TRUE] <- 1
    wide$SharedSets <- rowSums(wide[, 2:ncol(wide)])
    wide <- wide[order(wide$SharedSets, decreasing = TRUE), ]
    result <- new("Venn",
                input = x,
                raw = raw,
                sep = sep,
                GroupNames = GroupNames,
                result = res,
                detail = detail,
                wide = wide)
    return(result)
}





