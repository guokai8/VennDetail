##' @rdname getFeature
setMethod("getFeature", signature = (object="Venn"), function(object, subset,
        rlist, userowname=TRUE, gind=NULL, sep="_", wide=FALSE){
    dd <- object@result
    wd <- object@wide
    wd$Detail <- as.character(wd$Detail)
    if(missing(subset)){
        subset <- unique(dd$Subset)
    }
    lhs <- dd%>%filter(.data$Subset %in% subset)
    lhs$Detail <- as.character(lhs$Detail)
    if(isTRUE(wide)){
        lhs <- wd %>% filter(.data$Detail %in% lhs$Detail)
    }
    if(isFALSE(userowname)){
        if(is.null(gind)){
            gind <- rep(1, length(rlist))
        }
        if(length(gind) < length(rlist)){
            stop("Please specify name for each column you want use!")
        }
        rlist <- Map(function(x, y).setrownames(x, y), rlist, gind)
    }
    if(is.null(names(rlist))){
        names(rlist) <- paste("Group", seq_along(rlist), sep = "")
    }
    name <- names(rlist)
    rlist <- Map(function(x, y).pasten(x, y, sep = sep), rlist, name)
    rlist <- lapply(rlist, function(x).add_colnames(x))
    rr <- Reduce(function(x, y) rowjoin(x, y, fun = "full_join"), rlist)
    rr$RowNxyz <- as.character(rr$RowNxyz)
    rhs <- left_join(lhs, rr, by = c("Detail"="RowNxyz"))
    return(rhs)
})
