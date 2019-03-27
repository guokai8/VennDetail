##' @name getFeature Venn
##' @title getFeature provides a way to combine list of user supplied data
##' frames with Venn object
##' @description GetFeature allows users to extract sets from venn object
##' into a table format along with accompanying information from the data frames
##' provided in the rlist argument
##' @rdname getFeature
##' @return data.frame with subset information and details from the user
##' supplied data frame
##' @importFrom dplyr filter_
##' @importFrom dplyr left_join
##' @importFrom dplyr full_join
##' @importFrom dplyr right_join
##' @importFrom tibble rownames_to_column
##' @importFrom magrittr %>%
##' @param object Venn object
##' @param group Character vector giving the names of the user-defined
##' set to extract
##' @param rlist List of user-supplied data frames to combine with
##' venndetail result
##' @param userowname Boolean indicating whether to use row names to join
##' data frames or not (default: TRUE)
##' @param gind Column name or index of each user-supplied data.frame to
##' use to join data frames(valid only when userowname=FALSE)
##' @param sep Character string used to separate the terms when concatenating
##' group names into
##' new separation character for new column names in the resulting data frame
##' @param wide Boolean indicating whether to use wide format(default:FALSE)
##' @export
##' @examples
##' A <- sample(1:100,  40,  replace = FALSE);
##' B <- sample(1:100,  60,  replace = FALSE);
##' C <- sample(1:100,  40,  replace = FALSE);
##' dA <- data.frame(A = A, "FC" = rnorm(40))
##' dB <- data.frame(B = B, "FC" = rnorm(60))
##' dC <- data.frame(C = C, "FC" = rnorm(40))
##' res <- venndetail(list(A = A, B = B, C = C))
##' rhs <- getFeature(res,  group = "Shared",  rlist = list(dA, dB, dC),
##'    userowname= FALSE, gind = rep(1,  3))
setMethod("getFeature", signature = (object="Venn"), function(object, group,
        rlist, userowname=TRUE, gind=NULL, sep="_", wide=FALSE){
    dd <- object@result
    wd = object@wide
    wd$Detail <- as.character(wd$Detail)
    if(missing(group)){
        group = unique(dd$Group)
    }
    lhs <- dd%>%filter_(~Group%in%group)
    lhs$Detail <- as.character(lhs$Detail)
    if(wide == TRUE){
      lhs <- wd %>% filter_(~Detail%in%lhs$Detail)
    }
    if(userowname == FALSE){
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
    rlist <- lapply(rlist,  function(x).add_colnames(x))
    rr <- Reduce(function(x, y) rowjoin(x, y, fun = "full_join"), rlist)
    rr$RowNxyz <- as.character(rr$RowNxyz)
    rhs <- left_join(lhs, rr, by = c("Detail"="RowNxyz"))
    return(rhs)
})
