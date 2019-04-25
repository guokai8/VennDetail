##' @param object Venn object
##' @param wide Boolean indicating whether to return wide format(default:FALSE)
##' @return return dataframe and print header of dataframe
##' @export
##' @author Kai Guo
setGeneric("result", function(object, wide = FALSE)
    standardGeneric("result")
)
#result<-function(object,wide=FALSE){
#    UseMethod("result",object)
#}
##' @param object Venn object
##' @return Numeric vector with set names and amounts for each set
##' @export
##' @author Kai Guo
setGeneric("detail", function(object)
    standardGeneric("detail")
)

##' @name dplot
##' @rdname dplot
##' @aliases dplot,Venn-method
##' @docType methods
##' @title Dplot function allows users to visualize the detail function
##' in the form of a barplot
##' @description The amount of members within each group determined by
##' venndetail will be displayed as a bar plot.
##' This will include all groups such as shared, pairwise, and unique.
##' The order of the figure can be adjusted by the users by using the order
##' argument.The text size argument will allow users to change the size of
##' the numbers above the bars indicating the total number of members within
##' each group.
##' @return Produces a bar plot displaying the total counts within each group
##' @export
##' @author Kai Guo
setGeneric("dplot", function(object, order = FALSE, textsize = 5)
    standardGeneric("dplot")
)
##' @name rowjoin
##' @importFrom dplyr full_join
##' @importFrom dplyr left_join
##' @importFrom dplyr right_join
##' @importFrom dplyr anti_join
##' @importFrom dplyr semi_join
##' @export
setGeneric("rowjoin",function(x, y, fun = "fun_join")
    standardGeneric("rowjoin")
)
##' @name getFeature
##' @rdname getFeature
##' @aliases getFeature,Venn-method
##' @docType methods
##' @export
##' @author Kai Guo
setGeneric("getFeature",function(object, subset, rlist, userowname = TRUE,
                                gind = NULL, sep = "_", wide = FALSE)
    standardGeneric("getFeature")
)
##' @rdname vennpie
##' @name vennpie
##' @aliases vennpie,Venn-method
##' @title Pie plot shows shared and unique sets
##' @docType methods
##' @export
##' @author Kai Guo
setGeneric("vennpie", function(object, subset = NULL, top = 31, min = 0,
                        color = NULL,revcolor = "lightgrey", any = NULL,
                        show.number = TRUE,
                        show.x = TRUE, sep = "_", log = FALSE,
                        base = NULL, percentage = FALSE)
    standardGeneric("vennpie")
)
##' @name getSet
##' @rdname getSet
##' @export
setGeneric("getSet",function(object, subset = NULL,
                        min = 0, wide = FALSE)
    standardGeneric("getSet")
)
