##' @param object venn object
##' @return return dataframe and print header of dataframe
##' @export
##' @author Kai Guo
result<-function(object){
    UseMethod("result",object)
}
##' @param object venn object
##' @return Numeric vector with group names and amounts for each group
##' @export
##' @author Kai Guo
detail<-function(object){
    UseMethod("detail",object)
}
##' @name dplot
##' @rdname dplot
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
setGeneric("dplot",function(object,order=FALSE,...){
    standardGeneric("dplot")
})
##' @name rowjoin
##' @param x dataframe x
##' @param y dataframe y
##' @param fun join type
##' @export
setGeneric("rowjoin",function(x,y,...){
    standardGeneric("rowjoin")
})
##' @name getFeature
##' @rdname getFeature
##' @export
##' @author Kai Guo
setGeneric("getFeature",function(object,group, rlist, userowname=TRUE,gind=NULL,
                                sep="_",...){
    standardGeneric("getFeature")
})
##' @rdname vennpie
##' @name vennpie
##' @title Pie plot shows shared and unique sets
##' @export
##' @author Kai Guo
setGeneric("vennpie",function(object,group=NULL,color=NULL,revcolor="lightgrey",
                        any=NULL,show.number=TRUE,show.x=TRUE,sep="_",log=FALSE,
                        base=NULL,percentage=FALSE,...){
    standardGeneric("vennpie")
})

##' Merge two or more venn object by group name
##'
##' @name merge
##' @rdname merge
##' @author Kai Guo
setGeneric("merge",function(object,ignore.case=FALSE,useupper=TRUE,plot=FALSE,
                        ...){
    standardGeneric("merge")
})
