##' result generic
##' @param x venn object
##' @return return dataframe and print header of dataframe
##' @export
##' @author Kai Guo
result<-function(x){
  UseMethod("result",x)
}
##' detail generic
##' @param x venn object
##' @return vector include numbers belong to different sets
##' @export
##' @author Kai Guo
detail<-function(x){
  UseMethod("detail",x)
}
##' dplot generic
##' @name dplot
##' @rdname dplot-methods
##' @title detail plot
##' @return plot
##' @export
##' @author Kai Guo
setGeneric("dplot",function(object,order=FALSE,...){
  standardGeneric("dplot")
})
##' get generic
##'
##' @name get
##' @rdname get-methods
##' @title get detail
##' @return return dataframe and print the header of dataframe
##' @export
##' @author Kai Guo
setGeneric("get",function(object,group,...){
  standardGeneric("get")
})
##' rowjoin generic
##' @name rowjoin
##' @rdname rowjoin-methods
##' @title join data.frame based on rownames
##' @param x dataframe x
##' @param y dataframe y
##' @param fun join type
##' @return dataframe with join results
##' @export
##' @author Kai Guo
setGeneric("rowjoin",function(x,y,...){
  standardGeneric("rowjoin")
})
##' getFeature generic
##'
##' @name getFeature
##' @rdname getFeature-methods
##' @title get feature based on venn results
##' @return dataframe with all venn information and details from input
##' @export
##' @author Kai Guo
setGeneric("getFeature",function(object,group, rlist, userowname=TRUE,gind=NULL,sep="_",...){
  standardGeneric("getFeature")
})
##' Make vennpie figure
##'
##' @name vennpie
##' @rdname vennpie-methods
##' @title pie plot show shared and unique part
##' @return plot
##' @export
##' @author Kai Guo
setGeneric("vennpie",function(object,...){
  standardGeneric("vennpie")
})

##' Merge two or more venn object by group name
##'
##' @name merge
##' @rdname merge-methods
##' @title merge two or more venn object
##' @param object list of venn object
##' @param ingore.case ingore case of group name
##' @param useupper use uppercase for all group name
##' @param plot plot figure or not
##' @return venn object
##' @author Kai Guo
setGeneric("merge",function(object,ingore.case=F,useupper=T,plot=FALSE,...){
  standardGeneric("merge")
})
