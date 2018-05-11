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
##' @param object venn results
##' @param ... addtional parameters
##' @return plot
##' @export
##' @author Kai Guo
setGeneric("dplot",function(object,...){
  standardGeneric("dplot")
})
##' get generic
##'
##' @docType method
##' @name get
##' @rdname get-methods
##' @title get detail
##' @param x venn object
##' @param ... additional parameters
##' @return return dataframe and print the header of dataframe
##' @export
##' @author Kai Guo
setGeneric("get",function(object,...){
  standardGeneric("get")
})
