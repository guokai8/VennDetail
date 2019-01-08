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
##' @rdname dplot
##' @title detail plot
##' @return barplot with number of item in each set
##' @export
##' @author Kai Guo
setGeneric("dplot",function(object,order=FALSE,...){
  standardGeneric("dplot")
})
##' get generic
##'
##' @name get
##' @rdname get
##' @title get detail
##' @return return dataframe and print the header of dataframe
##' @export
##' @author Kai Guo
setGeneric("get",function(object,group,...){
  standardGeneric("get")
})
##' rowjoin generic
##' @name rowjoin
##' @rdname rowjoin
##' @title join data.frame based on rownames
##' @param x dataframe x
##' @param y dataframe y
##' @param fun join type
##' @return dataframe with join results
##' @author Kai Guo
setGeneric("rowjoin",function(x,y,...){
  standardGeneric("rowjoin")
})
##' getFeature generic
##'
##' @name getFeature
##' @rdname getFeature
##' @title get feature based on venn results
##' @return dataframe with all venn information and details from input
##' @export
##' @author Kai Guo
setGeneric("getFeature",function(object,group, rlist, userowname=TRUE,gind=NULL,sep="_",...){
  standardGeneric("getFeature")
})
##' Make vennpie figure
##' @rdname vennpie
##' @name vennpie
##' @title pie plot show shared and unique part
##' @param object venn object
##' @param group set name you want display
##' @param color vector of color you want use for the group
##' @param revcolor backgroup color
##' @param show.number display the element number of the group or not
##' @param log use log transform or not
##' @param base log base
##' @param sep separate for new colnames
##' @param percentage display percentage format or not
##' @return plot
##' @export
##' @author Kai Guo
setGeneric("vennpie",function(object,group=NULL,color=NULL,
                              revcolor="lightgrey",any=NULL,
                              show.number=TRUE,show.x=TRUE,
                              sep="_",log=FALSE,base=NULL,percentage=FALSE,...){
  standardGeneric("vennpie")
})

##' Merge two or more venn object by group name
##'
##' @name merge
##' @rdname merge
##' @title merge two or more venn object
##' @param object list of venn object
##' @param ingore.case ingore case of group name
##' @param useupper use uppercase for all group name
##' @param plot plot figure or not
##' @return venn object
##' @author Kai Guo
setGeneric("merge",function(object,ingore.case=FALSE,useupper=TRUE,plot=FALSE,...){
  standardGeneric("merge")
})
