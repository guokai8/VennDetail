##' @param x venn object
##' @return return dataframe and print header of dataframe
##' @export
##' @author Kai Guo
result<-function(x){
  UseMethod("result",x)
}
##' @param x venn object
##' @return Vector including group names and total member amounts for each group
##' @export
##' @author Kai Guo
detail<-function(x){
  UseMethod("detail",x)
}
##' @name dplot
##' @rdname dplot
##' @title The dplot function allows users to visualize the detail function in the form of a barplot
##' @description The amount of members within each group determined by venndetail will be displayed as a bar plot.
##' This will include all groups such as shared, pairwise, and unique. By default the order of the bars will be ascending.
##' This can be adjusted by the user using the order argument. The text size argument will allow users to change
##' the size of the numbers above bars indicating the total members within each group.
##' @return Produces a bar plot displaying the total counts within each group
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
##' @name rowjoin
##' @param x dataframe x
##' @param y dataframe y
##' @param fun join type
##' @export
##' @author Kai Guo
setGeneric("rowjoin",function(x,y,...){
  standardGeneric("rowjoin")
})
##' @name getFeature
##' @rdname getFeature
##' @export
##' @author Kai Guo
setGeneric("getFeature",function(object,group, rlist, userowname=TRUE,gind=NULL,sep="_",...){
  standardGeneric("getFeature")
})
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
