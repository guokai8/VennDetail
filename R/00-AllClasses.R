##' Class 'venn'
##' This class include all information from venndetail
##' @name venn-class
##' @aliases venn-class
##'    plot, venn-method
##'    summary, venn-method
##' @docType class
##' @slot GroupNames group names input
##' @slot result intersect and difference for groups
##' @slot detail number belongs to each group
##' @exportClass venn
##' @author Kai Guo
##' @keywords classes
setClass("venn",
         representation = representation(
           raw="vector",
           GroupNames="vector",
           result="data.frame",
           detail="vector"))

