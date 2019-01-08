##' Class 'venn'
##' This class include all information from venndetail
##' @name venn-class
##' @aliases venn-class
##'    plot, venn-method
##'    summary, venn-method
##' @docType class
##' @slot input orginal input
##' @slot raw summary of the input
##' @slot sep separate delim
##' @slot GroupNames group names input
##' @slot result intersect and difference for groups
##' @slot detail number belongs to each group
##' @exportClass venn
##' @author Kai Guo
##' @keywords classes
setClass("venn",representation = representation(input="list",raw="vector",sep="character",
                                                GroupNames="vector",result="data.frame",detail="vector"))

