##' Class 'venn'
##' This class includes all information from venndetail
##' @name venn-class
##' @aliases venn-class
##'    plot, venn-method
##'    summary, venn-method
##' @docType class
##' @slot input orginal input datasets
##' @slot raw summary of the input datasets
##' @slot sep separation character
##' @slot GroupNames input group names
##' @slot result shared or unique sets
##' @slot detail shared of unique number belongs to each sets
##' @exportClass venn
##' @author Kai Guo
##' @keywords classes
setClass("venn",representation = representation(input="list",raw="vector",
                sep="character",GroupNames="vector",result="data.frame",
                detail="vector"))
