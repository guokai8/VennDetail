##' Class 'Venn'
##' This class includes all information from venndetail
##' @name Venn-class
##' @aliases Venn-class
##' @docType class
##' @slot input orginal input datasets
##' @slot raw summary of the input datasets
##' @slot sep separation character
##' @slot GroupNames input group names
##' @slot result shared or unique sets
##' @slot detail shared of unique number belongs to each sets
##' @slot wide result in wide format
##' @exportClass Venn
##' @author Kai Guo
##' @keywords classes
setClass("Venn", representation = representation(input="list", raw="vector",
                sep="character", GroupNames="vector", result="data.frame",
                detail="vector", wide="data.frame"))
