##' @title T2DM
##' @description T2DM data are differential expression genes (DEGs) with
##' annotation from the publication by Hinder et al. The data contains three
##' DEG sets from three different tissues (Cortex,SCN,Glom). DEGs were
##' determined by using Cuffdiff with a false discovery rate (FDR) < 0.05
##' between groups with or without pioglitazone treatment.
##' @format A list of data frame with five columns individually:
##' \describe{
##' \item{Entrez}{Entrez gene IDs}
##' \item{Symbol}{HGNC symbols}
##' \item{Annotation}{Gene function}
##' \item{log2FC}{log2 Fold Change}
##' \item{FDR}{False Discovery Rate}
##' }
##' @examples
##' T2DM
"T2DM"
