## ----install, eval = FALSE----------------------------------------------------
# if (!requireNamespace("BiocManager"))
#     install.packages("BiocManager")
# BiocManager::install("VennDetail")

## ----load, results = 'hide', message = FALSE----------------------------------
library(VennDetail)
data(T2DM)

## ----quick--------------------------------------------------------------------
ven <- venndetail(list(Cortex = T2DM$Cortex$Entrez, SCN = T2DM$SCN$Entrez,
                    Glom = T2DM$Glom$Entrez))

## ----fig1, fig.width = 6, fig.height = 5, fig.align = "center"----------------
##traditional venn diagram
plot(ven)

## ----fig2, fig.width = 6, fig.height = 5, fig.align = "center"----------------
##Venn-Pie format
plot(ven, type = "vennpie")

## ----fig3, fig.width = 6, fig.height = 5, fig.align = "center"----------------
##Upset format
plot(ven, type = "upset")

## ----get----------------------------------------------------------------------
## List the subsets name
detail(ven) 
head(getSet(ven, subset = c("Shared", "SCN")), 10)

## ----result-------------------------------------------------------------------
## long format: the first column lists the subsets name, and the second column
## shows the genes included in the subsets
head(result(ven))
## wide format: the first column lists all the genes, the following columns
## display the groups name (three tissues) and the last column is the total 
## number of the gene shared by groups.
head(result(ven, wide = TRUE))

## ----fig4, fig.width = 6, fig.height = 5, fig.align = "center"----------------
vennpie(ven, any = 1, revcolor = "lightgrey")

## ----fig5, fig.width = 6, fig.height = 5, fig.align = "center"----------------
vennpie(ven, log = TRUE)

## ----fig6, fig.width = 6, fig.height = 5, fig.align = "center"----------------
set.seed(123)
A <- sample(1:1000, 400, replace = FALSE)
B <- sample(1:1000, 600, replace = FALSE)
C <- sample(1:1000, 350, replace = FALSE)
D <- sample(1:1000, 550, replace = FALSE)
E <- sample(1:1000, 450, replace = FALSE)
venn <- venndetail(list(A = A, B = B, C= C, D = D, E = E))
vennpie(venn, min = 4)

## ----getfeature---------------------------------------------------------------
head(getFeature(ven, subset = "Shared", rlist = T2DM))

## ----fig7, fig.width = 6, fig.height = 5, fig.align = "center"----------------
dplot(ven, order = TRUE, textsize = 4)

