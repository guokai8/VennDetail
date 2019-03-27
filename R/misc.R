##' @importFrom utils head
##' @method head Venn
##' @export
head.Venn <- function(x, n=6L, ...){
    head(x@result, n, ...)
}
##' @importFrom utils tail
##' @method tail Venn
##' @export
tail.Venn <- function(x, n=6L, ...){
    tail(x@result, n, ...)
}
##' @method dim Venn
##' @export
dim.Venn <- function(x) {
    dim(x@result)
}
##' @method [ Venn
##' @export
`[.Venn` <- function(x,  i,  j) {
    x@result[i, j]
}
##' @method $ Venn
##' @export
`$.Venn` <- function(x,  name) {
    x@result[,  name]
}
##' @method result Venn
##' @title Extract the result from venn object
##' @description Result will return output in a table format including the
##' contents of the sets included in the venndetail object
##' @rdname result
##' @examples
##' A <- sample(1:100, 40, replace = FALSE);
##' B <- sample(1:100, 60, replace = FALSE);
##' C <- sample(1:100, 40, replace = FALSE);
##' res <- venndetail(list(A = A, B = B, C = C))
##' result <- result(res)
##' @export
setMethod("result", signature = (object="Venn"), function(object, wide=FALSE){
  if(wide == TRUE){
    dd <- as.data.frame(object@wide)
  }else{
    dd <- as.data.frame(object@result)
  }
  return(dd)
})
##' @method detail Venn
##' @title Detail function provides a way to display the amount of members in
##' each group
##' @description  The objective of this function is to summarizes the overlaps
##' across groups identified by venndetail without creating diagram.
##' @rdname detail
##' @examples
##' A <- sample(1:100,  40,  replace = FALSE);
##' B <- sample(1:100,  60,  replace = FALSE);
##' C <- sample(1:100,  40,  replace = FALSE);
##' res <- venndetail(list(A = A, B = B, C = C))
##' detail(res)
##' @export
setMethod("detail", signature = (object="Venn"), function(object){
    object@detail
})
##' @method plot Venn
##' @title Plot Venn object
##' @description The plot function allows users to graphically display the
##' groups and overlap between groups in their venn class object through a
##' variety of graph types such as a bar plot,  traditional venn,
##' or venn pie chart.
##' @rdname plot
##' @return different type of graphics based on user chose
##' @importFrom VennDiagram venn.diagram
##' @importFrom UpSetR upset
##' @param x Venn object
##' @param cex A numerical value giving the text size for venndiagram
##' @param margin Number giving the amount of whitespace around the diagram in
##' grid units
##' @param type Use venn, vennpie or upset (default: venn)
##' @param filename Filename for output figure.
##' @param col Character vector giving the color of the circles.
##' @param mycol Character vector giving the filled color for
##' VennDiagram circles.
##' @param cat.cex Numeric vector giving the size of the category names.
##' @param alpha A number giving the transparency value.
##' @param cat.fontface A character giving the fontface (font style) for
##' category name.
##' @param abbr Boolean indicating whether to abbreviate set names
##' (default: FALSE).
##' @param minlength Minmal length for the set name.
##' @param text.scale Numeric vector of text sizes for upset diagram
##' (ylab, yaxis, xlab, set name, xaxis, insection).
##' @param abbr.method a character string specifying the method used.
##' Partial matches allowed. (default: both side).
##' @param piecolor Character vector giving the colors of the sets(vennpie).
##' @param revcolor Character giving the color for the non-selected
##' sets(vennpie).
##' @param show.number Boolean indicating whether to display the
##' element numbers of the sets or not (default: TRUE)(vennpie).
##' @param log Boolean indicating whether to transform the data in
##' log scale(vennpie).
##' @param base Base value for log transformation(vennpie).
##' @param sep Character string used to separate the terms when concatenating
##' group names into new column names (colnames)(vennpie).
##' @param percentage Boolean indicating whether to display set percentages
##' (default: FALSE)(vennpie).
##' @param show.x Boolean indicating whether to show set labels outside the
##' circle (default: TRUE)(vennpie).
##' @param any Number to indicate selected sets,  such as 1 means any unique
##' sets,  2 means any sets shared by two groups(vennpie).
##' @param sets.x.label x-axis label (upset)
##' @param mainbar.y.label y-axis label (upset)
##' @param nintersects Number of intersections to plot. If set to NA,  all
##' intersections will be plotted.
##' @param ... further arguments passed to or from other methods
##' @inheritParams UpSetR::upset
##' @examples
##' A <- sample(1:100, 40, replace = FALSE);
##' B <- sample(1:100, 60, replace = FALSE);
##' C <- sample(1:100, 40, replace = FALSE);
##' res <- venndetail(list(A = A, B = B, C = C))
##' plot(res, type = "venn")
##' @export
##' @author Kai Guo
plot.Venn <- function(x, type = "venn", col = "black", sep = "_",
                    mycol = c("dodgerblue", "goldenrod1", "darkorange1",
                    "seagreen3", "orchid3"),cat.cex = 1.5, alpha = 0.5, cex = 2,
                    cat.fontface = "bold",
                    margin = 0.05, text.scale = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
                    filename = NULL, piecolor = NULL, revcolor = "lightgrey",
                    any = NULL, show.number = TRUE, show.x = TRUE, log = FALSE,
                    base = NULL, percentage = FALSE, sets.x.label = "Set Size",
                    mainbar.y.label = "Intersection Size", nintersects = 40,
                    abbr= FALSE, abbr.method = "both.sides", minlength = 3, ...)
{
    result <- x
    x <- x@input
    if(type == "venn"&&length(x) <= 5){
    #require(VennDiagram)
        n=length(x)
        p<-venn.diagram(x, filename = filename,
                    col = col,
                    fill = mycol[seq_len(n)],
                    alpha = alpha,
                    cex = cex,
                    cat.col = mycol[seq_len(n)],
                    cat.cex = cat.cex,
                    cat.fontface = cat.fontface,
                    #cat.pos=cat.pos,
                    #cat.dist=cat.dist,
                    margin = margin)
        grid.draw(p)
        rfile = list.files(pattern = "*.log")
        file.remove(rfile)
    }
    if(length(x) > 5 & type != "upset"){
      type = "vennpie"
    }
    if(type == "vennpie"){
        print(vennpie(result, sep = sep, color = piecolor, revcolor = revcolor,
        any = any, show.number = show.number, show.x = show.x, log = log,
        base = base, percentage = percentage))
    }
    if(type == "upset"){
        if(length(x) <= 5){
            upset(fromList(x),  nsets = length(x), sets.x.label = sets.x.label,
            mainbar.y.label = mainbar.y.label, nintersects = nintersects,
            point.size = 5, sets.bar.color = mycol[seq_along(x)],
            text.scale = text.scale)
        }else{
            upset(fromList(x),  nsets = length(x), sets.x.label = sets.x.label,
            mainbar.y.label = mainbar.y.label, nintersects = nintersects,
            point.size = 5, sets.bar.color = setcolor(length(x)),
            text.scale = text.scale)
        }
    }

}
##' @importFrom magrittr %>%
##' @importFrom dplyr select_
##' @importFrom dplyr everything
##' @param x data frame
.add_colnames <- function(x){
    if(sum(grepl("RowNxyz", colnames(x))) == 0){
        x$RowNxyz <- rownames(x)
    }else{
        colnames(x)[which(grepl("RowNxyz", colnames(x)))] <- "RowNxyz"
    }
    return(x%>%select_("RowNxyz", 'everything()'))
}

.pasten <- function(x, name, sep="_"){
    colnames(x) <- paste(name, colnames(x), sep=sep)
    return(x)
}
.setrownames <- function(x, gin){
    if(is.character(gin)){
        ind = which(colnames(x)==gin)
    }else{
        ind = gin
    }
    colnames(x)[ind] <- "RowNxyz"
    return(x)
}
##' @method merge Venn
##' @title Merge two or more venndetail obejcts
##' @description Merge will combine multiple venn diagrams to allow comparison
##' between multiple groups
##' @rdname merge
##' @importFrom purrr flatten
##' @importFrom methods slot
##' @param x Venn object
##' @param y Venn object
##' @param ignore.case Boolean indicating whether to ignore case of
##' group names (default: FALSE)
##' @param useupper Boolean indicating whether to use uppercases for
##' group names (default: TRUE)
##' @param plot Boolean indicating whether to plot figure or not
##' (default: FALSE)
##' @param ... arguments for venndetail
##' @return venn object
##' @examples
##' A <- sample(1:100,  40,  replace = FALSE);
##' B <- sample(1:100,  60,  replace = FALSE);
##' C <- sample(1:100,  40,  replace = FALSE);
##' res1 <- venndetail(list(A = A,  B = B))
##' res2 <- venndetail(list(A = A,  C = C))
##' res <- merge(res1,  res2)
##' @export
merge.Venn <- function(x, y, ignore.case = FALSE,
                            useupper = TRUE, plot = FALSE, ...){
    object = list(x, y)
    input = lapply(object,  function(x)slot(x, "input"))
    input = flatten(input)
    nam = names(input)
    if(ignore.case==TRUE){
        if(useupper==TRUE){
            nam = toupper(nam)
            names(input) <- nam
        }else{
            nam = tolower(nam)
            names(input) <- nam
        }
    }
    input = input[unique(nam)]
    ven = venndetail(input,  ...)
    return(ven)
}
##'
setAs(from = "data.frame",  to = "Venn",  def=function(from){
    Group = from$Group
    Detail = from$Detail
    GroupNames = vector()
    raw = vector()
    input = data.frame()
    sep = character()
    detail = as.vector(table(from$Group))
    names(detail) = names(table(Group))
    result = data.frame(Group, Detail)
    new("venn",
        input = input,
        raw = raw,
        sep = sep,
        GroupNames = GroupNames,
        result = result,
        detail = detail)
})
##'
setAs(from = "list",  to = "Venn",  def = function(from){
    Group = from$Group
    Detail = from$Detail
    GroupNames = from$GroupName
    raw = from$raw
    input = data.frame()
    sep = character()
    detail = as.vector(table(Group))
    names(detail) = names(table(Group))
    result = data.frame(Group, Detail)
    new("venn",
        input = input,
        raw = raw,
        sep = sep,
        GroupNames = GroupNames,
        result = result,
        detail = detail)
})
##' @title Join data.frame based on rownames
##' @description join two dataframes by rownames
##' @rdname rowjoin
##' @method rowname join
##' @importFrom dplyr full_join
##' @importFrom dplyr left_join
##' @importFrom dplyr right_join
##' @importFrom dplyr anti_join
##' @importFrom dplyr semi_join
##' @param x data.frame x
##' @param y data.frame y
##' @param fun Different join format: left_join, full_join, right_join
##' (default:full_join)
##' @return dataframe with join results
##' @examples
##' library(dplyr)
##' A <- sample(1:100,  40,  replace = FALSE);
##' B <- sample(1:100,  60,  replace = FALSE);
##' dA <- data.frame(A = A, "FC" = rnorm(40))
##' dB <- data.frame(B = B, "FC" = rnorm(60))
##' rownames(dA) <- A
##' rownames(dB) <- B
##' rowjoin(dA, dB)
##' @export
##' @author Kai Guo
setMethod("rowjoin", signature(x = "data.frame", y = "data.frame"),
        function(x, y, fun = "full_join"){
    x <- .add_colnames(x)
    y <- .add_colnames(y)
    f = match.fun(fun)
    return(f(x, y, by = c("RowNxyz" = "RowNxyz")))
})
##' @name setcolor
##' @title return colors with given a vector
##' @description Setcolor will provide a list of color vectors based on
##' the number used as an input.
##' @importFrom grDevices colors
##' @param x Number of color
##' @return color vector
##' @examples
##' mycol <- setcolor(10)
##' mycol
##' @export
##' @author Kai Guo
setcolor<-function(x){
    mycolor =c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
            "#1B9E77", "brown", "#7570B3", "#E7298A", "#7FC97F", "#A6761D",
            "#BEAED4", "#FDC086", "#FFFF99", "chartreuse1", "cyan3", "purple",
            "pink4", "cyan", "royalblue", "violet", "springgreen2", "gold3",
            "darkseagreen4", "#E5D8BD",
            "#00AFBB", "#FC4E07", "#9999FF", "#FF9326",
            "#984EA3", "#F781BF", "#B3B3B3",
            "#CCCCCC", "#666666", "#01665E", "#542788")
    if(x < length(mycolor)){
        res <- mycolor[seq_len(x)]
    }else{
        res <- c(mycolor, sample(colors(), x, replace = FALSE))
    }
    return(res)
}
