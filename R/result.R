#' @title Extract results from a Venn object
#' @description Retrieves results from a Venn object in long or wide format
#' @param object A Venn object
#' @param wide Logical: should results be returned in wide format? Default: FALSE
#' @return A data.frame containing subset information
#' @importFrom methods is slot
#' @author Kai Guo
#' @export
#' @examples
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' res <- venndetail(list(A = A, B = B, C = C))
#' # Get results in long format
#' result_long <- result(res)
#' # Get results in wide format
#' result_wide <- result(res, wide = TRUE)
setMethod("result", signature = (object="Venn"), function(object, wide = FALSE) {
  if (isTRUE(wide)) {
    return(object@wide)
  } else {
    return(object@result)
  }
})

#' @title Get subset details from a Venn object
#' @description Returns a named numeric vector with counts for each subset
#' @param object A Venn object
#' @return A named numeric vector with counts for each subset
#' @author Kai Guo
#' @export
#' @examples
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' res <- venndetail(list(A = A, B = B, C = C))
#' detail(res)
setMethod("detail", signature = (object="Venn"), function(object) {
  return(object@detail)
})
