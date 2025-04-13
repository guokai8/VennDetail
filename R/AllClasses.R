#' @title Venn Class
#' @description S4 class to store and manage set intersection data and visualizations
#' @name Venn-class
#' @aliases Venn-class
#' @docType class
#' @slot input A list containing the original input datasets
#' @slot raw A named vector with counts of elements in each input set
#' @slot sep The character used to separate set names in subset labels
#' @slot GroupNames A character vector of input group names
#' @slot result A data.frame containing subset information (subset name and elements)
#' @slot detail A named vector with counts of elements in each subset
#' @slot wide A data.frame with subset information in wide format for easier analysis
#' @slot metadata A list to store additional metadata about the analysis
#' @exportClass Venn
#' @author Kai Guo
#' @examples
#' \dontrun{
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' venn_obj <- venndetail(list(A = A, B = B, C = C))
#' # Access the detail slot
#' venn_obj@detail
#' }
setClass("Venn",
         representation = representation(
           input = "list",         # Original input datasets
           raw = "vector",         # Count of elements in each input set
           sep = "character",      # Separator for set names
           GroupNames = "vector",  # Names of input groups
           result = "data.frame",  # Main result with subsets
           detail = "vector",      # Number of elements in each subset
           wide = "data.frame",    # Results in wide format
           metadata = "list"       # Additional metadata (new)
         ),
         prototype = prototype(
           input = list(),
           raw = numeric(),
           sep = "_",
           GroupNames = character(),
           result = data.frame(),
           detail = numeric(),
           wide = data.frame(),
           metadata = list()
         ),
         validity = function(object) {
           # Validation checks
           if (length(object@GroupNames) == 0) {
             return("GroupNames must not be empty")
           }
           if (length(object@GroupNames) != length(object@input)) {
             return("GroupNames length must match input length")
           }
           if (length(object@raw) != length(object@input)) {
             return("raw vector length must match input length")
           }
           if (length(object@sep) != 1) {
             return("sep must be a single character")
           }
           # All validation passed
           return(TRUE)
         }
)

#' @title Create a new Venn object
#' @description Constructor function for creating Venn objects with validation
#' @param input A list of input sets
#' @param raw A named vector with counts
#' @param sep The separator character
#' @param GroupNames Names of the input groups
#' @param result The result data.frame
#' @param detail The detail vector
#' @param wide The wide-format data.frame
#' @param metadata Additional metadata (optional)
#' @return A new Venn object
#' @author Kai Guo
#' @export
#' @examples
#' \dontrun{
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' raw <- c(A = 40, B = 60)
#' groups <- c("A", "B")
#' # Create a new Venn object manually (normally done by venndetail function)
#' venn_obj <- newVenn(input = list(A = A, B = B), raw = raw,
#'                     GroupNames = groups, ...)
#' }
newVenn <- function(input, raw, sep = "_", GroupNames, result, detail, wide, metadata = list()) {
  obj <- new("Venn",
             input = input,
             raw = raw,
             sep = sep,
             GroupNames = GroupNames,
             result = result,
             detail = detail,
             wide = wide,
             metadata = metadata
  )

  # Validate the object
  validObject(obj)

  return(obj)
}
