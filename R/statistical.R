#' @title Perform statistical tests on set intersections
#' @description Performs statistical tests to evaluate the significance of set intersections
#' @param object A Venn object
#' @param universe Size of the universe for hypergeometric test (default: NULL, will use the union of all sets)
#' @param method Statistical method to use: "hypergeometric" or "permutation" (default: "hypergeometric")
#' @param nperm Number of permutations if method="permutation" (default: 1000)
#' @param adjust.method Method for multiple testing correction (default: "BH")
#' @param include_singles Logical: include tests for single sets? (default: FALSE)
#' @return A data.frame with statistical test results
#' @importFrom stats phyper p.adjust
#' @importFrom methods is slot
#' @author Kai Guo
#' @export
#' @examples
#' # Create a Venn object
#' A <- sample(1:1000, 100, replace = FALSE)
#' B <- sample(1:1000, 150, replace = FALSE)
#' C <- sample(1:1000, 120, replace = FALSE)
#' res <- venndetail(list(A = A, B = B, C = C))
#'
#' # Perform statistical tests
#' stats <- vennStats(res)
#'
#' # With custom universe size
#' stats <- vennStats(res, universe = 2000)
#'
#' # Using permutation test
#' stats <- vennStats(res, method = "permutation", nperm = 500)
setMethod("vennStats", signature = (object="Venn"), function(object,
                                                             universe = NULL,
                                                             method = c("hypergeometric", "permutation"),
                                                             nperm = 1000,
                                                             adjust.method = "BH",
                                                             include_singles = FALSE) {
  # Get the method
  method <- match.arg(method)

  # Get set details
  set_sizes <- object@raw
  detail <- object@detail
  subset_names <- names(detail)

  # Determine universe size if not provided
  if (is.null(universe)) {
    universe <- length(unique(unlist(object@input)))
  }

  # Initialize results data frame
  results <- data.frame(
    Subset = subset_names,
    Size = as.numeric(detail),
    Expected = numeric(length(subset_names)),
    Fold_Enrichment = numeric(length(subset_names)),
    P_Value = numeric(length(subset_names)),
    Adjusted_P_Value = numeric(length(subset_names)),
    stringsAsFactors = FALSE
  )

  # Calculate statistics for each subset
  for (i in seq_along(subset_names)) {
    subset <- subset_names[i]
    size <- detail[i]

    # Identify which sets are in this subset
    if (subset == "Shared") {
      # All sets are included in "Shared"
      included_sets <- object@GroupNames
    } else {
      # Parse subset name to identify included sets
      included_sets <- strsplit(subset, object@sep)[[1]]
    }

    # Skip single sets if not requested
    if (!include_singles && length(included_sets) == 1) {
      results$Expected[i] <- NA
      results$Fold_Enrichment[i] <- NA
      results$P_Value[i] <- NA
      next
    }

    # For disjoint subsets, we need to account for exclusions
    excluded_sets <- setdiff(object@GroupNames, included_sets)

    if (method == "hypergeometric") {
      # Calculate expected overlap
      # For n sets with sizes s1, s2, ..., sn and universe size U,
      # the expected overlap by chance is (s1 * s2 * ... * sn) / U^(n-1)
      expected <- prod(set_sizes[included_sets]) / (universe ^ (length(included_sets) - 1))
      if (length(excluded_sets) > 0) {
        # Adjust for excluded sets
        expected <- expected * prod((universe - set_sizes[excluded_sets]) / universe)
      }

      # Fold enrichment (observed / expected)
      fold_enrichment <- ifelse(expected > 0, size / expected, NA)

      # P-value calculation using hypergeometric test
      if (length(included_sets) >= 2) {
        # Get the first set size
        white_balls <- set_sizes[included_sets[1]]
        # Calculate total balls in the universe
        total_balls <- universe
        # Calculate the probability adjustment for other sets
        prob_adjustment <- prod(set_sizes[included_sets[-1]] / universe)
        # Calculate expected black balls
        black_balls <- total_balls - white_balls

        # Calculate the p-value (probability of seeing this many or more shared elements)
        if (size > 0) {
          p_value <- phyper(size - 1, white_balls, black_balls,
                            white_balls * prob_adjustment,
                            lower.tail = FALSE)
        } else {
          p_value <- 1
        }
      } else {
        # For single-set subsets, p-value is not meaningful
        p_value <- NA
        fold_enrichment <- NA
      }
    } else if (method == "permutation") {
      # Permutation test - more accurate but slower

      # Create a function to calculate intersection size under the null hypothesis
      calculateRandomIntersectionSize <- function() {
        # Create random sets of the same sizes
        random_sets <- lapply(set_sizes, function(n) {
          sample(1:universe, n, replace = FALSE)
        })

        # Calculate the intersection
        intersection <- Reduce(intersect, random_sets[included_sets])

        # For disjoint subsets, we need to remove elements in excluded sets
        if (length(excluded_sets) > 0) {
          excluded_elements <- Reduce(union, random_sets[excluded_sets])
          intersection <- setdiff(intersection, excluded_elements)
        }

        return(length(intersection))
      }

      # Run permutation test
      permutation_sizes <- replicate(nperm, calculateRandomIntersectionSize())

      # Calculate expected size and p-value
      expected <- mean(permutation_sizes)
      fold_enrichment <- ifelse(expected > 0, size / expected, NA)
      p_value <- sum(permutation_sizes >= size) / nperm
    }

    # Store results
    results$Expected[i] <- expected
    results$Fold_Enrichment[i] <- fold_enrichment
    results$P_Value[i] <- p_value
  }

  # Remove NA p-values before adjusting
  valid_indices <- !is.na(results$P_Value)
  if (sum(valid_indices) > 0) {
    adjusted_p <- rep(NA, nrow(results))
    adjusted_p[valid_indices] <- p.adjust(results$P_Value[valid_indices], method = adjust.method)
    results$Adjusted_P_Value <- adjusted_p
  }

  # Sort by p-value (excluding NAs)
  results <- results[order(results$P_Value, na.last = TRUE), ]

  return(results)
})

#' @title Perform enrichment analysis on set intersections
#' @description Performs enrichment analysis to identify overrepresented categories in set intersections
#' @param object A Venn object
#' @param annotation A data frame with annotation data (e.g., Gene Ontology terms)
#' @param id_col Column in the annotation data frame containing identifiers matching elements in the Venn object
#' @param category_col Column in the annotation data frame containing category information
#' @param subsets Character vector of subset names to analyze (default: NULL, all subsets)
#' @param min_overlap Minimum number of elements a category must share with a subset (default: 3)
#' @param adjust.method Method for multiple testing correction (default: "BH")
#' @param sig_threshold Significance threshold for p-values (default: 0.05)
#' @return A data.frame with enrichment analysis results
#' @importFrom stats phyper p.adjust
#' @author Kai Guo
#' @export
#' @examples
#' # Create a Venn object with gene sets
#' A <- sample(1:1000, 100, replace = FALSE)
#' B <- sample(1:1000, 150, replace = FALSE)
#' C <- sample(1:1000, 120, replace = FALSE)
#' res <- venndetail(list(A = A, B = B, C = C))
#'
#' # Create simulated annotation data
#' gene_ids <- 1:1000
#' categories <- sample(c("Category1", "Category2", "Category3", "Category4"),
#'                      1000, replace = TRUE)
#' anno <- data.frame(GeneID = gene_ids, Category = categories)
#'
#' # Perform enrichment analysis
#' enrichment <- vennEnrichment(res, anno, "GeneID", "Category")
vennEnrichment <- function(object, annotation, id_col, category_col,
                           subsets = NULL, min_overlap = 3,
                           adjust.method = "BH", sig_threshold = 0.05) {
  # Validate inputs
  if (!is(object, "Venn")) {
    stop("object must be a Venn object")
  }

  if (!id_col %in% colnames(annotation)) {
    stop("id_col not found in annotation data frame")
  }

  if (!category_col %in% colnames(annotation)) {
    stop("category_col not found in annotation data frame")
  }

  # Get subset elements
  if (is.null(subsets)) {
    # Use all subsets
    subsets <- names(object@detail)
  } else {
    # Validate subset names
    invalid_subsets <- setdiff(subsets, names(object@detail))
    if (length(invalid_subsets) > 0) {
      stop("Invalid subset names: ", paste(invalid_subsets, collapse = ", "))
    }
  }

  # Extract element details for each subset
  subset_elements <- lapply(subsets, function(subset) {
    elements <- getSet(object, subset = subset)$Detail
    return(elements)
  })
  names(subset_elements) <- subsets

  # Get all unique categories
  all_categories <- unique(annotation[[category_col]])

  # Get all unique elements
  all_elements <- unique(annotation[[id_col]])

  # Calculate background size (total number of annotated elements)
  background_size <- length(all_elements)

  # Get elements for each category
  category_elements <- lapply(all_categories, function(cat) {
    annotation[[id_col]][annotation[[category_col]] == cat]
  })
  names(category_elements) <- all_categories

  # Initialize results list
  all_results <- list()

  # Analyze each subset
  for (subset_name in subsets) {
    subset_elems <- subset_elements[[subset_name]]
    subset_size <- length(subset_elems)

    # Skip empty subsets
    if (subset_size == 0) {
      next
    }

    # Initialize results for this subset
    subset_results <- data.frame(
      Subset = character(),
      Category = character(),
      Subset_Size = integer(),
      Category_Size = integer(),
      Overlap = integer(),
      Expected = numeric(),
      Fold_Enrichment = numeric(),
      P_Value = numeric(),
      stringsAsFactors = FALSE
    )

    # Analyze each category
    for (category in all_categories) {
      cat_elems <- category_elements[[category]]
      cat_size <- length(cat_elems)

      # Skip categories with too few elements
      if (cat_size < min_overlap) {
        next
      }

      # Calculate overlap
      overlap <- length(intersect(subset_elems, cat_elems))

      # Skip categories with too little overlap
      if (overlap < min_overlap) {
        next
      }

      # Calculate expected overlap by chance
      expected <- subset_size * (cat_size / background_size)

      # Calculate fold enrichment
      fold_enrichment <- overlap / expected

      # Calculate p-value using hypergeometric test
      # Here we're testing for the probability of seeing this many or more elements
      # from the category in our subset
      p_value <- phyper(
        overlap - 1,           # At least this many overlapping elements
        cat_size,              # Number of elements in the category
        background_size - cat_size, # Number of elements not in the category
        subset_size,           # Size of our subset
        lower.tail = FALSE     # P(X >= overlap)
      )

      # Add results
      subset_results <- rbind(subset_results, data.frame(
        Subset = subset_name,
        Category = category,
        Subset_Size = subset_size,
        Category_Size = cat_size,
        Overlap = overlap,
        Expected = expected,
        Fold_Enrichment = fold_enrichment,
        P_Value = p_value,
        stringsAsFactors = FALSE
      ))
    }

    # Adjust p-values for multiple testing
    if (nrow(subset_results) > 0) {
      subset_results$Adjusted_P_Value <- p.adjust(subset_results$P_Value, method = adjust.method)
      # Sort by p-value
      subset_results <- subset_results[order(subset_results$P_Value), ]
      # Add significance flag
      subset_results$Significant <- subset_results$Adjusted_P_Value < sig_threshold

      # Add to all results
      all_results[[subset_name]] <- subset_results
    }
  }

  # Combine all results
  if (length(all_results) > 0) {
    combined_results <- do.call(rbind, all_results)
    rownames(combined_results) <- NULL
    return(combined_results)
  } else {
    return(data.frame(
      Subset = character(),
      Category = character(),
      Subset_Size = integer(),
      Category_Size = integer(),
      Overlap = integer(),
      Expected = numeric(),
      Fold_Enrichment = numeric(),
      P_Value = numeric(),
      Adjusted_P_Value = numeric(),
      Significant = logical(),
      stringsAsFactors = FALSE
    ))
  }
}
