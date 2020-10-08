chatty <- function(verbose) {
  if (verbose) {
    function(...) cat(..., sep = "\n")
  } else {
    function(...) invisible(NULL)
  }
}

clean_penguins <- function() {
  keep_vars <- c(
    "species", "sex", "island",
    "bill_length_mm",
    "bill_depth_mm"
  )
  penguins <- palmerpenguins::penguins[, keep_vars]
  names(penguins) <- gsub("_mm", "", keep_vars, fixed = T)
  penguins[rowSums(is.na(penguins)) == 0, ]
}

