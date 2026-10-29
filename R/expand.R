#' Expand a scenario into a table of single-value runs
#'
#' @description Converts vectors in a `movesr_scenario` into a data.frame where
#' each row is one executable scenario. This is the input to
#' \code{\link{movesr_run_grid}}.
#' @param scenario A `movesr_scenario`.
#' @return A data.frame with one row per combination of supplied values.
#' @export
movesr_expand_scenario <- function(scenario) {
  if (!inherits(scenario, "movesr_scenario")) {
    stop("scenario must be created by movesr_scenario()")
  }
  fields <- lapply(scenario, function(x) unique(x))
  fields <- fields[!vapply(fields, is.null, logical(1))]
  out <- expand.grid(fields, KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE)
  out
}
