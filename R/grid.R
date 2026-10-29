#' Run a grid of scenarios
#'
#' @description Runs `movesr_run()` once per row of a scenario table and
#' combines the resulting factor tables. Tables read from the portable database
#' are cached between rows.
#' @param database A `movesr_database` object or path.
#' @param scenarios A data.frame or list of named scenario values. Each row is
#' converted with `do.call(movesr_scenario, as.list(row))`. A single
#' `movesr_scenario` is also accepted.
#' @param duckdb Optional DuckDB CLI path.
#' @return A list with `rates` (combined data.table), `runs` (one element per
#' scenario), and `scenarios`.
#' @details The grid is executed sequentially. This is the current expansion
#' mechanism; a future engine will share intermediate tables across rows.
#' @export
movesr_run_grid <- function(database, scenarios, duckdb = NULL) {
  if (inherits(scenarios, "movesr_scenario")) {
    grid <- movesr_expand_scenario(scenarios)
    rows <- lapply(seq_len(nrow(grid)), function(i) {
      do.call(movesr_scenario, as.list(grid[i, , drop = FALSE]))
    })
  } else if (is.data.frame(scenarios)) {
    rows <- lapply(seq_len(nrow(scenarios)), function(i) {
      do.call(movesr_scenario, as.list(scenarios[i, , drop = FALSE]))
    })
  } else if (is.list(scenarios)) {
    rows <- lapply(scenarios, function(x) {
      if (inherits(x, "movesr_scenario")) x else do.call(movesr_scenario, x)
    })
  } else {
    stop("scenarios must be a movesr_scenario, data.frame or list")
  }
  if (!length(rows)) stop("No scenarios supplied")
  runs <- lapply(rows, function(s) movesr_run(database, s, duckdb = duckdb))
  rates <- data.table::rbindlist(lapply(runs, `[[`, "rates"), fill = TRUE)
  list(rates = rates, runs = runs, scenarios = rows)
}
