box::use(
  SMAManager[registries]
)

#' @export
portfolio_ids <- function() {
  ls(registries$portfolios)
}

#' @export
portfolios <- function() {
  portfolio_ids <- portfolio_ids()
  portfolios <- lapply(
    portfolio_ids,
    \(x) get(x, envir = registries$portfolios)
  )
  names(portfolios) <- portfolio_ids
  portfolios
}

#' @export
derived_portfolios <- function() {
  portfolios <- portfolios()
  is_derived <- sapply(portfolios, inherits, "SMA")
  portfolios[is_derived]
}

#' @export
derived_portfolio_ids <- function() {
  vapply(derived_portfolios(), \(x) x$get_short_name(), character(1))
}

#' @export
base_portfolios <- function() {
  portfolios <- portfolios()
  is_derived <- sapply(portfolios, inherits, "SMA")
  portfolios[!is_derived]
}

#' @export
base_portfolio_ids <- function() {
  ids <- sapply(base_portfolios(), \(x) x$get_id())
  names(ids) <- NULL
  ids
}
