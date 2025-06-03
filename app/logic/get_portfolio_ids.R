box::use(
  SMAManager[registries],
  stats[setNames],
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
  setNames(portfolios, portfolio_ids)
}

#' @export
derived_portfolios <- function() {
  portfolios <- portfolios()
  is_derived <- sapply(portfolios, inherits, "SMA")
  portfolios[is_derived]
}

#' @export
derived_portfolio_ids <- function() {
  setNames(sapply(derived_portfolios(), \(x) x$get_short_name()), NULL)
}

#' @export
base_portfolios <- function() {
  portfolios <- portfolios()
  is_derived <- sapply(portfolios, inherits, "SMA")
  portfolios[!is_derived]
}

#' @export
base_portfolio_ids <- function() {
  setNames(sapply(base_portfolios(), \(x) x$get_short_name()), NULL)
}
