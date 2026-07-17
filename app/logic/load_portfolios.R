box::use(
  Rblpapi[blpConnect, defaultConnection],
  replikit[BloombergDataProvider, set_security_data_provider],
  replikitdata[db_connect, get_db_connection, load_all_portfolios_from_db],
)

#' Load every portfolio and SMA (with rules and holdings) from the database.
#'
#' Replaces the per-portfolio Enfusion loaders: portfolios, SMA rules, and
#' holdings are defined in Postgres and hydrated in a fixed number of batched
#' queries. Market data (prices, deltas, rule fields) is refreshed through the
#' Bloomberg provider as the final step of loading, so no separate refresh
#' call is needed afterwards.
#'
#' Requires a running Bloomberg terminal session and a PG_PASSWORD
#' environment variable for the database connection.
#' @export
load_portfolios <- function() {
  # Bloomberg: connect once per R session
  blp <- tryCatch(defaultConnection(), error = function(e) NULL)
  if (is.null(blp)) blpConnect()
  set_security_data_provider(BloombergDataProvider$new())

  # Postgres: reuse the cached connection when it is still valid
  db_ok <- tryCatch({
    get_db_connection()
    TRUE
  }, error = function(e) FALSE)
  if (!db_ok) db_connect()

  load_all_portfolios_from_db()
  invisible(NULL)
}
