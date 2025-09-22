box::use(
  SMAManager[create_portfolio_from_enfusion],
)

#' @export
load_ccmf <- function() {
  create_portfolio_from_enfusion(
    long_name    = "Callodine Capital Master Fund",
    short_name   = "ccmf",
    holdings_url = paste0(
      "https://webservices.enfusionsystems.com/mobile/",
      "rest/reportservice/exportReport?",
      "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
      "CCMF+-+Positions.ppr"
    )
  )
  invisible(NULL)
}
