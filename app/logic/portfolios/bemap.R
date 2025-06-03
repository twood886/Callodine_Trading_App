box::use(
  Rblpapi[bdp, blpConnect],
)

#' @export
load_bemap <- function() {
  blpConnect()
  bemap <- SMAManager::create_sma_from_enfusion(
    long_name = "Citco Bank Canada Ref BEMAP Master Fund Ltd.",
    short_name = "bemap",
    base_portfolio = "ccmf",
    holdings_url = paste0(
      "https://webservices.enfusionsystems.com/mobile/",
      "rest/reportservice/exportReport?",
      "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
      "BEMAP+Consolidated+Position+Listing+-+Options.ppr"
    ),
    trade_url = paste0(
      "https://webservices.enfusionsystems.com/mobile/",
      "rest/reportservice/exportReport?",
      "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
      "BEMAP_Trade_Detail.trb"
    )
  )

  # Create bemap rules
  bemap$add_rule(SMAManager::.sma_rule(
    sma_name = "bemap",
    rule_name = "position under 4.99% shares outstanding",
    scope = "position",
    definition = function(security_id, sma) {
      sec_type <- bdp(security_id, "EX028")$EX028
      dplyr::case_when(
        sec_type == "Equity" ~ 1 / bdp(security_id, "DS381")$DS381,
        TRUE ~ 0
      )
    },
    max_threshold = 0.0499,
    min_threshold = -Inf
  ))

  bemap$add_rule(SMAManager::.sma_rule(
    sma_name = "bemap",
    rule_name = "no mlps except on swap",
    scope = "position",
    definition = function(security_id, sma) {
      bdp(security_id, "DS213")$DS213 == "MLP"
    },
    swap_only = TRUE
  ))

  bemap$add_rule(SMAManager::.sma_rule(
    sma_name = "bemap",
    rule_name = "no partnerships except on swap",
    scope = "position",
    definition = function(security_id, sma) {
      bdp(security_id, "DS674")$DS674 == "Partnership Shares"
    },
    swap_only = TRUE
  ))

  bemap$add_rule(SMAManager::.sma_rule(
    sma_name = "bemap",
    rule_name = "no etps except on swap",
    scope = "position",
    definition = function(security_id, sma) {
      bdp(security_id, "DS213")$DS213 == "ETP"
    },
    swap_only = TRUE
  ))

  bemap$add_rule(SMAManager::.sma_rule(
    sma_name = "bemap",
    rule_name = "no ents except on swap",
    scope = "position",
    definition = function(security_id, sma) {
      bdp(security_id, "DS213")$DS213 == "ETN"
    },
    swap_only = TRUE
  ))

  bemap$add_rule(SMAManager::.sma_rule(
    sma_name = "bemap",
    rule_name = "no bdcs except on swap",
    scope = "position",
    definition = function(security_id, sma) {
      bdp(security_id, "BI005")$BI005 == "BDCs"
    },
    swap_only = TRUE
  ))


  bemap$add_rule(SMAManager::.sma_rule(
    sma_name = "bemap",
    rule_name = "liquidity",
    scope = "position",
    definition = function(security_id, sma) {
      sec_type <- Rblpapi::bdp(security_id, "EX028")$EX028
      dplyr::case_when(
        sec_type == "Equity" ~ 1 / bdp(security_id, "HS020")$HS020,
        TRUE ~ 0
      )
    },
    max_threshold = 1.83,
    min_threshold = -1.83
  ))
  invisible(NULL)
}