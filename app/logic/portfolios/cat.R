box::use(
  SMAManager[.security, .sma_rule, create_sma_from_enfusion],
)

#' @export
load_caty <- function() {
  caty <- create_sma_from_enfusion(
    long_name = "Catenary SMA",
    short_name = "caty",
    base_portfolio = "ccmf",
    holdings_url = paste0(
      "https://webservices.enfusionsystems.com/mobile/",
      "rest/reportservice/exportReport?",
      "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
      "CAT+Consolidated+Position+Listing+-+Options.ppr"
    ),
    trade_url = paste0(
      "https://webservices.enfusionsystems.com/mobile/",
      "rest/reportservice/exportReport?",
      "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
      "CAT_Trade_Detail.trb"
    )
  )

  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Gross Exposure Under 250% of NAV",
    scope = "portfolio",
    bbfields = NULL,
    definition = function(security_id, portfolio) {
      security <- lapply(security_id, function(id) .security(id))
      price <- vapply(security, function(x) x$get_price(), numeric(1))
      und_p <- vapply(security, function(x) x$get_underlying_price(), numeric(1))
      type <- vapply(security, \(x) x$get_instrument_type(), character(1))
      price[type == "Option"] <- und_p[type == "Option"]
      delta <- vapply(security, function(x) x$get_delta(), numeric(1))
      nav <- portfolio$get_nav()
      exp <- (delta * price) / nav
      setNames(exp, security_id)
    },
    swap_only = FALSE,
    max_threshold = 2.5,
    min_threshold = 0,
    gross_exposure = TRUE
  ))

  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Net Exposure +/- 50% of NAV",
    scope = "portfolio",
    bbfields = NULL,
    definition = function(security_id, portfolio) {
      security <- lapply(security_id, function(id) .security(id))
      price <- vapply(security, function(x) x$get_price(), numeric(1))
      und_p <- vapply(security, function(x) x$get_underlying_price(), numeric(1))
      type <- vapply(security, \(x) x$get_instrument_type(), character(1))
      price[type == "Option"] <- und_p[type == "Option"]
      delta <- vapply(security, function(x) x$get_delta(), numeric(1))
      nav <- portfolio$get_nav()
      exp <- (delta * price) / nav
      setNames(exp, security_id)
    },
    swap_only = FALSE,
    max_threshold = 0.5,
    min_threshold = -0.5,
    gross_exposure = FALSE
  ))

  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Gross Position under 12% of NAV",
    scope = "position",
    bbfields = c(NULL),
    definition = function(security_id, sma) {
      nav <- sma$get_nav()
      price <- vapply(security_id, \(id) .security(id)$get_price(), numeric(1))
      price / nav
    },
    swap_only = FALSE,
    max_threshold = 0.12,
    min_threshold = -0.12
  ))

  caty$add_rule(SMAManager::.sma_rule(
    sma_name = "caty",
    rule_name = "liquidity",
    scope = "position",
    bbfields = c("HS013"),
    definition = function(security_id, sma) {
      volume <- sapply(security_id, \(id) .security(id)$get_rule_data("HS013"))
      1 / volume
    },
    max_threshold = 1,
    min_threshold = -1
  ))

  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Positon under 2.5% of Shares Outstanding",
    scope = "position",
    bbfields = c("DS381"),
    definition = function(security_id, sma) {
      sec_type <- vapply(security_id, \(id) .security(id)$get_instrument_type(), character(1)) #nolint
      shares_out <- sapply(security_id, \(id) .security(id)$get_rule_data("DS381")) #nolint
      dplyr::case_when(
        sec_type == "Equity" ~ 1 / shares_out,
        TRUE ~ 0
      )
    },
    swap_only = FALSE,
    max_threshold = 0.025,
    min_threshold = -Inf
  ))

  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Non-US Equity on Swap Only",
    scope = "position",
    bbfields = "DS290",
    definition = function(security_id, sma) {
      permitted_countries <- c("US")
      iss_cty <- sapply(security_id, \(id) .security(id)$get_rule_data("DS290"))
      !iss_cty %in% permitted_countries
    },
    swap_only = TRUE
  ))

  caty$add_rule(SMAManager::.sma_rule(
    sma_name = "caty",
    rule_name = "no mlps except on swap",
    scope = "position",
    bbfields = "DS213",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS213"))
      sec_typ == "MLP"
    },
    swap_only = TRUE
  ))

  caty$add_rule(SMAManager::.sma_rule(
    sma_name = "caty",
    rule_name = "no partnerships except on swap",
    scope = "position",
    bbfields = "DS674",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS674"))
      sec_typ == "Partnership Shares"
    },
    swap_only = TRUE
  ))

  invisible(caty)
}