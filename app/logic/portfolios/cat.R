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
      "CAT+-+Positions.ppr"
    )
  )

  # 1) Gross Exposure ----------------------------------------------------------
  # The maximum gross market value of positions in the Investment Account shall
  # not exceed 250% of Notional Capital.  
  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Gross Exposure Under 250% of NAV",
    scope = "portfolio",
    bbfields = NULL,
    definition = function(security_id, portfolio) {
      security <- lapply(security_id, function(id) .security(id))
      price <- vapply(security, \(x) x$get_price(), numeric(1))
      nav <- portfolio$get_nav()
      exp <- price / nav
      names(exp) <- security_id
      exp
    },
    swap_only = FALSE,
    max_threshold = 2.5,
    min_threshold = 0,
    gross_exposure = TRUE
  ))

  # 2) Net Exposure ------------------------------------------------------------
  # The maximum net market value exposure (i.e., the value of long positions
  # minus the exposure on open short positions) will be no greater than 50% of
  # the Notional Capital of the Investment Account.
  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Net Exposure +/- 50% of NAV",
    scope = "portfolio",
    bbfields = NULL,
    definition = function(security_id, portfolio) {
      security <- lapply(security_id, \(id) .security(id))
      price <- vapply(security, \(x) x$get_price(), numeric(1))
      nav <- portfolio$get_nav()
      exp <- price / nav
      names(exp) <- security_id
      exp
    },
    swap_only = FALSE,
    max_threshold = 0.5,
    min_threshold = -0.5,
    gross_exposure = FALSE
  ))

  # 3) Beta-weighted Exposure --------------------------------------------------
  # The maximum Beta-weighted (calculated in accordance with CAAM’s customary
  # risk management practices) net market value exposure will be no greater than
  # 40% of the Notional Capital of the Investment Account.
  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Beta-Weighted Exposure < 40% of NAV",
    scope = "portfolio",
    bbfields = c("RK001"),
    definition = function(security_id, portfolio) {
      security <- lapply(security_id, \(id) .security(id))
      price <- vapply(security, \(x) x$get_price(), numeric(1))
      beta <- sapply(security_id, \(id) .security(id)$get_rule_data("RK001"))
      beta[is.na(beta)] <- 1
      nav <- portfolio$get_nav()
      exp <- (price * beta) / nav
      names(exp) <- security_id
      exp
    },
    swap_only = FALSE,
    max_threshold = 0.4,
    min_threshold = -Inf,
    gross_exposure = FALSE
  ))

  # 4) Max positions size - % of NAV -------------------------------------------
  # The maximum gross market value (long or short), inclusive of any
  # appreciation, in any individual positions in the Investment Account in any
  # individual issuer will be no greater than 12% of the Notional Capital of the
  # Investment Account.
  caty$add_rule(SMAManager::.sma_rule(
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

  # 5) Liquidity ---------------------------------------------------------------
  # No security position with a gross market value that is greater than 100% of
  # the arithmetic mean of the daily trading volumes of a security for the
  # immediately preceding 90 business days of such security may be purchased for
  # the Investment Account.
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

  # 6) Max Position size - % of Shares Outstanding -----------------------------
  # Without the prior written consent of Client, Investment Manager will not
  # permit the Investment Account to own more than 2.5% of any class of equity
  # security of an issuer.
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

  # 7) Market Cap --------------------------------------------------------------
  # To the extent the Investment Account invests in publicly traded equity
  # securities, only securities of issuers with individual market
  # capitalizations of US$500 or more, may be purchased for the Investment
  # Account.
  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Market Cap over $500MM",
    scope = "position",
    bbfields = c("RR902"),
    definition = function(security_id, sma) {
      except <- c("www us equity")
      mktcap <- vapply(
        security_id,
        \(id) .security(id)$get_rule_data("RR902"),
        numeric(1)
      )
      dplyr::case_when(
        mktcap < 500e6 & !tolower(security_id) %in% except ~ 1,
        TRUE ~ 0
      )
    },
    max_threshold = 0,
    min_threshold = 0,
    swap_only = FALSE
  ))

  # 8) Limited Markets and Futures ---------------------------------------------
  # Investments in Limited Markets (as set out in Schedule II) and Futures shall
  # be subject to the following limitations determined at the time of
  # investment:
  # a.	not more than 10% of the Notional Capital of the Investment Account
  # Assets may be invested (in the aggregate) in Limited Markets;
  caty$add_rule(SMAManager::.sma_rule(
    sma_name = "caty",
    rule_name = "limited markets",
    scope = "portfolio",
    bbfields = "DS290",
    definition = function(security_id, sma) {
      limited_markets <- c(
        "AU", "AT", "BE", "BM", "CA", "CZ", "DK", "FI", "FR", "DE", "HK", "HU",
        "IE", "IL", "IT", "JP", "LU", "MX", "NL", "NZ", "NO", "PL", "PT", "SG",
        "ZA", "ES", "SE", "CH", "TH", "TR", "GB"
      )
      security <- lapply(security_id, function(id) .security(id))
      price <- vapply(security, \(x) x$get_price(), numeric(1))
      nav <- sma$get_nav()
      exp <- price / nav
      names(exp) <- security_id
      dplyr::case_when(
        security_id %in% limited_markets ~ exp,
        TRUE ~ 0
      )
    },
    swap_only = FALSE,
    max_threshold = 0.1,
    min_threshold = -Inf,
    gross_exposure = TRUE
  ))
  # b.	not more than 10% of the Notional Capital of the Investment Account
  # Assets may be invested (in the aggregate) in Futures; and 
  # c.	CAAM, in its sole discretion, may at any time limit the maximum Notional
  #  Capital of the Investment Account which may be invested in a specific
  # Limited Markets. 

  # 9) Non-US Issuers; MLPs; PTPs ----------------------------------------------
  # Equities of non-U.S. issuers, master limited partnerships and any other
  # publicly traded partnerships shall be permitted only if such instruments are
  # accessed through as a total return swap.
  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Non-US Equity on Swap Only",
    scope = "position",
    bbfields = "DS290",
    definition = function(security_id, sma) {
      permitted_countries <- c("US")
      sec_type <- vapply(security_id, \(id) .security(id)$get_instrument_type(), character(1)) #nolint
      iss_cty <- sapply(security_id, \(id) .security(id)$get_rule_data("DS290"))
      dplyr::case_when(
        sec_type != "Equity" ~ FALSE,
        iss_cty %in% permitted_countries ~ FALSE,
        TRUE ~ TRUE
      )
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