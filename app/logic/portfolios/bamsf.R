box::use(
  Rblpapi[bdp, blpConnect],
  SMAManager[.security, .sma_rule, create_sma_from_enfusion],
  stats[setNames],
)

#' @export
load_bamsf <- function() {
  blpConnect()
  bamsf <- create_sma_from_enfusion(
    long_name = "Blackstone Alternative Multi-Strategy Fund",
    short_name = "bamsf",
    base_portfolio = "ccmf",
    holdings_url = paste0(
      "https://webservices.enfusionsystems.com/mobile",
      "/rest/reportservice/exportReport?",
      "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
      "BAMSF+Consolidated+Position+Listing+-+Options.ppr"
    ),
    trade_url = paste0(
      "https://webservices.enfusionsystems.com/mobile/",
      "rest/reportservice/exportReport?",
      "name=shared%2FTaylor%2F",
      "SMA_Mgr_Reports%2FBAMSF_Trade_Detail.trb"
    )
  )


  bamsf$add_rule(.sma_rule(
    sma_name = "bamsf",
    rule_name = "Position under 10% of NAV",
    scope = "position",
    definition = function(security_id, sma) {
      nav <- sma$get_nav()
      price <- vapply(security_id, \(id) .security(id)$get_price(), numeric(1))
      price / nav
    },
    swap_only = FALSE,
    max_threshold = 0.10,
    min_threshold = -Inf
  ))

  bamsf$add_rule(.sma_rule(
    sma_name = "bamsf",
    rule_name = "Securities Related Issuer Positions under 8% of NAV",
    scope = "position",
    definition = function(security_id, sma) {
      nav <- sma$get_nav()
      gics <- bdp(security_id, "DX203")$DX203
      price <- vapply(security_id, \(id) .security(id)$get_price(), numeric(1))
      dplyr::case_when(
        gics == "4020" ~ price / nav,
        TRUE ~ 0
      )
    },
    swap_only = FALSE,
    max_threshold = 0.08,
    min_threshold = -Inf
  ))

  bamsf$add_rule(.sma_rule(
    sma_name = "bamsf",
    rule_name = "Securities Related Issuer Positons under 0.5% of Shares Outstanding", #nolint
    scope = "position",
    definition = function(security_id, sma) {
      shares_out <- bdp(security_id, "DS381")$DS381
      gics <- bdp(security_id, "DX203")$DX203
      dplyr::case_when(
        gics == "4020" ~ 1 / shares_out,
        TRUE ~ 0
      )
    },
    swap_only = FALSE,
    max_threshold = 0.005,
    min_threshold = -Inf
  ))

  bamsf$add_rule(.sma_rule(
    sma_name = "bamsf",
    rule_name = "Positon under 1.5% of Shares Outstanding",
    scope = "position",
    definition = function(security_id, sma) {
      sec_type <- vapply(security_id, \(id) .security(id)$get_instrument_type(), character(1)) #nolint
      shares_out <- bdp(security_id, "DS381")$DS381
      dplyr::case_when(
        sec_type == "Equity" ~ 1 / shares_out,
        TRUE ~ 0
      )
    },
    swap_only = FALSE,
    max_threshold = 0.015,
    min_threshold = -Inf
  ))

  bamsf$add_rule(.sma_rule(
    sma_name = "bamsf",
    rule_name = "No REITs",
    scope = "position",
    definition = function(security_id, sma) {
      sec_type <- vapply(security_id, \(id) .security(id)$get_instrument_type(), character(1)) #nolint
      reit <- bdp(security_id, "DX203")$DX203 == "6010"
      dplyr::case_when(
        sec_type == "Equity" & reit ~ 1,
        TRUE ~ 0
      )
    },
    swap_only = FALSE,
    max_threshold = 0,
    min_threshold = 0
  ))

  bamsf$add_rule(.sma_rule(
    sma_name = "bamsf",
    rule_name = "Investment Company Positions under 1.5% of NAV",
    scope = "position",
    definition = function(security_id, sma) {
      nav <- sma$get_nav()
      sec_type <- vapply(security_id, \(id) .security(id)$get_instrument_type(), character(1)) #nolint
      bics_5 <- bdp(security_id, "BI015")$BI015
      price <- vapply(security_id, \(id) .security(id)$get_price(), numeric(1))
      dplyr::case_when(
        sec_type == "Fund" ~ price / nav,
        sec_type == "Equity" & bics_5 == "1411101011" ~ price / nav,
        TRUE ~ 0
      )
    },
    swap_only = FALSE,
    max_threshold = 0.015,
    min_threshold = -Inf
  ))

  bamsf$add_rule(.sma_rule(
    sma_name = "bamsf",
    rule_name = "Investment Company Positions under 0.5% Shares Outstanding", #nolint
    scope = "position",
    definition = function(security_id, sma) {
      sec_type <- vapply(security_id, \(id) .security(id)$get_instrument_type(), character(1)) #nolint
      bics_5 <- bdp(security_id, "BI015")$BI015
      shares_out <- bdp(security_id, "DS381")$DS381
      dplyr::case_when(
        sec_type == "Fund" ~ 1 / shares_out,
        sec_type == "Equity" & bics_5 == "1411101011" ~ 1 / shares_out,
        TRUE ~ 0
      )
    },
    swap_only = FALSE,
    max_threshold = 0.005,
    min_threshold = -Inf
  ))


  bamsf$add_rule(.sma_rule(
    sma_name = "bamsf",
    rule_name = "Total Securities Related Issuer Positions under 8% of NAV",
    scope = "portfolio",
    definition = function(security_id, portfolio) {
      security <- lapply(security_id, \(id) .security(id))
      type <- vapply(security, \(x) x$get_instrument_type(), character(1))
      gics <- bdp(security_id, "DX203")$DX203
      price <- vapply(security, \(x) x$get_price(), numeric(1))
      underlying_price <- vapply(security, \(x) x$get_underlying_price(), numeric(1)) #nolint
      price[type == "Option"] <- underlying_price[type == "Option"]
      delta <- vapply(security, \(x) x$get_delta(), numeric(1))
      nav <- portfolio$get_nav()
      exp <- (delta * price) / nav
      exp[gics != "4020"] <- 0
      setNames(exp, security_id)
    },
    max_threshold = 0.08,
    min_threshold = -Inf,
    gross_exposure = FALSE
  ))

  bamsf$add_rule(.sma_rule(
    sma_name = "bamsf",
    rule_name = "Total Investment Company Positions under 1.5% of NAV",
    scope = "portfolio",
    definition = function(security_id, portfolio) {
      security <- lapply(security_id, \(id) .security(id))
      type <- vapply(security, \(x) x$get_instrument_type(), character(1))
      bics_5 <- bdp(security_id, "BI015")$BI015
      price <- vapply(security, \(x) x$get_price(), numeric(1))
      underlying_price <- vapply(security, \(x) x$get_underlying_price(), numeric(1)) #nolint
      price[type == "Option"] <- underlying_price[type == "Option"]
      delta <- vapply(security, \(x) x$get_delta(), numeric(1))
      nav <- portfolio$get_nav()
      exp_ <- (delta * price) / nav
      exp <- rep(0, length(security_id))
      exp[bics_5 == "1411101011"] <- exp_[bics_5 == "1411101011"]
      exp[type == "Fund"] <- exp_[type == "Fund"]
      setNames(exp, security_id)
    },
    max_threshold = 0.015,
    min_threshold = -Inf,
    gross_exposure = FALSE
  ))


  .bics_rule <- function(security_id, portfolio, tgt_bics = NULL) {
    security <- lapply(security_id, \(id) .security(id))
    type <- vapply(security, \(x) x$get_instrument_type(), character(1))
    bics_lvl2 <- vapply(security, \(x) x$get_bics_level_2(), character(1))
    bics_lvl3 <- vapply(security, \(x) x$get_bics_level_3(), character(1))
    bics <- bics_lvl3
    bics[type == "FixedIncome"] <- bics_lvl2[type == "FixedIncome"]
    price <- vapply(security, \(x) x$get_price(), numeric(1))
    underlying_price <- vapply(security, \(x) x$get_underlying_price(), numeric(1)) #nolint
    price[type == "Option"] <- underlying_price[type == "Option"]
    delta <- vapply(security, \(x) x$get_delta(), numeric(1))
    nav <- portfolio$get_nav()
    exp <- (delta * price) / nav
    exp[bics != tgt_bics] <- 0
    setNames(exp, security_id)
  }


  industry_limit <- list(
    c("101010", "EQ Advertising & Marketing", 0),
    c("101011", "EQ Cable & Satellite", 0.19),
    c("101012", "EQ Entertainment Content", 0.10),
    c("101013", "EQ Publishing & Broadcasting", 0.06),
    c("101014", "EQ Internet Media & Services", 0.10),
    c("101101", "EQ Telecommunications", 0.18),
    c("111010",	"EQ Apparel & Textile Prod",	0.20),
    c("111011",	"EQ Automotive", 0.18),
    c("111012",	"EQ Home Construction", 0.10),
    c("111013",	"EQ Home & Office Products", 0.06),
    c("111014",	"EQ Leisure Products", 0.13),
    c("111110",	"EQ Consumer Services", 0.08),
    c("111111",	"EQ Leisure Facilities & Services", 0.30),
    c("111210",	"EQ Wholesale - Discretionary", 0.06),
    c("111211",	"EQ Retail - Discretionary", 0.30),
    c("111212",	"EQ E-Commerce Discretionary", 0.08),
    c("121010",	"EQ Food", 0.12),
    c("121011",	"EQ Beverages", 0.12),
    c("121012",	"EQ Tobacco & Cannabis", 0.19),
    c("121013",	"EQ Household Products", 0.18),
    c("121110",	"EQ Wholesale - Consumer Staples", 0.09),
    c("121111",	"EQ Retail - Consumer Staples", 0.13),
    c("131010",	"EQ Oil & Gas Producers", 0.30),
    c("131011",	"EQ Oil & Gas Services & Equipment", 0.09),
    c("131110",	"EQ Renewable Energy", 0.12),
    c("141010",	"EQ Banking", 0.18),
    c("141110",	"EQ Asset Management", 0.47),
    c("141111",	"EQ Specialty Finance", 0.47),
    c("141112",	"EQ Institutional Financial Svcs", 0.20),
    c("141210",	"EQ Insurance", 0.14),
    c("151010",	"EQ Real Estate Owners & Developers", 0.05),
    c("151012",	"EQ Real Estate Services",	0.06),
    c("151011",	"EQ REIT", 0.22),
    c("161010",	"EQ Biotech & Pharma", 0.30),
    c("161011",	"EQ Health Care Facilities & Services", 0.16),
    c("161012",	"EQ Medical Equipment & Devices", 0.16),
    c("171010",	"EQ Diversified Industrials", 0),
    c("171011",	"EQ Aerospace & Defense", 0.10),
    c("171012",	"EQ Electrical Equipment", 0.09),
    c("171013",	"EQ Machinery", 0.11),
    c("171014",	"EQ Transportation Equipment", 0.06),
    c("171015",	"EQ Industrial Intermediate Production", 0),
    c("171110",	"EQ Industrial Support Services", 0.15),
    c("171111",	"EQ Commercial Support Services", 0.18),
    c("171112",	"EQ Transportation & Logistics", 0.10),
    c("171113",	"EQ Engineering & Construction", 0.09),
    c("181010",	"EQ Chemicals", 0.12),
    c("181011",	"EQ Construction Materials",	0.07),
    c("181012",	"EQ Containers & Packaging", 0),
    c("181013",	"EQ Forestry, Paper & Wood Products", 0),
    c("181014",	"EQ Steel", 0.09),
    c("181015",	"EQ Metals & Mining", 0.12),
    c("191010",	"EQ Technology Hardware", 0.11),
    c("191011",	"EQ Semiconductors", 0.14),
    c("191110",	"EQ Software", 0.13),
    c("191111",	"EQ Technology Services", 0.13),
    c("201010",	"EQ Electric Utilities", 0.19),
    c("201011",	"EQ Gas & Water Utilities", 0.14),
    c("201012",	"EQ Electric & Gas Marketing & Trading", 0),
    c("501010",	"EQ Supranationals", 0),
    c("501110",	"EQ Sovereign Government", 0),
    c("501111",	"EQ Sovereign Agencies", 0),
    c("501112",	"EQ Governmental Banks", 0),
    c("501210",	"EQ Regional", 0),
    c("501211",	"EQ Local", 0)
  )

  lapply(
    industry_limit,
    \(x) {
      bamsf$add_rule(
        .sma_rule(
          sma_name = "bamsf",
          rule_name = paste0(x[2], " ", as.numeric(x[3]) * 100, "% Net Exp"),
          scope = "portfolio",
          definition = function(security_id, portfolio) {
            .bics_rule(security_id, portfolio, x[1])
          },
          max_threshold = as.numeric(x[3]),
          min_threshold = -Inf,
          gross_exposure = FALSE
        )
      )
      invisible(TRUE)
    }
  )
}