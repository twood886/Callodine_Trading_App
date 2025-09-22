box::use(
  SMAManager[.security, .sma_rule, create_sma_from_enfusion],
)

#' @export
load_qsma <- function() {
  qsma <- create_sma_from_enfusion(
    long_name = "Qube Capital Management",
    short_name = "qsma",
    base_portfolio = "ccmf",
    holdings_url = paste0(
      "https://webservices.enfusionsystems.com/mobile/",
      "rest/reportservice/exportReport?",
      "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
      "QUBE+-+Positions.ppr"
    )
  )
  
  ## Restrictions ##############################################################

  # 1.) Permitted Countries
  # The Investment Manager shall not cause the Account to invest in securities
  # listed in countries other than the US (a "Permitted Country")
  qsma$add_rule(.sma_rule(
    sma_name = "qsma",
    rule_name = "Permitted Country of Listing",
    scope = "position",
    bbfields = "DS144",
    definition = function(security_id, sma) {
      permitted_countries <- c("US")
      exc_cty <- sapply(security_id, \(id) .security(id)$get_rule_data("DS144"))
      dplyr::case_when(
        exc_cty %in% permitted_countries ~ 0,
        TRUE ~ 1
      )
    },
    swap_only = FALSE,
    max_threshold = 0,
    min_threshold = 0
  ))

  # 2.) Derivatives
  # The Investment Manager shall not cause the Account to hold, trade or
  # purchase positions synthetically other than equity portfolio swaps
  # containing listed single name equities;

  # 3) IPOs/New Issues
  # The Investment Manager shall not cause the Account to invest in any offering
  # of equity securities for cash pursuant to a registration statement or a
  # notification on Form 1-A or Form 1-E filed under the Securities Act, as
  # described in Rule 105 under Regulation M promulgated under the Securities
  # Exchange Act of 1934;

  # 4) Max Position Size - % of Shares Oustanding
  # The Investment Manager shall not cause the Account to hold more than 1% of
  # shares or other equity interests issued by a company that is organised under
  # the laws of, headquartered in, incorporated in, or with a principal place of
  # business in the United States of America;
  qsma$add_rule(.sma_rule(
    sma_name = "qsma",
    rule_name = "Position under 1% of Shares Outstanding",
    scope = "position",
    bbfields = "DS381",
    definition = function(security_id, sma) {
      shares <- sapply(security_id, \(id) .security(id)$get_rule_data("DS381"))
      1 / shares
    },
    swap_only = FALSE,
    max_threshold = 0.01,
    min_threshold = -Inf
  ))

  # 5) Indemnity ---------------------------------------------------------------
  # The Investment Manager shall not cause the Account to make or give any
  # indemnity, guarantee, warranty or other contractual commitment
  # (other than customary undertakings and commitments regarding settlement of
  # trades and customary warranties as to ownership of assets attributable to
  # the Account);

  # 6) Restricted List ---------------------------------------------------------
  # The Investment Manager shall not cause the Account to invest in securities
  # that are designated by the Investment Manager from time to time in writing
  # (and so communicated to the Sub-Investment Manager) as restricted and shall
  # not otherwise effect any transactions on behalf of the Account that are
  # designated by the Investment Manager in writing from time to time (and so
  # communicated to the Sub-Investment Manager) as prohibited, in each case,
  # until the Investment Manager has notified the Sub-Investment Manager in
  # writing that such restriction/prohibition is waived or is no longer in
  # effect.

  # 7) Investment Companies ----------------------------------------------------
  # The Investment Manager shall not cause the Account to invest in shares or
  # other equity interests issued by investment companies that are registered
  # under the Company Act, or that have elected to be treated as “business
  # development companies” under the Company Act, in each case in excess of
  # 0.05% of the relevant investment company’s or “business development
  # company’s” market capitalisation; provided, however, that the aforementioned
  # restrictions shall not apply in the event that the Account invests in such
  # shares or equity interests through swaps or other derivative instruments
  # that are subject to settlement solely in cash; 
  qsma$add_rule(.sma_rule(
    sma_name = "qsma",
    rule_name = "BDCs on Swap Only",
    scope = "position",
    bbfields = "BI005",
    definition = function(security_id, sma) {
      bics_5 <- sapply(security_id, \(id) .security(id)$get_rule_data("BI005"))
      bics_5 == "BDCs"
    },
    swap_only = TRUE
  ))
  qsma$add_rule(.sma_rule(
    sma_name = "qsma",
    rule_name = "1940 Act Investment Company on Swap Only",
    scope = "position",
    bbfields = "FD634",
    definition = function(security_id, sma) {
      inv_comp <- sapply(security_id, \(id) .security(id)$get_rule_data("FD634"))
      dplyr::case_when(
        is.na(inv_comp) ~ FALSE,
        TRUE ~ TRUE
      )
    },
    swap_only = TRUE
  ))

  # 8) Exchange Limits ---------------------------------------------------------
  # The Investment Manager shall not cause the Account to invest in any
  # securities or other instruments that would cause the Account to breach the
  # limits set by the relevant exchange; provided that the Investment Manager
  # reserves the right to vary the limit in respect of the Account in the light
  # of the overall limit applicable to QSMA by providing written notice thereof
  # to the Sub-Investment Manager;

  # 9) MLPs and PTPs -----------------------------------------------------------
  # The Investment Manager shall not cause the Account to hold, trade or
  # purchase positions at any time, whether directly or synthetically (e.g., on
  # swap) in any US master limited partnerships, US publicly traded partnership
  # or other entity treated as a partnership for US federal income tax purposes
  # whether or not such entity is organised in the United States;
  qsma$add_rule(SMAManager::.sma_rule(
    sma_name = "qsma",
    rule_name = "no mlps",
    scope = "position",
    bbfields = "DS213",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS213"))
      dplyr::case_when(
        sec_typ == "MLP" ~ 1,
        TRUE ~ 0
      )
    },
    max_threshold = 0,
    min_threshold = 0
  ))
  qsma$add_rule(SMAManager::.sma_rule(
    sma_name = "qsma",
    rule_name = "no partnerships",
    scope = "position",
    bbfields = "DS674",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS674"))
      dplyr::case_when(
        sec_typ == "Partnership Shares" ~ 1,
        TRUE ~ 0
      )
    },
    max_threshold = 0,
    min_threshold = 0
  ))

  # 10) REITs
  # The Investment Manager shall not cause the Account to invest in any asset
  # that would cause the Account to recognise any income or loss that (a) is
  # treated as effectively connected with the conduct of a “trade or business
  # within the United States” as defined under Section 864 of the US Internal
  # Revenue Code of 1986, as amended (the “Code”), (b) is derived from the
  # conduct of any commercial activity (whether within or outside the United
  # States) within the meaning of Section 1.892-4T of the US Treasury
  # Regulations or (c) arises from the disposition of a “United States real
  # property holding corporation and/or United States real property interest”
  # (as defined in Section 897(c) of the Code); 
  qsma$add_rule(SMAManager::.sma_rule(
    sma_name = "qsma",
    rule_name = "no REITs",
    scope = "position",
    bbfields = "DS213",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS213"))
      dplyr::case_when(
        sec_typ == "REIT" ~ 1,
        TRUE ~ 0
      )
    },
    max_threshold = 0,
    min_threshold = 0
  ))

  # 11) New Issues
  # The Investment Manager shall not cause the Account to invest in any equities
  # that constitute a “New Issue”, as such is defined under FINRA Conduct Rule
  # 5130;
  qsma$add_rule(SMAManager::.sma_rule(
    sma_name = "qsma",
    rule_name = "no new issues",
    scope = "position",
    bbfields = "PR175",
    definition = function(security_id, sma) {
      act <- sapply(security_id, \(id) .security(id)$get_rule_data("PR175"))
      dplyr::case_when(
        act == "INAC" ~ 1,
        TRUE ~ 0
      )
    },
    max_threshold = 0,
    min_threshold = 0
  ))

  # 12) 144A -------------------------------------------------------------------
  # The Investment Manager shall not cause the Account to invest in “restricted
  # securities” as that term is defined and used in Rule 144 under the
  # Securities Act of 1933, as amended;
  qsma$add_rule(SMAManager::.sma_rule(
    sma_name = "qsma",
    rule_name = "no sanctioned securities",
    scope = "position",
    bbfields = c("DT346", "DW781", "DT347", "DT350", "DT348", "DQ061"),
    definition = function(security_id, sma) {
      ofac <- sapply(security_id, \(id) .security(id)$get_rule_data("DT346"))
      dod <- sapply(security_id, \(id) .security(id)$get_rule_data("DW781"))
      eu <- sapply(security_id, \(id) .security(id)$get_rule_data("DT347"))
      uk <- sapply(security_id, \(id) .security(id)$get_rule_data("DT350"))
      un <- sapply(security_id, \(id) .security(id)$get_rule_data("DT348"))
      eo14071 <- sapply(security_id, \(id) .security(id)$get_rule_data("DQ061"))
      mapply(
        function(...) {
          args <- list(...)
          ifelse(all(args == F), 0, 1)
        },
        ofac, dod, eu, uk, un, eo14071
      )
    },
    max_threshold = 0,
    min_threshold = 0
  ))

  # 13) Sanctioned Activities --------------------------------------------------
  # The Investment Manager shall not cause the Account to invest in securities
  # of any company undertaking sanctioned activities (e.g., production of
  # anti-personnel mines or cluster munitions, terrorism or terrorism financing,
  # transnational organised crime, malicious cyber activity,
  # narcotics trafficking, weapons of mass destruction (WMD) proliferation,
  # or human rights abuses);

  # 14) Sanctioned Persons -----------------------------------------------------
  # The Investment Manager shall not cause the Account to invest in securities
  # issued or controlled by any Person designated on a Sanctions List;

  # 15) SPACs ------------------------------------------------------------------
  # The Investment Manager shall not cause the Account to invest in any SPAC if
  # (A) (i) more than 18 months has elapsed since the SPAC completed its initial
  # public offering and (ii) the SPAC is not party to a binding agreement to
  # acquire a target in a business combination transaction; or
  # (B) more than 24 months has elapsed since the SPAC completed its initial
  # public offering; 

  # 16) Sanctioned Countries ---------------------------------------------------
  # The Investment Manager shall not cause the Account to invest in securities
  # of any company which is domiciled, incorporated or which has its primary
  # listing in a Sanctioned Country; any stock issued by any government of any
  # Sanctioned Country; any financial instrument listed on any exchange in a
  # Sanctioned Country and all financial instruments involving the currency of
  # any Sanctioned Country and all deposit or other accounts in the currency of
  # a Sanctioned Country. “Sanctioned Country” shall mean jurisdictions upon
  # which the United Nations Security Council (UNSC), OFAC, OFSI or the
  # European Union has imposed financial sanctions upon, which includes
  # Afghanistan, Central African Republic, Cuba, Democratic Republic of the
  # Congo, Democratic People’s Republic of Korea, Eritrea, Guinea-Bissau, Iran,
  # Iraq, Lebanon, Libya, Russia, Somalia, South Sudan, Sudan, Yemen and any
  # other jurisdiction that were announced as being financially sanctioned by
  # the UNSC from time to time;

  # 17) Cryptocurrency ---------------------------------------------------------
  # The Investment Manager shall not cause the Account to invest in
  # cryptocurrency ETFs (e.g., ETFs that track the value of cryptocurrencies
  # by investing in futures contracts for digital currency) or invest in digital
  # currencies directly.
  qsma$add_rule(SMAManager::.sma_rule(
    sma_name = "qsma",
    rule_name = "no cryptocurrency ETFs or digital currencies",
    scope = "position",
    bbfields = "DQ451",
    definition = function(security_id, sma) {
      crypto <- sapply(security_id, \(id) .security(id)$get_rule_data("DQ451"))
      dplyr::case_when(
        crypto == "Direct" ~ 1,
        TRUE ~ 0
      )
    },
    max_threshold = 0,
    min_threshold = 0
  ))


  ## Investment Guidelines #####################################################

  # 1) Gross Exposure ----------------------------------------------------------
  # Gross exposure: gross exposure, as measured by the sum of the absolute
  # values of total long and total short exposure, of the Account not to exceed
  # 200% of the net asset value of the Account. Equity exposure will be
  # reflected as the total market value, with options represented as their
  # delta-adjusted equivalent market value. Long equity positions hedged with
  # equity options on the same issuer can be shown on a net delta adjusted basis
  # prior to being included in the portfolio leverage calculation. 
  qsma$add_rule(.sma_rule(
    sma_name = "qsma",
    rule_name = "Gross Exposure Under 200% of NAV",
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
    max_threshold = 2,
    min_threshold = 0,
    gross_exposure = TRUE
  ))

  # 2) Net Exposure ------------------------------------------------------------
  # Net Exposure: net exposure, as measured by the absolute difference of the
  # absolute value of the total long exposure excluding SPAC positions
  # (SPAC positions are beta adjusted to 0) and the absolute value of the total
  # short exposure of the Account not to exceed +/- 50% of the net asset value
  # of the Account.
  qsma$add_rule(.sma_rule(
    sma_name = "qsma",
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

  # 3) Concentration -----------------------------------------------------------
  # Concentration: a position, either long or short, in any single issuer shall
  # not exceed 15% of the Account’s net asset value.
  qsma$add_rule(.sma_rule(
    sma_name = "qsma",
    rule_name = "Position under 15% of NAV",
    scope = "position",
    bbfields = c(NULL),
    definition = function(security_id, sma) {
      nav <- sma$get_nav()
      price <- vapply(security_id, \(id) .security(id)$get_price(), numeric(1))
      price / nav
    },
    swap_only = FALSE,
    max_threshold = 0.15,
    min_threshold = -0.15
  ))

  # 4) Liquidity ----------------------------------------------------------------
  # prior to making an investment, the Sub-Investment Manager shall determine,
  # in its sole discretion, that such investment is a Level One or a Level Two
  # asset as classified by GAAP’s Accounting Standards Codification 820. In
  # addition, the Sub-Investment Manager must have a reasonable expectation at
  # the time of making an investment that such investment can be 100% monetised
  # within 5 Trading Days under normal market conditions using 20% market
  # participation over the immediately preceding three months when measured in
  # respect of the Account. Investments that cannot be 100% monetised within 5
  # Trading Days using such 20% market participation are not eligible for the
  # Account. For the avoidance of doubt, for the purposes of the above
  # calculation, the Sub-Investment Manager’s holding of the investment for the
  # benefit of the Account must be aggregated with all other holdings of the
  # same investment by the Sub-Investment Manager in any other funds and/or
  # managed accounts.
  qsma$add_rule(SMAManager::.sma_rule(
    sma_name = "qsma",
    rule_name = "liquidity",
    scope = "position",
    bbfields = c("HS021"),
    definition = function(security_id, sma) {
      volume <- sapply(security_id, \(id) .security(id)$get_rule_data("HS021"))
      1 / volume
    },
    max_threshold = 1,
    min_threshold = -1
  ))

  invisible(qsma)
}