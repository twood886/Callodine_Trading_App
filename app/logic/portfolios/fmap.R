box::use(
  SMAManager[.security, .sma_rule, create_sma_from_enfusion],
)

#' @export
load_fmap <- function() {
  fmap <- create_sma_from_enfusion(
    long_name = "Citco Bank Canada Ref Blackstone CSP-MST FMAP Fund",
    short_name = "fmap",
    base_portfolio = "ccmf",
    holdings_url = paste0(
      "https://webservices.enfusionsystems.com/mobile/",
      "rest/reportservice/exportReport?",
      "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
      "FMAP+-+Positions.ppr"
    )
  )

  # 1) Investment Strategy -----------------------------------------------------
  # The investment objective of the Sub-Account is to provide strong
  # risk-adjusted total returns with low market correlation and a focus on the
  # preservation of capital. The Sub Manager intends to achieve this objective
  # by primarily targeting the investment universe of equity income securities.
  # The Sub-Manager expects to emphasize bottom-up (individual company analysis)
  # and top-down (macro-economic and market projected strategy selection)
  # analysis.

  # 2) Pari-Passu with HF ------------------------------------------------------
  # The Sub-Manager shall trade the Sub-Account pari-passu with the Flagship
  # Master Fund based on the Sub-Account NAV, subject to the restrictions below,
  # and except with respect to 
  # (i) the deployment of capital received from investor subscriptions to the
  # Flagship Fund or increases in the Sub-Account NAV (but only to the extent
  # necessary to maintain pro-rata allocations) and
  # (ii) funding investor redemptions from the Flagship Fund or decreases in the
  # Sub-Account NAV (but only to the extent necessary to maintain pro-rata
  # allocations) and 
  # (iii) periodic rebalancing of the Sub-Account due to the price movements of
  # underlying positions.

  # 3) Restricted List ---------------------------------------------------------
  # The Sub-Manager will refrain from acquiring for the Sub-Account any
  # individual equity security (or derivative based thereon) issued by issuers
  # as the Manager may instruct by way of reasonable prior written notice to the
  # Sub-Manager from time to time (the “Restricted List”).  For the avoidance of
  # doubt, in the event that the Sub-Account holds any individual equity
  # security named on the Restricted List on the date that the Sub-Manager
  # receives such list (the “Restricted Securities”), the Sub-Manager shall not
  # be required to sell the relevant Restricted Securities, but the Sub-Manager
  # may not add to the relevant positions until such time as the Manager
  # derestricts such Restricted Securities by written notice to the Sub-Manager.
  # The Manager agrees that it will provide the Sub-Manager with written notice
  # as soon as reasonably practicable when any Restricted Security is removed
  # from the Restricted List.

  # 4) Australian Securities ---------------------------------------------------
  # The Sub-Manager shall not cause the Sub-Account to acquire or hold more 
  # than 0.50% of the outstanding equity securities of any company which was 
  # formed or incorporated in Australia (an “Australian Company”) without the 
  # prior written consent of the Manager. For purposes of the foregoing 
  # restriction, to the extent that the Sub-Account holds an interest in an 
  # Australian Company on swap or holds a depositary receipt of an Australian 
  # Company, the Sub-Account shall be deemed to hold the equity securities of 
  # such Australian Company that are represented by such swap or depository 
  # receipt.
  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "Australian Securities - position under 0.5% shares outstanding",
    scope = "position",
    bbfields = c("EX028", "DS381", "DX650"),
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("EX028"))
      shares_out <- sapply(security_id, \(id) .security(id)$get_rule_data("DS381")) #nolint
      cntry <- sapply(security_id, \(id) .security(id)$get_rule_data("DX650")) #nolint
      dplyr::case_when(
        sec_typ == "Equity" & cntry == "AU" ~ 1 / shares_out,
        TRUE ~ 0
      )
    },
    max_threshold = 0.005,
    min_threshold = -Inf
  ))

  # 5) Maximum Ownership -------------------------------------------------------
  # The Sub-Manager shall not cause the Sub-Account to acquire more than 3.99% 
  # (measured at the Sub-Account level) of the outstanding equity securities of
  # any company which is listed in a country other than Australia (i.e., the
  # shares of which are listed for quotation in the official list of a stock
  # exchange in a country other than Australia and none of the shares of which 
  # are listed for quotation in the official list of a stock exchange in
  # Australia) without the prior written consent of the Manager. 
  # Subject to Section 5(a) of the Investment Guidelines, the Sub-Manager shall 
  # not cause the Sub-Account to acquire or hold in excess of 4.99% of the 
  # outstanding equity securities of a class which is registered pursuant to 
  # Section 12 of the Exchange Act. To the extent either of the foregoing
  # restrictions in paragraphs (a) and (b) is breached due to a decrease in the
  # number of outstanding shares of the issuer, the Sub-Manager will promptly
  # bring the Sub-Account into compliance with such restriction.
  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "Non US / AU Securities - position under 3.99% shares outstanding",
    scope = "position",
    bbfields = c("EX028", "DS381", "DX560"),
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("EX028"))
      shares_out <- sapply(security_id, \(id) .security(id)$get_rule_data("DS381")) #nolint
      cntry <- sapply(security_id, \(id) .security(id)$get_rule_data("DX650")) #nolint
      dplyr::case_when(
        sec_typ == "Equity" & cntry != "US" ~ 1 / shares_out,
        TRUE ~ 0
      )
    },
    max_threshold = 0.0399,
    min_threshold = -Inf
  ))

  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "US Securities - position under 4.99% shares outstanding",
    scope = "position",
    bbfields = c("EX028", "DS381", "DX560"),
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("EX028"))
      shares_out <- sapply(security_id, \(id) .security(id)$get_rule_data("DS381")) #nolint
      cntry <- sapply(security_id, \(id) .security(id)$get_rule_data("DX650")) #nolint
      dplyr::case_when(
        sec_typ == "Equity" & cntry == "US" ~ 1 / shares_out,
        TRUE ~ 0
      )
    },
    max_threshold = 0.0499,
    min_threshold = -Inf
  ))

  # 6) Illiquid Investments ----------------------------------------------------
  # The Sub-Manager shall not cause the Sub-Account to make any private 
  # investments or any investments that it does not reasonably expect could be 
  # liquidated in an orderly manner without materially comprising the value of
  # realization proceeds within 61 days.
  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "liquidity",
    scope = "position",
    bbfields = c("EX028", "HS020"),
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("EX028"))
      volume <- sapply(security_id, \(id) .security(id)$get_rule_data("HS020"))
      dplyr::case_when(
        sec_typ == "Equity" ~ 1 / volume,
        TRUE ~ 0
      )
    },
    max_threshold = 1.83,
    min_threshold = -1.83
  ))

  # 7) Investment Companies ----------------------------------------------------
  # The Sub-Adviser shall not cause the Sub-Account to acquire or hold the
  # outstanding voting shares of any investment company, including without
  # limitation mutual funds, exchange-traded funds (“ETFs”), registered 
  # closed-end funds, and business development companies (“BDCs”) other than
  # through a swap agreement or option.
  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "no etps except on swap",
    scope = "position",
    bbfields = "DS213",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS213"))
      sec_typ == "ETP"
    },
    swap_only = TRUE
  ))

  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "no etns except on swap",
    scope = "position",
    bbfields = "DS213",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS213"))
      sec_typ == "ETN"
    },
    swap_only = TRUE
  ))

  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "no bdcs except on swap",
    scope = "position",
    bbfields = "BI005",
    definition = function(security_id, sma) {
      bics_5 <- sapply(security_id, \(id) .security(id)$get_rule_data("BI005"))
      bics_5 == "BDCs"
    },
    swap_only = TRUE
  ))

  # 8) No Activism -------------------------------------------------------------
  # The Sub-Manager shall not take an “activist posture” with respect to any
  # investment in the Sub-Account without the prior consent of the Manager.
  # As used herein, “activist posture” means, with respect to an investment,
  # taking an action on behalf of itself or its clients that would be
  # inconsistent with qualifying (A) to file a Schedule 13G in respect of any
  # filings required under Section 13(d) or 13(g) of the Securities Exchange Act
  # of 1934 and Regulation 13D-G thereunder or (B) to rely on the 16 C.F.R.
  # (HSR) section 802.9 exemption relating to acquisitions solely for the
  # purpose of investment (the “Passive Investment Exemption”) in respect of any
  # filings required under the Hart-Scott-Rodino Antitrust Improvements Act of
  # 1976, as amended; in each case, regardless of whether any such filing would
  # be required, the applicable filing threshold for filing has been reached,
  # or the Sub-Manager is the applicable filing person or “ultimate parent
  # entity.”  

  # 10) PTPs and MLPs ----------------------------------------------------------
  # The Sub-Manager may only invest in publicly traded partnerships and/or
  # master limited partnerships to the extent permitted under Section 2(e) of
  # the Agreement.  
  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "no mlps except on swap",
    scope = "position",
    bbfields = "DS213",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS213"))
      sec_typ == "MLP"
    },
    swap_only = TRUE
  ))

  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "no partnerships except on swap",
    scope = "position",
    bbfields = "DS674",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS674"))
      sec_typ == "Partnership Shares"
    },
    swap_only = TRUE
  ))

  # 11) US Real Property Interests ---------------------------------------------
  # The Sub-Manager shall not invest any portion of the assets in the
  # Sub-Account in any issuer (i) which is a “real estate investment trust”
  # (as defined in Section 856 of the Code) if such investment would cause the
  # Sub-Account to hold greater than 4% of such issuer at any given time and
  # (ii) that the Sub-Manager reasonably determines is a “United States real
  # property holding corporation” (as defined in Section 897(c)(2) of the Code)
  # if such investment would cause the Sub-Account to hold greater than 4% of
  # such issuer at any given time.
  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "REITs - no more than 4% shares outstanding",
    scope = "position",
    bbfields = c("DS213", "DS381"),
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS213"))
      shares_out <- sapply(security_id, \(id) .security(id)$get_rule_data("DS381")) #nolint
      dplyr::case_when(
        sec_typ == "REIT" ~ 1 / shares_out,
        TRUE ~ 0
      )
    },
    max_threshold = 0.04,
    min_threshold = -Inf
  ))

  # 12) MNPI -------------------------------------------------------------------
  # Neither the Sub-Manager nor the Manager shall provide the other party with
  # any material non-public information with respect to an issuer.

  # 13) Gross Exposure Limit ---------------------------------------------------
  # The Sub-Manager shall ensure that the Gross Exposure (as defined below) of
  # the Sub-Account will not exceed 250% of the Sub-Account NAV.
  fmap$add_rule(.sma_rule(
    sma_name = "fmap",
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


  # 14) Net Exposure Limit -----------------------------------------------------
  # The Sub-Manager shall ensure that the Net Exposure of the Sub-Account is
  # not greater than 50% or less than –50% of the Sub-Account NAV.
  fmap$add_rule(.sma_rule(
    sma_name = "fmap",
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

  # 15) Cryptocurrency Investments ---------------------------------------------
  # The Sub-Manager shall not cause the Sub-Account to make any investments
  # into spot/physical virtual currency or virtual currency derivatives
  # (e.g., bitcoin futures, options or swaps).  
  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
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

  # 16) China Outbound Investment Restrictioon ---------------------------------
  # The Sub-Manager shall not knowingly permit any assets of the Sub-Account to
  # be used in a transaction that
  # (i) a U.S. person would be prohibited from entering pursuant to the Outbound
  # Investment Laws (as defined below) or
  # (ii) would require a U.S. person to file a notification with the U.S.
  # Department of the Treasury or other competent authority pursuant to the
  # Outbound Investment Laws.  “Outbound Investment Laws” means Executive Order
  # 14105 of August 9, 2023, “Addressing United States Investments in Certain
  # National Security Technologies and Products in Countries of Concern,” its
  # implementing regulations (as presently set forth in 31 C.F.R. Part 850), and
  # any similar laws enacted after the date hereof.

  # 17) Options ----------------------------------------------------------------
  # It is understood that the Sub-Account may hold long options, covered options
  # or long option spreads. If the Sub-Manager wishes to write an uncovered
  # option, the Manager must approve beforehand (such approval not to be
  # unreasonably withheld or delayed).

  # 18) Credit Default Swaps ---------------------------------------------------
  # The Sub-Account may hold single-reference name Credit Default Swaps (“CDS”).
  # Such positions are expected to be generally for hedging default risk of long
  # cash bond positions of equal notional size in the same obligor, or for
  # establishing cheaper long-credit positions instead of cash bonds.
  # Opportunistically, the Sub-Account may buy single-reference name CDS
  # protection as an overlay protection of the equity portfolio. The Sub-Manager
  # shall obtain prior approval from the Manager before establishing CDS
  # positions outside those parameters (such approval not to be unreasonably
  # withheld or delayed).

  # 19) Swap Counties ----------------------------------------------------------
  # The Sub-Manager shall only trade for the Sub-Account securities listed in
  # the countries set forth on Annex A hereto on swap, save as otherwise
  # authorized by the Manager.
  # Argentina, Australia, Brazil, Chile, China, Colombia, Czech Republic,
  # France, Great Britain, Greece, Hong Kong, India, Indonesia, Ireland, Israel
  # Japan, Malaysia, Mexico, Peru, Philippines, Poland, Portugal, Romania
  # Russian Federation, South Africa, South Korea, Spain, Taiwan, Thailand
  # Turkey, Vietnam
  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "swap only non-approved countries",
    scope = "position",
    bbfields = "DS290",
    definition = function(security_id, sma) {
      swap_countries <- c(
        "AR", "AU", "BR", "CL", "CN", "CO", "CZ", "FR", "GB", "GR", "HK",
        "IN", "ID", "IE", "IL", "JP", "MY", "MX", "PE", "PH", "PL", "PT",
        "RO", "RU", "ZA", "KR", "ES", "TW", "TH", "TR", "VN"
      )
      cntry <- sapply(security_id, \(id) .security(id)$get_rule_data("DS290"))
      cntry %in% swap_countries
    },
    swap_only = TRUE
  ))

  invisible(fmap)
}
