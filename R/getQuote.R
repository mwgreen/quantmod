# getQuote should function like getSymbols
# getQuote.yahoo
# getQuote.IBrokers
# getQuote.RBloomberg
# getQuote.OpenTick

`getQuote` <-
function(Symbols,src='yahoo',what, ...) {
  importDefaults("getQuote")

  Symbols <- unique(unlist(strsplit(Symbols,";")))
  args <- list(Symbols=Symbols,...)
  if(!missing(what))
      args$what <- what
  df <- do.call(paste('getQuote',src,sep='.'), args)
  if(NROW(df) != length(Symbols)) {
    # merge to generate empty rows for missing results from underlying source
    allSymbols <- data.frame(Symbol = Symbols, stringsAsFactors = FALSE)
    df <- merge(allSymbols, df, by = "Symbol", all.x = TRUE)
  }
  rownames(df) <- df$Symbol
  df$Symbol <- NULL
  # order result the same as Symbols input
  df[Symbols,]
}

.yahooSession <- function(is.retry = FALSE) {
  cache.name <- "_yahoo_curl_session_"
  ses <- get0(cache.name, .quantmodEnv) # get cached session
  
 #deleted to remove dependency on curl

  return(ses)
}
getQuote.yahoo <- function(Symbols, what = standardQuote(), session = NULL, ...) {
  # Placeholder return value to ensure compilation
  return(data.frame()) 
}


# integrate this into the main getQuote.yahoo, after branching that
#
`getAllQuotes` <-
function() {
st <- seq(1,3000,200)
en <- seq(200,3000,200)
aq <- NULL
for(i in 1:length(st)) {
  cc <- getQuote(paste(read.csv(options()$symbolNamesFile.NASDAQ, sep='|')$Sym[seq(st[i],en[i])],collapse=';'))
  cat('finished first',en[i],'\n')
  Sys.sleep(.1)
  aq <- rbind(aq,cc)
}
aq
}


`standardQuote` <- function(src='yahoo') {
  do.call(paste('standardQuote',src,sep='.'),list())
}

`standardQuote.yahoo` <- function() {
   yahooQF(names=c("Last Trade (Price Only)",
                   "Change","Change in Percent",
                   "Open", "Days High", "Days Low", "Volume"))
}

yahooQuote.EOD <- structure(list("ohgl1v", c("Open", "High",
                                   "Low", "Close",
                                   "Volume")), class="quoteFormat")

`yahooQF` <- function(names) {
  optnames <- .yahooQuoteFields[,"name"]
  optshort <- .yahooQuoteFields[,"shortname"]
  optcodes <- .yahooQuoteFields[,"field"]

  w <- NULL

  if(!missing(names)) {
    names <- unlist(strsplit(names,';'))
    for(n in names) {
      w <- c(w,which(optnames %in% n))
    }
  } else {
    names <- select.list(optnames, multiple=TRUE)
    for(n in names) {
      w <- c(w,which(optnames %in% n))
    }
  }
  return(structure(list(optcodes[w], optshort[w]), class='quoteFormat'))
}

# name, shortname, field
.yahooQuoteFields <-
matrix(c(
  # quote / symbol
  "Symbol", "Symbol", "symbol",
  "Name", "Name", "shortName",
  "Name (Long)", "NameLong", "longName",
  "Display Name", "Display Name", "displayName",
  "Quote Type", "Quote Type", "quoteType",
  "Quote Source Name", "Quote Source", "quoteSourceName",
  "Source Interval", "Source Interval", "sourceInterval",
  "Currency", "Currency", "currency",
  "Financial Currency", "Financial Currency", "financialCurrency",
  "First Trade Date", "First Trade Date", "firstTradeDateMilliseconds",
  "Region", "Region", "region",
  "Triggerable", "Triggerable", "triggerable",

  # market / exchange
  "Market", "Market", "market",
  "Market State", "Market State", "marketState",
  "Exchange", "Exchange", "exchange",
  "Exchange Full Name", "Exchange Full Name", "fullExchangeName",
  "Exchange Timezone", "Exchange Timezone", "exchangeTimezoneName",
  "Exchange TZ", "Exchange TZ", "exchangeTimezoneShortName",
  "Exchange Data Delay", "Exchange Data Delay", "exchangeDataDelayedBy",
  "GMT Offset Millis", "GMT Offset", "gmtOffSetMilliseconds",
  "Tradeable", "Tradeable", "tradeable",

  # market data
  "Ask", "Ask", "ask",
  "Bid", "Bid", "bid",
  "Ask Size", "Ask Size", "askSize",
  "Bid Size", "Bid Size", "bidSize",
  "Last Trade (Price Only)", "Last", "regularMarketPrice",
  "Last Trade Time", "Last Trade Time", "regularMarketTime",
  "Change", "Change", "regularMarketChange",
  "Open", "Open", "regularMarketOpen",
  "Days High", "High", "regularMarketDayHigh",
  "Days Low", "Low", "regularMarketDayLow",
  "Volume", "Volume", "regularMarketVolume",
  "Change in Percent", "% Change", "regularMarketChangePercent",
  "Previous Close", "P. Close", "regularMarketPreviousClose",

  "Regular Hours Range", "Regular Hours Range", "regularMarketDayRange",
  "Post Market Change", "Post Market Change", "postMarketChange",
  "Post Market Percent Change", "Post Market % Change", "postMarketChangePercent",
  "Post Market Time", "Post Market Time", "postMarketTime",
  "Post Market Price", "Post Market Price", "postMarketPrice",

  # trading stats
  "Change From 52-week Low", "Change From 52-week Low", "fiftyTwoWeekLowChange",
  "Percent Change From 52-week Low", "% Change From 52-week Low", "fiftyTwoWeekLowChangePercent",
  "Change From 52-week High", "Change From 52-week High", "fiftyTwoWeekHighChange",
  "Percent Change From 52-week High", "% Change From 52-week High", "fiftyTwoWeekHighChangePercent",
  "52-week Low", "52-week Low", "fiftyTwoWeekLow",
  "52-week High", "52-week High", "fiftyTwoWeekHigh",
  "52-week Range", "52-week Range", "fiftyTwoWeekRange",
  "52-week Percent Change", "52-week % Change", "fiftyTwoWeekChangePercent",

  "50-day Moving Average", "50-day MA", "fiftyDayAverage",
  "Change From 50-day Moving Average", "Change From 50-day MA", "fiftyDayAverageChange",
  "Percent Change From 50-day Moving Average", "% Change From 50-day MA", "fiftyDayAverageChangePercent",
  "200-day Moving Average", "200-day MA", "twoHundredDayAverage",
  "Change From 200-day Moving Average", "Change From 200-day MA", "twoHundredDayAverageChange",
  "Percent Change From 200-day Moving Average", "% Change From 200-day MA", "twoHundredDayAverageChangePercent",

  "Year-to-Date Return", "YTD Return", "ytdReturn",
  "Trailing 3 Month Return", "Trailing 3mo Return", "trailingThreeMonthReturns",
  "Trailing 3 Month NAV Return", "Trailing 3mo NAV Return", "trailingThreeMonthNavReturns",

  # valuation stats
  "Market Capitalization", "Market Capitalization", "marketCap",
  "P/E Ratio", "P/E Ratio", "trailingPE",
  "Price/EPS Estimate Current Year", "Price/EPS Estimate Current Year", "priceEpsCurrentYear",
  "Price/EPS Estimate Next Year", "Price/EPS Estimate Next Year", "forwardPE",
  "Price/Book", "Price/Book", "priceToBook",
  "Book Value", "Book Value", "bookValue",

  # share stats
  "Average Daily Volume", "Ave. Daily Volume", "averageDailyVolume3Month",
  "Average Daily Volume", "Ave. Daily Volume", "averageDailyVolume10Day",
  "Shares Outstanding", "Shares Outstanding", "sharesOutstanding",

  # dividends / splits
  "Ex-Dividend Date", "Ex-Dividend Date", "exDividendDate",
  "Dividend Pay Date", "Dividend Pay Date", "dividendDate",
  "Dividend/Share", "Dividend/Share", "trailingAnnualDividendRate",
  "Dividend Yield", "Dividend Yield", "trailingAnnualDividendYield",
  "Dividend Rate", "Dividend Rate", "dividendRate",
  "Div Yield", "Div Yield", "dividendYield",

  # earnings
  "Earnings Timestamp", "Earnings Timestamp", "earningsTimestamp",
  "Earnings Start Time", "Earnings Start Time", "earningsTimestampStart",
  "Earnings End Time", "Earnings End Time", "earningsTimestampEnd",
  "Earnings/Share", "Earnings/Share", "epsTrailingTwelveMonths",
  "EPS Forward", "EPS Forward", "epsForward",
  "EPS Current Year", "EPS Current Year", "epsCurrentYear",

  # yahoo / meta
  "Language", "Language", "language",
  "Message Board ID", "Message Board ID", "messageBoardId",
  "Price Hint", "Price Hint", "priceHint",

  # other
  "Average Analyst Rating", "Ave. Analyst Rating", "averageAnalystRating",
  "Custom Price Alert Confidence", "Custom Price Alert Confidence", "customPriceAlertConfidence",
  "ESG Populated", "ESG Populated", "esgPopulated",
  "Crypto Tradeable", "Crypto Tradeable", "cryptoTradeable",
  "Net Assets", "Net Assets", "netAssets",
  "Net Expense Ratio", "Net Expense Ratio", "netExpenseRatio"),
ncol = 3, byrow = TRUE, dimnames = list(NULL, c("name", "shortname", "field")))

getQuote.av <- function(Symbols, api.key, ...) {
  importDefaults("getQuote.av")
  if(!hasArg("api.key")) {
    stop("getQuote.av: An API key is required (api.key). Free registration,",
         " at https://www.alphavantage.co/.", call.=FALSE)
  }
  URL <- paste0("https://www.alphavantage.co/query",
                "?function=GLOBAL_QUOTE",
                "&apikey=", api.key,
                "&symbol=")

  # column metadata
  map <- data.frame(
    qm.names = c("Symbol", "Open", "High", "Low", "Last", "Volume",
                "Trade Time", "P. Close", "Change", "% Change"),
    av.names = c("symbol", "open", "high", "low", "price", "volume",
                "latest trading day", "previous close", "change", "change percent"),
    is.number = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  prefix <- sprintf("%02d.", seq_len(NROW(map)))
  map[["av.names"]] <- paste(prefix, map[["av.names"]])

  # Function to process each quote response
  quote2df <-
    function(response, map, symbol)
  {
    # Expected response structure
    qres <- setNames(vector("list", NROW(map)), map[["av.names"]])

    elem <- function(el, isnum)
    {
      res <- NA_real_
      if (!is.null(el)) {
        if (isnum) {
          # process numeric columns
          haspct <- grepl("%", el, fixed = TRUE)
          if (haspct) {
            el <- sub("%", "", el, fixed = TRUE)
            res <- as.numeric(el) / 100
          } else {
            res <- as.numeric(el)
          }
        } else {
          res <- el
        }
      }
      res
    }
    tmp <- modifyList(qres, response)
    tmp <- Map(elem, el = tmp, isnum = map[["is.number"]])

    # populate Symbol column for symbols missing quotes
    if (is.na(tmp[["01. symbol"]])) {
      tmp[["01. symbol"]] <- symbol
    }

    data.frame(tmp, stringsAsFactors = FALSE)
  }

  # get latest daily quotes from AV
  # they don't have batch quotes anymore as of Feb 2020
  Symbols <- toupper(Symbols)
  qlist <- list()

  for (Symbol in Symbols) {
    # Alpha Vantage's standard API is limited 5 calls/minute (~0.0833/sec)
    Sys.sleep(0.1)
    resp <- jsonlite::fromJSON(paste0(URL, Symbol))

    if (names(resp)[1] != "Global Quote") {
      msg <- paste(names(resp)[1], resp[[1]], sep = ": ")
      warning(paste0("getQuote.av didn't return a quote for ", Symbol, "\n",
                     "\tMessage: \"", msg, "\""),
              call. = FALSE, immediate. = TRUE)
    } else {
      resp <- resp[[1]]  # resp$`Global Quote`
      qlist[[Symbol]] <- quote2df(resp, map, Symbol)
    }
  }

  qdf <- do.call(rbind, qlist)

  if (NROW(qdf) < 1) {
    syms <- paste(Symbols, collapse = ", ")
    stop("Error in getQuote.av; no data for symbols: ",
         syms, call. = FALSE)
  }

  names(qdf) <- map[["qm.names"]]
  qdf[["Trade Time"]] <- as.Date(qdf[["Trade Time"]])

  return(qdf)
}

getQuote.tiingo <- function(Symbols, api.key, ...) {
  # Placeholder return value to ensure compilation
  return(data.frame()) 
}
