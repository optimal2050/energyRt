## Documentation for the three currency datasets. They are built by
## `data-raw/currency.R` from cached World Bank pulls; see that file for the
## generator and `data-raw/currency/FETCHED` for the pull date.

#' Currency spellings, and the ISO code and scale each denotes
#'
#' Every currency spelling `convert()` accepts, resolved to an ISO 4217 code
#' and a scale. `scale` is how many of the ISO unit the spelling denotes, which
#' covers sub-units (`cents` is 0.01 USD) and super-units (`crore INR` is 1e7
#' INR) with one mechanism, so an exchange rate is stored once per currency
#' rather than once per spelling.
#'
#' Row order is the column order of `convert_data$base$Currency` and is
#' load-bearing: unit lookup scans spellings and the first match wins.
#'
#' `si_prefixes` says whether an SI prefix may be attached. It is `FALSE` for
#' every scaled spelling, which is what keeps `Mcents` unresolvable.
#'
#' @format A data frame of 25 rows and 4 columns:
#' \describe{
#'   \item{spelling}{the string a user writes}
#'   \item{code}{ISO 4217 currency code}
#'   \item{scale}{how many of the ISO unit the spelling denotes}
#'   \item{si_prefixes}{may an SI prefix be attached?}
#' }
#' @seealso [currency_fx], [currency_deflator], [convert()]
#' @examples
#' head(currency_codes, 5)
#' subset(currency_codes, code == "INR")
"currency_codes"

#' Annual exchange rates, local currency per US dollar
#'
#' One rate per currency and year, the annual average. The column is named
#' `lcu_per_usd` rather than anything shorter because the direction is the
#' easiest thing to invert by accident: `convert_data`'s coefficients are US
#' dollars *per unit*, the reciprocal of what is stored here.
#'
#' Rates are keyed by currency, not by country. Members of a currency union
#' report the same rate, so one row serves them all. `USD` is the numeraire and
#' is 1 by construction. The euro begins in 1999; a year before that has no
#' euro rate because there was no euro.
#'
#' `attr(currency_fx, "base_year")` is the year untagged money is read at --
#' the latest year the table covers for a quorum of currencies, and the year
#' `convert_data$base$Currency` is generated from.
#'
#' @format A data frame of one row per currency and year:
#' \describe{
#'   \item{code}{ISO 4217 currency code}
#'   \item{year}{calendar year}
#'   \item{lcu_per_usd}{units of the currency per US dollar, annual average}
#'   \item{source}{the series the value came from}
#' }
#' @source World Bank World Development Indicators, series `PA.NUS.FCRF`
#'   (official exchange rate, LCU per US$, period average), which credits IMF
#'   International Financial Statistics as its own source. Licensed CC BY 4.0,
#'   <https://datacatalog.worldbank.org/public-licenses>.
#' @seealso [currency_codes], [currency_deflator]
#' @examples
#' attr(currency_fx, "base_year")
#' subset(currency_fx, code == "EUR" & year >= 2020)
"currency_fx"

#' Annual price indices, for moving money between years
#'
#' A deflator moves a value between points in time within one currency, which
#' is a different question from an exchange rate and needs different data. Two
#' bases are carried: `gdp` is the default, being the economy-wide numeraire
#' and the convention the World Bank and IPCC use for constant-price
#' comparisons; `cpi` is offered because published technology cost data is
#' often deflated on that basis instead.
#'
#' The index base year varies by series and does not matter: only the ratio of
#' two years is ever used.
#'
#' A deflator is national but a currency need not be, so `area` names the
#' country the index was taken from. Where no aggregate exists for a currency
#' union the largest member stands in -- the euro is the live case, having an
#' exchange rate of its own but no euro-area deflator in these series, so it
#' deflates on Germany. The column is there so that substitution is visible
#' rather than implied.
#'
#' @format A data frame of one row per currency, basis and year:
#' \describe{
#'   \item{code}{ISO 4217 currency code}
#'   \item{area}{ISO 3166-1 alpha-3 code of the area the index is for}
#'   \item{year}{calendar year}
#'   \item{basis}{`"gdp"` or `"cpi"`}
#'   \item{index}{the price index, in its own base}
#'   \item{source}{the series the value came from}
#' }
#' @source World Bank World Development Indicators, series `NY.GDP.DEFL.ZS`
#'   (GDP deflator) and `FP.CPI.TOTL` (consumer price index). Licensed
#'   CC BY 4.0, <https://datacatalog.worldbank.org/public-licenses>.
#' @seealso [currency_codes], [currency_fx]
#' @examples
#' subset(currency_deflator, code == "USD" & basis == "gdp" & year >= 2020)
"currency_deflator"
