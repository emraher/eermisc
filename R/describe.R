#' My describe
#' @description My describe
#' @param dataframe a dataframe
#' @return a psych describe
#' @author Emrah Er
#' @examples
#'
#' @export describe
#' # -------------------------------------------------------------------------- ###
# Create function for summary statistics
# -------------------------------------------------------------------------- ###
# 1. n
# 2. Mean
# 3. Median
# 4. sd
# 5. min
# 6. max
# 7. sum na
# 8. sum inf
describe <- function(x){
  valid <- function(y){sum(!is.na(y))}
  sumna <- function(y){sum(is.na(y))}
  suminf <- function(y){sum(is.infinite(y))}
  sumnna <- function(y){sum(!is.na(y))}
  sumninf <- function(y){sum(!is.infinite(y))}
  nvar = ncol(x)

  stats = matrix(rep(NA, nvar * 10), ncol = 10)
  rownames(stats) <- colnames(x)
  stats[, 1]  <- apply(x, 2, valid)
  stats[, 2]  <- apply(x, 2, mean, na.rm = TRUE)
  stats[, 3]  <- apply(x, 2, median, na.rm = TRUE)
  stats[, 4]  <- apply(x, 2, sd, na.rm = TRUE)
  stats[, 5]  <- apply(x, 2, min, na.rm = TRUE)
  stats[, 6]  <- apply(x, 2, max, na.rm = TRUE)
  stats[, 7]  <- apply(x, 2, sumna)
  stats[, 8]  <- apply(x, 2, suminf)
  stats[, 9]  <- apply(x, 2, sumnna)
  stats[, 10] <- apply(x, 2, sumninf)

  answer <- data.frame("NObs"      = stats[, 1],
                       "Mean"   = stats[, 2],
                       "Median" = stats[, 3],
                       "SD"     = stats[, 4],
                       "Min"    = stats[, 5],
                       "Max"    = stats[, 6],
                       "NAs"  = stats[, 7],
                       "Infs" = stats[, 8],
                       "nNAs" = stats[, 9],
                       "nInfs" = stats[, 10])

  class(answer) <- c("psych", "describe", "data.frame")
  return(answer)
}
