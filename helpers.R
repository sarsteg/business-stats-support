
# janitor::round_half_up() -----------------------------------------------------
round_half_up <- function(x, digits = 0) {
  multiplier <- 10^digits
  
  sign(x) * floor(abs(x) * multiplier + 0.5) / multiplier
}


# descr:freq() -----------------------------------------------------

# Includes x and w
freq <- function(x, w = NULL, useNA = "no", digits = 2) {
  
  # If no weights are supplied, use a weight of 1 for each observation
  if (is.null(w)) {
    w <- rep(1, length(x))
  }
  
  # Basic checks
  if (length(x) != length(w)) {
    stop("x and w must have the same length.")
  }
  
  if (!is.numeric(w)) {
    stop("w must be numeric.")
  }
  
  # Handle missing values
  if (useNA == "no") {
    keep <- !is.na(x) & !is.na(w)
    x <- x[keep]
    w <- w[keep]
  }
  
  # Weighted frequencies
  values <- unique(x)
  
  frequency <- sapply(values, function(v) {
    sum(w[x == v], na.rm = TRUE)
  })
  
  # Sort values in a sensible order
  ord <- order(values)
  values <- values[ord]
  frequency <- frequency[ord]
  
  percent <- frequency / sum(frequency) * 100
  
  result <- data.frame(
    Value = values,
    Frequency = frequency,
    Percent = round_half_up(percent, digits),
    Cumulative_Frequency = cumsum(frequency),
    Cumulative_Percent = round_half_up(cumsum(percent), digits),
    row.names = NULL
  )
  
  total <- data.frame(
    Value = "Total",
    Frequency = sum(frequency),
    Percent = 100,
    Cumulative_Frequency = NA,
    Cumulative_Percent = NA
  )
  
  rbind(result, total)
}


# ONLY inludes x
freq <- function(x, useNA = "no", digits = 2) {
  tab <- table(x, useNA = useNA)
  
  frequency <- as.vector(tab)
  percent <- frequency / sum(frequency) * 100
  
  result <- data.frame(
    Value = names(tab),
    Frequency = frequency,
    Percent = round_half_up(percent, digits),
    Cumulative_Frequency = cumsum(frequency),
    Cumulative_Percent = round_half_up(cumsum(percent), digits),
    row.names = NULL
  )
  
  total <- data.frame(
    Value = "Total",
    Frequency = sum(frequency),
    Percent = 100,
    Cumulative_Frequency = NA,
    Cumulative_Percent = NA
  )
  
  rbind(result, total)
}





# mode -------------------------------------------------------------------------
# https://www.r-bloggers.com/2016/07/computing-the-mode-in-r/
mode <- function(x){
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}