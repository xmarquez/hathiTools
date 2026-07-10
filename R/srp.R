hash_fun <- function(token, dims = 160) {
  if (dims > 160) {
    start <- hash_fun(token, dims = 160)
    remainder <- hash_fun(paste0(token, "_"), dims = dims - 160)
    return(c(start, remainder))
  }
  if(dims < 160) {
    start <- hash_fun(token, dims = 160)
    return(start[1:dims])
  }

  res <- rawToBits(digest::digest(token,
                                  algo = "sha1",
                                  serialize = FALSE,
                                  raw = TRUE))

  as.logical(res)*2 - 1

}

hash_tokens <- function(tokens, dims) {
  res <- sapply(tokens, hash_fun, simplify = TRUE, dims = dims)
  res
}

log_transform <- function(counts) {
  counts <- pmax(0, log((counts/sum(counts))*1e5))
  counts
}

srp <- function(ef, dims = 160, ignore_pos = TRUE, log = TRUE) {
  token <- POS <- count <- NULL
  if(ignore_pos) {
    ef <- ef %>%
      dplyr::count(token, wt = count)
  } else {
    ef <- ef %>%
      dplyr::mutate(token = paste(token, POS, sep = "_")) %>%
      dplyr::count(token, wt = count)
  }
  if(log) {
    ef$n <- log_transform(ef$n)
  }
  as.vector(hash_tokens(ef$token, dims = dims) %*% ef$n)
}

srp_htid <- function(htid,
                     dir = getOption("hathiTools.ef.dir"),
                     cache_format = getOption("hathiTools.cacheformat"),
                     dims = 160,
                     ignore_pos = TRUE,
                     log = TRUE) {
  ef <- get_hathi_counts(htid, dir = dir,
                         cache_format = cache_format)
  srp(ef, dims = dims, ignore_pos = ignore_pos, log = log)
}
