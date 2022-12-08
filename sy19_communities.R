

test <- function (data) {
  return(strsplit(data, ",")[[1]])
}

newdata <- apply(communities_data, FUN = test, MARGIN = 1)

newdata2 <- t(newdata)
