

test <- function (data) {
  return(strsplit(data, ",")[[1]])
}

newdata <- apply(communities_data, FUN = test, MARGIN = 1)

newdata2 <- t(newdata)


data <- read.csv("communities_train.csv")

# Missing values article --> https://journalofbigdata.springeropen.com/articles/10.1186/s40537-021-00516-9
