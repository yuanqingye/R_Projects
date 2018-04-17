library(arulesSequences)
data(zaki)
frequent_sequences <- cspade(zaki, parameter=list(support=0.5))

s1 <- cspade(zaki, parameter = list(support = 0), 
             control   = list(verbose = TRUE, tidLists = TRUE))

s2 <- cspade(zaki, parameter = list(support = 0.3), 
             control   = list(verbose = TRUE, tidLists = TRUE))

as(s1,"data.frame")
summary(tidLists(s1))
transactionInfo(tidLists(s1))

# Parameter
# Slots
# support:
#   a numeric value specifying the minimum support of a sequence (default 0.1, range [0,1]).
# maxsize:
#   an integer value specifying the maximum number of items of an element of a sequence (default 10, range > 0).
# maxlen:
#   an integer value specifying the maximum number of elements of a sequence (default 10, range > 0).
# mingap:
#   an integer value specifying the minimum time difference between consecutive elements of a sequence (default none, range >= 1).
# maxgap:
#   an integer value specifying the maximum time difference between consecutive elements of a sequence (default none, range >= 0).
# maxwin:
#   an integer value specifying the maximum time difference between any two elements of a sequence (default none, range >= 0).

as(frequent_sequences,"data.frame")

t <- zaki
transactionInfo(t)$classID <-
  as.integer(transactionInfo(t)$sequenceID) %% 2 + 1L
s3 <- cspade(t, parameter = list(support = 0.4, maxgap = 5))
as(s3, "data.frame")

## use timing constraint
s2 <- cspade(zaki, parameter = list(support = 0.4, maxgap = 5))
as(s2, "data.frame")

## replace timestamps
t <- zaki
transactionInfo(t)$eventID <-
  unlist(tapply(seq(t), transactionInfo(t)$sequenceID,
                function(x) x - min(x) + 1), use.names = FALSE)
as(t, "data.frame")
s4 <- cspade(t, parameter = list(support = 0.4))
s4
identical(as(s1, "data.frame"), as(s4, "data.frame"))

## work around
s5 <- cspade(zaki, parameter = list(support = .25, maxgap = 5))
length(s5)
k <- support(s5, zaki, control   = list(verbose = TRUE,
                                        parameter = list(maxwin = 5)))
table(size(s5[k == 0]))

## Not run: 
## use generated data
t <- read_baskets(con  = system.file("misc", "test.txt", package =
                                       "arulesSequences"),
                  info = c("sequenceID", "eventID", "SIZE"))
summary(t)
## use low support
s6 <- cspade(t, parameter = list(support = 0.0133), 
             control   = list(verbose = TRUE))
summary(s6)