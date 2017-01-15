## ------------------------------------------------------------------------
lower <- factor(letters[1:3], levels = letters[2:4])
upper <- factor(LETTERS[1:5], levels = LETTERS[1:4])
(combined <- c(lower, upper))

## ------------------------------------------------------------------------
library(refactor)

string <- c("a", "b", "c")
cfactor(string, levels = c("b", "c", "d"))

## ------------------------------------------------------------------------
easy_to_dectect <- c("EUR 11 - EUR 20", "EUR 1 - EUR 10", "EUR 21 - EUR 22")
factor(easy_to_dectect, ordered = TRUE) # correctly detects level

## ------------------------------------------------------------------------
hard_to_dectect <- c("EUR 21 - EUR 22", "EUR 100 - 101", 
                     "EUR 1 - EUR 10", "EUR 11 - EUR 20")

factor(hard_to_dectect, ordered = TRUE)

## ------------------------------------------------------------------------
cfactor(hard_to_dectect, ordered = TRUE, sep = "-")

## ------------------------------------------------------------------------
identical(
  cfactor(hard_to_dectect, ordered = TRUE, sep = NULL),
   factor(hard_to_dectect, ordered = TRUE)
)

## ------------------------------------------------------------------------
cfactor(x = c("a", "b", "c"), levels = c("a", "b", "c"), labels = c("a", "letter b", "b"))

## ------------------------------------------------------------------------
data <- sample(x = 1:10, size = 20, replace = TRUE)
index <- data.frame(encoding = 1:10,
                    label = letters[1:10])

cfactor(data, levels = index$encoding, labels = index$label)

## ------------------------------------------------------------------------
data <- data.frame(var1 = sample(x = 1:10, size = 20, replace = TRUE),
                  var2 = rep(1:2, 20),
                  var3 = sample(20),
                  var4 = 2, 
                  var5 = sample(row.names(USArrests), size = 20),
                  stringsAsFactors = FALSE)
head(data)

index <- data.frame(var = rep(paste0("var", 1:3), c(10, 2, 20)),
                    encoding = c(1:10, 1:2, 1:20),
                    label = c(letters[1:10], c("male", "female"), LETTERS[1:20]))
head(index)

## ------------------------------------------------------------------------
final <- head(index_cfactor(data = data, index = index, variable = "var", 
                            ordered = c(TRUE, TRUE, FALSE)))

print(final)
sapply(final, class)

## ---- echo = FALSE, include = TRUE---------------------------------------
c("1 to 4", "5 to 6") # properly separated
c("1 to 4", "4 to 6") # not properly separated
c("from 1,000 to 2,000", "from 2000 to 4,000") # comma separated and 'from' and 'to'
c("4.0 / 4.1", "4.2 / 4.3") # point and slash separator
c("one minute", "three minutes", "1 hour")

## ------------------------------------------------------------------------
random <- sample(100)
cut.default(random, breaks = seq(0, 100, by = 10))[1:10]

## ------------------------------------------------------------------------
cut(random, breaks = seq(0, 100, by = 10))[1:10]

## ------------------------------------------------------------------------
cut(sample(10), breaks = c(0, 3, 5))

## ------------------------------------------------------------------------
cut(sample(10), breaks = c(1, 4, 6, 8, 9, 10))

## ------------------------------------------------------------------------
cut(sample(10), breaks = c(10, 0, 3))

## ------------------------------------------------------------------------
cut(sample(10), breaks = c(1, 2.6, 5.1, 10))

## ------------------------------------------------------------------------
some_letters <- cfactor(letters, ordered = TRUE)
head(cut(some_letters, breaks = c("a", "q", "z"), 
         labels = c("beginning of the alphabet", "the rest of the alphabeth"), 
         right = TRUE, include.lowest = TRUE))

## ------------------------------------------------------------------------
c(cfactor("a"), cfactor("b"))
cc(cfactor("a"), cfactor("b"))

## ------------------------------------------------------------------------
cc(cfactor(c("a", "b"), ordered = T), cfactor(c("b", "c", "d"), ordered = T))

