library(readxl)
setwd("~/Documents/recherche-enseignement/code/R/rmetrics/Rmetrics2/rmetrics/pkg/randtoolbox/share")
ORIG1111 <- as.matrix(as.data.frame(read_xls("initmj-D-Wuertz.xls", sheet=1, col_names = FALSE)))
colnames(ORIG1111) <- NULL
dim(ORIG1111)
head(ORIG1111)

ORIG1111.text <- paste(apply(ORIG1111, 1, paste, collapse=","), collapse=",\n")
cat(ORIG1111.text, file="ORIG1111.mj.txt")


ORIG1111.const.S <- as.matrix(as.data.frame(read_xls("initmj-D-Wuertz.xls", sheet=4, col_names = TRUE)))

head(ORIG1111.const.S)

s <- ORIG1111.const.S[,"s"]
s2 <- matrix(s, ncol=10, byrow = TRUE)
# add dimension 1
s2.text <- c("1", apply(s2, 1, paste, collapse=","))
s2.text <- paste(s2.text, collapse=",\n")
cat(s2.text, file="ORIG1111.s.txt")


a <- ORIG1111.const.S[,"a"]
a2 <- matrix(a, ncol=10, byrow = TRUE)
# add dimension 1
a2.text <- c("0", apply(a2, 1, paste, collapse=","))
a2.text <- paste(a2.text, collapse=",\n")
cat(a2.text, file="ORIG1111.a.txt")
