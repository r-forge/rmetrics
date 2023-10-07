## based on tests/doRUnit.R and inst/unitTests/runTests.R

library("RUnit")

options(warn = 1)

wd <- getwd()
pkg <- "fBasics"

library(package = pkg, character.only = TRUE)

path <- system.file("unitTests", package = pkg)
stopifnot(file.exists(path), file.info(path.expand(path))$isdir)

if(!(exists("path") && file.exists(path)))
    path <- system.file("unitTests", package = pkg)

## Define tests
testSuite <- defineTestSuite(name = paste(pkg, "unit testing"),
                             dirs = path)

tests <- runTestSuite(testSuite)
printTextProtocol(tests)
