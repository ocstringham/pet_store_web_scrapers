

install.packages("devtools")
library(devtools)

install.packages("yaml")
library(yaml)

install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
library(binman)

install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
library(wdman)


install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")
library(RSelenium)
