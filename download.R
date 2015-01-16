# download integrated surface data from the National Climatic Data Center
library("devtools")
install_github("NateByers/ISDr")
library(ISDr)
downloadISD("IN", c(2007, 2012))
