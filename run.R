list.of.packages <- c("shiny")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=TRUE)

runApp("/home/rodrigo/R/app_controle_financeiro/",host = "0.0.0.0",port=4242)
