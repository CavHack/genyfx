##---library and dependencies----------
library(magrittr)
library(dplyr)
library(tibble)
library(tidyverse)
library(tidyquant)
library(TTR)
library(rowr)
library(foreach)
library(anytime)
library(timekit)
library(quantmod)
library(QuantTools)
library(dygraphs)
library(PerformanceAnalytics)
library(DMwR2)
library(Rlof)

##-------Function-----------------------

##----Cotir-----------------------------
evalq({pr <- pr.OHLCV(Data, Open, High, Low, Close, Volume)
	}, 
	env)
##------------
head(env$pr)
tail(env$pr)
##-----------