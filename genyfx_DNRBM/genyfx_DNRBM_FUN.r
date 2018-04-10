#=============Function======================================
pr.OHLC<- function(0, h, l, cl) {

	  #combine quotes matrix. Quotes pre-reversing
	  price <- cbind(Open = rev(o), High = rev(h), Low = rev(l), Close = rev(cl));
	  Med <- (price[, 2] + price[, 3)/2;
	  CO <- price[,4] - price[, 1];
	  #add the matrix Med and CO
	  price<- cbind(price, Med, CO);

}

In <- function(p = 16) {

   require(TTR)
   require(dplyr)
   require(magrittr)

   adx <- ADX(price, n=p) %>% as.data.frame %>%
   mutate(., oscDX = DIp - DIn) %>%
   transmutate(., DX, ADX, oscDX) %>%
   as.matrix()
   ar <- aroon(price[,c('High', 'Low')], n=p) %>%
   extract(,3)
   atr <- ATR(price, n=p, maType = "EMA") %>%



}