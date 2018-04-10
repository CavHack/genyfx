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
   extract(, 1:2)
   cci <- CCI(price[ ,2:4], n=p)
   chv <- chaikinVolatility(price[ ,2:4], n=p)
   cmo <- CMO(price[ ,'Med'], n=p)
   macd <- MACD(price[ ,'Med'], 12, 26, 9) %>%
   as.data.frame() %>%
   mutate(., vsig = signal %>%
   	     diff %>% c(NA, .) %>% multiply_by((10)) %>%


   transmute(., sign = signal, vsig) %>%
	     as.matrix()
   rsi <- RSI(price[, 'Med'], n=p)
   stoh <- stoch(price[, 2:4], nFastK = p,
   	   	 nFastD = 3, nSlowD = 3,
		 maType = "EMA") %>%
		 as.data.frame() %>%
		 mutate(., oscK = fastK - fastD) %>%
		 transmute(., slowD, oscK) %>%
		 as.matrix()
    smi <- SMI(price[ ,2:4],n = p, nFast = 2, 
             nSlow = 25, nSig = 9)
    kst <- KST(price[ ,4])%>% as.data.frame() %>% 
				mutate(., oscKST = kst - signal) %>%
				select(.,oscKST) %>% as.matrix()
  In <- cbind(adx, ar, atr, cci, chv, cmo, macd, 
              rsi, stoh, smi, kst)
  return(In)
}
		 
}