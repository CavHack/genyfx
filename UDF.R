##=====UDF===================================
pr.OHLCV <- function(d, o, h, l, cl, v) {

	 require('magrittr')
	 require('dplyr')
	 require('anytime')
	 
	 price <- cbind( Data = rev(d),
	       	  	 Open = rev(o), High = rev(h),
			 Low = rev(l), Close = rev(cl),
			 Vol = rev(v)) %>% as.tibble()

	price$Data %<>% anytime(., tz = "CET")
	return(price)


}