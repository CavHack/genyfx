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

ZZ <- function(pr = price, ch = ch , mode="m") {
  require(TTR)
  require(magrittr)
  if (ch > 1) ch <- ch/(10 ^ (Dig - 1))
  if (mode == "m") {pr <- pr[ ,'Med']}
  if (mode == "hl") {pr <- pr[ ,c("High", "Low")]}
  if (mode == "cl") {pr <- pr[ ,c("Close")]}
  zz <- ZigZag(pr, change = ch, percent = F, 
               retrace = F, lastExtreme = T)
  n <- 1:length(zz)
  dz <- zz %>% diff %>% c(., NA)
  sig <- sign(dz)
  for (i in n) { if (is.na(zz[i])) zz[i] = zz[i - 1]}
  return(cbind(zz, sig))
}

form.data <- function( n = 16, z = 37, len = 500) {
	require(magrittr)
	x <- In( p =n)
	out <- ZZ(ch = z, mode = "m")
	data <- cbind(x, y = out[, 2]) %>%
	as.data.frame %>% head(., (nrow(x)-len))%>%
	na.omit😂
	data$y <- as.factor(data$y)
	return(data)
}

cleaning <- function(n = 16, z= 25, cut = 0.9 ){
	data <- form.data(n,z)
	descCor <- cor(data[ ,-ncol(data)])
	#summary(descCor[upper.tri(descCor)])
	highCor <- caret::findCorrelation(descCor, cutoff = cut)
	data <- data[ , -highCor]
	return(data)
}

prepareBest <- function(n, z, cut, norm, meth) {
	require(randomUniformForest)
	data.f <<- cleaning(n = n, z=z, cut =cut)
	idx <- rminer::holdout(y = data.f$y)
	x.train <- data.f[idx$tr, -ncol(data.f)]
	x.test <-  data.f[idx$ts, -ncol(data.f)]
  	y.train <- data.f[idx$tr, ncol(data.f)]
  	y.test <- data.f[idx$ts, ncol(data.f)]
 	if(norm){
    prep <- caret::preProcess(x = x.train, method = meth)
    #c("center", "scale","spatialSign"), "range"
    x.train <- predict(prep, x.train)
    x.test <- predict(prep, x.test)
	
	
}

ruf <- randomUniformForest(X = x.train,
						   Y = y.train,
						   xtest = x.test,
						   ytest = y.test,
						   mtry = 1, ntree = 300,
						   threads = 2,
						   nodesize = 1
				
)
imp.ruf <- importance(ruf, Xtest = x.test)
best <- imp.ruf$localVariableImportance$classVariableImportance %>%
head(., 10) %>% rownames()
return(best)
}

prepareTrain <- function(x , y, 
                         rati, mod = "stratified", 
                         balance = F, 
                         norm, meth)
{
  require(magrittr)
  require(dplyr)
  t <- rminer::holdout(y = y, ratio = rati,
                       mode = mod)
  train <- cbind(x[t$tr, ], y = y[t$tr])
  if(balance){
    train <- caret::upSample(x = train[ ,best], 
                             y = train$y, 
                             list = F)%>% tbl_df
    train <- cbind(train[ ,best], select(train, y = Class))
  }
  test <- cbind(x[t$ts, ], y = y[t$ts])
  if (norm) {
    prepr <<- caret::preProcess(train[ ,best], method = meth)
    train = predict(prepr, train[ ,best])%>% cbind(., y = train$y)
    test =  predict(prepr, test[ ,best] %>% cbind(., y = test$y))
  }
  DT <- list(train = train,
             test = test)
  return(DT)
}

#----PreTraining---------------------------------------

pretrainDBN <- function(L, Bs, dS, nE, nCD, InM = 0.5, FinM = 0.9) {
	require(darch)
	#create object DArch
	dbn <- newDArch(layers = L, batchSize = Bs, logLevel = 5)
	#set initial moment
	setInitialMomentum(dbn) <- InM
	#set final moment
	setFinalMomentum(dbn) <- FinM
	#set time of switching moments from initial to final
	setMomentumSwitch(dbn) <- round(0.8 * nE)
	dbn <- preTrainDArch(dbn, dataSet = dS, 
							numEpoch = nE,
							numCD = nCD,
							trainOutputLayer = T)
							
	return(dbn)
	
}

fineMod <- function(variant=1, dbnin, dS, 
					hd = 0.5, id = 0.2,
                    act = c(2,1), nE = 10)
{
  setDropoutOneMaskPerEpoch(dbnin) <- FALSE
  setDropoutHiddenLayers(dbnin) <- hd
  setDropoutInputLayer(dbnin) <- id
  layers <<- getLayers(dbnin)
  stopifnot(length(layers)==length(act))
  if(variant < 0 || variant >2) {variant = 1}
  for(i in 1:length(layers)){
    fun <- actFun %>% extract2(act[i])
    layers[[i]][[2]] <- fun
  }
  setLayers(dbnin) <- layers
  if(variant == 1 || variant == 2){ # backpropagation
    if(variant == 2){# rpropagation
      #setDropoutHiddenLayers(dbnin) <- 0.0
      setFineTuneFunction(dbnin) <- rpropagation
    }
    mod = fineTuneDArch(darch = dbnin, 
                        dataSet = dS, 
                        numEpochs = nE,
                        bootstrap = T)
    return(mod)
  }
}

prepareTest<- function(n, z, norm, len =501)
{
	x <- In(p = n) %>% na.omit %>% extract( ,best) %>%
	tail(., len)
	CO <- price[, "CO"] %>% tail(., len)
	if (norm) {
		x <- predict(prepr, x)
	}
	dt <- cbind(x=x, CO= CO) %>% as.data.frame()
	return(dt)
}

testAcc <- function(obj, typ = "bin") {
	require(fTrading)
	x <- DT.test[, best]
	CO <- DT.test$CO
	out <- predict(obj, newdata = x, type = typ)
	if(soft) {out <- max.col(out) - 1} else {out %<>% as.vector()}
	acc <- length(y.ts[y.ts == out])/length(y.ts) %>%
	round(., digits = 4)
	return(list(Acc = acc, y.ts = y.ts, y = out))
}


