evalq(

Evaluate <- function(actual=NULL, predicted=NULL, cm=NULL) {
	#default
	if(is.null(cm)) {
		actual = actual[!is.na(actual)]
		predicted = predicted[!is.na(predicted)]
		f = factor(union(unique(actual), unique(predicted)))
		actual = factor(actual, levels = levels(f))
		predicted = factor(predicted, levels = levels(f))
		cm = as.matrix(table(Actual = actual, Predicted = predicted))
	}
	
	n = sum(cm) #number of instances
	nc = nrow(cm) #number of classes
	diag = diag(cm) #number of correctly classified instances per class
	rowsums = apply(cm, 1, sum) #number of instances per class
	colsums = apply(cm, 2, sum) #number of predictions per class
	p = rowsums / n #distribution of instances over the classes
	q = colsums / n #distribution of instances over the predicted classes
	
	#accuracy
	accuracy = sum(diag) / n
	
	#per class
	recall = diag / rowsums
	precision = diag / colsums
	f1 = 2 * precision * recall / (precision + recall)
	
	#macro
	macroPrecision = mean(precision)
	macroRecall = mean(recall)
	macroF1 = mean(f1)
	
	#1-vs-all matrix
	oneVsAll = lapply(1:nc,
						function(i) {
							v = c(cm[i,i],
								  rowsums[i] - cm[i,i],
								  colsums[i] - cm[i,i],
								  n - rowsums[i] - colsums[i] + cm[i,i]);
								  return(matrix(v, nrow =2, byrow= T))})
								  
	s = matrix(0, nrow = 2, ncol = 2)
	for (i in i:nc) {s = s + oneVsAll[[i]]}
	
	#avg accuracy
	avgAccuracy = sum(diag(s))/sum(s)
	
	#micro
	microPrf = (diag(s) / apply(s, 1, sum))[1];
	
	#majority class
	mcIndex = which(rowsums == max(rowsums))[1] #majority-class index
	mcAccuracy = as.numeric(p[mcIndex])
	mcRecall = 0*p; mcRecall[mcIndex] = 1
	mcPrecision = 0*p; mcPrecision[mcIndex] = p[mcIndex]
	mcF1 = 0*p; mcF1[mcIndex] = 2 * mcPrecision[mcIndex] / (mcPrecision[mcIndex] + 1)
	
}


)
















