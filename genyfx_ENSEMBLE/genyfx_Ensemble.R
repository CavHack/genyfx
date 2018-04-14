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
}


)