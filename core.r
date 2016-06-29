library(discretization)
library(stringi)
library(foreign)
library(e1071)
library(stringi)

CAIM <- function(dataMatrix){
	return(disc.Topdown(dataMatrix, method=1)$Disc.data)
}

MDLP <- function(dataMatrix){
	return(mdlp(dataMatrix)$Disc.data)
}

openData <- function(name, dir="data/"){
	# Uso: openData("iris.arff")
	return(read.arff(stri_join(dir, name)))
}

naiveBayesTrain <- function(X, Y, Laplace = 0){
	# Clasificamos los datos
	model <- naiveBayes(X, Y, laplace = Laplace)

	return(model)
}

naiveBayesTest <- function(model, testSet, Y){
	# Generamo predictor
	pdt <- predict(model, testSet)

	# Número de aciertos
	aciertos <- 0

	for (i in 1:length(pdt)) {
		if (pdt[i] == Y[i]){
			aciertos <- aciertos + 1 
		}
	}

	return(aciertos/length(pdt))
}


objetiveFunction <- function(parms, myData){
	# 1: CAIM; 			Valores: 0, 1
	# 2: MDLP; 			Valores: 0, 1
	# 3: naiveBayes;	Valores: 0, 1
	# 		4: laplace;	real no negativo

	N <- length(myData[,1])
	p <- length(myData)

	# Conjunto de entrenamiento
	s <- seq(1, N, 2)
	trainSet <- myData[s,]

	# Conjunto de prueba
	s <- seq(2, N, 2)
	testSet <- myData[s,]

	# matriz de datos brutos
	X <- trainSet[,-p]

	if (parms[1] == 1){
		# Matriz de datos discretos
		X <- CAIM(X)
	}

	if (parms[2] == 1){
		# Matriz de datos discretos
		X <- MDLP(X)
	}

	# Vector de clases
	Y <- trainSet[,p]

	if (parms[3] == 1 || 1){
		# Generamos el modelo
		model <- naiveBayesTrain(X, Y, Laplace=parms[4])
	}

	# Asignamos valores del conjunto de prueba
	X <- testSet[,-p]
	Y <- testSet[,p]

	# Probamos el clasificador
	test <- naiveBayesTest(model, X, Y)

	# regresa índice de aciertos
	return(test)

}

nextSolution <- function (solution, index){
	doCaim <- sample(c(0,1), 1)
	doMdlp <- sample(c(0,1), 1)
	doNaive <- sample(c(0,1), 1)
	if (doNaive == 1){
		laplace <- runif(1, min = 0, max = 15)
	}else{
		laplace <- 0
	}

	return( c(doCaim, doMdlp, doNaive, laplace) )
}


localSearch <- function(initialSol, dataSet, iter = 10, progress=0, pbar = NULL){
	solution <- initialSol
	oldVal <- 0
	for (i in 1:iter) {

		if (progress != 0){
			gtkMainIterationDo(FALSE)
			gtkProgressBarSetFraction(pbar, i/iter)
		}

		newVal <- objetiveFunction(initialSol, dataSet)
		if (oldVal < newVal){
			oldVal <- newVal
			solution <- initialSol
		}

		initialSol <- nextSolution(initialSol, 0)
	}

	solution[5] <- oldVal

	return(solution)
}


analisys <- function (solution){
	msg <- "El resultado del análisis es:\n"
	if (solution[1] == 1){
		msg <- stri_join(msg, "\n\n>>>> Descretizar datos con CAIM.\n\n")
	}

	if (solution[2] == 1){
		msg <- stri_join(msg, ">>>> Descretizar datos con MDLP.\n\n")
	}

	if (solution[1] == 0 && solution[2] == 0 ){
		msg <- stri_join(msg, ">>>> No discretizar los datos.\n\n")
	}

	if (solution[3] == 1 || 1){
		msg <- stri_join(msg, ">>>> Se recomienda usar el clasificador Naive Bayes.\n")
		msg <- stri_join(msg, "\tCon valor de laplace = ", stri_c(solution[4]))
	}

	msg <- stri_join(msg, "\n\n===================================\n")
	tmp <- solution[5]*100
	msg <- stri_join(msg, "Valor de confiabilidad del \n", stri_c(tmp), "%")
	msg <- stri_join(msg, "\n===================================\n")


	return (msg)

}