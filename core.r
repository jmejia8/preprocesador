library(discretization)
library(stringi)
library(foreign)

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


localSearch <- function(initialSol, dataSet, iter = 10){
	solution <- initialSol
	oldVal <- 0
	for (i in 1:iter) {
		newVal <- objetiveFunction(initialSol, dataSet)
		if (oldVal < newVal){
			oldVal <- newVal
			solution <- initialSol
		}

		initialSol <- nextSolution(initialSol, 0)
	}

	return(solution)
}