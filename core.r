library(discretization)
library(foreign)
library(e1071)
library(stringi)
library(party)

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

naiveBayesTrain <- function(myClass, mdata, Laplace = 0){
	# Clasificamos los datos
	names(mdata)[myClass] <- "class.selected"
	model <- naiveBayes(class.selected ~ ., data = mdata, laplace = Laplace)

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


objetiveFunction <- function(parms, myData, myClass){
	# 1: CAIM; 			Valores: 0, 1
	# 2: MDLP; 			Valores: 0, 1
	# 3: naiveBayes;	Valores: 0, 1
	# 		4: laplace;	real no negativo

	if (parms[1] == 1){
		# Matriz de datos discretos
		myData <- CAIM(myData)
	}

	if (parms[2] == 1){
		# Matriz de datos discretos
		myData <- MDLP(myData)
	}

	N <- length(myData[,1])
	p <- length(myData)

	# Conjunto de entrenamiento
	s <- sample(1:N, 75)
	trainSet <- myData[s,]

	# Conjunto de prueba
	s <- sample(1:N, 75)
	testSet <- myData[s,]

	# Vector de clases
	X <- trainSet[,-p]
	Y <- trainSet[,p]

	if (parms[3] == 1){
		# Generamos el modelo
		model <- naiveBayesTrain(myClass, trainSet, Laplace=parms[4])
	}else{
		names(trainSet)[myClass] <- "class.selected"
		model <- ctree(class.selected ~., trainSet)
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

	return( c(doCaim, doMdlp, doNaive, laplace, 0) )
}


localSearch <- function(initialSol, dataSet, myClass, iter = 10, progress=FALSE, pbar = NULL, tview = NULL){
	solution <- initialSol
	oldVal <- 0

	for (i in 0:iter) {

		if (progress){
			# gtkMainIterationDo(FALSE)
			while(gtkEventsPending()) gtkMainIteration()
			gtkProgressBarSetFraction(pbar, i/iter)
		}

		# Obtenemos el promedio de entrenamiento-prueba
		newVal <- 0
		cossVal <- 5
		for (j in 1:cossVal){
			newVal <- newVal + objetiveFunction(initialSol, dataSet, myClass)
		}

		newVal <- newVal/cossVal

		if (oldVal < newVal){
			oldVal <- newVal
			solution <- initialSol
		}

		initialSol[5] <- newVal
		status(initialSol, tview, i)

		initialSol <- nextSolution(initialSol, 0)
	}

		solution[5] <- oldVal

	return(solution)
}


analisys <- function (solution){
	msg <- "\n\n------------\n\nEl resultado del análisis es:\n\n"
	if (solution[1] == 1){
		msg <- stri_join(msg, "\n\n>>>> Descretizar datos con CAIM.\n\n")
	}

	if (solution[2] == 1){
		msg <- stri_join(msg, ">>>> Descretizar datos con MDLP.\n\n")
	}

	if (solution[1] == 0 && solution[2] == 0 ){
		msg <- stri_join(msg, ">>>> No discretizar los datos.\n\n")
	}

	if (solution[3] == 1){
		msg <- stri_join(msg, ">>>> Se recomienda usar el clasificador Naive Bayes.\n")
		msg <- stri_join(msg, "\tCon valor de laplace = ", stri_c(solution[4]))
	}else{
		msg <- stri_join(msg, ">>>> Se recomienda usar un árbol de decisiones.\n")

	}

	msg <- stri_join(msg, "\n\n===================================\n")
	tmp <- solution[5] * 100
	msg <- stri_join(msg, "Confiabilidad del \n", stri_c(round(tmp, digits=1)), "%")
	msg <- stri_join(msg, "\n===================================\n")


	return (msg)

}

status <- function(solution, textBox, iter){
	msg <- "\n"

	msg <- stri_join(msg, stri_c(iter), ".- \t")

		if (solution[1] == 1){
		msg <- stri_join(msg, "CAIM -> ")
	}

	if (solution[2] == 1){
		msg <- stri_join(msg, "MDLP -> ")
	}

	if (solution[1] == 0 && solution[2] == 0 ){
		msg <- stri_join(msg, "Sin discretizar datos ->")
	}

	# msg <- stri_join(msg, " \t ")

	if (solution[3] == 1){
		msg <- stri_join(msg, "Naive Bayes")
		msg <- stri_join(msg, " laplace = ", stri_c(round(solution[4], digits=2)))
		msg <- stri_join(msg, " -> ")
	}else{
		msg <- stri_join(msg, "árbol de decisiones ->")

	}
	
	tmp <- solution[5] * 100
	msg <- stri_join(msg, "\t ", stri_c(round(tmp, digits=2)), "%")

	txt.buff <- textBox$getBuffer()
	bounds <- txt.buff$getBounds()
	txt.buff$insert(bounds$end, msg)
	gtkTextViewSetBuffer(textBox, txt.buff)

}