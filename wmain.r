#!/usr/bin/env Rscript

require("RGtk2")
library("reader")
source("core.r")

initGraphicalInterface <- function(){

	# Variables globales
	myData <- NULL #openData(fname, dir="")
	fname <- ""

	onselectFileClicked <- function (button, user.data){
	dialog <- gtkFileChooserDialog(title = "Selecciona Archivo", 
	                     parent = NULL, action = "open",
	                     "gtk-ok", GtkResponseType["ok"],
	                     "gtk-cancel", GtkResponseType["cancel"],
	                     show = TRUE)


	gSignalConnect(dialog, "response", 
	               f = function(dialog, response, data) {
	                 if(response == GtkResponseType["ok"]) {
	                   file.name <- dialog$getFilename()
	                   entry.filename$setText(file.name)
	                 }
	                 dialog$destroy()
	               })
	}

	onStartClicked <- function (button, user.data){
		pb$setText("")

		if (combo["active"] < 1){
			windowError("Primero cargue los datos y luego seleccione la clase.")
			return(0)
		}

		fname <- entry.filename$getText()
		myData <- openData(fname, dir="")

		###########################
		txt.buff <- gtkTextBufferNew()
		txt.buff$text <- "Iniciando análisis..."
		gtkTextViewSetBuffer(tv, txt.buff)	
		###########################
		mc <- combo["active"]
		val <- localSearch(c(1,1,1,5, 0), myData, myClass = mc, iter=spinbutton["value"], progress=TRUE, pbar=pb)

		txt.buff <- gtkTextBufferNew()
		msg <- analisys(val)
		txt.buff$text <- msg
		gtkTextViewSetBuffer(tv, txt.buff)		

		# Pone la barra de progreso en cero
		gtkProgressBarSetFraction(pb, 0)
		pb$setText("análisis terminado.")
	}

	onLoadDataClicked <- function(button, user.data){
		fname <- entry.filename$getText()

		if (!is.file(fname)){
			windowError("Eliga un archivo válido")
			return(0)
		}

		myData <- openData(fname, dir="")

		myClasses <- names(myData)
		sapply(myClasses, combo$appendText)
		gtkMainIterationDo(FALSE)
		combo["active"] <- 0
	}

	windowError <- function (msg){
		ErrorBox <- gtkDialogNewWithButtons("Error",window, "modal","gtk-ok", GtkResponseType["ok"])
		
		box1 <- gtkVBoxNew()
		box1$setBorderWidth(24)
		
		ErrorBox$getContentArea()$packStart(box1)
		
		box2 <- gtkHBoxNew()
		box1$packStart(box2)
		
		ErrorLabel <- gtkLabelNewWithMnemonic(msg)
		box2$packStart(ErrorLabel)
		
		response <- ErrorBox$run()
		
		if (response == GtkResponseType["ok"])
			ErrorBox$destroy()
	}

	window <- gtkWindow()
	window["title"] <- "Preprocesador de Datos"


	frame <- gtkFrameNew("Controles")
	window$add(frame)

	box1 <- gtkVBoxNew(FALSE)
	box1$setBorderWidth(10)
	frame$add(box1)   #add box1 to the frame


	box2 <- gtkHBoxNew(FALSE) #distance between elements
	box2$setBorderWidth(15)
	box1$add(box2)


	options <- gtkHBoxNew(FALSE) #distance between elements
	options$setBorderWidth(5)
	box1$add(options)

	box4 <- gtkHBoxNew() #distance between elements
	box4$setBorderWidth(15)
	box1$add(box4)

	selectFile <- gtkButton("Buscar Archivo")
	box2$packStart(selectFile, FALSE, FALSE, 0)

	entry.filename <- gtkEntryNew()
	entry.filename$setText("/home/jesus/Develop/R/verano2016/data/iris.arff")
	entry.filename$setWidthChars(50)
	box2$packStart(entry.filename)

	btn.load <- gtkButton("Cargar datos")
	options$packStart(btn.load)

	combo <- gtkComboBoxNewText ()
	myClasses <- c("Selecciona una clase:")
	sapply (myClasses, combo$appendText )
	options$packStart(combo)

	lb.spin <- gtkLabelNewWithMnemonic("Iteraciones:")
	spinbutton <- gtkSpinButton(min = 1, max = 1000, step = 1)
	spinbutton["value"] <- 10
	spinbutton["width-request"] <- 100
	box4$packStart(lb.spin, FALSE, FALSE, 0)
	box4$packStart(spinbutton, FALSE, FALSE, 0)

	btn.start <- gtkButton("Analizar")
	btn.start["width-request"] <- 50
	box4$packStart(btn.start)

	# Agrega separador
	box1$packStart(gtkHSeparatorNew())

	box3 <- gtkHBoxNew() #distance between elements
	# box3$setBorderWidth()
	box1$add(box3)   #add box1 to the frame

	tv <- gtkTextViewNew()
	tv["width-request"] <- 100
	tv["height-request"] <- 300
	txt.buff <- gtkTextBufferNew()
	txt.buff$text <- "Aún no hay información..."
	gtkTextViewSetBuffer(tv, txt.buff)

	box3$packStart(tv)

	pb <- gtkProgressBar()
	box1$packStart(pb)

	# Señales
	gSignalConnect(selectFile, "clicked", onselectFileClicked)
	gSignalConnect(btn.start, "clicked", onStartClicked)
	gSignalConnect(btn.load, "clicked", onLoadDataClicked)
	gSignalConnect(window, "delete-event", function (event, ...){
		print("Saliendo...")
		quit()
		})

	while (TRUE){
		Sys.sleep(1)
	}

}


initGraphicalInterface()
