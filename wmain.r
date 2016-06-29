require("RGtk2")
source("core.r")

initGraphicalInterface <- function(){

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

	onStarClicked <- function (button, user.data){

		fname <- entry.filename$getText()

		myData <- openData(fname, dir="")
		###########################
		txt.buff <- gtkTextBufferNew()
		txt.buff$text <- "Iniciando análisis..."
		gtkTextViewSetBuffer(tv, txt.buff)	
		###########################
		
		val <- localSearch(list(1,1,1,5, 0), myData, iter=10, progress=1, pbar=pb)

		txt.buff <- gtkTextBufferNew()
		msg <- analisys(val)
		txt.buff$text <- msg
		gtkTextViewSetBuffer(tv, txt.buff)		
	}

	window <- gtkWindow()
	window["title"] <- "Preprocesador de Datos"


	frame <- gtkFrameNew("Controles")
	window$add(frame)

	box1 <- gtkVBoxNew(FALSE)
	box1$setBorderWidth(30)
	frame$add(box1)   #add box1 to the frame

	box2 <- gtkHBoxNew(FALSE, spacing= 50) #distance between elements
	box2$setBorderWidth(24)
	box1$add(box2)   #add box1 to the frame


	selectFile <- gtkButton("Buscar Archivo")
	box2$packStart(selectFile, FALSE, FALSE, 0)

	entry.filename <- gtkEntryNew()
	entry.filename$setText("/home/jesus/Develop/R/verano2016/data/iris.arff")
	entry.filename$setWidthChars(50)
	box2$packStart(entry.filename)

	btn.start <- gtkButton("Analizar")
	box1$packStart(btn.start, FALSE, FALSE, 0)

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
	gSignalConnect(btn.start, "clicked", onStarClicked)


}


initGraphicalInterface()











# label.t1 <- gtkLabelNew("Discretizar:")
# box3$packStart(label.t1, FALSE, FALSE, 0)

# label.discret <- gtkLabelNew("Sí")
# box3$packStart(label.discret, FALSE, FALSE, 0)

# box3$packStart(gtkHSeparatorNew(), TRUE, TRUE, 0)

# label.t2 <- gtkLabelNew("Métodos:")
# box3$packStart(label.t2, FALSE, FALSE, 0)

# label.met <- gtkLabelNew("CAIM -> MDLP")
# box3$packStart(label.met, FALSE, FALSE, 0)


# box4 <- gtkHBoxNew(FALSE, spacing= 15) #distance between elements
# box4$setBorderWidth(24)
# box1$add(box4)   #add box1 to the frame

# label.t3 <- gtkLabelNew("Clasificador\nRecomendado")
# box4$packStart(label.t3, FALSE, FALSE, 0)

# box5 <- gtkHBoxNew(FALSE, spacing= 15) #distance between elements
# box5$setBorderWidth(24)
# box4$add(box5)   #add box1 to the frame

# label.clasif <- gtkLabelNew("Naive Bayes \nKnn")
# box4$packStart(label.clasif, FALSE, FALSE, 0)

