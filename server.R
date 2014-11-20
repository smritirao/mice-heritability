################################################################

# Shiny Heatmap

################################################################

usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1]))
        install.packages(p, dep = TRUE)
    require(p, character.only = TRUE)
}
usePackage("shiny")
usePackage("devtools")
devtools::install_github("jcheng5/d3-heatmap")
usePackage("heatmap")

library(shiny)
library(heatmap)
source("heatmapJoe.R")
source("HeatmapU19_DWB_gabby.R")

################################################################

# Shiny Server

################################################################

shinyServer(function(input, output) {
	
################################################################

# Reactive input variable
  
################################################################

  datasetInput <- reactive({
  		input$variable
	})
	
################################################################
	
# Output heatmaps	
  
################################################################

  output$heatmap <- renderHeatmap({
  	
  	# Output Original Heatmap across all Crosses
  	if(datasetInput() == "crosses"){
      dat[as.vector(unlist(sapply(unique(dat[,"Cross"]),function(x)which(x==dat[,"Cross"])))),9:48] -> second
	
		  # remove full NA columns
		  second[,!as.vector(unlist(sapply(1:dim(second)[2],function(x)sum(is.na(second[,x])) == dim(second)[1])))] -> second_v2

		  V1 <- var(second_v2,na.rm=T)
		  cov.cor1 <- cov2cor(V1)
		  cov.cor1.m <- data.matrix(cov.cor1)
  	
  	  # Output Heatmap based on Specific Cross Type	
  	}else if(datasetInput() == "13140x3015"){
      dat[as.vector(unlist(sapply(datasetInput(),function(x)which(x==dat[,"Cross"])))),9:48] -> second

  		# remove full NA columns
  		second[,!as.vector(unlist(sapply(1:dim(second)[2],function(x)sum(is.na(second[,x])) == dim(second)[1])))] -> second_v2
  		V1 <- var(second_v2, na.rm=T)
  		#remove 0's in columns
  		colnames(V1)[which(sapply(1:dim(V1)[1],function(x)sum(V1[,x] == "0") == dim(V1)[2]))] -> remove_var
  
  		V1[-as.vector(unlist(sapply(remove_var,function(x)which(x==row.names(V1))))),] -> V2
  
  		V2[,-as.vector(unlist(sapply(remove_var,function(x)which(x==colnames(V2)))))] -> V3
  
  		cov.cor1 <- cov2cor(V3)
  		cov.cor1.m <- data.matrix(cov.cor1)

  	}else{	
      dat[as.vector(unlist(sapply(datasetInput(),function(x)which(x==dat[,"Cross"])))),9:48] -> second
	
  		# remove 100% NA columns
  		second[,!as.vector(unlist(sapply(1:dim(second)[2],function(x)sum(is.na(second[,x])) == dim(second)[1])))] -> second_v2
  
  		V1 <- var(second_v2)
  		cov.cor1 <- cov2cor(V1)
  		cov.cor1.m <- data.matrix(cov.cor1)
   }
  })
  	  ################################################################

# Output legends
 	 	  	################################################################

	output$legend <- renderPlot({
		
		if(datasetInput() == "crosses"){
      dat[as.vector(unlist(sapply(unique(dat[,"Cross"]),function(x)which(x==dat[,"Cross"])))),9:48] -> second
	
			# remove full NA columns
			second[,!as.vector(unlist(sapply(1:dim(second)[2],function(x)sum(is.na(second[,x])) == dim(second)[1])))] -> second_v2

			V1 <- var(second_v2,na.rm=T)
			cov.cor1 <- cov2cor(V1)
			cov.cor1.m <- data.matrix(cov.cor1)
			plot.new()		
			image(as.matrix(1:100), col=topo.colors(256),axes=F)
      axis(side=1,at=c(0,.25,.5,.75,1),labels=seq(range(cov.cor1.m)[1],range(cov.cor1.m)[2],length.out=5))
  	
		# Output Heatmap based on Specific Cross Type	
  		}else if(datasetInput()=="13140x3015"){
        dat[as.vector(unlist(sapply(datasetInput(),function(x)which(x==dat[,"Cross"])))),9:48] -> second

  			# remove full NA columns
  			second[,!as.vector(unlist(sapply(1:dim(second)[2],function(x)sum(is.na(second[,x])) == dim(second)[1])))] -> second_v2
  			V1 <- var(second_v2, na.rm=T)
  			#remove 0's in columns and rows
  			colnames(V1)[which(sapply(1:dim(V1)[1],function(x)sum(V1[,x] == "0") == dim(V1)[2]))] -> remove_var
  
  			V1[-as.vector(unlist(sapply(remove_var,function(x)which(x==row.names(V1))))),] -> V2
  
  			V2[,-as.vector(unlist(sapply(remove_var,function(x)which(x==colnames(V2)))))] -> V3
  
  			cov.cor1 <- cov2cor(V3)
  			cov.cor1.m <- data.matrix(cov.cor1)
  			plot.new()		
  			image(as.matrix(1:100), col=topo.colors(256),axes=F)
  			axis(side=1,at=c(0,.25,.5,.75,1),labels=seq(range(cov.cor1.m)[1],range(cov.cor1.m)[2],length.out=5))

  		}else{	
        dat[as.vector(unlist(sapply(datasetInput(),function(x)which(x==dat[,"Cross"])))),9:48] -> second
	
  			# remove full NA columns
  			second[,!as.vector(unlist(sapply(1:dim(second)[2],function(x)sum(is.na(second[,x])) == dim(second)[1])))] -> second_v2
  
  			V1 <- var(second_v2)
  			cov.cor1 <- cov2cor(V1)
  			cov.cor1.m <- data.matrix(cov.cor1) # may not output if too many NA rows
  			plot.new()		
  			image(as.matrix(1:100), col=topo.colors(256),axes=F)
        axis(side=1,at=c(0,.25,.5,.75,1),labels=seq(range(cov.cor1.m)[1],range(cov.cor1.m)[2],length.out=5))
  		}	
    	},width=800,height=150)
  
}) # Closes Shiny Server function