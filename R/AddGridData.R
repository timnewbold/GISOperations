AddGridData <- function(gridData,dataFrame,columnName,longName="Longitude",latName="Latitude"){
  
  if (class(gridData)!="SpatialGridDataFrame"){
    stop("Error: gridData needs to be a SpatialGridDataFrame object")
  }
  if (class(dataFrame)!="data.frame"){
    stop("Error: dataFrame needs to be a data frame")
  }
  if (class(columnName)!="character"){
    stop("Error: columnName needs to be a character string")
  }
  
  cat("Processing gridded data\n")
  
  # Get the cell sizes of the grid
  cell.size.y<-slot(slot(gridData,"grid"),"cellsize")[1]
  cell.size.x<-slot(slot(gridData,"grid"),"cellsize")[2]
  
  # Get the longitude and latitude of the top left of the grid 
  long.left<-bbox(gridData)[1,1]
  lat.bottom<-bbox(gridData)[2,1]
  
  # Get the numbers of cells in each dimension of the grid
  ncols<-slot(slot(gridData,"grid"),"cells.dim")[1]
  nrows<-slot(slot(gridData,"grid"),"cells.dim")[2]
  
  # Make a matrix containing the grid data
  gridData.matrix<-matrix(gridData$band1,nrow=nrows,ncol=ncols,byrow=T)
  
  image(gridData.matrix)
  
  cat("Finding matching cells\n")
  
  long.cells <- ceiling((dataFrame[,longName] - long.left) / cell.size.x)
  lat.cells <- floor((dataFrame[,latName] - lat.bottom) / cell.size.y)
  
  cat("Warning: ",length(union(which(long.cells<=0),union(which(lat.cells<=0),
                               union(which(long.cells>ncols),which(lat.cells>nrows))))),
      " records outside bounds of grid data\n",sep="")
  
  long.cells[(long.cells<=0)]<-NA
  lat.cells[(lat.cells<=0)]<-NA
  long.cells[(long.cells>ncols)]<-NA
  lat.cells[(lat.cells>nrows)]<-NA
  
  dataFrame[,columnName] <- gridData.matrix[cbind(lat.cells,long.cells)]
  
  return(dataFrame)
}