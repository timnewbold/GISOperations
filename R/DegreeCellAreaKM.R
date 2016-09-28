DegreeCellAreaKM <- function(lat, height, width) {
  # Returns the area in km squared of a grid cell in degrees of arc
  # lat - the latitudinal centre of the cell
  # height, width - the size of the grid cell in degrees
  
  # TODO Unit test
  # TODO Reference for this method
  
  radians <- function(theta) theta*pi/180.0
  
  # Convert the latitude into radians
  lat.rad <- radians(lat)
  
  # The equatorial and polar radii of the Earth in km
  eq.radius <-  6378137
  pol.radius <- 6356752.3142
  
  # Calculate cell area
  angular.eccentricity <- acos(radians(pol.radius/eq.radius))
  ecc.sq <- sin(radians(angular.eccentricity))^2
  flattening <- 1-cos(radians(angular.eccentricity))
  temp.val <- (eq.radius*cos(lat.rad))^2+(pol.radius*sin(lat.rad))^2
  m.phi <- ((eq.radius*pol.radius)^2)/(temp.val^1.5)
  n.phi <- (eq.radius^2)/sqrt(temp.val)
  lat.length <- pi/180*m.phi/1000
  long.length <- pi/180*cos(lat.rad)*n.phi/1000
  return (lat.length*height*long.length*width)
}
