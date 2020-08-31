nearest_polygon <- function(coords, polys){
  #new CRS to change coordinate system
  newCRS <- sprintf("+proj=aeqd +lon_0=%f +lat_0=%f", coords[1], coords[2]) %>% 
    CRS()
  #tranform coordinates and polygons to planar coordinate system
  polys <- spTransform(polys, newCRS)
  pts <- matrix(coords, ncol = 2) %>%
    SpatialPoints(., CRS("+proj=longlat"))
  ptsAEQD <- spTransform(pts, newCRS)
  #find nearest polygon
  gDists <- gDistance(ptsAEQD, polys, byid=TRUE)
  nearestPoly <- polys[which.min(gDists),]
  #return result
  return(nearestPoly)
}