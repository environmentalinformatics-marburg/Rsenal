#' Calculate selected geometry variables from raster data
#' @description This function calculates geoemtry parameters from Raster data.
#' @param x A rasterLayer with any values but where patches are seperated by surrounding NA values
#' @return A RasterStacks containing the geoemtry parameters. The names are as following;
#' Patches: The ID of the Patches
#' Ar: The area of the individual patches
#' SI: The shape index calculated by SDMTools
#' CA: The core area calculated by SDMTools
#' Up: The perimeter of the indivisual patches
#' CAI: The core area index calculated by SDMTools
#' PAR:The perimeter area ratio calculated by SDMTools
#' Re: The radius of the inner circle
#' distEdges: The distcance from edges 
#' Ru: The radius of the outer circle
#' OIC: The inner-outer-circle ratio
#' CI1: inner compactness
#' CO1: outer compactness
#' CI2: inner compactness2
#' CO2: outer compactness2
#' CCI1: corrected compactness index 1
#' CCI2: corrected compactness index 2
#' CO: compactness
#' SHD: Shoreline development
#' C1: Circularity
#' E: Elongateness
#' TR:Thinness Ratio
#' CR: Circularity ratio
#' C2: Circularity2
#' FR: Form ratio
#' EI: Elipticity index
#' SF1: Shape factor
#' GSI: Grain shape index
#' SF2: Shape factor 2
#' C3: Circularity 3
#' SF3: Shape factor 3
#' 
#' For decription of the indices see list in Borg 1998: Vergleichende Analyse von Formindizes zur Characterisierung 
#' von Landschaftsobjekten unter ökologischen Aspekten. 
#' Zeitschrift für Photogrammetrie und Fernerkundung 4: 108-119
#' @author Hanna Meyer
#' @seealso \code{SDMTools},  \code{clump}, \code{borg_indices}
#' @examples
#' RasterLayer<-raster("inst/training.rst")
#' shape<-geometryVariables(RasterLayer)
#' plot(shape)
#' 

geometryVariables <- function(x){
     require(SDMTools)
     require(raster)
     Patches<-clump(x)
     Stats<-PatchStat(Patches)
     # patch area:
     Area <- reclassify(Patches, cbind(Stats$patchID,Stats$area))
     #shape index:
     shapeIndex <- reclassify(Patches, cbind(Stats$patchID,Stats$shape.index))
     #core area:
     coreArea <- reclassify(Patches, cbind(Stats$patchID,Stats$core.area.index))
     #perimeter:
     perimeter <- reclassify(Patches, cbind(Stats$patchID,Stats$perimeter))
     #core.area.index
     coreAreaIndex <- reclassify(Patches, cbind(Stats$patchID,Stats$core.area.index))
     #the ratio of the patch perimeter (m) to area (m2)
     perimAreaRatio <- reclassify(Patches, cbind(Stats$patchID,Stats$perim.area.ratio))
     #distance to edge:
     edges<-boundaries(Patches, type='inner')
     distEdges<- gridDistance(edges,origin=1) 
     values(distEdges)[is.na(values(Patches))]<-NA
     #innerCircle (largest circle)= maximum distance from edge
     tmp<-zonal(distEdges,Patches,fun="max")
     innerCircle<-Patches
     innerCircle<-reclassify(innerCircle,tmp)
     ##### outer circle
     oci<-c()
  
     for (i in 1:max(values(Patches),na.rm=TRUE)){
      cp<-Patches
      cp[cp!=i]<-NA
      cpp<-rasterToPolygons(cp,dissolve=TRUE)
      centroid<-gCentroid(cpp, byid=TRUE,id=attributes(cpp)$plotOrder)
      
      dist<- distanceFromPoints(Patches, centroid)
      dist[is.na(cp)]<-NA
      oci[i]<-max(values(dist),na.rm=TRUE)
     }
     outerCircle <- reclassify(Patches, cbind(Stats$patchID,oci))
     outerInnerCircle <- outerCircle-innerCircle

  ### Indices listed and/or developed by Borg 98
    borg<-borgIndices(Ar=Area,Ur=perimeter,De=innerCircle*2,Du=outerCircle*2)
    
  
  ##############################################################################  
     result<-stack(Patches,Area,shapeIndex,coreArea,perimeter,
                   coreAreaIndex, perimAreaRatio,innerCircle,distEdges,outerCircle,
                   outerInnerCircle,borg)
     names(result)<-c("Patches","Ar","SI","CA",
                     "Up", "CAI","PAR",
                     "Re","distEdges","Ru","OIC",names(borg))
     return(result)  
}