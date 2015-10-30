# Locality class
# sets up and establishes all necessary methods for the creation of location tags
# contains a latitude and longitude

# representation for locality
localityRep <- representation(latitude = "numeric", longitude = "numeric")

# setting locality class
setClass("Locality", representation = localityRep)

# constructor
# args are a latitude and a longitude coordinate
Locality <- function(lat, long){
  if(is.na(lat) || is.na(long)){
    print("Locality failed to create because improper arguments were passed.")
    quit()
  }
  return (new("Locality", latitude = lat, longitude = long))
}

# getter for latitude
setGeneric("getLatitude", function(object) standardGeneric("getLatitude"))
setMethod("getLatitude", signature("Locality"), function(object){
  return (object@latitude)
})

# getter for longitude
setGeneric("getLongitude", function(object) standardGeneric("getLongitude"))
setMethod("getLongitude", signature("Locality"), function(object){
  return (object@longitude)
})