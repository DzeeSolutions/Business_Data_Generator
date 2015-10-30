# Consumer class
# sets up the consumer class and establishes all necessary methods
# can purchase items

Consumer <- setRefClass("Consumer", fields = list(
  
  consumerId = "character",name = "character", age = "numeric", 
  location = "Locality", homeStoreID = "character"
  
), methods = list(
  
  # purchases an item off the given menu
  purchase = function(items, probs){
    return(sample(items, 1, replace = TRUE, probs))
  }
  
))