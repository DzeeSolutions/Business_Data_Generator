# store class
#   defines the store object and all necessary methods
#   contains menus and the customers that are in the store at any point in time

Store <- setRefClass("Store", fields = list(
  storeID = "character",name = "character", items = "list", 
  location = "Locality", customers = "list"
))