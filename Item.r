# Item class
# sets up and establishes all necessary methods for items
# This class represents an item that a customer can purchase - it has a price 
# that can be adjusted.

Item <- setRefClass("Item", fields = list(
  
  productId = "character", name = "character", price = "numeric", 
  timestamp = "POSIXct", probabilityOfPurchase = "numeric"
  
), methods = list(
  
  # increases the price of the item by 1 dollar
  hikePrice = function(){
    price <<- price + 1
  },
  
  # decreases the price of the item by 1 dollar
  rollbackPrice = function(){
    if(price >= 1){
      price <<- price - 1
    }
  }
  
))