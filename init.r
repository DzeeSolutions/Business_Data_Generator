# init.R
# THIS CLASS IS NOW DEPRECATED. ALL INITIALIZATION IS NOW PERFORMED IN 
# INITIALIZE ECHOSYSTEM
# This file creates all relevant variables and runs the necessary simulations

library(random)
library(chron)
source("../Locality.r")
source("../Item.r")
source("../Menu.r")
source("../Store.r")
source("../Consumer.r")
source("../Promotions.r")

# Store location object
loc <- Locality(40, 105)

# List holding the items that will go on the menu
itemList <- list()

# loop generates the items
for(i in c(1:10)){
  name <- paste("Item", i)
  id <- paste("product", i)
  item <- Item(id, name, i)
  itemList[i] = item
}

# the breakfast menu, with the items created in the loop
startTime <- as.POSIXct("09:00:00", format = "%H:%M:%S")
endTime <- as.POSIXct("11:00:00", format = "%H:%M:%S")
breakfastMenu <- Menu("Morning Menu", itemList, c(startTime, endTime))

# the store object - eventually this will need to be adjusted to have multiple 
# stores
McDonalds <- Store$new(name = "McDonalds", defaultMenus = breakfastMenu, 
                       location = loc)

# creates the desired number of customers
# returns a list of customers
createCustomers <- function(number){
  customerList <- list()
  for(i in c(1:number)){
    name <- paste("Guy", i)
    id <- paste("person", i)
    location <- Locality(rpois(1,40), rpois(1,105))
    customer <- Consumer(name, id, i, location)
    customerList[i] <- customer
  }
  return (customerList)
}

# simulates the customers walking in - this function is currently not working
# properly
simulateWalkIns <- function(customerList, dateRange){
  dates <- seq(dateRange[1], dateRange[2], by="1 day")
  
  # this walkIns vector stores the number of customers that walk in each day 
  # to make plotting easy
  walkIns <- c()
  for(i in c(1:length(dates))){
    if(is.weekend(dates[i])){
      walkIns[i] <- round(runif(1, min = 0.6, max = 0.8) * length(customerList))
    }else{
      walkIns[i] <- round(runif(1, min = 0.3, max = 0.5) * length(customerList))
    }
    
    # the walkingCustomers takes a sample of the total customers and stores those 
    # objects as the number of customers that actually enter the store
    walkingCustomers <- sample(customerList, walkIns[i])
    
    # the store object then keeps track of the customers inside it
    McDonalds <- walkInStore(McDonalds, walkingCustomers)
  }
  assign("McDonalds", McDonalds, envir = .GlobalEnv)
  x = data.frame(dates, walkIns)
  return (x)
}

# getter for the store object
# currently obsolete, but will be useful in multiple store simulations
getStore <- function(){
  return (McDonalds)
}

# simulates the customers purchasing an item. This method is also not complete yet
# simulatePurchases <- function(store, probs, promos){
#   
#   customers <- getCustomers(store)
#   bMenu <- getBreakfastMenu(store)
#   purchasedItems <- list()
#   print("--------------------------------------")
#   print(getProbs(getBreakfastMenu(getStore())))
#   print("custs right before update:")
#   print(length(getCustomers(getStore())))
#   bMenu <- updateProbs(store, probs)
#   
#   #must be edited for scaleability
#   McDonalds <- setBreakfastMenu(store, bMenu)
#   assign("McDonalds", McDonalds, envir = .GlobalEnv)
#   
#   lapply(promos, function(p){
#     m <- addPromotions(p, getBreakfastMenu(getStore()))
#     
#     #must be edited for scaleability
#     McDonalds <- setBreakfastMenu(store, m)
#     assign("McDonalds", McDonalds, envir = .GlobalEnv)
#   })
#   
#   bMenu <- getBreakfastMenu(getStore())
#   
#   # runs through the list of customers and has them purchase something
#   for(i in c(1:length(customers))){
#     currentCust <- customers[[i]]
#     purchasedItems[i] <- purchase(currentCust, bMenu)
#   }
#   
#   print("probs of menu")
#   print(getProbs(bMenu))
#   # this method needs to change to be more scaleable
#   # clears customers stored each simulation so they don't accumulate
#   McDonalds <- closeStore(McDonalds)
#   assign("McDonalds", McDonalds, envir = .GlobalEnv)
#   return (purchasedItems)
# }

# updates the items purchase probabilities as necessary
# updateProbs <- function(store, probs){
#   print("Beginning menu update.")
#   print(length(getCustomers(getStore())))
#   probVector <- unlist(strsplit(probs, ","))
#   probVector <- as.numeric(probVector)
#   
#   menu <- getBreakfastMenu(store)
#   menu <- setProbs(menu, probVector)
#   print("Menu has been updated.")
#   
#   return (menu)
# }