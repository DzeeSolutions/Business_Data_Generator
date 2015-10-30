# simulations
#   runs all simulations, including consumer walk ins and purchases
library(chron)
source("../helperFunctions.r")
source("../walkInInfoObject.r")
source("../Promotions.r")

# simulateWalkIns
#   enters customers into stores based on date and has them purchase items
#
# @param customerList - the list of customers that could potentially enter this
#     store
# @param dateRange - the range of dates in which this simulation will be run
# @param storeID - the id of the store object that these customers will be entering
# @param t - the current time in the simulation given in character format
#
# @return - a data frame of dates vs. customers entered
simulateWalkIns <- function(customerList, dateRange, index, t, promos){
  #readStores()
  storeID <- stores[[index]]$storeID
  print(paste("Calculations for Store: ", index, " with id: ", storeID, sep = ""))
  print(paste("Beginning Walk in simulation for store id: ", storeID, sep = ""))
  dates <- seq(dateRange[1], dateRange[2], by="1 day")
  purchasedItems <- list()
  # this walkIns vector stores the number of customers that walk in each day 
  # to make plotting easy
  walkIns <- c()
  revenue <- c()
  currentStore <- stores[[index]]  
  
  items <- currentStore$items
  currentItems <- getCurrentItems(items, t)
  promotions <- unlist(promos)
  currentItems <- updatePromotions(currentItems, promos)
  
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
    currentStore$customers <- walkingCustomers
    stores[[index]] <- currentStore
    #writeStores(stores)
     setStores(stores)
     
    p <- simulatePurchases(currentStore, currentItems)
    purchasedItems <- append(purchasedItems, p)
    if(length(currentStore$customers) == 0){
      prices <- 0
    }else{
      prices <- sapply(p, function(i){
        i$price
      })
    }
    revenue[i] <- sum(prices)
    
    currentStore$customers <- list()
    stores[[index]] <- currentStore
    #writeStores(stores)
     setStores(stores)
  }
  x <- WalkInInfo$new(dateSeq = dates, walkInNums = walkIns, purchaseRecord = purchasedItems,
                      revenuePerDay = revenue)
  print(paste("Length of purchased Items list: ", length(purchasedItems), sep=""))
  print("---------------------------------------")
  return (x)
}

# simulates the customers purchasing an item.
#   @param storeIndex - the index of the current store in the list of stores
simulatePurchases <- function(store, items){
  #readStores()
  probs <- sapply(items, function(item){
    item$probabilityOfPurchase
  })
  
  # NOTE: this solution seems very inefficient, should probably be made more elegant
  purchasedItems <- list()
  if(length(store$customers) == 0){
    return (purchasedItems)
  }
  for(i in c(1:length(store$customers))){
    cust <- store$customers[i]
    cust <- cust[[1]]
    numPurchases <- rpois(1,2)
    if(numPurchases != 0){
      for(j in c(1:numPurchases)){
        purchasedItems <- c(purchasedItems, cust$purchase(items, probs))
      } 
    }
  }
  return (purchasedItems)
}

# gets the customers with the given stores as their home store
#   @param index - the index of the store we want to get customers for in the list
getStoreCustomers <- function(index){
  #readStores()
  #readCustomers()
  id <- stores[[index]]$storeID
  if(length(stores) == 1){
    return (customers)
  }
  custs <- lapply(customers, function(c){
    if(c$homeStoreID == id){
      c
    }
  })
  custs <- custs[!sapply(custs, is.null)]
  return (custs)
}

# add items to the specified stores
#   This function adds items inputted by the user to the stores the user wants to
#   assign them to.
#
#   @param itemsInfo - the data frame of item infomation
#   @param dests - the stores that these additional items should be assigned to
addItems <- function(itemsInfo, dests){
  #readStores()
  items <- apply(itemsInfo, 1, function(itemInfo){
    itemInfo <- as.character(itemInfo)
    startTime <- as.POSIXct(itemInfo[4], format = "%H:%M:%S")
    endTime <- as.POSIXct(itemInfo[5], format = "%H:%M:%S")
    i <- Item$new(productId=itemInfo[1], name=itemInfo[2], price=as.numeric(itemInfo[3]),
                  timestamp=c(startTime, endTime), probabilityOfPurchase=as.numeric(itemInfo[6]))
  })
  
  lapply(dests, function(dest){
    index <- strsplit(dest, split = " ")
    index <- index[[1]]
    index <- as.numeric(index[2])

    print("Adding items to stores.")

    stores[[index]]$items <- append(stores[[index]]$items, items)
  })
  #writeStores(stores)
  assign("stores", stores, envir = .GlobalEnv)
}

# gets the items that are for sale at a given time
#   @param ites - the list of all items in the store
#   @param t - the current time
getCurrentItems <- function(ites, t){
  currentItems <- lapply(ites, function(item){
    itemTimings <- seq(item$timestamp[1], item$timestamp[2], by = "min")
    time <- as.POSIXct(t, format = "%H:%M:%S")
    if(time %in% itemTimings){
      item
    }
  })
  currentItems <- currentItems[!sapply(currentItems, is.null)]
  return (currentItems)
}