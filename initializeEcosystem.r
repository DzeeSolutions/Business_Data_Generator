# initializeEchosystem
#   sets up the simulation by creating all necessary stores, items, customers,
#   and any other necessary initial values
library(geosphere)
source("../Locality.r")
source("../Item.r")
source("../Consumer.r")
source("../Store.r")
source("../helperFunctions.r")

# stores variable holds all of our store objects
stores <- list()
# customers variable holds all of our customer objects
customers <- list()
# generic items variable holds the 30 generic items each store has
items <- list()

# these variables enable me to check if the file uploaded has been changed for 
# any field
prevItemData <- data.frame()
prevConsumerData <- data.frame()
prevStoreData <- data.frame()

# getStores
#   returns the list of stores
getStores <- function(){
  return (stores)
}

# setStores
#   sets the list of stores and assigns it to the global list of stores
#   @param s - the list of stores we wish to assign globally
setStores <- function(s){
  assign("stores", s, env = .GlobalEnv)
}

# initializes all of our store objects
#   @param n - the number of stores to initialize
initStores <- function(n){
  l <- list()
  if(length(stores) > n){
    l <- stores[1:n]
    print(paste("Closing ", (length(stores) - n), " stores.", sep = ""))
  }else if(length(stores) < n){
    l <- lapply(c(1:(n-length(stores))), function(i){
      Store$new(storeID = generateStoreID(),name=generateName(), 
                items = initItems(30), 
                location = Locality(sample(37:41, 1, replace = FALSE), 
                                    sample(103:106, 1, replace = FALSE)), 
                customers = list())
    })
    print(paste("Opening ", length(l), " more stores.", sep =""))
    l <- append(stores, l)
  }else{
    l <- stores
  }
 # writeStores(l)
  setStores(l)
}

# initializes stores from file
#   @param info - the data frame of store information to create stores from
initStoresFromFile <- function(info){
  s <- apply(info, 1, function(r){
    sInfo <- as.character(r)
    Store$new(storeID=sInfo[1], name=sInfo[2], items=initItems(length(items)),
              location=Locality(as.numeric(sInfo[3]), as.numeric(sInfo[4])))
  })
  #writeStores(s)
  assign("stores", s, envir = .GlobalEnv)
}


# Initializes random items to be added to stores
#   @param n - the number of items to create
initItems <- function(n){
  print(length(items))
  if(length(items) == n){
    return (items)
  }else if(length(items) < n){
    startTime <- as.POSIXct("06:00:00", format = "%H:%M:%S")
    endTime <- as.POSIXct("10:00:00", format = "%H:%M:%S")
    earlyWindow <- c(startTime, endTime)
    
    startTime <- as.POSIXct("10:01:00", format = "%H:%M:%S")
    endTime <- as.POSIXct("16:00:00", format = "%H:%M:%S")
    midWindow <- c(startTime, endTime)
    
    startTime <- as.POSIXct("16:01:00", format = "%H:%M:%S")
    endTime <- as.POSIXct("22:00:00", format = "%H:%M:%S")
    lateWindow <- c(startTime, endTime)
    windows <- list(earlyWindow, midWindow, lateWindow)
    
    plusItems <- lapply(c(1:(n-length(items))), function(x){
      Item$new(productId = generateItemID(), name = generateName(), price = round(runif(1, 1, 10), 2),
               timestamp = sample(windows, 1)[[1]], probabilityOfPurchase = 
                 round(runif(1, c(0:1)), 2))
    })
    items <- append(plusItems, items)
  }else{
    items <- items[1:n]
  }
  assign('items', items, env= .GlobalEnv)
  return (items)
}

# initializes the generic items based on a file
#   $param info - the data frame containing info about the items to create
initItemsFromFile <- function(info){
  it <- apply(info, 1, function(i){
    iInfo <- as.character(i)
    timeStart <- as.POSIXct(iInfo[4], format = "%H:%M:%S")
    timeEnd <- as.POSIXct(iInfo[5], format = "%H:%M:%S")
    Item$new(productId=iInfo[1], name=iInfo[2], price=as.numeric(iInfo[3]),
             timestamp=c(timeStart, timeEnd), probabilityOfPurchase=as.numeric(iInfo[6]))
  })
  assign("items", it, envir = .GlobalEnv)
}

# initializes the customers in the simulation
#   @param n - the number of customers to create
initCustomers <- function(n){
  custs<-list()
  if(length(customers) > n){
    custs <- customers[1:n]
    print(paste("Customers being reduced to ", length(custs)))
  }else if(length(customers) < n){
    custs <- lapply(c(1:(n-length(customers))), function(x){
      Consumer$new(consumerId = generateConsumerID(), name = generateConsumerName(),
                   age = round(runif(1, min = 10, max = 50)),
                   location = Locality(sample(37:41, 1, replace = TRUE), 
                                       sample(103:106, 1, replace = TRUE)))
    })
    print(paste(length(custs), " customers being added"))
    custs <- append(customers, custs)
  }else{
    custs <- customers
  }
  #writeCustomers(custs)
  assign("customers", custs, env = .GlobalEnv)
}

# initializes the customers from a file
#   @param info - the data frame of customer information
initCustomersFromFile <- function(info){
  custs <- apply(info, 1, function(c){
    custInfo <- as.character(c)
    Consumer$new(consumerId=custInfo[1], name=custInfo[2], age=as.numeric(custInfo[3]), 
                 location=Locality(as.numeric(custInfo[4]), as.numeric(custInfo[5])))
  })
  #writeCustomers(custs)
  assign("customers", custs, env = .GlobalEnv)
}

# calculates and sets the home store for each consumer
calculateHomeStores <- function(){
  print("Beginning calculation of home stores.")
  if(length(stores) == 0){
    print("Stores have not initialized yet.")
  }else if(length(stores) == 1){
    print("There is only one store, so all customers go there.")
    lapply(customers, function(cust){
      cust$homeStoreID <- stores[[1]]$storeID
    })
    assign("customers", customers, env = .GlobalEnv)
  }else{
    print("There are multiple stores. Calculating home stores.")
    
    storesTable <- data.frame(character(length(stores)), numeric(length(stores)),
                              numeric(length(stores)), stringsAsFactors = FALSE)
    names(storesTable) <- c("storeID", "Latitude", "Longitude")
    for(i in c(1:length(stores))){
      s <- stores[[i]]
      loc <- s$location
      storesTable[i,] <- c(s$storeID, getLatitude(loc), getLongitude(loc))
    }
    
    lapply(customers, function(cust){
      loc <- cust$location
      lat <- getLatitude(loc)
      long <- getLongitude(loc)
      coord <- c(long, lat)
      v <- apply(storesTable, 1, function(x){
        distCosine(coord, c(as.numeric(x[3]), as.numeric(x[2])))
      })
      cust$homeStoreID <- storesTable[match(min(v), v), 1]
    })
    #writeCustomers(customers)
    assign("customers", customers, env = .GlobalEnv)
  }
}