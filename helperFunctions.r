# helperFunctions
#   this file holds some of the simple helper functions that help the simulation 
#   run

library(babynames)
prevConsumerID <- 4
prevItemID <- 8
prevStoreID <- 7
modulus <- 7999893

# generates random names for our objects
generateName <- function(length=5){
  name <- paste(sample(c(letters, LETTERS, 1:9),length, replace = TRUE),collapse = "")
  return (name)
}

# returns a name for a consumer
generateConsumerName <- function(){
  return (sample(babynames[,3], 1))
}

# generates a list of times(from 12:00AM to 11:00PM) that the parameters use
generateTimeList <- function(){
  times <- lapply(c(6:22), function(n){
    paste(n, ":00:00", sep = "")
  })
  return (unlist(times))
}

# generates a unique consumer id
generateConsumerID <- function(){
  nextNum <- (12*(prevConsumerID + 78))%%modulus
  assign("prevConsumerID", nextNum, env = .GlobalEnv)
  return(paste("c", nextNum, sep = ""))
}

# generates a unique item id
generateItemID <- function(){
  nextNum <- (13*(prevItemID + 67))%%modulus
  assign("prevItemID", nextNum, env = .GlobalEnv)
  return(paste("i", nextNum, sep = ""))
}

# generates a unique store id
generateStoreID <- function(){
  nextNum <- (11*(prevStoreID + 59))%%modulus
  assign("prevStoreID", nextNum, env = .GlobalEnv)
  return(paste("s", nextNum, sep = ""))
}

# finds where the store with the given id is in the list
getStoreIndex <- function(id){
  index <- -1
  for(i in c(1:length(stores))){
    s <- stores[[i]]
    if(s$storeID == id){
      print("Setting index variable in getStoreIndex.")
      index <- i
    }
  }
  print(paste("Index of store in list is ", index, sep = ""))
  return (index)
}

generateItemList <- function(it){
  names <- sapply(it, function(i){
    i$name
  })
  ids <- sapply(it, function(i){
    i$productId
  })
  result <- mapply(function(n, id){
    n = id
  }, names, ids)  
  
  return (result)
}

# writes the store list to a file
writeStores <- function(s){
  if(is.null(s)){
    print("Stores list object was null; not writing to file.")
  }else{
    save(s, ascii = FALSE, file = "../DataStores/storesList.txt") 
  }
}

# writes the customer list to a file
writeCustomers <- function(c){
  if(is.null(c)){
    print("Customers list object was null; not writing to file.")
  }else{
    save(c, ascii = FALSE, file = "../DataStores/customersList.txt") 
  }
}

# reads the store list from the file
readStores <- function(){
  load("../DataStores/storesList.txt")
}

# reads the customers list from the file
readCustomers <- function(){
  load("../DataStores/customersList.txt")
}