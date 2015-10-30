# this file contains all the code relating to promotions

# stores the previous promotion ids so inactive promotions are removed
prevPromotions <- c()

# adds a promotion given a product id for an item by raising the probability 
# of its purchase
updatePromotions <- function(products, ids){
  updatedProducts <- lapply(products, function(p){
    if(p$productId %in% ids && !(p$productId %in% prevPromotions)){
      print(p)
      print(paste("Applying promotion to products: ", p$productId, sep = ""))
      p$probabilityOfPurchase <- p$probabilityOfPurchase + 0.1 
      print(p)
    }else if(p$productId %in% prevPromotions && !(p$productId %in% ids)){
      print(p)
      print(paste("Removing promotions from product: ", p$productId, sep = ""))
      p$probabilityOfPurchase <- p$probabilityOfPurchase - 0.1
      print(p)
    }
    p
  })
  prevPromotions <- ids
  assign("prevPromotions", prevPromotions, envir = .GlobalEnv)
  return (updatedProducts)
}