# server class for shiny application

library(shiny)
source("../initializeEcosystem.r")
source("../simulations.r")

shinyServer(function(input, output, session){
  
  #renders sentence  that shows user the selected number of customers
  output$customerValue <- renderText({
    initCustomers(input$numCustomers)
    calculateHomeStores()
    paste("You have chosen to run the simulation with ", length(customers), 
          " customers.", sep = "")
  })
  
  output$storesTabs = renderUI({
    initStores(input$numStores)
    calculateHomeStores()
    updateSelectizeInput(session, "promotionSelect", choices = generateItemList(getCurrentItems(items, input$time)))
    updateSelectizeInput(session, "itemAddDest", choices = paste("Store", c(1:length(stores))))
    myTabs = lapply(c(1:input$numStores), function(x){
      s <- stores[[x]]
      tabPanel(paste("Store ", x), value = s$storeID, 
               plotOutput(paste("WalkInPlot", x, sep = "")), 
               plotOutput(paste("PurchasePlot", x, sep = "")),
               plotOutput(paste("RevenuePlot", x, sep = "")),
               textOutput(paste("TotalRevenue", x, sep = "")))
    })
    
    l <- list(id = "tabs")
    l <- append(l, myTabs)
    do.call(tabsetPanel, l)
  })
  
  output$storesValue <- renderText({
    numStores <- input$numStores
    input$numCustomers
    input$newItems
    
    # uploads items to the simulation
    if(!is.null(input$userItems)){
      newItemInfo <- input$userItems
      newItemInfo <- read.csv(newItemInfo$datapath, header = TRUE, sep = ",")
      if(!identical(prevItemData, newItemInfo)){
        initItemsFromFile(newItemInfo)
        st <- lapply(stores, function(s){
          s$items <- items
          s
        })
        setStores(st)
        updateSelectizeInput(session, "promotionSelect", choices = generateItemList(getCurrentItems(items, input$time)))
        prevItemData <- newItemInfo 
        assign("prevItemData", prevItemData, envir=.GlobalEnv)
      }
    }
    # uploads stores to the simulation
    if(!is.null(input$userStores)){
      newStoreInfo <- input$userStores
      newStoreInfo <- read.csv(newStoreInfo$datapath, header = TRUE, sep = ",")
      if(!identical(prevStoreData, newStoreInfo)){
        print("Initializing new stores from file.")
        initStoresFromFile(newStoreInfo)
        updateSliderInput(session, "numStores", value = length(stores))
        prevStoreData <- newStoreInfo 
        assign("prevStoreData", prevStoreData, envir = .GlobalEnv)
      }
    }
    # uploads customers to the simulation
    if(!is.null(input$userCustomers)){
      newConsumerInfo <- input$userCustomers
      newConsumerInfo <- read.csv(newConsumerInfo$datapath, header = TRUE, sep = ",")
      if(!identical(prevConsumerData, newConsumerInfo)){
        initCustomersFromFile(newConsumerInfo)
        updateSliderInput(session, "numCustomers", value = length(customers))
        prevConsumerData <- newConsumerInfo
        calculateHomeStores()
        assign("prevConsumerData", prevConsumerData, envir = .GlobalEnv)
      }
    }
    input$promotionSelect
    lapply(c(1:input$numStores), function(i){
      walkInName <- paste("WalkInPlot", i, sep = "")
      purchaseName <- paste("PurchasePlot", i, sep = "")
      revenueName <- paste("RevenuePlot", i, sep = "")
      revenueStatementName <- paste("TotalRevenue", i, sep ="")
      custs <- getStoreCustomers(i)
      print(paste("Store ", i ," has ",  length(custs), "customers."))
      print(paste("Total length of customers list: ", length(customers)))
      walkInObj <- simulateWalkIns(custs, input$days, i, input$time, input$promotionSelect)
      output[[walkInName]] <- renderPlot({
        print("Rendering Walk-In plot.")
        barplot(walkInObj$walkInNums, names = walkInObj$dateSeq, main = "Walk In Graph", xlab = "Dates", ylab = "Number of people walking in")
      })
      output[[purchaseName]] <- renderPlot({
        print("Rendering Purchase plot.")
        names <- sapply(walkInObj$purchaseRecord, function(i){
          i$name
        })
        barplot(table(names), main = "Purchased Items Record", xlab = "Items", ylab = "Number purchased")
      })
      output[[revenueName]] <- renderPlot({
        print("Rendering Revenue plot.")
        barplot(walkInObj$revenuePerDay, names = walkInObj$dateSeq, main = "Revenue per day ($)", xlab = "Dates", ylab = "$")
      })
      output[[revenueStatementName]] <- renderText({
        paste("The store earned $", sum(walkInObj$revenuePerDay), " over this period.", sep ="")
      })
    })
    paste("You have asked to create ", input$numStores, " stores.", sep ="")
  })
  
  output$itemFileStatus <- renderText({
    itemsInfo <- input$newItems
    if(is.null(itemsInfo)){
      "If you would like to add items unique to a store, please upload a file of items."
    }else{
      itemsInfo <- read.csv(itemsInfo$datapath, header = TRUE, sep = ",") 
      addItems(itemsInfo, input$itemAddDest)
      "Items have been added. If you want to add more unique items, upload another file."
    }

  })
  
  # renders promotion status and applies promotions
  # NEED TO SET UP REMOVE PROMOTION
  output$promotionStatus <- renderText({
    paste("You have selected the item with the productId: ", input$promotionSelect, sep = "")
  })
})