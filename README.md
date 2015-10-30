This is a business data generator. 

It uses random data generation to create an entire, functioning business model.

Program setup:

Locality - an object that stores a latitude and a longitude; will be used to store 
  locations in the simulation

Item - an object that represents something that can be bought; in this case it will
  be a food item
  
Menu - an object that stores a list of items and has other properties such as time
  that limit when those items can be bought; it also contains the probability of 
  each item being bought
  
Store - an object that has multiple menus and keeps track of the customers inside of
  it at all times; in our case, this object represents a restaurant
  
Consumer - an object that represents a person; this person will be able to walk in   to stores, purchase items, and more

walkInInfoObject - an object that contains how many people walked in each day and 
  a list of the items those people purchased; it keeps track of the results of all   the activities consumers can engage in inside a store
  
Promotions - an object that represents a discount or promotion that affects the     sale of items

Other important files:

initializeEchosystem - initializes all of the locations, items, menus, stores, and   consumers necessary for the simulation; it updates these objects as requested by   the user in the shiny application

simulations - runs all of the simulations in this model, such as walking in,       purchasing items, setting up promotions, and more

helperFunctions - some additional functions that make the rest of the code         simpler, such as creating random store/item/consumer ids

