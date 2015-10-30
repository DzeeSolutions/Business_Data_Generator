# walk in info object
#   contains info about the number of people that walked in each day and records their
#   purchases over that time

WalkInInfo <- setRefClass("WalkInInfo", fields = list(
  dateSeq = "Date",walkInNums = "numeric", purchaseRecord = "list", revenuePerDay = "numeric"
))