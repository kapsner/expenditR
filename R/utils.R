addRow <- function(buyer, note, date, money){
  
  stopifnot(
    note != "",
    nchar(note) > 2,
    is.numeric(money),
    money > 0
  )
  
  outdat <- data.table::data.table(
    cbind(
      "Buyer" = buyer,
      "Note" = note,
      "Date" = date,
      "Money" = money
    ))
  return(outdat)
}

addRowPr <- function(buyer, debtor, note, date, money){
  
  stopifnot(
    note != "",
    nchar(note) > 2,
    is.numeric(money),
    money > 0
  )
  
  outdat <- data.table::data.table(
    cbind(
      "Buyer" = buyer,
      "Debtor" = debtor,
      "Note" = note,
      "Date" = date,
      "Money" = money
    ))
  return(outdat)
}