---
title: "S3 inventory class"
output: html_document
---



## Creating a small inventory class for QuickBooks

I've written a small S3 class for creating `qb_nonInventoryItem` objects. The 
`qb_add` generic generates some SQL to create new items in QuickBooks. 

### My constructor looks like this: 


```r
qb_nonInventoryItem <- function(FullName = NULL, 
                                RevAccount = NULL, 
                                ExpAccount = NULL, 
                                Cost = NULL, 
                                Price = NULL, 
                                Mfg = NULL, 
                                MfgPart = NULL, 
                                Description = NULL, 
                                CustDesc = NULL, 
                                Vendor = NULL, 
                                ... ) {
  item <- append(
    list(
      FullName = as.character(FullName),
      RevAccount = as.character(RevAccount),
      ExpAccount = as.character(ExpAccount),
      Cost = as.double(Cost),
      Price = as.double(Price),
      Mfg = Mfg,
      MfgPart = as.character(MfgPart),
      Description = Description,
      CustDesc = CustDesc,
      Vendor = Vendor
    ), list(...)
  )
  
  item <- structure(item, class = 'qb_nonInventoryItem')
  
  return(item)
}
```


### My generic and helper function: 


```r
qb_add <- function(x, 
                   verbose = TRUE, 
                   error_type = FALSE) {
  UseMethod('qb_add')
}

# First define a helper function for making the SQL 
.insert <- function(table = NULL,
                    qb_object = NULL) {
  if (is.null(table)) {
    stop("Must choose a table to insert.", call. = FALSE)
  }
  
  # remove NULL values 
  qb_object <- purrr::compact(qb_object)
  
  values <- purrr::map_if(qb_object, is.character, ~paste0("'", gsub("'", "''", .x), "'"))
  
  values <- paste0(values, collapse = ", ")
  
  ins_names <- paste0(names(qb_object), collapse = ", ")
  
  stmt <- paste0('INSERT INTO ', table, ' (', ins_names, ') VALUES (', values, ')')
  
  return(stmt)
}


# Method for my nonInventoryItem
qb_add.qb_nonInventoryItem <- function(.nonInventoryItem, 
                                       verbose = TRUE, 
                                       error_type = FALSE) {
  
  add_item <- .insert('NonInventoryItem', .nonInventoryItem)
  
  if (verbose == TRUE) {
    print(add_item)
  }
  
  # Leave these off for now
  #qb_resp <- RODBC::sqlQuery(.qodbcR$qb_con, add_item, errors = error_type)
  #list(error = return(qb_resp))
}
```



## Testing it out 


```r
qb_add(
  qb_nonInventoryItem(FullName = 'ScrewDriver', 
                      RevAccount = '40100', 
                      ExpAccount = '50100', 
                      Cost = 10.21, 
                      Price = 20.99, 
                      Mfg = 'Amazon', 
                      MfgPart = 'xx-1213-pp', 
                      Description = 'Normal screw drive', 
                      CustDesc = 'World\'s best screw driver!!1111', 
                      Vendor = 'BestBuy'
  ), verbose = TRUE)
```

```
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('ScrewDriver', '40100', '50100', 10.21, 20.99, 'Amazon', 'xx-1213-pp', 'Normal screw drive', 'World''s best screw driver!!1111', 'BestBuy')"
```

## Add many items 

This is the part that I'm struggling with. This works but feels more like a hack. 


```r
# Some data source 
test_data <- tibble::tibble(
  FullName = paste0('Product ', 1:10),
  RevAccount = '40100', 
  ExpAccount = '50100', 
  Cost = round(runif(10, .50, 100), 2), 
  Price = round(Cost * 1.5, 2),
  Mfg = paste0('Mfg ', 1:10),
  MfgPart = paste0(sample(letters, 10), '-xxxx'),
  Description = 'Some Description', 
  CustDesc = 'Customer Description', 
  Vendor = 'Vendor'
)

zz <- lapply(
  dplyr::group_split(test_data, FullName), function(x) {
    test_Items <- qb_nonInventoryItem(
      x$FullName, 
      x$RevAccount, 
      x$ExpAccount, 
      x$Cost, 
      x$Price, 
      x$Mfg, 
      x$MfgPart, 
      x$Description, 
      x$CustDesc, 
      x$Vendor
    )
    
  })

lapply(zz, qb_add)
```

```
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 1', '40100', '50100', 32.97, 49.45, 'Mfg 1', 'e-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 10', '40100', '50100', 52.29, 78.44, 'Mfg 10', 'q-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 2', '40100', '50100', 42.43, 63.64, 'Mfg 2', 'k-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 3', '40100', '50100', 22.44, 33.66, 'Mfg 3', 's-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 4', '40100', '50100', 12.88, 19.32, 'Mfg 4', 'd-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 5', '40100', '50100', 58.8, 88.2, 'Mfg 5', 'a-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 6', '40100', '50100', 74.59, 111.89, 'Mfg 6', 'p-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 7', '40100', '50100', 23.94, 35.91, 'Mfg 7', 'f-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 8', '40100', '50100', 65.77, 98.66, 'Mfg 8', 'y-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 9', '40100', '50100', 34.19, 51.28, 'Mfg 9', 'h-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
```

```
## [[1]]
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 1', '40100', '50100', 32.97, 49.45, 'Mfg 1', 'e-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## 
## [[2]]
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 10', '40100', '50100', 52.29, 78.44, 'Mfg 10', 'q-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## 
## [[3]]
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 2', '40100', '50100', 42.43, 63.64, 'Mfg 2', 'k-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## 
## [[4]]
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 3', '40100', '50100', 22.44, 33.66, 'Mfg 3', 's-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## 
## [[5]]
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 4', '40100', '50100', 12.88, 19.32, 'Mfg 4', 'd-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## 
## [[6]]
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 5', '40100', '50100', 58.8, 88.2, 'Mfg 5', 'a-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## 
## [[7]]
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 6', '40100', '50100', 74.59, 111.89, 'Mfg 6', 'p-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## 
## [[8]]
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 7', '40100', '50100', 23.94, 35.91, 'Mfg 7', 'f-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## 
## [[9]]
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 8', '40100', '50100', 65.77, 98.66, 'Mfg 8', 'y-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
## 
## [[10]]
## [1] "INSERT INTO NonInventoryItem (FullName, RevAccount, ExpAccount, Cost, Price, Mfg, MfgPart, Description, CustDesc, Vendor) VALUES ('Product 9', '40100', '50100', 34.19, 51.28, 'Mfg 9', 'h-xxxx', 'Some Description', 'Customer Description', 'Vendor')"
```

## Possible solution 

I think what I really want is to iterate across each element in my list 
by it's index?


```r
zd <- data.frame(arg1 = 1:3, 
                 arg2 = c('first', 'second', 'third'), 
                 arg3 = c(100.12, 32.12, 12.30))

for (i in 1:nrow(zd)) {
  print(paste0(names(zd), ' = ', zd[i,]))
}
```

```
## [1] "arg1 = 1"      "arg2 = first"  "arg3 = 100.12"
## [1] "arg1 = 2"      "arg2 = second" "arg3 = 32.12" 
## [1] "arg1 = 3"     "arg2 = third" "arg3 = 12.3"
```

```r
zl <- list(arg1 = 1:3, 
           arg2 = c('first', 'second', 'third'), 
           arg3 = c(100.12, 32.12, 12.30))

for (i in seq_along(zl)) {
  pp <- as.data.frame(zl)
  print(paste0(names(pp), ' = ', pp[i,]))
}
```

```
## [1] "arg1 = 1"      "arg2 = first"  "arg3 = 100.12"
## [1] "arg1 = 2"      "arg2 = second" "arg3 = 32.12" 
## [1] "arg1 = 3"     "arg2 = third" "arg3 = 12.3"
```
