
<!-- README.md is generated from README.Rmd. Please edit that file -->

# oopR

<!-- badges: start -->

<!-- badges: end -->

A how to guide for implementing a S3 object oriented programming style
in R.

Building data structures in R.

limited number of complex objects.

``` r

library(tibble)

print

UseMethod('print')

print.data.frame
```

## Simple exercise

``` r

# Creates a constructor with a class of 'qb_journal'
qb_journal <- function(RefNum = NULL, 
                       TxnDate = NULL, 
                       DebitAccount = NULL, 
                       DebitClass = NULL, 
                       DebitEntity = NULL,
                       CreditAccount = NULL, 
                       CreditClass = NULL, 
                       CreditEntity = NULL, 
                       Amount = NULL, 
                       Memo = NULL, 
                       ...) {
  journal <- structure(
    list(
      RefNum= RefNum,
      TxnDate = TxnDate,
      DebitAccount= DebitAccount,
      DebitClass= DebitClass,
      DebitEntity = DebitEntity,
      CreditAccount = CreditAccount,
      CreditClass = CreditClass,
      CreditEntity= CreditEntity,
      Amount= Amount,
      Memo= Memo
    ), class = 'qb_journal'
  )
  
  return(journal)
}

# Build a journal entry object 
journal_entry <- qb_journal(
  RefNum = '100',
  TxnDate = as.Date('2020-01-01'), 
  DebitAccount = 'Advertising Expense',
  CreditAccount = 'Accounts Payable',
  Amount = 100.23,
  Memo = 'Advertising expense for new ad campaign'
)

# Confirm the class == qb_journal and has a names attribute with all of the 
# variables need for creating a journal entry 
attributes(journal_entry)

# Create a generic qb_add function for adding new information  to QuickBooks
qb_add <- function(x, ...) {
  UseMethod('qb_add')
}

# Helper insert statement - will format the SQL for creating a new journal entry. 
.insert_statement <- function(table = NULL, 
                              qb_object = NULL) {
  if (is.null(table)) {
    stop("Must choose a table to insert.", call. = FALSE)
  }
  
  qb_object <- purrr::compact(qb_object)
  
  values <- purrr::map_if(qb_object, is.character, ~paste0("'", gsub("'", "''", .x), "'"))
  
  values <- paste0(values, collapse = ", ")
  
  ins_names <- paste0(names(qb_object), collapse = ", ")
  
  stmt <- paste0('INSERT INTO ', table, ' (', ins_names, ') VALUES (', values, ')')
  
  return(stmt)
}
# Validate there is good SQL 
.insert_statement('SomePath', list(RefNum = '100', Date = '2020-01-01', Amount = 100))
.insert_statement('SomePath', journal_entry)

# Create a method for the qb_add generic to add new journal entries to QuickBooks
qb_add.qb_journal <- function(x, ...) {
  
  journal <- list(
    debit = .insert_statement('JournalDebitLine', x),
    credit = .insert_statement('JournalCreditLine', x),
    header = .insert_statement('Journal', x)
  )
  
  return(journal) 
  
}

# Create a method for printing journal entries 
print.qb_journal <- function(x, ...) {
  cat('\nNew journal entry no. xxxx-xxx-xxx\n\n\n')
  dplyr::as_tibble(purrr::compact(x))
}

# Test it out: 
print(journal_entry)
# You do not need to wrap the object in print to get the desired behavior
# of your generic print function 
journal_entry

# The output 
qb_add(journal_entry)
```

``` r
# Looking at the Date method for the generic function print reveals a `max` 
# argument. This means when printing Date objects, we can specify an additional 
# argument `max` to print that will limit the length of the Date vector printed 
# to the consle. 
print.Date

x_date <- as.Date('2020-01-01')
dataframe_dates <- data.frame(dates = c(as.Date('2020-01-01'), '2020-05-02', '2022-05-06'))

attributes(x_date)
typeof(x_date)
unclass(x_date)

attributes(dataframe_dates)
typeof(dataframe_dates)
unclass(dataframe_dates)

attributes(dataframe_dates$dates)
typeof(dataframe_dates$dates)
unclass(dataframe_dates$dates)

print(x_date, max = 10)
print(dataframe_dates$dates, max = 1)
```

## Getting started

Below are a list of helpful links for learning more about object
oriented programming, the benefits, and how to do it in R.

  - <https://adv-r.hadley.nz/s3.html>
  - <https://rstudio-education.github.io/hopr/s3.html>
  - <https://www.datamentor.io/r-programming/object-class-introduction/>
  - <https://www.cyclismo.org/tutorial/R/s3Classes.html>

SO -
<https://stackoverflow.com/questions/46393146/r-creating-s3-object-with-separate-internal-and-external-names>

## S3

S3 allows you to overload functions by defining different methods for
one generic. The arguments for each method must contain all the
arguments to the generic. Both the generic and the method should contain
a … argument.

Use `ls.str()` to get a better picture of what your list or environemnt
object contain.

``` r
pp <- list(a = 1, b = c(2, 5, 6, 7), c = 3)
ls.str(pp)
```

The `print` generic function could have an unlimited number of methods
using the convention outlined below.

``` r

someObject <- function(x, y) {
  structure(
    list(
      x = x, 
      y = y
    ), class = 'someObject'
  )
}

print.someObject <- function(x, ...) {
  cat('Print is a generic function.\n')
  cat('You can extend the utility of print by creating new methods.\n')
  cat('Create a new method by writing a function like:\n    `print.new_method`.\n\n')
  
  value <- unlist(x)
  key <- names(x) 
  
  z <- paste0(key, ' = ', value, collapse = '\n')
  cat(z)
  
}

the_object <- someObject(x = 'howdy', y = 'partner')

is.object(the_object)
sloop::otype(the_object)
```

for the generic.
