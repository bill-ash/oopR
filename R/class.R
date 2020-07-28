# Build a constructor for a non-inventory item and a qb-add generic function

#' Insert helper
#'
#' @param table
#' @param qb_object
#'
#' @return
#'
#' @examples
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

# ***
# Constructors  -----------------------------------------------------------
# ***


#' Journal object
#'
#' @param RefNum
#' @param TxnDate
#' @param DebitAccount
#' @param DebitClass
#' @param DebitEntity
#' @param CreditAccount
#' @param CreditClass
#' @param CreditEntity
#' @param Amount
#' @param Memo
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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





#' Generic add function
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
qb_add <- function(x, ...) {
  UseMethod('qb_add')
}
# Create a method for the qb_add generic to add new journal entries to QuickBooks
qb_add.qb_journal <- function(x, ...) {

  journal <- list(
    debit = .insert_statement('JournalDebitLine', x),
    credit = .insert_statement('JournalCreditLine', x),
    header = .insert_statement('Journal', x)
  )

  return(journal)

}

qb_add.qb_nonInventoryItem <- function() {


}

# Create a method for printing journal entries
print.qb_journal <- function(x, ...) {
  cat('\nNew journal entry no. xxxx-xxx-xxx\n\n\n')
  dplyr::as_tibble(purrr::compact(x))
}


# Create a method for printing journal entries
print.qb_nonInventoryItem <- function(x, ...) {
  cat('\nNew journal entry no. xxxx-xxx-xxx\n\n\n')
  dplyr::as_tibble(purrr::compact(x))
}



