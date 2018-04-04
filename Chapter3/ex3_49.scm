#lang sicp
; Consider a database scenario where you have a table with
; person id and person profile. Then a table with account id,
; person id and account details. If 2 transactions happen
; concurrently: one trying to delete all a person from all accounts they hold,
; and another trying to print the details of a particular account,
; including the account holder's details. While the first one will hold
; the person row's lock, and then the account row lock, the 2nd one
; will do the reverse and cause a deadlock. The details aren't known
; beforehand that we can do ordered locking.