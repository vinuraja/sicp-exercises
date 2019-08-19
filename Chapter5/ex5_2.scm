#lang sicp
(controller
 test-counter
   (test (op >) (reg counter) (reg n))
   (branch (label factorial-done))
   (assign product (op mul) (reg product) (reg counter))
   (assign counter (op add) (reg counter) (const 1))
   (goto (label test-counter))
 factorial-done)