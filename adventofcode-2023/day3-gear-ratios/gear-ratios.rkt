#lang racket/base
(require racket/file)
(require math/array)

(define (sub-it line) (substring line 1 4))

(define (load_puzzle name)
  (file->string name))

(sub-it
  (load_puzzle "puzzle_input"))

; (array-ref (array #[#[1 2] #[10 20]]) #[1 1])

