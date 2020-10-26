;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Fig14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 300)
(define HEIGHT 300)
(define CENTER (/ WIDTH 2))

(define MTSCRN (empty-scene WIDTH HEIGHT))
(define DOT (circle 5 "solid" "red"))

(define (place-dot y) (place-image DOT CENTER y MTSCRN))
(define (swap-dot y) (place-image DOT y CENTER MTSCRN))
(define (change y ke) (- HEIGHT y))




(big-bang HEIGHT [to-draw place-dot] [on-tick sub1] [on-key change] [stop-when zero?])