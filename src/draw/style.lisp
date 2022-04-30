(in-package #:sudoku-diagrams-draw)

(defvar *title-font* "Helvetica")
(defvar *title-font-rgb* '(0.0 0.0 0.0))
(defvar *title-font-proportion* 0.6)

(defvar *row-column-label-font* "Helvetica")
(defvar *row-column-label-font-rgb* '(0.2 0.2 0.2))
(defvar *row-column-label-font-proportion* 0.3)

(defvar *digit-font* "Helvetica")
(defvar *digit-font-rgb* '(0.0 0.0 0.0))
(defvar *given-digit-font-proportion* 0.8)

(defvar *cell-stroke-rgb* '(0.4 0.4 0.4))
(defvar *cell-stroke-width* (points 1))
(defvar *cell-fill-rgb* '(1.0 1.0 1.0))
(defvar *box-stroke-rgb* '(0.2 0.2 0.2))
(defvar *box-stroke-width* (points 2))
(defvar *grid-stroke-rgb* '(0.0 0.0 0.0))
(defvar *grid-stroke-width* (points 4))

(defvar *highlight-fill-rgb* '(0.678 0.967 0.690))

(defvar *diamond-stroke-rgb* '(0.0 0.0 0.0))
(defvar *diamond-stroke-width* (points 0.5))
(defvar *diamond-proportion* 0.5)
(defvar *dark-diamond-fill-rgb* '(0.596 0.820 0.582))
(defvar *light-diamond-fill-rgb* '(1.0 1.0 1.0))

(defvar *circle-stroke-rgb* '(0.0 0.0 0.0))
(defvar *circle-stroke-width* (points 0.5))
(defvar *circle-proportion* 0.5)
(defvar *dark-circle-fill-rgb* '(0.596 0.820 0.582))
(defvar *light-circle-fill-rgb* '(1.0 1.0 1.0))
