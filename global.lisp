(defpackage #:nass.global
  (:use :cl :eos)
  (:documentation "This package is assumed to be loaded before any other
  package in the nass system. This defines things like master test suites
  and global data."))
(in-package :nass.global)
(def-suite :nass
    :description "Master suite for the assembler tests.")