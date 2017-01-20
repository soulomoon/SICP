#lang scheme
(require (planet "amb.ss" ("murphy" "amb.plt" 1 1)))
(amb-collect (list (amb 1 2 3) (amb 1 2 3)))