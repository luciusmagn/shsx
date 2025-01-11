#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("shsx/lib"
    (exe: "shsx/main" bin: "shsx")))
