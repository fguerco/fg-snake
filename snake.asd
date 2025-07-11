(defsystem :snake
  :name "Simple snake game in Common Lisp"
  :version "1.0.0"
  :author "Felipe Guerço Oliveira <felipeguerco@gmail.com>"
  :license "MIT"
  :depends-on (:cl-tui :bordeaux-threads)
  :build-operation "asdf:program-op"
  :entry-point "fg-snake:run"
  :components ((:module "src"
                :serial t
                :components ((:file "core")
                             (:file "snake")))))
