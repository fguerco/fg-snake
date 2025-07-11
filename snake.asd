(defsystem :snake
  :name "Simple snake game in Common Lisp"
  :version "1.0.0"
  :author "Felipe Guerço Oliveira <felipeguerco@gmail.com>"
  :license "MIT"
  :depends-on (:cl-tui :bordeaux-threads)
  :components ((:module "src"
                :serial t
                :components ((:file "snake")))))
