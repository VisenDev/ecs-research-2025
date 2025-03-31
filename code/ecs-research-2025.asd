(asdf:defsystem
  :ecs-research-2025
  :description "Honors Research Project"
  :version "0.0.1"
  :author "Robert Burnett"
  :license "MIT"
  ;:serial t
  :components
  ((:file "macros")
   (:file "testing")
   (:file "main" :depends-on ("macros" "testing")))
  )
