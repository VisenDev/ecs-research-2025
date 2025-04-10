(asdf:defsystem
  "ecs-research-2025"
  :description "Honors Research Project"
  :version "0.0.1"
  :author "Robert Burnett"
  :license "MIT"
  :depends-on ("uiop" "asdf")
  :components
  ((:file "macros")
   (:file "testing")
   (:file "main" :depends-on ("macros" "testing")))
  )

(asdf:defsystem
  "ecs-research-2025/executable"
  :build-operation program-op
  :build-pathname "a.out" ;; shell name
  :entry-point "cl-user::run-tests" ;; thunk
  :depends-on ("ecs-research-2025")
  :components
  ((:file "main"))
  )

