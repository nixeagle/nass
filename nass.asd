(asdf:defsystem :nass
  :depends-on (:nutils :convert :eos :flexi-streams :binary-data)
  :serial t
  :components
  ((:file "packages")
   (:file "global")
   (:file "types")
   (:module :convert
            :components
            ((:file "octets")))
   (:file "util")
   (:module :src
            :components
            ((:module :arch
                      :components
                      ((:module :4004
                                :components
                                ((:file "intel-4004")))))))
   (:file "nass")))