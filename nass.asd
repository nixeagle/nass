(asdf:defsystem :nass
  :depends-on (:nutils :eos :flexi-streams :binary-data)
  :serial t
  :components
  ((:file "packages")
   (:file "global")
   (:file "types")
   #+ () (:module :convert
            :components
            ((:file "octets")))
   (:file "util")
   (:module :src
            :serial t
            :components
            ((:file "c-types")
             (:file "abstract")
             (:module :arch
                      :serial t
                      :components
                      ((:module :4004
                                :components
                                ((:file "intel-4004")))
                       (:file "x86oids")
                       (:module :x86oid
                                :components
                                ((:file "mod-reg-rm-types")))
                       (:module :i8086
                                :components
                                ((:file "init-i8086")))))))
   (:file "nass")))