(in-package :nass.arch.4004)

(deftype word (&optional (size 1))
  "Words in 4004 instruction set is 8 bits long."
  `(nass-type:octet ,size))

(defparameter +op-codes+
  (plist-hash-table
   '(:NOP #x00
     :JCN #x01
     :FIM #x02
     :FIN #x03
     :JIN #x03
     :JUN #x04
     :JMS #x05
     :INC #x06
     :ISZ #x07
     :ADD #x08
     :SUB #x09
     :LD  #x0A
     :XCH #x0B
     :BBL #x0C
     :LDM #x0D
     :CLB #xF0
     :CLC #xF1
     :IAC #xF2
     :CMC #xF3
     :CMA #xF4
     :RAL #xF5
     :RAR #xF6
     :TCC #xF7
     :DAC #xF8
     :TCS #xF9
     :STC #xFA
     :DAA #xFB
     :KBP #xFC
     :DCL #xFD
     :SRC #x02
     :WRM #xE0
     :WMP #xE1
     :WRR #xE2
     :WPM #xE3
     :WR0 #xE4
     :WR1 #xE5
     :WR2 #xE6
     :WR3 #xE7
     :SBM #xE8
     :RDM #xE9
     :RDR #xEA
     :ADM #xEB
     :RD0 #xEC
     :RD1 #xED
     :RD2 #xEE
     :RD3 #xEF))
  "Mnemonic to opcode mapping for the 4004 CPU.")

