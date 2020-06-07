;; Convert series of * to Roman numerals

#(((:M a) #(("" "Input series of * like **** ****** *******....
" :O :C b)))
((:M b) #(("" "_" :I :T)))
((:M) #(("*" "I") ("IIIII" "V") ("IIII" "IV") ("VV" "X") ("VIV" "IX")
        ("XXXXX" "L") ("XXXX" "XL") ("LL" "C") ("LXL" "XC") ("CCCCC" "D")
        ("CCCC" "CD") ("DD" "M") ("DCD" "CM") ("" "_" :T)))

; Print the result

((:M) #(("_M" "_*M") ("_D" "_*D") ("_C" "_*C") ("_L" "_*L") ("_X" "_*X")
        ("_V" "_*V") ("_I" "_*I") ("_ " "_* ")("*M" "M" :O)
        ("*D" "D" :O) ("*C" "C" :O) ("*L" "L" :O) ("*X" "X" :O)
        ("*V" "V" :O) ("*I" "I" :O) ("* " " " :O) (" " "")))

((:M)#(("_" "
Try again? (y/n)
" :O)("" "_" :T)))

; Going back, if the answers is "y"

((:M)#(("_" "" :I) ("y" "" :R a) ("n" "" :R end))))
