;; Convert stars to Roman numerals

#(((:M) #(("" "Input a series of stars like ****....
" "," :O)))
((:M) #(("" "_" "," :I)))
((:M) #(("*" "I") ("IIIII" "V") ("IIII" "IV") ("VV" "X") ("VIV" "IX")
        ("XXXXX" "L") ("XXXX" "XL") ("LL" "C") ("LXL" "XC") ("CCCCC" "D")
        ("CCCC" "CD") ("DD" "M") ("DCD" "CM") ("" "_" ",")))

; Print the result

((:M) #(("_M" "_*M") ("_D" "_*D") ("_C" "_*C") ("_L" "_*L") ("_X" "_*X")
        ("_V" "_*V") ("_I" "_*I") ("*M" "M" :O)
        ("*D" "D" :O) ("*C" "C" :O) ("*L" "L" :O) ("*X" "X" :O)
        ("*V" "V" :O) ("*I" "I" :O)))

((:M)#(("_" "
Try again? (y/n)
" :O)("" "_" ",")))

; Going back, if the answer is "y"

((:M)#(("_" "" :I)("y" "" "^")("n" "" ","))))
