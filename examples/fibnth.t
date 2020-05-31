#::= fibnth.t - Print the nth fibonacci number, n input in decimal
#::= (C) 2003 Laurent Vogel
#::= GPL version 2 or later (http://www.gnu.org/copyleft/gpl.html)

#::= Thue info at: http://www.catseye.mb.ca/esoteric/thue/
#::= This is modified thue where only foo::=~ outputs a newline.

#::= 'n' converts from decimal to unary-coded decimal (UCD).
n9::=*********,n
n8::=********,n
n7::=*******,n
n6::=******,n
n5::=*****,n
n4::=****,n
n3::=***,n
n2::=**,n
n1::=*,n
n0::=,n
n-::=-

#::= '.' prints an UCD number and eats it.
.*********,::=.~9
.********,::=.~8
.*******,::=.~7
.******,::=.~6
.*****,::=.~5
.****,::=.~4
.***,::=.~3
.**,::=.~2
.*,::=.~1
.,::=.~0

~9::=~9
~8::=~8
~7::=~7
~6::=~6
~5::=~5
~4::=~4
~3::=~3
~2::=~2
~1::=~1
~0::=~0

#::= messages moving over ',', '*', '|'
*<::=<*
,<::=<,
|<::=<|

0>*::=*0>
0>,::=,0>
0>|::=|0>
1>*::=*1>
1>,::=,1>
1>|::=|1>
2>*::=*2>
2>,::=,2>
2>|::=|2>

#::= Decrement n. if n is >= 0, send message '2>';
#::= when n becomes negative, we delete the garbage using 'z' and
#::= print the left number using '.'.
*,-::=,2>
,,-::=,-*********,
|,-::=z
z*::=z
z,::=z
z|::=.

#::= move the left number to the right, reversing it (0 becomes 9, ...)
#::= reversal is performed to help detect the need for carry. As the 
#::= order of rule triggering is undetermined in Thue, a rule matching 
#::= nine consecutive * would not work.
*c<::=c<0>
,c<::=c<1>
|c<::=2>
0>d*::=d
1>d::=d*********,

#::= when the copy is done, 'd' is at the right place to begin the sum.
2>d::=f<|

#::= add left to right. 'f' moves left char by char when prompted by a '<'.
#::= it tells 'g' to its right what the next char is.
*f<::=f*0>
,f<::=f,1>

#::= when done for this sum, decrement nth.
|f<::=-|

#::= upon receiving '0>' 'g' drops an 'i' that increments the current digit.
0>g::=ig
*i::=<
,i::=i,*********
|i::=<|********,*********

#::= '1>' tells 'g' to move left to the next decimal position, 
#::= adding digit 0 if needed (i.e. nine '*' in reversed UCD)
*1>g::=1>g*
,1>g::=<g,
|1>g::=|<*********g,

#::= '2>' tells 'g' that the sum is done. We then prepare for copy (moving 
#::= a copy cursor 'c' to the correct place at the beginning of the left 
#::= number) and reverse (using 'j') the right number back.
*2>g::=2>g*
,2>g::=2>g,
|2>g::=c|*********j

#::= 'j' reverses the right number back, then leaves a copy receiver 'd'
#::= and behind it a sum receiver 'g'.
*j*::=j
j,*::=,********j
j,,::=,*********j,
j,|::=<,dg|

#::= '?' used to input n
?::=:::

#::= the initial data is set up, so that after reading n ('?'), converting 
#::= it to UCD ('n') and decrementing it (-), message '2>' sent by '-' 
#::= will start when hitting 'g' the first swap + sum cycle
#::= for numbers 0 (left) and 1 (reversed, right).
::=

|n?-|,|********g,|

