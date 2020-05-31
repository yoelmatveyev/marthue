#::=  Thue version of 99 bottles of beer
#::=  Laurent Vogel,  http://lvogel.free.fr
#::=  (uses modified Thue: rhs == '~' prints newline)
#::=
#::=  for 999 bottles, add one more comma ',' to the last line :-)

_n::=_.n
.n::=~No more
_b::=_.b
.b::=~ bottle
_s::=_.s
.s::=~s
_o::=_.o
.o::=~ of beer
_w::=_.w
.w::=~ on the wall
_c::=_.c\
.c::=~,
_t::=_.t\
.t::=~Take one down, pass it around,
_d::=_.d\
.d::=~.
_\::=_.\
.\::=~

_*********,::=*********,_9
9::=~9
_********,::=********,_8
8::=~8
_*******,::=*******,_7
7::=~7
_******,::=******,_6
6::=~6
_*****,::=*****,_5
5::=~5
_****,::=****,_4
4::=~4
_***,::=***,_3
3::=~3
_**,::=**,_2
2::=~2
_*,::=*,_1
1::=~1
_,::=,_0
0::=~0

,_r::=_r,
*_r::=_r*
[_r::=[_

***,_m::=_r**,
,**,_m::=_r,*,
,*,_m::=_r,,
,,_m::=,_m*********,
[**,_m*::=[_*,*
[**,_mb::=_1bowd\*,bowc*,bodtnb
[*,_m::=[_

_g::=_bsowcrbsodtmbsowd\rg

::=

[*,,,_mg
