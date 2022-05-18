# Introduction

A semi-Thue system is a rewriting system over strings from a (usually finite) alphabet based on a simple set of string replacement rules, denoted in mathematics as *s → t*. If *s* is a substring of a given string *S*, it may be replaced by *t*. Rules may be applied in any order and if a certain string on the left side of the rule appears more than once as a substring in **S**, any possible replacement may take place. Consider the following system:
 
a → b

a → c

c → d
 
If this system is applied to "aa", it may result in:
 
Step 1: ba, ab, ac, ca

Step 2: **bb**, bc, cb, cc, ad, da

Step 3: **bd**, **db**, cd, dc

Step 4: **dd**
 
Possible "terminating" outcomes, to which none of the rules may be applied anymore, are marked as bold.
 
Consider a more simple system:
 
a → b

b → a
 
If applied to "aa", it never terminates, fluctuating forever randomly between "aa", "ab", "ba" and "bb".
 
Consider an even simpler system:
 
a → a
 
If applied to "aa" it does not seem to do anything, but it does not terminate either, endlessly transforming the same string to itself.
  
Strings on both sides of the rules may empty. An empty string on the left side results in random insertion of its right side into *S*, including attachment to the beginning or to the end of it.

Although this system in general is non-deterministic, it's proven to be Turing-complete. It is possible to perform in it deterministic computations of any complexity by cleverly designing the rules and the starting string.
 
The closely related and similar Markov algorithm, also known as "normal algorithm" is totally deterministic. Rules are applied in order, only the first occurrence of the substring from the left is considered on each step, after each step the list of replacements is checked again and again from the starts. Any rule, if applicable, may be marked as terminating. Non-terminating rules are denoted as *s → t*, while the terminating ones as *s →. t*. Consider the previous examples:

a → b

a → c

c → d
 
Applied to "aa":
 
Step 1: ba

Step 2: **bb**

The second example:

a → b

b → a
 
As with the semi-Thue system, it does not terminate, but makes a deterministic infinite loop:

aa->ba->bb->ab->aa

The third example, *a → a*, does not terminate either, while *a →. a* does after replacing "a" once by itself.

Markov algorithms are also known to be Turing-complete and due to their determinism often require much less steps and efforts for a given computation.

# Overcoming the limitations.

Both semi-Thue systems and Markov algorithms are used as Turing tarpits and for exercises in theoretical computer science. In 2000, John Colagioia invented the esoteric Thue language, which is basically a semi-Thue system interpreter augmented with an input and output facility. Interpreters of Markov algorithms have been implemented in nearly all mainstream programming languages. 

One major obstacle of writing considerably long programs in Thue is its non-determinism. All replacement rules remain simultaneously active. Markov algorithms may be easily augmented by input and output, but they suffer from somewhat different problems: replacements are looked for only from the beginning of the string and after each step the algorithm looks through the entire list of previously steps.

The Marthue (as in Markov+Thue) language was designed to overcome these problems.

A Marthue program is a sequence of independent Markov or Thue programs applied to the same string. After one programs is terminated, the computation passes on to the next one until the end of the sequence.

Each rule in each Markov or Thue block may be marked explicitly for searching a matching substring from the beginning, from the end or randomly. Each block may be marked as forward, backward or randomly searching by default. Otherwise, the rules follow the usual scheme: Thue rules are applied randomly, while Markov rules only forward from the left.

Each rule may cause input, output or both. A matching substring may be replaced by the user’s input. If the right side of the rule in not empty, the input is concatenated with it according to the current searching direction.

For output, the matching substring is removed and the replacing string is printed out. If a rule is marked for both input and output, the string on the right side is printed out and the matching string is replaced by the user’s input.

Each block may be labeled. Labels must be non-constant Lisp symbols, such as *a*, *a11*, but not strings, numbers, keys etc.

Each rule, if applicable, may call another block after its application as a function. The computation continues from there block by block until an explicit return operator. Function calls are recorded in a stack, allowing nested and recursive function calls.

An non-constant Lisp symbol following the two strings of the rule causes a call to the matching labeled block. The :R key reverses its meaning and uses the symbol as a return address, similar to the Lisp operator RETURN-FROM. The :R key without an address returns from the last call, similar to the Lisp operator RETURN.

Any attempt to call the current block or return to it is ignored. Markov and Thue algorithms are self-recursive in themselves. Recursion between the algorithms is possible, e.g. calling B from A and C from B until some rule invokes a return back to A.

If several blocks are identically labeled, the function call chooses randomly one of them.

Importantly, calling a non-existent function or return without a corresponding former call on the stack causes the entire program to halt.

# Syntax

While this Marthue implementation contains a loader for both Thue and Markov programs, they are internally converted to Lisp structures containing full information about the program, its original input and its current state, if the user wished to run it step by step or by a certain number of steps. The structure also contains the jump stack and the counter of steps.

As this implementation is written in Common Lisp, Marthue programs are internally be written in S-expressions. The code of each program is either a list or, preferably, an array of lists. Each list consists of its label list and its list or, preferably, an array of rules. Arrays are preferred as more efficient and economic data structures.

The syntax of the block’s label list may contain the following:

:T - for Thue vs. Markov

:M - for Markov vs. Thue (unnecessary, useful only for readability, as the blocks are Markov algorithms by default)

:F - forward search by default

:B - backward search by default

:X - random search

Both :B and :F mean random search by default

If the label list contains a Lisp symbol, it’s interpreted as its label.

For example,  (:T :B Thue1)  declares a modified Thue program, which searches for substrings backward by default and is labeled **Thue1**.

The rules array consists of rule lists, which contain the following:

1. Left-side substring. If empty, it must be explicitly written as "".

2. Replacement substring. If empty, it must be explicitly written as "".

3. Label symbol (optional).

Keys:

1. :B , :F or both for forward, backward or random search. Random search can also be denoted by :X.

2. :I, :O or both for input and output. When combined, the replacement string is printed and the left-side original string is replaced by the input.

3. :N ("next") for terminating the computation of the current block and going to the next block. If combined with a label, it means a conditional jump (the substring that trigers it is itself the condition). An attempt to jump to a non-existent label causes the entire program to stop.

4. :R for returning from a function call. When used with a label, if attempts to return to the corresponding block, somewhat similar to Lisp's special operator **return-from**. When the stack is empty, attempting to return stops the entire program. If the label is not found, the return attempt is ignored.

5. :C or whatever, just for the sake on convenience and readability, may denote a function call, although the presence of the label itself is sufficient and is interpreted by default as a function call. An attempt to call a non-existent block stops the entire program.

6. The combination of :R and :N is interpreted as return, except when the label is not found. In this case, the program jumps to the next block.

7. The combination of :R and :C is reserved for possible future extensions and is currently interpreted as :R.

Examples:

("_cat_" "Smokey" Thue_cat :F :O) means searching forward for "_cat_", removing it, printing out "Smokey" and jumping to a block named "Thue_cat".

("F" "apples" :F :B)  means randomly searching for "F" and replacing it by "apples"

("" "" Uncond_blk) - unconditional function call to Uncond_blk.

# Programming in internal format

Here is a simple example of a series of Markov-like programs, which ask for a string of binary digits and attach delimiters to it, followed by a Thue program for binary decrement, followed by another Markov-like block that prints out the result:

#(\
((:M) #(("" "Input a binary number:" :O :N)))\
((:M) #(("" "" :I :N)))\
((:M) #(("" "\_" :N)))\
((:M :B) #(("" "\_" :N)))\
((:T) #(("0_" "0--") ("1_" "0") ("10--" "01") ("00--" "0--1") ("\_1--" "@")("\_0--" "1") ("\_0" "")))\
((:M) #(("\_1" "\_*1")("\_0" "\_*0")("*1" "1" :O) ("*0" "0" :O)("\_" ""))))

# Marthue file format

All strings in Marthue program that don't contain "::" or "->" are treated as comments. Additionally, comments may be added after "::" in block description lines.

The "::" indicates either a block description or, when followed by "->", or a rule description with additional functionality. In both cases, the substring before "::" is treated as an opcode, optionally followed by a label. The opcode is a string made of the same letters as the internal Lisp format. Block termination may be denoted either as N::[original_string]->[new_string] or as [original_string]->.[new_string]. To use "->.", "::", "->" inside the rules, use backslashes. "\n" denotes a newline inside the rule. 

The above-described binary decrement as a file written in Mathue format:

:: Binary decrement\
ON::->Input a binary number:\
:: Input\
IN::->\
::\
N::->\_\
B::\
N::->\_\
T:: Thue algorithm for decrement\
0\_->0--\
1\_->0\
10--->01\
00--->0--1\
\_1--->@\
\_0--->1\
\_0->\
:: Printing the result\
\_1->\_*1\
\_0->\_*0\
O::*1->1\
O::*0->0\
\_->

# Running a program

Thue and Markov programs may be loaded by the functions (load-thue-program) and (load-markov-program). Marthue program by may be loaded directly from REPL or from a file by (load-marthue-program). 

To run the above given example in Marthue internal format, it's recommended to load it first to a variable:

CL-MARTHUE> (defparameter program1 (load-lisp-marthue\ 
#(\
((:M) #(("" "Input a binary number:" :O :N)))\
((:M) #(("" "" :I :N)))\
((:M) #(("" "\_" :N)))\
((:M :B) #(("" "\_" :N)))\
((:T) #(("0_" "0--") ("1_" "0") ("10--" "01") ("00--" "0--1") ("\_1--" "@")("\_0--" "1") ("\_0" "")))\
((:M) #(("\_1" "\_*1")("\_0" "\_*0")("*1" "1" :O) ("*0" "0" :O)("\_" ""))))))

To load a file in Marthue format, use the function (load-marthue-program "/FILE_PATH").

; Run it:

CL-MARTHUE> (marthue-run program1)
