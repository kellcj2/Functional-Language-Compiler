# Functional-Language-Compiler
x86_64 asm

* Integers:&nbsp; 1 in the least significant bit
* Booleans: 10 in the least two significant bits
* Tuples:&nbsp;&nbsp;&nbsp;&nbsp; 00 in the least two significant bits

## Language Grammar:

<b>&lt;program></b> := </br>
&nbsp;&nbsp;&nbsp;  | ((define main (input) &lt;expr>)) </br>
&nbsp;&nbsp;&nbsp;  | (&lt;definition list> (define main (input) &lt;expr>)) </br>
</br>
<b>&lt;definition list></b> := </br>
&nbsp;&nbsp;&nbsp;  | &lt;definition> </br>
&nbsp;&nbsp;&nbsp;  | &lt;definition> &lt;definition list> </br>
</br>
<b>&lt;definition></b> := </br>
&nbsp;&nbsp;&nbsp;  | (define &lt;identifier> () &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (define &lt;identifier> (&lt;parameter list>) &lt;expr>) </br>

<b>&lt;parameter list></b> := </br>
&nbsp;&nbsp;&nbsp;  | &lt;identifier> </br>
&nbsp;&nbsp;&nbsp;  | &lt;identifier> &lt;parameter list> </br>

<b>&lt;expr></b> := </br>
&nbsp;&nbsp;&nbsp;  | &lt;integer> </br>
&nbsp;&nbsp;&nbsp;  | &lt;identifier> </br>
&nbsp;&nbsp;&nbsp;  | true </br>
&nbsp;&nbsp;&nbsp;  | false </br>
&nbsp;&nbsp;&nbsp;  | (let (&lt;bindings>) &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (if &lt;expr> &lt;expr> &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (add1 &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (sub1 &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (is-num &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (is-bool &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (+ &lt;expr> &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (- &lt;expr> &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (* &lt;expr> &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (&lt; &lt;expr> &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (> &lt;expr> &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (== &lt;expr> &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (print &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | &lt;function app> </br>
&nbsp;&nbsp;&nbsp;  | (tup &lt;expr list>) </br>
&nbsp;&nbsp;&nbsp;  | (tup-len &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (tup-get &lt;expr> &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (is-tup &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (while &lt;expr> <expr>) </br>
&nbsp;&nbsp;&nbsp;  | (begin &lt;expr list>) </br>
&nbsp;&nbsp;&nbsp;  | (:= &lt;id> &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (= &lt;expr> &lt;expr>) </br>

<b>&lt;bindings></b> := </br>
&nbsp;&nbsp;&nbsp;  | (&lt;identifier> &lt;expr>) </br>
&nbsp;&nbsp;&nbsp;  | (&lt;identifier> &lt;expr>) &lt;bindings> </br>
</br>
<b>&lt;function app></b> := </br>
&nbsp;&nbsp;&nbsp;  | (&lt;identifier>) </br>
&nbsp;&nbsp;&nbsp;  | (&lt;identifier> &lt;expr list>) </br>
</br>
<b>&lt;expr list></b> := </br>
&nbsp;&nbsp;&nbsp; | &lt;expr> </br> 
&nbsp;&nbsp;&nbsp; | &lt;expr> &lt;expr list> </br>
