package teste2;

import java.io.*; 
import java_cup.runtime.*;


import java.lang.System;

%%

%public
%class Lexer
%cup 

/*%implements java_cup.runtime.Scanner
%function next_token
%type java_cup.runtime.Symbol
%eofval{
  return new java_cup.runtime.new Symbol(new new Symbol(Sym.EOF);
%eofval}
%eofclose*/

%line
%column
%type java_cup.runtime.Symbol
%unicode

%eofval{ 
    return new Symbol(Sym.EOF) ;
%eofval}




identifier = {letter} ( {letter} | {unicode_digit} )*

illegal_identifier = {unicode_digit} ( {letter} | {unicode_digit} )*

letter        = {unicode_letter} | "_"
decimal_digit = [0-9]
octal_digit   = [0-7]
hex_digit     = [0-9] | [A-F] | [a-f]

newline        = \n
unicode_char   = [^\n]
unicode_char_dash   = [^"\""\n\\]

unicode_letter = [:letter:]
unicode_digit  = [:digit:]

int_lit     = {decimal_lit} | {octal_lit} | {hex_lit}
decimal_lit = [1-9] {decimal_digit}*
octal_lit   = "0"  {octal_digit}*
hex_lit     = "0" ( "x" | "X" ) {hex_digit} {hex_digit}*

float_lit = {decimals} "." {decimals}? {exponent}? | {decimals} {exponent} | "." {decimals} {exponent}?
decimals  = {decimal_digit}  {decimal_digit}*
/* ##################################### why not digit+ ########################################################### */
exponent  = ( "e" | "E" ) ("+" | "-")? {decimals}

imaginary_lit = ({decimals} | {float_lit}) "i"

rune_lit         = "'" ( {unicode_value} | {byte_value} ) "'"
unicode_value    = {unicode_char} | {little_u_value} | {big_u_value} | {escaped_char}
unicode_value_dash    = {unicode_char_dash} | {little_u_value} | {big_u_value} | {escaped_char}
byte_value       = {octal_byte_value} | {hex_byte_value}
octal_byte_value = "\\" {octal_digit} {octal_digit} {octal_digit}
hex_byte_value   = "\\" "x" {hex_digit} {hex_digit}
little_u_value   = "\\" "u" {hex_digit} {hex_digit} {hex_digit} {hex_digit}
big_u_value      = "\\" "U" {hex_digit} {hex_digit} {hex_digit} {hex_digit}
                          {hex_digit} {hex_digit} {hex_digit} {hex_digit}
escaped_char     = "\\" ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | "\\" | "'" | "\"" )
/* ##################################### weird quotes up and down here ########################################################### */

string_lit             = {raw_string_lit} | {interpreted_string_lit}
raw_string_lit         = "'" ( {unicode_char} | {newline} )* "'" /////////////ticks were here
interpreted_string_lit = "\"" ( {unicode_value_dash} | {byte_value}  )* "\""

/* $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ NOT IN GO SPECIFICATIONS $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]
WhiteSpace     = {LineTerminator} | [ \t\f]

Comment = {GeneralComment} | {LineComment}

GeneralComment   = "/*"([^*]|\*+[^*/])*\*+"/"
// Comment can be the last line of the file, without line terminator.
LineComment     = "//" {InputCharacter}* {LineTerminator}?

/* LAST MOMENT ADDITIONS */


%%



<YYINITIAL> { 
"func" {  return new Symbol(Sym.funcd); }
"break" { return new Symbol(Sym.breakd); }
"default" return new Symbol(Sym.defaultd); }
"interface" { return new Symbol(Sym.interfaced); }
"select" { return new Symbol(Sym.select); }
"case" { return new Symbol(Sym.cased); }
"defer" { return new Symbol(Sym.defer); }
"go" { return new Symbol(Sym.go); }
"map" { return new Symbol(Sym.map); }
"struct" { return new Symbol(Sym.struct); }
"chan" { return new Symbol(Sym.chan); }
"else" { return new Symbol(Sym.elsed); }
"goto" { return new Symbol(Sym.gotod); }
"package" { return new Symbol(Sym.packaged); }  // notice the little devil !!!
"switch" { return new Symbol(Sym.switchd); }
"const" { return new Symbol(Sym.constd); }
"fallthrough" { return new Symbol(Sym.fallthrough); }
"if" { return new Symbol(Sym.ifd); }
"range" { return new Symbol(Sym.range); }
"type" { return new Symbol(Sym.type); }
"continue" { return new Symbol(Sym.continued); }
"for" { return new Symbol(Sym.ford); }
"import" { return new Symbol(Sym.importd); }
"return" { return new Symbol(Sym.returnd); }
"var" { return new Symbol(Sym.var); }


"bool" |
"byte" |
"complex64" |
"complex128" |
"error" |
"float32" |
"float64" |
"int" |
"int8" |
"int16" |
"int32" |
"int64" |
"rune" |
"string" |
"uint" |
"uint8" |
"uint16" |
"uint32" |
"uint64" |
"uintptr" { }



"true"|
"false"|
"iota" { }

"nil" { }

"append" |
"cap" |
"close" |
"complex" |
"copy" |
"delete" |
"imag" |
"len" |
"make" |
"new" |
"panic" |
"print" |
"println" |
"real" |
"recover" { }



  {identifier} { return new Symbol(Sym.identifier, yytext()); }
  {int_lit} { return new Symbol(Sym.int_lit, yytext()); } 
  {float_lit} { return new Symbol(Sym.float_lit, yytext()); }
  {imaginary_lit} { return new Symbol(Sym.imaginary_lit, yytext()); } 
  {rune_lit} { return new Symbol(Sym.rune_lit, yytext()); }
  {string_lit} { return new Symbol(Sym.string_lit, yytext()); }


"+" {return new Symbol(Sym.plus_op);}
"&" {return new Symbol(Sym.amp_op);}
//"+=" |  // would be dealt in the parser as assign_op
//"&=" |
"&&" {return new Symbol(Sym.and_op);}
"==" {return new Symbol(Sym.equal_equal_op);}
"!=" {return new Symbol(Sym.not_equal_op);}
"(" {return new Symbol(Sym.open_brac);}
")" {return new Symbol(Sym.close_brac);}
"-" {return new Symbol(Sym.minus_op);}
"|" {return new Symbol(Sym.pipe_op);}
//"-=" |
//"|=" |
"||" {return new Symbol(Sym.or_op);}
"<" {return new Symbol(Sym.less_op);}
"<=" {return new Symbol(Sym.less_equal_op);}
"[" {return new Symbol(Sym.open_square_brac);}
"]" {return new Symbol(Sym.close_square_brac);}
"*" {return new Symbol(Sym.star_op);}
"^" {return new Symbol(Sym.pow_op);}
//"*=" |
//"^=" |
"<-" {return new Symbol(Sym.chan_op);}
">" {return new Symbol(Sym.greater_op);}
">=" {return new Symbol(Sym.greater_equal_op);}
"{" {return new Symbol(Sym.open_curly_brac);}
"}" {return new Symbol(Sym.close_curly_brac);}
"/" {return new Symbol(Sym.div_op);}
"<<" {return new Symbol(Sym.left_shift_op);}
//"/=" |
//"<<=" |
"++" {return new Symbol(Sym.inc_op);}
"=" {return new Symbol(Sym.equal);}
":="  {return new Symbol(Sym.short_ass);}
"," {return new Symbol(Sym.comma);}
";" {return new Symbol(Sym.semicolon);}
"%" {return new Symbol(Sym.mod_op);}
">>" {return new Symbol(Sym.right_shift_op);}
//"%=" |
//">>=" |
"--"  {return new Symbol(Sym.dec_op);}
"!" {return new Symbol(Sym.not_op);}
"..." {return new Symbol(Sym.tri_dot);}
"." {return new Symbol(Sym.dot);}
":" {return new Symbol(Sym.colon);}
//"&^=" |
"&^" {return new Symbol(Sym.amp_pow_op);}


  {Comment}                      { /* ignore */ }
  /*\n                             {append_tokens_to_file();}*/
  {WhiteSpace}                   { }
  /* error fallback */
  {illegal_identifier}           { }
  [^]                            { }
}

