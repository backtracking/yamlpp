(*
 * yamlpp - a simple HTML preprocesseur
 * Copyright (C) 2001 Jean-Christophe FILLI�TRE
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU General Public License version 2 for more details
 * (enclosed in the file GPL).
 *)

(*s This is yamlpp, a very simple HTML preprocesseur inspired by
    Nicolas Thiery's htmlpp (see \verb!htmlpp.sourceforge.net!). This
    is a simpler version with only languages, \verb!#def! and \verb!#ifdef!
    features. *)

{

open Printf
open Lexing

(*s Command line options: selected language and output channel. Output
    is set to standard output by default. It may be set to a file with
    [set_output_to_file]. [close_output] closes the output channel if
    different from the standard output. [output_string] prints a
    string on the output channel. *)

let lang = ref "fr"
let out_channel = ref stdout

let set_output_to_file f = 
  out_channel := open_out f

let close_output () =
  if !out_channel != stdout then close_out !out_channel

let output_string s = Pervasives.output_string !out_channel s

(*s Macros are stored in the hash table [macros]. *)

let macros = (Hashtbl.create 97 : (string,string) Hashtbl.t)

let add_macro = Hashtbl.add macros

let find_macro m = 
  try Hashtbl.find macros m 
  with Not_found -> eprintf "*** warning: undefined macro %s\n" m; ""

let is_macro = Hashtbl.mem macros

(*s Buffer for macros' definitions. *)

let mbuf = Buffer.create 1024

(*s Function to extract the macro name in a tag \verb!<#def name>! or
    \verb!<#ifdef name>!. *)

let extract_name =
  let r = Str.regexp "\\([a-zA-Z0-9]+\\)[ \t\n]*>" in
  fun s -> 
    try
      let _ = Str.search_forward r s 0 in Str.matched_group 1 s
    with Not_found -> 
      assert false

(*s The reference [skip_tag] contains the name of the closing tag that
    should stop the skipping of input. It is used for both languages
    and \verb!#ifdef! tags. The boolean [serious] indicates whether the
    beginning of the document (tag \verb!<html>!) was already encountered. *)

let skip_tag = ref ""

}

(*s Regular expressions shortcuts. *)

let space = [' ' '\t' '\n']
let ident = ['0'-'9' 'a'-'z' 'A'-'Z']+
let lang = "fr" | "en" | "it"

(*s The entry point of the filter is [process]. If we encounter a
    \verb!#def! then we read the body in buffer [mbuf] with entry
    [def_body] and we store the new macro with [add_macro], possibly
    hiding a previous macro with same name.  If we encounter a
    \verb!#ifdef! then we skip everything until \verb!#/ifdef!. *)

rule process = parse
  | "<#def" space+ ident space* ">"
      { let m = extract_name (lexeme lexbuf) in
	Buffer.clear mbuf;
	def_body lexbuf;
	add_macro m (Buffer.contents mbuf);
	process lexbuf }
  | "<#ifdef" space+ ident space* ">"
      { let m = extract_name (lexeme lexbuf) in
	if not (is_macro m) then begin 
	  skip_tag := "ifdef"; skip_until lexbuf
	end;
	process lexbuf }
  | "<#" lang ">" 
      { let s = lexeme lexbuf in
	let l = String.sub s 2 (String.length s - 3) in
	if l <> !lang then begin skip_tag := l; skip_until lexbuf end;
	process lexbuf }
  | "</#" ident ">"
      { process lexbuf }
  | "<#" ident ">"
      { let s = lexeme lexbuf in
	let m = String.sub s 2 (String.length s - 3) in
	let body = find_macro m in
	let lb = from_string body in
	process lb;
	process lexbuf }
  | eof 
      { () }
  | _ 
      { output_string (lexeme lexbuf); process lexbuf }

(*s Reads a macro body and stores it in buffer [mbuf]. *)

and def_body = parse
  | "</#def>"
      { () }
  | eof 
      { eprintf "Error: Unterminated macro definition\n"; exit 1}
  | _
      { Buffer.add_string mbuf (lexeme lexbuf); def_body lexbuf }

(*s Skips input until a closing tag with name [!skip_tag]. *)

and skip_until = parse
  | "</#" ident ">"
      { let s = lexeme lexbuf in
	let flag = String.sub s 3 (String.length s - 4) in
	if flag <> !skip_tag then skip_until lexbuf }
  | eof 
      { eprintf "*** warning: couldn't find end of flag %s\n" !skip_tag; 
	flush stderr }
  | _ 
      { skip_until lexbuf }

{

(*s Processing a file is just calling [process]. *)

let process_file f = 
  let c = open_in f in
  let lb = from_channel c in
  process lb;
  close_in c

(*s Usage with minimal documentation. *)

let usage () =
  prerr_endline "usage: yamlpp [-l lang] [-o file] files";
  prerr_endline "";
  prerr_endline "Languages:        <#fr> ... </#fr> <#en> ... </#en>";
  prerr_endline "Macro definition: <#def m> ... </#def>";
  prerr_endline "Macro use:        <#m>";
  prerr_endline "Macro test:       <#ifdef m> ... </#ifdef>";
  prerr_endline "Predefined macro: <#language>";
  prerr_endline "";
  prerr_endline "Copyright (C) 2001 Jean-Christophe FILLI�TRE";
  exit 1

(*s Parsing of the command line. *)

let rec parse = function
  | [] -> []
  | ("-h" | "-?" | "-help" | "--help") :: _ -> usage ()
  | ("-l" | "--language") :: l :: args -> lang := l; parse args
  | ("-l" | "--language") :: [] -> usage ()
  | ("-o" | "--output") :: f :: args -> set_output_to_file f; parse args
  | ("-o" | "--output") :: [] -> usage ()
  | f :: args -> f :: (parse args)

(*s Main program. *)

let main () = 
  let files = parse (List.tl (Array.to_list Sys.argv)) in
  add_macro "language" !lang;
  List.iter process_file files;
  close_output ()
	  
let _ = Printexc.catch main ()

}