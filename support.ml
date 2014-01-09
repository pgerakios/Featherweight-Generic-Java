open Format

module Error = struct
exception Exit of int

exception Impossible  of string

let print_string = Pervasives.prerr_string
let print_int i = Pervasives.prerr_string (string_of_int i)

let debugMode = ref false 
let found_errors = ref false 

let foundErrors () = !found_errors

let strOf = Printf.sprintf

type info = FI of string * int * int | UNKNOWN
let i2s = 
  function
    FI(f,l,c) ->strOf "%s: %d.%d" f l c
  | UNKNOWN ->
      strOf "<Unknown file and line>: "
let i2p () = i2s 

let cyan    s = "\027[36m\027[1m" ^ s ^ "\027[0m" 
let red     s = "\027[31m\027[1m" ^ s ^ "\027[0m"
let magenta s = "\027[35m\027[1m" ^ s ^ "\027[0m"
let yellow  s = "\027[33m\027[1m" ^ s ^ "\027[0m"
let green   s = "\027[32m\027[1m" ^ s ^ "\027[0m"

type color = Cyan | Red | Magenta | Yellow | Green 

let cs2s (color, inf , str1, str2) =
   let fn  =
      begin match color with
          Cyan -> cyan
        | Red -> red
        | Magenta -> magenta
        | Yellow -> yellow
        | Green -> green
      end 
    
   in strOf "%s\n%s\n%s\n" (fn ("[" ^ str1 ^ "]")) 
      begin match inf with
           Some(info) -> strOf "%a\t%s" i2p info str2
         | None -> str2
      end
      (fn ("[/"^str1^"]"))


let cs2p () = cs2s

let cs2out cs = 
   Printf.fprintf stderr "%s" (cs2s cs);
   flush stderr
   
let er i format = 
  found_errors:=true;
  Printf.ksprintf 
    (fun x -> cs2out (Red, Some i, "ERROR", x)) format;
  raise (Exit 1)



let warn i format = 
  Printf.ksprintf
    (fun x -> Printf.fprintf stderr "\027[33m\027[1mWarning:\027[0m %s" 
     (strOf "%a\t%s" i2p i x);     
    flush stderr) format;
     raise (Exit 1)


let info format = 
  Printf.ksprintf
    (fun x -> cs2out (Green, None, "INFO", x)) format

let unimp format = 
  found_errors:=true;
  Printf.ksprintf 
    (fun x -> Printf.fprintf stderr "\027[31m\027[1mUnimplemented:\027[0m %s"
     x; 
    flush stderr) format;
     raise (Exit 1)


let dbg format = 
  Printf.ksprintf (fun x ->
    if !debugMode then cs2out (Yellow, None, "DEBUG", x)) format

let fmt2str : (string, unit, string, string) format4 -> string = 
 fun x -> Printf.ksprintf (fun x -> x) x

let raiseExn : 'b. (string -> exn) ->
                      (string, unit, string, string) format4 -> 'b =
 fun fn format ->
   let ex =  fn "????" in
   let str = 
      cs2s (Magenta, None, "EXCEPTION", 
      strOf "%s --- %s" (Printexc.to_string ex) (fmt2str format))
   in
      Printf.fprintf stderr "%s" str;
      flush stderr;
      raise ex


type 'a withinfo = {i: info; v: 'a}

let info2pos fi =
   begin match fi with 
     | FI(_, i, j) -> (i,j) 
     | UNKNOWN -> raise (Impossible "info2pos")
   end 

let dummyinfo = UNKNOWN

let createInfo f l c = FI(f, l, c)

let errf f = 
  print_flush(); 
  open_vbox 0; 
  open_hvbox 0; f(); print_cut(); close_box(); print_newline();
  raise (Exit 1)

let printInfo =
  (* In the text of the book, file positions in error messages are replaced
     with the string "Error:" *)
  function
    FI(f,l,c) ->
      print_string f; 
      print_string ":"; 
      print_int l; print_string "."; 
      print_int c; print_string ":"
  | UNKNOWN ->
      print_string "<Unknown file and line>: "

let errfAt fi f = errf(fun()-> printInfo fi; print_space(); f())

let err s = errf (fun()-> print_string "Error: "; print_string s; print_newline())

let error fi s = errfAt fi (fun()-> print_string s; print_newline())

let warning s =
  print_string "Warning: "; print_string s;
  print_newline()

let warningAt fi s =
  printInfo fi; print_string " Warning: ";
  print_string s; print_newline()



let setDebug b = debugMode:= b
let isDebug () = !debugMode
let onDebug f = 
   if !debugMode then f ()
   else ()


let dbgTrace str x = 
        onDebug ( fun _ ->
            Printf.fprintf stderr  
            "\n%sCaught exception: %s\n%s\n\n"
            (yellow ("[" ^ str ^ "]"))
            (red (Printexc.to_string x))
            (green  (Printexc.get_backtrace ())))

let dbgTry str fnCatch fnTry = 
   if not (isDebug ()) then  fnTry () 
   else 
   begin try
      fnTry () 
   with x -> 
      if fnCatch x then 
      begin
         dbgTrace str x;
         raise x
      end
      else raise x
   end


end

(* ---------------------------------------------------------------------- *)

module Pervasive = struct

type info = Error.info

let pr format =  
  Printf.ksprintf 
    (fun x -> Printf.fprintf stderr "%s" x;
     flush stderr) format

    type color = Cyan | Red | Magenta | Yellow | Green 

let cyan    s = "\027[36m\027[1m" ^ s ^ "\027[0m" 
let red     s = "\027[31m\027[1m" ^ s ^ "\027[0m"
let magenta s = "\027[35m\027[1m" ^ s ^ "\027[0m"
let yellow  s = "\027[33m\027[1m" ^ s ^ "\027[0m"
let green   s = "\027[32m\027[1m" ^ s ^ "\027[0m"


 let prc color format =  
  Printf.ksprintf 
    (fun x -> Printf.fprintf stderr "%s" 
      (begin match color with 
        Red -> red
      | Cyan -> cyan
      | Magenta -> magenta
      | Yellow -> yellow
      | Green -> green
      end x);
     flush stderr) format
 
   (*Pervasives.prerr_string*)
   (*Format.print_string*)

end (* module pervasive *)


