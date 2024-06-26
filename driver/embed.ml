let () =
  match Array.to_list Sys.argv with
  | [_; "-src"; src; "-ml"; ml; "-mli"; mli] ->
    let src = open_in src in
    let mli = open_out mli in
    output_string mli "(* Generated by embed.ml *)\n";
    output_string mli "val value : string\n";
    let ml = open_out ml in
    output_string ml "(* Generated by embed.ml *)\n";
    output_string ml "let value =\n";
    begin try
        while true do
          let line = input_line src ^ "\n" in
          if String.length line < 2 || String.sub line 0 2 <> ";!" then
            output_string ml ("  \"" ^ String.escaped line ^ "\" ^\n")
        done
      with
      | End_of_file -> output_string ml "  \"\""
    end
  | _ ->
    prerr_endline "Usage: ocaml embed.ml -src <src> -ml <ml> -mli <mli>"
