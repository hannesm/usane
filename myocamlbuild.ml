open Ocamlbuild_plugin

let to_opt = List.fold_left (fun acc x -> [A "-ccopt"; A x] @ acc) []
let ccopt = to_opt [ "-O3" ; "-Wall" ]

let () =
  dispatch begin function
    | After_rules ->
      flag ["c"; "compile"] (S ccopt) ;
      flag ["link"; "library"; "ocaml"; "byte"; "use_usane"]
        (S ([A "-dllib"; A "-lusane_stubs"]));
      flag ["link"; "library"; "ocaml"; "native"; "use_usane"]
        (S ([A "-cclib"; A "-lusane_stubs"]));
      flag ["link"; "ocaml"; "link_usane"]
        (A "src/libusane_stubs.a");
      dep ["link"; "ocaml"; "use_usane"]
        ["src/libusane_stubs.a"];
    | _ -> ()
  end
