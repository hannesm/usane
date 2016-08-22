open Ocamlbuild_plugin

let () =
  dispatch begin function
  | After_rules ->
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
