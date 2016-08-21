open Ocamlbuild_plugin
open Command

let os = Ocamlbuild_pack.My_unix.run_and_read "uname -s"

let system_support_lib = match os with
| "Linux\n" -> [A "-cclib"; A "-lrt"]
| _ -> []

let () =
  dispatch begin function
  | After_rules ->
      flag ["link"; "library"; "ocaml"; "byte"; "use_usane"]
        (S ([A "-dllib"; A "-lusane_stubs"] @ system_support_lib));
      flag ["link"; "library"; "ocaml"; "native"; "use_usane"]
        (S ([A "-cclib"; A "-lusane_stubs"] @ system_support_lib));
      flag ["link"; "ocaml"; "link_usane"]
        (A "src/libusane_stubs.a");
      dep ["link"; "ocaml"; "use_usane"]
        ["src/libusane_stubs.a"];
  | _ -> ()
  end
