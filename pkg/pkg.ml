#use "topfind"
#require "topkg"
#require "topkg-jbuilder"

open Topkg

let () =
  Topkg_jbuilder.describe
    ~name:"alberto"
    ~readmes:[Pkg.std_file "README.md"]
    ~licenses:[Pkg.std_file "LICENSE"]
    ~change_logs:[Pkg.std_file "CHANGES"]
    ()
