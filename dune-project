(lang dune 3.24)

(name lfc)

(generate_opam_files true)

; (source
;  (github username/reponame))

(authors "Nathan Farlow")

(maintainers "Nathan Farlow")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name lfc)
 (synopsis "Decode Leapster LFC voice streams to WAV")
 (depends
  (ocaml
   (>= 5.4))
  dune
  core
  core_kernel
  core_unix
  async
  ppx_jane))

