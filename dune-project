(lang dune 3.8)

(name multicore-bench)

(generate_opam_files true)

(implicit_transitive_deps false)

(authors "Vesa Karvonen <vesa.a.j.k@gmail.com>")

(maintainers "Vesa Karvonen <vesa.a.j.k@gmail.com>")

(source
 (github ocaml-multicore/multicore-bench))

(homepage "https://github.com/ocaml-multicore/multicore-bench")

(license ISC)

(using mdx 0.4)

(package
 (name multicore-bench)
 (synopsis
  "Framework for writing multicore benchmark executables to run on current-bench")
 (depends
  (ocaml
   (>= 4.13.0))
  (domain-local-await
   (>= 1.0.1))
  (multicore-magic
   (>= 2.1.0))
  (mtime
   (>= 2.0.0))
  (yojson
   (>= 2.1.0))
  (domain_shims
   (>= 0.1.0))
  (mdx
   (and
    (>= 2.3.1)
    :with-test))
  (odoc
   (and
    (>= 2.2.0)
    :with-doc))))
