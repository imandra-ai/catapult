(lang dune 2.0)
(generate_opam_files true)
(license MIT)
(homepage "https://github.com/AestheticIntegration/catapult")
(authors "simon@imandra.ai")
(maintainers "simon@imandra.ai")
(source (github AestheticIntegration/catapult.git))
(name catapult)

(package
  (name catapult)
  (synopsis "Tracing system based on the Catapult/TEF format")
  (depopts mtime)
  (depends
    (odoc :with-doc)
    (ocaml (>= "4.08"))))
