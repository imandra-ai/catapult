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
  (depends
    (odoc :with-doc)
    (ocaml (>= "4.08"))))

(package
  (name catapult-file)
  (synopsis "File logger for catapult")
  (depends
    mtime
    (catapult (= :version))
    (yojson (and (>= 1.6) (< 2.0)))
    (odoc :with-doc)
    (ocaml (>= "4.08"))))

(package
  (name catapult-client)
  (synopsis "Network client for catapult, to be paired with catapult-daemon")
  (depends
    mtime
    (catapult (= :version))
    (odoc :with-doc)
    (ocaml (>= "4.08"))))

(package
  (name catapult-daemon)
  (synopsis "Daemon for reliable multi-process logging with catapult")
  (depends
    (odoc :with-doc)
    (catapult (= :version))
    logs
    ocaml-protoc
    camlzip))
