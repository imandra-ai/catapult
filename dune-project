(lang dune 2.0)
(generate_opam_files true)
(license MIT)
(homepage "https://github.com/AestheticIntegration/catapult")
(authors "simon@imandra.ai")
(maintainers "simon@imandra.ai")
(source (github AestheticIntegration/catapult.git))
(version 0.1)
(name catapult)

(package
  (name catapult)
  (synopsis "Tracing system based on the Catapult/TEF format")
  (depopts
    ptime
    mtime)
  (depends
    base-threads
    base-unix
    (odoc :with-doc)
    (ocaml (>= "4.08"))))

(package
  (name catapult-file)
  (synopsis "File logger for catapult")
  (depends
    (catapult (= :version))
    (odoc :with-doc)
    (ocaml (>= "4.08"))))

(package
  (name catapult-sqlite)
  (synopsis "Sqlite-based backend for Catapult tracing")
  (depends
    (sqlite3 (>= "5.0"))
    directories
    (catapult (= :version))
    (odoc :with-doc)
    (ocaml (>= "4.08"))))

(package
  (name catapult-client)
  (synopsis "Network client for catapult, to be paired with catapult-daemon")
  (depends
    (catapult (= :version))
    (odoc :with-doc)
    (zmq (>= "5.0"))
    (ocaml (>= "4.08"))))

(package
  (name catapult-daemon)
  (synopsis "Daemon for reliable multi-process logging with catapult")
  (depends
    (odoc :with-doc)
    (catapult (= :version))
    (catapult-sqlite (= :version))
    (zmq (>= "5.0"))
    (logs (>= "0.7"))))
