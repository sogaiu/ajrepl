(import ./sbb :as s)

(defn install
  [manifest &]
  (s/ddumpf "bundle script: %s hook" "install")
  (def [tos s] (s/get-os-stuff))
  (s/add-binscripts manifest [tos s]))

(defn check
  [&]
  (s/ddumpf "bundle script: %s hook" "check")
  (s/run-tests))

