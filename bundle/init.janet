(import ./sbb :as s)

(defn install
  [manifest &]
  (s/ddumpf "bundle script: %s hook" "install"))

(defn check
  [&]
  (s/ddumpf "bundle script: %s hook" "check")
  (s/run-tests))

