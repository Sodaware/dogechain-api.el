;; Simple method tests

(ert-deftest dogechain-api-test/can-get-address-balance ()
  (with-mock
   (mock (dogechain-api--get-simple "addressbalance" "TEST_ADDRESS") => "123456.789")
   (should (= 123456.789 (dogechain-api-get-address-balance "TEST_ADDRESS")))))

(ert-deftest dogechain-api-test/can-get-address-hash ()
  (with-mock
   (mock (dogechain-api--get-simple "addresstohash" "TEST_ADDRESS") => "SOME_HASH_HERE")
   (should (string= "SOME_HASH_HERE" (dogechain-api-address-to-hash "TEST_ADDRESS")))))

(ert-deftest dogechain-api-test/valid-address-p-returns-true-when-valid ()
  (with-mock
   (mock (dogechain-api--get-simple "checkaddress" "TEST_ADDRESS") => "1E")
   (should (dogechain-api-valid-address-p "TEST_ADDRESS"))))

(ert-deftest dogechain-api-test/valid-address-p-returns-false-when-invalid ()
  (with-mock
   (mock (dogechain-api--get-simple "checkaddress" "INVALID_ADDRESS") => "CK")
   (should-not (dogechain-api-valid-address-p "INVALID_ADDRESS"))))

(ert-deftest dogechain-api-test/decode-address-returns-version-and-hash ()
  (with-mock
   (mock (dogechain-api--get-simple "decode_address" "address") => "1E:HASH")
   (let ((result (dogechain-api-decode-address "address")))
     (should (string= "1E" (assoc-default :version result)))
     (should (string= "HASH" (assoc-default :hash result))))))

(ert-deftest dogechain-api-test/can-get-block-count ()
  (with-mock
   (mock (dogechain-api--get-simple "getblockcount") => "123456")
   (should (eq 123456 (dogechain-api-get-block-count)))))

(ert-deftest dogechain-api-test/can-get-difficulty ()
  (with-mock
   (mock (dogechain-api--get-simple "getdifficulty") => "123.456")
   (should (= 123.456 (dogechain-api-get-difficulty)))))

(ert-deftest dogechain-api-test/can-get-received-by-address ()
  (with-mock
   (mock (dogechain-api--get-simple "getreceivedbyaddress" "TEST_ADDRESS") => "123456.789")
   (should (= 123456.789 (dogechain-api-get-received-by-address "TEST_ADDRESS")))))

(ert-deftest dogechain-api-test/can-get-sent-by-address ()
  (with-mock
   (mock (dogechain-api--get-simple "getsentbyaddress" "TEST_ADDRESS") => "123456.789")
   (should (= 123456.789 (dogechain-api-get-sent-by-address "TEST_ADDRESS")))))

(ert-deftest dogechain-api-test/can-convert-hash-address ()
  (with-mock
   (mock (dogechain-api--get-simple "hashtoaddress" "TEST_HASH") => "TEST_ADDRESS")
   (should (string= "TEST_ADDRESS" (dogechain-api-hash-to-address "TEST_HASH")))))

(ert-deftest dogechain-api-test/can-get-total-currency ()
  (with-mock
   (mock (dogechain-api--get-simple "totalbc") => "123456.789")
   (should (= 123456.789 (dogechain-api-get-total-currency)))))

(ert-deftest dogechain-api-test/get-transactions-returns-list ()
  (with-mock
   (mock (dogechain-api--get-simple-json "transactions") => (read-fixture-as-json "transactions.json"))
   (let ((transactions (dogechain-api-get-transactions)))
     (should (= 5 (length transactions))))))

(ert-deftest dogechain-api-test/get-transactions-contains-keys ()
  (with-mock
   (mock (dogechain-api--get-simple-json "transactions") => (read-fixture-as-json "transactions.json"))
   (let* ((transactions (dogechain-api-get-transactions))
          (first-block (elt transactions 0)))
     (should (= 500 (assoc-default :block first-block)))
     (should (= 1386475886 (assoc-default :timestamp first-block)))
     (should (= 1 (assoc-default :transactions first-block))))))

(ert-deftest dogechain-api-test/get-network-statistics-returns-list ()
  (with-mock
   (mock (dogechain-api--get-simple-json "nethash") => (read-fixture-as-json "nethash.json"))
   (let ((statistics (dogechain-api-get-network-statistics)))
     (should (= 5 (length statistics))))))

(ert-deftest dogechain-api-test/get-network-statistics-contains-keys ()
  (with-mock
   (mock (dogechain-api--get-simple-json "nethash") => (read-fixture-as-json "nethash.json"))
   (let* ((statistics (dogechain-api-get-network-statistics))
          (first-result (elt statistics 0)))
     (should (= 500 (assoc-default :block first-result)))
     (should (= 1386475886 (assoc-default :timestamp first-result)))
     (should (= 6901641034498895230248057944249341782018790077074986006051269912821760 (assoc-default :target first-result)))
     (should (= 69016706544110646121312590156214853227120998882806968922155747617576697 (assoc-default :average-target first-result)))
     (should (= 0.003 (assoc-default :difficulty first-result)))
     (should (= 16777472 (assoc-default :hashes-to-win first-result)))
     (should (= 301 (assoc-default :average-interval first-result)))
     (should (= 5579 (assoc-default :hashes-per-second first-result))))))


;;blockNumber,time,target,avgTargetSinceLast,difficulty,hashesToWin,avgIntervalSinceLast,netHashPerSecond


;; Internal tests

(ert-deftest dogechain-api-test/can-create-simple-endpoint-without-params ()
  (should (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/"
                   (dogechain-api--build-simple-endpoint "testmethod"))))

(ert-deftest dogechain-api-test/can-create-simple-endpoint-with-params ()
  (should (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/param-1"
                   (dogechain-api--build-simple-endpoint "testmethod" '("param-1"))))
  (should (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/param-1/param-2"
                   (dogechain-api--build-simple-endpoint "testmethod" '("param-1" "param-2")))))

(ert-deftest dogechain-api-test/create-simple-endpoint-does-not-add-empty-params ()
  (should (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/"
                   (dogechain-api--build-simple-endpoint "testmethod" '(nil nil nil)))))
