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


;; Internal tests

(ert-deftest dogechain-api-test/can-create-simple-endpoint-without-params ()
  (should (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/"
                   (dogechain-api--build-simple-endpoint "testmethod"))))

(ert-deftest dogechain-api-test/can-create-simple-endpoint-with-params ()
  (should (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/param-1"
                   (dogechain-api--build-simple-endpoint "testmethod" '("param-1"))))
  (should (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/param-1/param-2"
                   (dogechain-api--build-simple-endpoint "testmethod" '("param-1" "param-2")))))
