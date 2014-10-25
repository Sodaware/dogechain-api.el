;; Simple method tests

(ert-deftest dogechain-api-test/can-get-address-balance ()
  (with-mock
   (mock (dogechain-api--get-simple "addressbalance" "TEST_ADDRESS") => (read-fixture-as-string "getaddressbalance-test_address.txt"))
   (should (= 123456.789 (dogechain-api-get-address-balance "TEST_ADDRESS")))))

(ert-deftest dogechain-api-test/can-get-address-hash ()
  (with-mock
   (mock (dogechain-api--get-simple "addresstohash" "TEST_ADDRESS") => "SOME_HASH_HERE")
   (should (string= "SOME_HASH_HERE" (dogechain-api-address-to-hash "TEST_ADDRESS")))))


;; Internal tests

(ert-deftest dogechain-api-test/can-create-simple-endpoint-without-params ()
  (should (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/"
                   (dogechain-api--build-simple-endpoint "testmethod"))))

(ert-deftest dogechain-api-test/can-create-simple-endpoint-with-params ()
  (should (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/param-1"
                   (dogechain-api--build-simple-endpoint "testmethod" '("param-1"))))
  (should (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/param-1/param-2"
                   (dogechain-api--build-simple-endpoint "testmethod" '("param-1" "param-2")))))
