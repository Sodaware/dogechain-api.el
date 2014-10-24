;; Internal tests

(ert-deftest dogechain-api-test/can-create-simple-endpoint-without-params ()
  (should (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/"
                   (dogechain-api--build-simple-endpoint "testmethod"))))

(ert-deftest dogechain-api-test/can-create-simple-endpoint-with-params ()
  (should (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/param-1"
                   (dogechain-api--build-simple-endpoint "testmethod" '("param-1"))))
  (should (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/param-1/param-2"
                   (dogechain-api--build-simple-endpoint "testmethod" '("param-1" "param-2")))))
