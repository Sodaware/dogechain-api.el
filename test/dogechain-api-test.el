;; Internal tests

(ert-deftest dogechain-api-test/can-create-simple-endpoint-without-params ()
  (should (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/"
                   (dogechain-api--build-simple-endpoint "testmethod"))))
