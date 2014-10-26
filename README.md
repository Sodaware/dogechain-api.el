# dogechain-api

## Description

**dogechain-api** is an Emacs library for working with the dogechain.info API.

It's not a particularly useful extension on its own, as there are no interactive
functions, but can be used to build something more interesting.

## Library Functions

The DogeChain API is split into two sections, a simple query API and a JSON
api.

### Simple Query API

The DogeChain simple query API docs can be found here:
http://dogechain.info/api/simple

The API functions are mapped as follows:

API Method           | Local Function
---------------------|--------------------------------------
addressbalance       | dogechain-api-get-address-balance
addresstohash        | dogechain-api-address-to-hash
checkaddress         | dogechain-api-valid-address-p
decode_address       | dogechain-api-decode-address
getblockcount        | dogechain-api-get-block-count
getdifficulty        | dogechain-api-get-difficulty
getreceivedbyaddress | dogechain-api-get-received-by-address
getsentbyaddress     | dogechain-api-get-sent-by-address
hashtoaddress        | dogechain-api-hash-to-address
nethash              | dogechain-api-get-network-statistics
totalbc              | dogechain-api-get-total-currency
transactions         | dogechain-api-get-transactions


#### dogechain-api-get-address-balance *address*

Get the total amount ever received, minus the total amount ever sent for
*address*.

```el
(dogechain-api-get-address-balance "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e")
=> 12345.6789
```

#### dogechain-api-address-to-hash *address*

Get the public key hash for *address*.

```el
(dogechain-api-address-to-hash "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e")
=> "F8783344AF8532A73DFA97EBDDFCC7527A2C6E5A"
```

#### dogechain-api-valid-address-p *address*

Check *address* for validity.

```el
(dogechain-api-valid-address-p "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e")
=> T

(dogechain-api-valid-address-p "INVALID")
=> NIL
```

#### dogechain-api-decode-address *address*

Get the version prefix and hash encoded in *address*. Returns an association
list containing `:version` and `:hash` keys.

```el
(dogechain-api-decode-address "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e")
=> ((:version . "1e")
   (:hash . "F8783344AF8532A73DFA97EBDDFCC7527A2C6E5A"))
```

#### dogechain-api-get-block-count

Get the current block number.

```el
(dogechain-api-get-block-count)
=> 123456
```

#### dogechain-api-get-difficulty

Get the difficulty of the last solved block.

```el
(dogechain-api-get-difficulty)
=> 123456
```

#### dogechain-api-get-recieved-by-address *address*

Get the total amount ever received by *address*.

```el
(dogechain-api-get-received-by-address "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e")
=> 123456.7890
```

#### dogechain-api-get-sent-by-address *address*

Get the total amount ever sent by *address*.

```el
(dogechain-api-get-sent-by-address "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e")
=> 123456.7890
```

#### dogechain-api-hash-to-address *hash*

Convert *hash* to a DogeCoin address.

```el
(dogechain-api-hash-to-address "F8783344AF8532A73DFA97EBDDFCC7527A2C6E5A")
=> "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e"
```

#### dogechain-api-get-network-statistics &optional *interval* *start* *stop*

Fetch statistics for the network. Returns a list of block information that
occurred between the *start* and *stop* block, spaced by *interval*. By default
it will fetch all information from the first to last block, spaced at 144 block
intervals.

Each item in the list contains the following keys:

| Key                | Description
|--------------------|-------------------------------------------
`:block`             | Height of the last block in *interval* + 1
`:timestamp`         | Unix timestamp for this block
`:target`            | Target for this block
`:average-target`    | Harmonic mean of `:target` over *interval*
`:difficulty`        | Difficulty for this block
`:hashes-to-win`     | Expected number of hashes required to solve a block of `:difficulty`
`:average-interval`  | Interval seconds, divided by blocks
`:hashes-per-second` | Estimated network hash rate for this block

**NOTE**: This function can return a lot of data!

```el
;; Example data here as targets can be very large!
(dogechain-api-get-network-statistics)
=> (((:block . 1)
    (:timestamp . 123456)
    (:target . 123456)
    (:average-target . 123)
    (:difficulty . 1234)
    (:hashes-to-win . 1000)
    (:average-interval . 12)
    (:hashes-per-second . 100))
   ;; More block information
   ((:block ... ))
  )
```


#### dogechain-api-get-total-currency

Get the total amount of currency ever mined.

```el
(dogechain-api-get-total-currency)
=> 12345678.90
```


#### dogechain-api-get-transactions

Get the amount of transactions for the previous block.

Returns a list of block transaction information. Each item in the list contains
the following keys:

| Key           | Description
|---------------|------------------------------------------
`:block`        | Block number
`:timestamp`    | Unix timestamp for this block
`:transactions` | Number of transactions made in this block

```el
(dogechain-api-get-transactions)
=> (((:block . 1)
    (:timestamp . 123456)
    (:transactions . 10))
   ((:block . 2)
    (:timestamp . 234560)
    (:transactions . 20))
    ;; And so on...
  )
```


## Licence

Copyright (C) 2014 Phil Newton

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
Street, Fifth Floor, Boston, MA 02110-1301, USA.
