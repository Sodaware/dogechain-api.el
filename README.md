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
totalbc              | dogechain-api-get-total-currency


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

*Deprecated by the API*


#### dogechain-api-get-network-statistics &optional *interval* *start* *stop*

*Deprecated by the API*


#### dogechain-api-get-total-currency

Get the total amount of currency ever mined.

```el
(dogechain-api-get-total-currency)
=> 12345678.90
```


#### dogechain-api-get-transactions

*Deprecated by the API*


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
