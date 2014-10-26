# dogechain-api

## Description

**dogechain-api** is an Emacs library for working with the dogechain.info API.

It's not a particularly useful extension on its own, as there are no interactive
functions, but can be used to build something more interesting.


## Simple Query API Functions

The DogeChain API docs can be found here: http://dogechain.info/api/simple

The API functions are mapped as follows:

API Method           | Local Function
------------------------------------------------------------
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


### dogechain-api-get-address-balance *address*

Get the total amount ever received, minus the total amount ever sent for
*address*.

```lisp
(dogechain-api-get-address-balance "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e")
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
