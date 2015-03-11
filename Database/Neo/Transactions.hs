module Database.Neo.Transactions where



{-
POST http://localhost:7474/db/data/transaction
Accept: application/json; charset=UTF-8
Content-Type: application/json

{
  "statements" : [ {
    "statement" : "CREATE (n {props}) RETURN n",
    "parameters" : {
      "props" : {
        "name" : "My Node"
      }
    }
  } ]
} --}

{--
201: Created
Content-Type: application/json
Location: http://localhost:7474/db/data/transaction/7

{
  "commit" : "http://localhost:7474/db/data/transaction/7/commit",
  "results" : [ {
    "columns" : [ "n" ],
    "data" : [ {
      "row" : [ {
        "name" : "My Node"
      } ]
    } ]
  } ],
  "transaction" : {
    "expires" : "Tue, 03 Feb 2015 13:07:32 +0000"
  },
  "errors" : [ ]
}
-}

{--

POST http://localhost:7474/db/data/transaction/commit
Accept: application/json; charset=UTF-8
Content-Type: application/json
{
  "statements" : [ {
    "statement" : "CREATE (n) RETURN id(n)"
  } ]
}
Example response

200: OK
Content-Type: application/json
{
  "results" : [ {
    "columns" : [ "id(n)" ],
    "data" : [ {
      "row" : [ 15 ]
    } ]
  } ],
  "errors" : [ ]
}


-- Given that you have an open transaction, you can send a roll back request. The server will roll back the transaction.
-- Example request

DELETE http://localhost:7474/db/data/transaction/3
Accept: application/json; charset=UTF-8
Example response

200: OK
Content-Type: application/json; charset=UTF-8
{
  "results" : [ ],
  "errors" : [ ]
}

bump timeout:
POST http://localhost:7474/db/data/transaction/2
Accept: application/json; charset=UTF-8
Content-Type: application/json
{
  "statements" : [ ]
}
Example response

200: OK
Content-Type: application/json
{
  "commit" : "http://localhost:7474/db/data/transaction/2/commit",
  "results" : [ ],
  "transaction" : {
    "expires" : "Tue, 03 Feb 2015 13:07:32 +0000"
  },
  "errors" : [ ]
}

--}