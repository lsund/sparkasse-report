# Sparkasse Reporter

Sparkasse does not provide you with a lot of online features, but it lets you
export a CSV file (actually seperated by semicolon, but has extension .csv).
This file contains information about all the transactions in a given timeframe.

The file has 17 columns as below, with the first row being the header row:

```
"Auftragskonto";"Buchungstag";"Valutadatum";"Buchungstext";"Verwendungszweck";"Glaeubiger ID";"Mandatsreferenz";"Kundenreferenz (End-to-End)";"Sammlerreferenz";"Lastschrift Ursprungsbetrag";"Auslagenersatz Ruecklastschrift";"Beguenstigter/Zahlungspflichtiger";"Kontonummer/IBAN";"BIC (SWIFT-Code)";"Betrag";"Waehrung";"Info"
```

The following n lines obey the structure of the header:

```
"DE13370501980123093569";"17.04.19";"17.04.19";"KARTENZAHLUNG";"2019-04-16T14:55 Debitk.2 2020-12 ";"";"";"54161226005670160419145513";"";"";"";"LICHT APOTHEKE//DUESSELDORF/DE";"DE79300606010104507991";"DAAEDEDDXXX";"-9,47";"EUR";"Umsatz gebucht"
```
We are interestied in (starting from 0) column 3, 4 and 11.
* Column 3 corresponds to "Buchungstext" (transaction text) and is referred to
  as "text" by the filters below
* Column 4 corresponds to "Verwendungszweck" (intended use) and is referred to
  as "ocr" by the filters below
* Column 11 corresponds to "Beguenstigter/Zahlungspflichtiger" (receiver/payer)
  and is referred to as "transactor" by the filters below


The input consists of two parts, a 'filter JSON file', which we'll refer to it as
`filters.json` and an 'exported Sparkasse CSV file' which we will refer to as
`sparkasse-export.csv`.

* `sparkasse-export.csv` contains the raw, exported CSV file from Sparkasse's
  homepage, contains 17 columns.


`filters.json` contains a JSON array of "filters" like this
```
[
  {
    "selector": "text",
    "content": "rent",
    "category": "Apartment"
  },
  {
    "selector": "ocr",
    "content": "Edeka",
    "category": "Supermarket"
  },
  {
    "selector": "transactor",
    "content": "John",
    "category": "Personal"
  }
]
```

Tip: The above filters.json can be generated with tools like `jo`, `jq` and `echo`:
```
jo -a $(jo selector=text content=rent category=apartment) \
  $(jo selector=ocr content=Edeka category=supermarket) \
  $(jo selector=transactor content=John category=Personal) \
  | jq > filters.json
```

Then invoke the program like this
```
sparkasse-reporter -i sparkasse-export.csv -f filters.json -o report.json
```

When successful, the file specified by `-o` is generated as a JSON report file,
containing fields `categorySums` and `unassignedTransactions`.

TODO explain `categorySums` and `unassignedTransactions`

