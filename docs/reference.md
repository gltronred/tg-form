# Reference

## Contents

- [Config and result sheets](#config-and-result-sheets)
- [Geography file](#geography-file)
- [Compiling](#compiling)
- [Running](#running)

## Config and result sheets

Sheets for configuration and responses have to be named `config` and `result`,
correspondingly. Each question has a row with its configuration in `config`
sheet and a column (or columns) with answers in `result` sheet.

A row for the question in `config` sheet has four columns: 
- **name**: it has to be the same on `config` and `result` sheet, 
- **description**: it is the question that user sees
- **type**: what is expected as an answer
- **extra**: depends on type, see table below for explanation

There are several types of questions you can ask.

<table>
<thead><tr><th>Type</th><th>Columns in `result`</th><th>Meaning</th></tr></thead>
<tbody>
<tr><td>**welcome**</td><td>0 columns in `result`</td><td>Welcome text that is shown when the user starts a bot</td></tr>
<tr><td>**time**</td><td>1 column</td><td>To save date-time of a response, no question is asked</td></tr>
<tr><td>**user**</td><td>1 column</td><td>To save a user-id, no question is asked</td></tr>
<tr><td>**text**</td><td>1 column</td><td>Any textual response</td></tr>
<tr><td>**enum**</td><td>1 column</td><td>Keyboard with a fixed variants of input. The `extra` column contains a list of rows of keyboard delimited by a `;;`. Each row contains texts of buttons, delimited by a `;`. For example, `yes;no;;maybe`</td></tr>
<tr><td>**int**</td><td>1 column</td><td>Integer</td></tr>
<tr><td>**num**</td><td>1 column</td><td>Any number (e.g. `1.23`)</td></tr>
<tr><td>**location**</td><td>2 columns - latitude, longitude</td><td>Coordinates of a location that user sends. The `extra` column contains precision (`coord`, `city`, `municip`, `district`, `region`) - coordinates that user sends will be rounded to coordinates of the nearest point from a list given in the [config file](#geography-file)</td></tr>
<tr><td>**thanks**</td><td>0 columns</td><td>Text that is shown when the user finishes filling form</td></tr>
</tbody>
</table>

## Geography file

When you use location field you can round coordinates that user sends to
coordinates of city, municipality or region. Therefore, you have to provide the
information about coordinates of cities etc.

Currently, geography file supports four-level hierarchy. These levels correspond
to the [Russian classification of administrative
division](https://en.wikipedia.org/wiki/OKATO): urban or rural settlement,
municipal formation, municipal district and federal subject. You are free to
assign your own meaning to these levels.

The geography file is a CSV file with the following fields.

- `okato` - identifier (e.g. - OKATO code)
- `name.4` - settlement name (coordinates are saved if you set `city` as precision)
- `lat.4` - settlement's latitude
- `lon.4` - settlement's longitude
- `name.3` - municipal formation (`municip`), can be empty
- `lat.3` - its latitude
- `lon.3` - its longitude
- `name.2` - municipal district (`district`), can be empty
- `lat.2` - its latitude
- `lon.2` - its longitude
- `name.1` - federal subject (`region`)
- `lat.1` - its latitude
- `lon.1` - its longitude

This repository contains an [example file](example-geo.csv) with information
about Russian cities in the required format. This information was
[collected](https://github.com/Hackathon-on-Internet-freedom/tg-stat/blob/master/docs/geo.md)
from Wikipedia.

You have to specify this file before [running](#running)

## Compiling

You have to [install](https://www.haskell.org/downloads/#minimal) `ghc` and
`cabal` to compile this code.

Clone repository and compile it

``` sh
git clone https://git.sr.ht/~rd/tg-form.git
cd tg-form
cabal v2-update
cabal v2-build
```

## Running

Config file is JSON-file that contains a Telegram token (to run a bot) and a
connection details (to cache form fields read from Google sheets).

``` json-with-comments
{
  "token": "YOUR-TELEGRAM-BOT-TOKEN",      // Get a token from Botfather bot and put it here
  "connection": "PATH/TO/SQLITE-DB-FILE",  // Forms are read from Google sheets and cached here
  "geo-file": "PATH/TO/GEO-FILE.CSV"       // Optional geography file
}
```

You have to obtain a Google service user credentials and save it to some file, e.g. `creds.json`.

Run your bot using the following command:

``` sh
GOOGLE_APPLICATION_CREDENTIALS=creds.json cabal v2-run config.json
```
