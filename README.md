# Telegram Forms Bot - tg_forms_bot

<a href="https://liberapay.com/gltronred/donate" target="_blank"><img src="https://img.shields.io/liberapay/receives/gltronred.svg?logo=liberapay" alt="Regular donation on Liberapay" ></a>

1. Prepare a Google sheet with questions
2. Send it to the [@tg_forms_bot](https://t.me/tg_forms_bot) and start collecting responses!
3. ?????
4. PROFIT!!!

# Getting Started

1. Create a new Google spreadsheet with two sheets: `config` and `result`.
2. Fill a `config` sheet with questions
![Config sheet](/getting-started-01-config.png)
3. Prepare a `result` sheet for responses.
![Result sheet](/getting-started-02-result.png)
4. Share the spreadsheet with a user `demo-bot-account@ozi-tg-cec.iam.gserviceaccount.com`.
Copy the address of the spreadsheet.
5. Start a [bot](https://t.me/tg_forms_bot). Send it a command `/newsheet
<address-of-spreadsheet>` and it will reply with a link. You can send this link
to your audience and start collecting responses.

You can fill this form yourself and leave us a feedback here: https://t.me/tg_forms_bot?start=TESTFORM

# Pricing

## It's free!

However, there are limitations. We are trying to keep them as small as possible,
but there are server payments, limits on Google API calls, programmer's
man-hours of supporting and other sad things.

Therefore, your free form will be available for two weeks (this duration can be
updated if demand in forms increases), and after that you will not be able to
collect new responses. Old responses are, of course, kept in your Google sheet.

## Supporting

If you like this software and want to help me develop it, you can support me.

If you subscribe to a [regular
donation](https://liberapay.com/gltronred/donate), I will be able to devote more
time to developing and supporting this project. It is well known that
mathematicians and programmers use coffee as a fuel, and you can [buy me a
coffee](https://buymeacoff.ee/gltronred) to support my work.

<a href="https://liberapay.com/gltronred/donate"><img alt="Donate using Liberapay" src="https://liberapay.com/assets/widgets/donate.svg"></a>

Of course, you can help the project by filling an issue or preparing PR - it is
open-source, after all.

## Self-hosting

The bot works on our servers. We do our best to make this process safe and
smooth. But you can have higher requirements for it that we can provide.

If you want to collect responses on your server, you can self-host your own
version of the bot. Please read [the manual](#reference) on building and
deploying the bot.

Also I can consult you in self-hosting the bot, or you can hire me to deploy it
to your server. Please [contact me](#author) for details.

## Commercial license

This software is licensed under [Affero GPL v3 or any later version](/LICENSE).
This means that if you use the unmodified bot on your server or if you modify
the bot you have to provide the source of the bot (or its modified version).

If use of the software under this license does not satify your organization's
legal department, commercial licenses are available. Please [contact
me](#author) for details.


# Reference

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

- **welcome** (0 columns in `result`): Welcome text that is shown to user when she starts a bot
- **time** (1 column): To save date-time of a response, no question is asked
- **user** (1 column): To save a user-id, no question is asked
- **text** (1 column): Any textual response
- **enum** (1 column): Keyboard with a fixed variants of input. The `extra` column contains a list of rows of keyboard delimited by a `;;`. Each row contains texts of buttons, delimited by a `;`. For example, `yes;no;;maybe`
- **int** (1 column): Integer
- **num** (1 column): Any number (e.g. `1.23`)
- **location** (2 columns - latitude, longitude): Coordinates of a location that user sends. The `extra` column contains precision (`coord`, `city`, `municip`, `district`, `region`) - coordinates that user sends will be rounded to coordinates of the nearest point from a list given in the [config file](#geography-file)
- **thanks** (0 columns): Text that is shown to the user when she finishes filling form

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

This repository contains an [example file](/example-geo.csv) with information
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

# Author

If you have any other questions, please don't hesitate to contact me directly:
[@gltronred](https://t.me/gltronred) or [gltronred@pm.me](mailto:gltronred@pm.me).

You can support my work on [liberapay](https://liberapay.com/gltronred/donate)
(regular donations) or [buymeacoffee](https://buymeacoff.ee/gltronred) (one-time
donation).

<a href="https://liberapay.com/gltronred/donate"><img alt="Donate using Liberapay" src="https://liberapay.com/assets/widgets/donate.svg"></a>

<a href="https://www.buymeacoffee.com/gltronred" target="_blank"><img src="https://cdn.buymeacoffee.com/buttons/default-orange.png" alt="Buy Me A Coffee" style="height: 51px !important;width: 217px !important;" ></a>
