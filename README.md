# Barberry Garden

A set of Emacs utilities for managing wine tasting events, tracking balances with participants, and publishing reviews. Built on top of [vulpea](https://github.com/d12frosted/vulpea) and [vino](https://github.com/d12frosted/vino).

## Dependencies

- Emacs 29.3+
- [dash](https://github.com/magnars/dash.el) 2.19.1+
- [s](https://github.com/magnars/s.el) 1.13.0+
- [vulpea](https://github.com/d12frosted/vulpea) 0.3.0+
- [vino](https://github.com/d12frosted/vino) 0.4.0+
- [widget-extra](https://github.com/d12frosted/widget-extra) 1.0.0+
- [hledger](https://hledger.org/) (for ledger functionality)

## Modules

### `brb`

Core utilities used by other modules:

- Price formatting and parsing (`brb-price-format`, `brb-price`)
- QPR (Quality-Price Ratio) calculation (`brb-qpr`)
- String table formatting (`brb-string-table`)

### `brb-event`

Event management for wine tastings:

- `brb-event-select` - interactively select an event
- `brb-event-create` - create a new event
- `brb-events-from-range` - query events within a date range

Events are org-mode notes tagged with `wine`, `event`, and `barberry/public`.

### `brb-ledger`

Balance tracking using hledger. Manages participant balances, deposits, charges, and spending.

**Configuration:**

```elisp
(setq brb-ledger-file "/path/to/ledger.journal")
;; Or separate files for reading/writing:
(setq brb-ledger-file '("/path/to/read.journal" "/path/to/write.journal"))
```

**Key functions:**

- `brb-ledger-display` - display ledger with balances and transactions
- `brb-ledger-deposit` - record a deposit from a participant
- `brb-ledger-charge` - charge an amount to a participant
- `brb-ledger-spend` - record spending from the shared balance
- `brb-ledger-spend-personal` - record personal spending
- `brb-ledger-convive-display-balance` - view a specific participant's history

**Customization:**

```elisp
(setq brb-ledger-display-limit 50)  ; number of transactions to show (default: 36)
```

### `brb-widget`

Custom widgets for brb UIs: `money-label`, `balance-label`, `balance-reversed-label`, `note-label`.

### `brb-sn`

Utilities for posting wine reviews to social networks (e.g., Vivino).

- `brb-sn-display` - show pending reviews grouped by event

## Development

```sh
# install deps
make prepare

# compile
make compile

# lint
make lint

# test
make test
```
