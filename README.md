# Conversational UI Prototype

This repo contains a prototype for develping a type-aligned finite state machine that can be used to present conversational UI flows.

## Setup

Make sure you have [Haskell Stack](https://www.haskellstack.org/)

```
stack build
```

Then run the executable (in OS X, usually in):

```
.stack-work/install/x86_64-osx/lts-6.12/7.10.3/bin/conversational-ui-exe
```

Or from GHCi:

```
stack ghci
```

And just call:

```
main
```

## Example

![Flow](docs/flow.png)

The project contains a prototype of *Try at Home* flow.

This flow is made of 2 (sub) flows:

- Try at Home flow
  - Size flow
  - Checkout flow

----

To play with the Web server checkout `git checkout scotty` branch.
