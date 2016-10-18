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

## Working with the Server

Start a conversation with:

```bash
curl --data "answer=&stack=[]" localhost:9176
```

This will return a JSON:

```javascript
{
  "stack":"[\"TryAtHome.Suspended AskProduct ()\"]",
  "question":"Which product do you want to buy?"
}
```

Reply to each question by including the returned stack in the POST:

```bash
curl --data "answer=123&stack=[\"TryAtHome.Suspended AskProduct ()\"]" localhost:9176
```

And continue the conversation
