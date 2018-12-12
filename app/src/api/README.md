## API Requests

All API requests are tagged with a *type* (in JSON it is a string such as `"ping"`, in Lisp code it is most-commonly a keyword as in `:ping`), and handling of requests is controlled by `handle-post-request`. Among other things, this function extracts the type from a HTTP POST request, and dispatches to an appropriate *handler*, i.e. a Lisp function which is responsible for doing most of request-specific work.

Roughly speaking, requests can be divided into two camps:

1. Some requests have a behavior which is agnostic to the QVM simulation method (e.g. `:ping`, `:version`).
2. Other requests have a behavior which depends on the simulation method (e.g. `:wavefunction`, `:multishot-measure`).

Handlers for requests of the first type are ordinary Lisp functions. Handlers for requests of the second type are generic functions to be specialized on their first argument (a value of type `simulation-method`).

### Undefined API Methods

In the event that a given request type is not implemented for a given simulation method, the handler method should signal an `api-method-not-implemented` condition.


