# server

This is my second (but first test-driven attempt) at writing an HTTP server in Haskell.
It is intended as both an exploration of Haskell and the HTTP protocol and is thus
inherently incomplete as my knowledge of both is also incomplete.  As I better 
understand each element, this codebase will continue to improve. I actually haven't
read the HTTP/1.1 specification I am presently implementing.

I am writing this by adding thin, vertical slices of atomic functionality.
This allows the product to be useful near immediately.  At present, I have written two
demonstration routes. The first can reach out to the internet via the client that 
was built in tandem with this server as a testing utility. The second will output
the contents of any file on the computer on which it is running.  *Take care!* 
Both routes will respond to any HTTP verb.

I hope you enjoy reading and modifying this as much as I have enjoyed creating it.

---
### How To Run

1. Install Stack
1. Run ```stack build --ghc-options="-Wall -Werror"``` inside the project directory
1. Run ```stack exec server-exe``` inside the project directory
1. Your server is now listening on port 8080.
1. Add more routes in the ```main.hs``` file.

\* I'm not certain that I haven't missed something

### How To Test
1. Install Stack
1. Run ```stack test```

--- 
### Complete
1. HTTP Client
    * [x] GET
    * [x] POST
    * [x] PUT
    * [x] DELETE
1. HTTP Server
    * [x] GET
    * [x] POST
    * [x] PUT
    * [x] DELETE
1. Headers
    * [x] Client request
    * [x] Client response
    * [x] Server request
    * [x] Server response
1. JSON writer

### Backlog
1. JSON parser

### Icebox
1. Lightweight document store
1. HTML templating DSL
1. Functional dependency injection
1. Route matching based on passed params or headers
1. HTTPS