Erlang Interpeter in Haskell
============================

This is a naive implementation of Erlang virtual machine in
Haskell. Few points:

* It is work in progress
* It interprets Core Erlang
* It uses Cloud Haskell library

Installation
------------

Requirements:
* Local checkout of all Cloud Haskell libraries at "dev" branch
* Erlang/OTP installed

Clone:
    git clone https://github.com/gleber/erlhask.git

Prepare the sandbox:

    cd erlhask
    cabal sandbox init
	cabal update
	cabal install cabal-install

Add add source dependency on Cloud Haskell packages, since ErlHask
requires "dev" version of Cloud Haskell:

    for i in path/to/cloud-haskell/*/; do
        cabal install add-source $i
    done

Install deps and configure everything:

    cabal install --only-dep --enable-tests
	cabal configure --enable-tests

Compile supplied .erl files to .core files:

    make samples

Evaluate boot:start/0 from samples/boot.{erl,core} module:

    make run

Roadmap
-------

Some major milestones in no particular order:

* Implement equivalent of init.erl behaviour
* Implement better module loading
* Implement support for error_handler.erl or equivalent behaviour
* Implement more BIFs
 * process monitoring, linking, process_info, etc.
 * ets module
 * os module
* Implement IO subsystem
* Support Erlang compile.erl to be able to pre-compile .erl files to
  .core files (i.e. to make ErlHask self-contained)
* Implement support for Erlang shell (or alternative REPL)
* Implement tracing
* Implement debugging support
* Implement distributed protocol
* Much-much-much more
