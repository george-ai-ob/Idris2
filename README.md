# Idris2
## An Alternative Idris2 C Compiler Back-end

This is a fork of [Idris2](https://github.com/idris-lang/Idris2)
This project is to investigate a possible improvement to the Idris2 compiler's C back-end  for the fulfilment of Bsc Software Engineering at the University of Strathclyde.

Installation and build instructions can be found [here](https://github.com/idris-lang/Idris2/blob/main/INSTALL.md).

Files related to this project can be found in [ProtoIR](https://github.com/george-ai-ob/Idris2/tree/main/src/Compiler/RefC/ProtoIR) and tests can be found in the related [test](https://github.com/george-ai-ob/Idris2/tree/main/tests/refc/protoIR) sub-directory.

To run the test suite run
```
make test only="refc/protoIR"
```
at root directory
