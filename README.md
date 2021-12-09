# XC: Scala DSL implementation and case study

## XC/Scala: an XC implementation as an internal Scala domain-specific language (DSL)

- **Where?** The source code of the implementation can be found under `src/main/scala`, package `xc`
- **What?** The DSL implementation includes the components described in the paper, namely:
    - `xc.NValues#NValue`: data structure modelling a neighbouring value
    - `xc.XCLang` and `xc.XCLangImpl`: interface with the XC constructs and corresponding implementation class
    - `XCLib`: provides reusable functions (building blocks) upon `XCLang`; examples include `collect`, `broadcast`, `distanceTo` (see examples in the paper)
    - `XCProgram`: class to be specialised and implemented (`main` method) in order to define an XC program
- **How??** 
