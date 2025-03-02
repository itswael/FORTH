# FORTH Interpreter in Haskell

A Haskell-based interpreter for a subset of FORTH, supporting basic arithmetic, stack operations, user-defined functions, and string manipulation.

## Folder Structure
```
FORTH/
│── FORTH/                    # Source code directory
│   ├── output/               # Output Files Corresponding to each Test
│   ├── tests/                # Test files 1-10
│   ├── Eval.hs               # Functional logic
│   ├── EvalSpec.hs           # Hspec tests for functions
│   ├── FORTH.cabal           # build file
│   └── Interpret.hs          # Interpreter logic
|   │── InterpretSpec.hs      # Interpreter Hspec tests
|   │── Main.hs               # Execution Entry Point
|   │── Requirements.md       # Assignment Requirements documentation
|   │── Setup.hs              # Entry point definition
|   │── Val.hs                # Variable Type definition
|   └── ValSpec.hs            # Variable Type definition Hspec tests
|
└── README.md                  # Project documentation
```

---

## **Prerequisites**
- **Haskell** (GHC 8.10+)
- **Cabal** (for dependency management)
- Packages: `hspec`, `QuickCheck`, `containers`, `flow`

---

## **Installation**
1. Clone the repository:
   ```
   git clone https://github.com/itswael/FORTH.git
   cd FORTH/FORTH
   ```
   OR
   Download and extract the Forth.zip then:
   ```
   cd FORTH/FORTH
   ```
2. Make sure you are inside the FORTH directory, Install dependencies:
    ```
    cabal install; cabal install hbase
    ```
   ```
   cabal install --lib hspec QuickCheck containers flow
   ```
---

## **Compilation and Test Execution**

For general instructions on how to use `cabal` see: https://www.haskell.org/cabal/users-guide/developing-packages.html

1. To compile the existing code do:
    ```
    cabal build
    ```
2. Running the unit Hspec tests use `runhaskell` command
    ```
    runhaskell ValSpec.hs
    runhaskell EvalSpec.hs
    runhaskell InterpretSpec.hs
    ```
3. To run the test cases:
    ```
    cabal run FORTH tests/t*.4TH
    ```
    The last argument is the test file, replace '*' with test case number from 1 to 10 both inclusive.
---

## Implemented Functionalities and test coverage:
* `Non-Empty Stack`                     - t2.4th.
* Arithmetic Operations `+, -, *, /, ^` - t1.4th, t3.4th, t4.4th, t5.4th.
* Next Line `CR`                        - t5.4th, t6.4th, t7.4th.
* Character at ASCII `EMIT`             - t6.4th.
* Convert to String `STR`               - t7.4th.
* Concat 2 strings `CONCAT2`            - t8.4th.
* Concat 3 strings `CONCAT3`            - t9.4th.
  ### Bonus
* `User Defined Functions`              - t10.4th.

---

## Situations Encountered
* Implementing `EMIT` function, I started tryied to create logic for implmenting a mapping table that would have been an exhaustive approach. Later recalled directly converting the integer value,
  to character using type conversion. Also utilized `floor value` conversion to get integer value if a float value is provided.
* Implementing `CR` function, it appeared to be a simple one, but when I implemented it, instead of moving cursor to next line, it was printing `\n`. Later fixed it by appending `"\n"` to the output directly.
* Implementing `+, -, *, /, ^` was easy as the multiplication was already provided, all it needed was understanding the sequence and `Abstrac Syntax Tree` for mathematical expressions to get correct results.
  ### Bonus
* Implementing `User Defined Functions` was the trickiest part of this Assignment. As there is lack of `Global Variables` only the `stack` was to be utilized. First I defined the syntax of the function,
  `: functionName Body ;` later defined the function if `:` encounters a different parsing of token will be done which will record the tokens in a separate stack and would be stored in a mapping table with
  `functionName -> Body`, once a `;` is parsed the function definition will end,
  and normal execution would continue. To maintain a separate map I used qualified Data.Map that act as an `environment` or `scope` to each function.
  In normal execution each token would be first searched in mapping table, if found the the tokens of the function body stored in the separte stack would be executed first, As soon as the token ends, the regular
  regular execution continues.
---
