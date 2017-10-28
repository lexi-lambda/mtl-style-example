# haskell-mtl-style-example

This project includes a small example of using the so-called “mtl style” to unit test effectful Haskell code in a completely pure, `IO`-free way. The program it tests is extremely simple, but it performs numerous side-effects, making it tricky to completely test without `IO`: it uses command-line arguments, reads a file, prints log messages, and uses the system clock to calculate its running time.

The code is divided into the following modules:

  - #### `MTLStyleExample.Main`

    This module implements the actual program logic. The logic is not especially complex, so this module is small compared to the rest of the code, but in a real program, it would likely dwarf the rest of the program in size.

    In addition to implementing the program logic, this module also contains the effectful transformer stack that implements the effects when running the program for “real”.

  - #### `MTLStyleExample.Interfaces`

    This module contains the mtl-style typeclasses that are used to decouple the effect interfaces used in the code from their implementations. It also includes implementations of some of the interfaces in `IO`, which are trivial by design—the real implementations must be trivial, or there is logic that is not under test coverage!

  - #### `MTLStyleExample.MainSpec`

    The unit tests for the logic defined in `MTLStyleExample.Main`. Note that all of the functionality of the program is tested in a completely pure way without needing to perform an effectful end-to-end or integration test.

  - #### `MTLStyleExample.Test.Stubs`

    This includes the “fake” test-time implementation of the interfaces defined in `MTLStyleExample.Interfaces`, and these implementations are used in `MTLStyleExample.MainSpec`. These attempt to implement the interfaces as faithfully as possible without needing to use an actual file system, the real system clock, or the real standard output.

    This code ends up actually being some of the most verbose in the entire project, but remember that each of these fake implementations only needs to be written *once* per effect, so even as the program logic grows, this module never needs to change.

For some additional information on one of the techniques used in this project (that is, using `DefaultSignatures` to make lifting instances derivable), see my blog post [Lifts for free: making mtl typeclasses derivable][lifts-for-free].

[lifts-for-free]: https://lexi-lambda.github.io/blog/2017/04/28/lifts-for-free-making-mtl-typeclasses-derivable/
