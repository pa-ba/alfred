This directory contains complete workflows for Alfred 2 that use the
examples from the [examples](../examples) subdirectory. These
workflows can be readily imported into Alfred 2. But the Haskell
binaries of the workflows may have to be recompiled using the
`recompile` script in each of the workflow directory.

## Installation

1. Import the desired workflow e.g. by double clicking the
   `.alfredworkflow` file.
2.  Find the directory in which the imported workflow has been
   installed: In the Alfred 2 application under the "Workflows" tab,
   find the imported workflow. Rightclick on the workflow and click on
   "Show in Finder".
3. Open the directory of the imported workflow in a terminal window and
   run the recompile script:

   ```shell
   > ./recompile
   ```
