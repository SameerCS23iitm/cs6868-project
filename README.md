# cs6868-project
**[The link to our video presentation](https://drive.google.com/file/d/1RyDUM6vBmNIBlJ2ADM2kPV8agRWE84SW/view?usp=sharing)**
## Repository Structure
- The source code is present in the `src` directory
- The `src/test` directory contains various tests(lin,stm,concurrent,sequential).
- The `src/lib/benchmarks` directory has benchmarking and plotting scripts.
- The `src/lib` directory has the implementations of both the universal constructions, consensus object, node, mutex-protected sequential implementations and purpose-built lock-free implementations of stack and queue
- The `src/lib/sequential` directory contains the sequential versions of BST,Stack,Queue, Skip List, Sorted List which can be passed to the universal constructions.
- The slides used for presentation are present in the root directory and are named `CS6868_Presentation.pdf`
- The report is present in the root directory and is named `Report.pdf`

## Configuration required to run the code
- Opam version 5.4.0 or above
- Dune version 3.21 or above

## Steps to run the code
- Use the above mentioned configurations of Opam and Dune
- Use the command ```dune build``` to build the project
- Use the command ```dune exec ./name.exe``` to run an executable. A file name `name.ml` produces an executable named `name.exe`. 
- To run the benchmark codes, you also need to specify the number of times to run as a command line argument,e.g., ```dune exec ./benchmark.exe <number of times to run>```
- To run the plotting scripts, create a virtual environment with the name `myvenv` in the directory `src/lib/benchmarks` using the command ```python3 -m venv myvenv```. Then, get into the virtual environment using the command ```source myvenv/bin/activate```. Install all the required libraries using the command ```pip3 install -r requirements.txt```. 
