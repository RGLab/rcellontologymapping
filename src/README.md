# cell-ontology-mapping

Generate leukocyte cell classifications using the decision tree from Maecker et al. (2012) given cell surface marker profiles

## Building

You will also need the Haskell stack build tool, see installation instructions at:
 * https://docs.haskellstack.org/en/stable/README/#how-to-install


After all that is installed, then run
 * stack build
 * stack install


I built this project in a computing environment with 
 * GCC version 9.3.0

You may need to install additional libraries, gmp, expat, bzip2 or use environmental modules:
 * module load GMP/6.2.0-GCCcore-9.3.0
 * module load expat/2.2.9-GCCcore-9.3.0
 * module load bzip2/1.0.8-GCCcore-9.3.0


## Example of running the program on Linux

```bash
$ cell-ontology-mapping classify "CD3e-, CD19+, CD24++, CD38++"
Cell classification: transitional B cell (http://purl.obolibrary.org/obo/CL_0000818)
Decision variables used: ["CD19","CD24","CD38","CD3e"]
```
