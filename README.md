# Spanning_tree

Program to find all possible spanning trees contained in input graph. Those trees are printed to the STDOUT.

## Setup
Change the current working directory to the root of the project.
```bash
cd __PROJECT_ROOT__
```

Simple Makefile is provided. To simply compile the program run the following command.
```bash
make
```

## Runtime
After successful compilation, program with reference input, can be run as follows:
```bash
make run
```

## Tests
Simple tests are provided in the /tests directory, they test few simple cases.

Run all tests:
```bash
make test
```


## Measured tests
| File                     | Runtime [ms] |
|--------------------------|--------------|
| edge                     | 18           |
| triangle                 | 25           |
| square                   | 21           |
| ref                      | 36           |
| connected_square         | 32           |
| connected_square_cross   | 37           |
| connected_square_cross+1 | 45           |

