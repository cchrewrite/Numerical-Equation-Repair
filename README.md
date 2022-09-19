# Numerical-Equation-Repair

The code is the implementation of the equation repair approach proposed by the paper:

Cheng-Hao Cai and Alan Bundy.
Repairing Numerical Equations in Analogically Blended Theories Using Reformation.
HLC2022: The 3rd International Workshop on Human-Like Computing, September 28-20, 2022, Windsor Great Park, UK.

To run the code, you will need:
1. Ubuntu 20 Operating System, which is available at: https://ubuntu.com/
2. Swi-Prolog (preferably v7.6.4), which can be installed by: apt-get install swi-prolog
3. Python 3 (preferably v3.6.9), which can be installed by: apt-get install python3
4. The Z3 solver with Python API, which can be installed by: pip install z3-solver

Steps:
1. On Ubuntu 20, download the code in this repository.
2. Open a terminal, and start Swi-Prolog by 'swipl'.
3. In Swi-Prolog, run the following query:
  ?- [numerical_equation_reformation].
  ?- example_EleForce_HLC_2022.
The screen should show the results.
