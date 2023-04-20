A Parallel 2D Unsteady Heat Equation Solver
===========================================

This example solves the heat quation using central differences in space
and explicit Euler time advancement.

* Main program: [heat-equation.f90](./heat-equation.f90)
* Module: [subdomain_m.f90](./subdomain_m.f90) (derived type and procedure interfaces)
* Submodule: [subdomain_s.f90](./subdomain_s.f90) (halo data structure and procedure definitions)
