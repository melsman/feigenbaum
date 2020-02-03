# Chaotic Bifurcation Diagrams in Futhark

*By Martin Elsman, DIKU*

This application demonstrates the use of
[Futhark](http://futhark-lang.org) for showing birfucation diagrams
for a number of non-linear discrete recurrence examples that
illustrate chaotic behavior. The application builds on the [Futhark
Lys Library](https://github.com/diku-dk/lys) and allows for the user
to zoom and navigate the area of interest (arrow keys, `z`, and
`x`). Currently, the application supports the following recurrence
equations, which can be altered between, dynamically, using the keys
`1`-`6`:

- **Logistic map**: The Logistic map takes a point *x_n* and computes
  a new point *x_{n+1}* using the equation:

  ````
  x_{n+1} = r * x_n * (1 - x_n)
  ````

  Here *r* is a parameter and in the setup values for *r* (ranging
  between 3.5 and 4.0) constitute values on the horizontal axis of the
  bifurcation diagram. The application uses the initial value *x_0* =
  0.25 for the calculation. For different parameters *r*, *x_n* will
  (1) converge towards a particular value, (2) result in an oscillating
  solution (jumping between multiple different values), or result in
  chaotic behavior (no repeated patterns).


- **SinCos map**: The SinCos map takes a point *(x_n,y_n)* and computes
  a new point *(x_{n+1},y_{n+1})* using the equations:

  ````
  x_{n+1} = sin (x_n + a * y_n)
  y_{n+1} = cos (b * x_n + y_n)
  ````

  Here *a* and *b* are parameters. The bifurcation diagram uses a
  fixed value of *a* = 2.82 whereas the values for *b* constitute
  values (ranging between 0 and 3) on the horizontal axis of the
  bifurcation diagram.  The implementation uses the initial point
  *(x_0,y_0) = (0.1,0.1)* for the calculations. It turns out that for
  different parameters *a* and *b*, *x_n* will (1) converge towards a
  particular value, (2) result in an oscillating solution (jumping
  between multiple different values), or result in chaotic behavior
  (no repeated patterns).

- **Tent map**: The Tent map takes a point *x_n* and computes
  a new point *x_{n+1}* using the equation:

  ````
  x_{n+1} = u * min (x_n,1-x_n)
  ````

  Here *u* is a parameter and in the setup values for *u* (ranging
  between 1.0 and 2.0) constitute values on the horizontal axis of the
  bifurcation diagram. The application uses the initial value *x_0* =
  0.1 for the calculation. For different parameters *u*, *x_n* will
  (1) converge towards a particular value, (2) result in an oscillating
  solution (jumping between multiple different values), or result in
  chaotic behavior (no repeated patterns).

- **Gaussian map**: The Gaussian map takes a point *x_n* and computes
  a new point *x_{n+1}* using the equation:

  ````
  x_{n+1}= exp(- a * x_n^2) + b
  ````

  Here *a* and *b* are parameters. The bifurcation diagram uses a
  fixed value of *a* = 6.2 whereas the values for *b* constitute
  values (ranging between -1 and 1) on the horizontal axis of the
  bifurcation diagram.  The implementation uses the initial point
  *x_0* = 0.1 for the calculations. It turns out that for
  different parameters *a* and *b*, *x_n* will (1) converge towards a
  particular value, (2) result in an oscillating solution (jumping
  between multiple different values), or result in chaotic behavior
  (no repeated patterns).

- **Henon map**: The Henon map takes a point *(x_n,y_n)* and computes
  a new point *(x_{n+1},y_{n+1})* using the equations:

  ````
  x_{n+1} = 1 - a * x_n^2 + y_n
  y_{n+1} = b * x_n
  ````

  Here *a* and *b* are parameters. The bifurcation diagram uses a
  fixed value of *b* = 0.3 whereas the values for *a* constitute
  values (ranging between 1 and 1.45) on the horizontal axis of the
  bifurcation diagram.  The implementation uses the initial point
  *(x_0,y_0) = (0.1,0.1)* for the calculations. It turns out that for
  different parameters *a* and *b*, *x_n* will (1) converge towards a
  particular value, (2) result in an oscillating solution (jumping
  between multiple different values), or result in chaotic behavior
  (no repeated patterns).

- **Logistic map (interpreted)**: This equation is implemented using
  an interpreted approach where the equation is implemented using a
  sequence of register file instructions that are executed by a simple
  interpreter written in Futhark. It turns out that this interpreted
  approach is only a factor of two slower than the hard-coded solution
  that requires complete knowledge of the particular recurrence
  equation at compile time. The interpretation approach can be
  beneficial in cases where a compile-once approach is desired and
  where the application needs to work for different code. Following
  the interpretation approach, the present application can be extended
  to take recurrence formulas as input at runtime.

The application is quite easy to extend to cover new equations; see
the source code for details.

## Building Birfucation Diagrams for Recurrences

Each supported recurrence system is defined by giving a concrete
record matching the following type:

````
  type^ sysdef 'p =
      {kind: kind,
       init: p,              -- initial state
       prj: p -> f64,        -- projection of vertical value
       next: f64 -> p -> p,  -- parameterised recurrence
       ns: (i32,i32),        -- pair of warm-up iteration number
                             -- and hot iteration number (drawing)
       p_rng: (f32,f32),     -- parameter range (horizontal axis)
       x_rng: (f32,f32)      -- range of vertical axis
       }
````

The application creates a frame for drawing many times a second and
parallelises the work done for each parameter value (the values for
the horizontal axis specified by the width of the window and the range
specified by the `p_rng` record field.

For each parameter value, the application first _warms up the
recurrence_ by, iteratively (and for each parameter value in
parallel), executing the `next` function a number of times (given by
the first component of the `ns` field) starting with the value
contained in the `init` field. The application then applies the `next`
function a number of times (specified by the second component of the
`ns` field) and records the resulting values for drawing points in the
frame.

## Running it

To build and run the application, do as follows:

````
$ futhark pkg sync
$ make
$ ./feigenbaum -i
````

## Here is how it looks:

![image](https://user-images.githubusercontent.com/1167803/73415056-03792f00-4311-11ea-949e-5a25756d2758.png)

## Copyright and License

Copyright (c) Martin Elsman
MIT License

## References

[1] List of Chaotic Maps. Wikipedia Article. See
https://en.wikipedia.org/wiki/List_of_chaotic_maps .
