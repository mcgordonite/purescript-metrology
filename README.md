# purescript-metrology

A Purescript library for checking manipulation of measurements with units at
compile time.

```purescript
distanceTravelled ∷ Quantity MetrePerSquareSecondUnit Number →
                    Quantity MetrePerSecondUnit Number →
                    Quantity SecondUnit Number →
                    Quantity MetreUnit Number
distanceTravelled a u t = u .*. t .+. 0.5 *. a .*. t .*. t
```

Units are defined as phantom types and relationships between them are encoded
in type class instances. The SI base and some derived units are provided in
Data.Metrology.SI, but it is straightforward to define your own.

Quantities are parameterised in the unit type and the type of the value. Numeric
operations on quantities also manipulate the unit.

## Usage

Usage examples are available in the examples directory.

The Data.Metrology.SI.Derived module shows how to compose units from base units.

## Limitations

This library is designed to catch all unit errors at compile time. It is not
possible to define new units at run time.

