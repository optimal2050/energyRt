# Recognised commodity properties

The registry of physical properties understood by `commodity@property`
and, in turn, by `convert()`. `quantity` declares which dimension pair a
property relates, which is what lets a conversion cross between measures
of the same commodity – `lhv` in "GJ/t" links Energy and Mass, `density`
in "t/m3" links Mass and Volume.

Property names are not restricted to this list: an unrecognised name is
kept, with a warning, so that data the package has not anticipated can
still be recorded. Names ending in `*` are prefix rules – `frac_C`,
`frac_H` and `frac_ash` all match `frac_*`.

## Usage

``` r
commodity_properties()
```

## Value

A data.frame with columns `property`, `quantity`, `default_unit`,
`bridges` and `description`.

## See also

[`newCommodity()`](https://energyRt.org/reference/newCommodity.md),
[`commodity_property()`](https://energyRt.org/reference/commodity_property.md)

Other commodity:
[`commodity_property()`](https://energyRt.org/reference/commodity_property.md),
[`newCommodity()`](https://energyRt.org/reference/newCommodity.md),
[`object_image()`](https://energyRt.org/reference/object_image.md)

## Examples

``` r
commodity_properties()
#>        property                  quantity default_unit bridges
#> 1           lhv               Energy/Mass         GJ/t     yes
#> 2           hhv               Energy/Mass         GJ/t     yes
#> 3       density               Mass/Volume         t/m3     yes
#> 4    molar_mass               Mass/Amount        g/mol     yes
#> 5        frac_*             dimensionless        kg/kg      no
#> 6     density_*               Mass/Volume         t/m3  on_ask
#> 7 heat_capacity Energy/(Mass*Temperature)      kJ/kg/K      no
#>                                                                         description
#> 1                                                         Lower (net) heating value
#> 2                                                      Higher (gross) heating value
#> 3                                        Density at the commodity's reference state
#> 4                                                                        Molar mass
#> 5           Element mass fraction from ultimate analysis ('mol/mol' for mole basis)
#> 6                               Density variant -- liquid, compressed, true vs bulk
#> 7 Specific heat capacity; reference only, cannot bridge without a temperature delta
```
