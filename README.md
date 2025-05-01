# NEH appropriations from 1966 to 2024

Figures showing the following funding trends for the National Endowment for the
Humanities:

1. Annual appropriations
1. Annual appropriations: nominal vs real (2024 dollars)

## To run

``` shell
cd scripts
Rscript appropriations.R
```

## Figure

![](https://raw.githubusercontent.com/btskinner/nehapprop/main/figures/neh_appropriations_nom.png)

![](https://raw.githubusercontent.com/btskinner/nehapprop/main/figures/neh_appropriations_real.png)

## NOTES

All data are publicly available.

The script uses the [`fredr`](https://sboysel.github.io/fredr/index.html)
package, which requires an API key. [See the package
manual](https://sboysel.github.io/fredr/articles/fredr.html) for information
about getting and storing an API key.

