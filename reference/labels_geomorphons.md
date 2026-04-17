# Obtenir les labels des géomorphons

Retourne un vecteur nommé avec les labels des classes de géomorphons.

## Usage

``` r
labels_geomorphons()
```

## Value

Un vecteur caractère nommé

## Examples

``` r
labels <- labels_geomorphons()
print(labels)
#>               1               2               3               4               5 
#>          "Plat"           "Pic"         "Crête"    "Épaulement"        "Éperon" 
#>               6               7               8               9              10 
#>         "Pente"         "Creux" "Pied de pente"        "Vallée"         "Fosse" 
```
