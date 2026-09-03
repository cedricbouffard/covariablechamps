# Obtenir la direction du vent dominant

Extrait la direction la plus fréquente (vent dominant) depuis le
résultat de la fonction \`obtenir_rose_vents()\`.

## Usage

``` r
obtenir_vent_dominant(rose_vents, format = c("both", "angle", "label"))
```

## Arguments

- rose_vents:

  Résultat de la fonction \`obtenir_rose_vents()\`

- format:

  Format de sortie: "angle" (degrés), "label" (ex: "SO"), ou "both"
  (défaut)

## Value

La direction dominante (numérique, caractère ou liste)

## Examples

``` r
if (FALSE) { # \dontrun{
rose <- obtenir_rose_vents(champ)
vent_dom <- obtenir_vent_dominant(rose)
print(vent_dom$label)
} # }
```
