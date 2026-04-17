# Extraire la ligne centrale d'un rectangle de haie

A partir d'un rectangle représentant une haie, extrait la ligne centrale
dans le sens de la longueur ainsi que ses deux extrémités.

## Usage

``` r
extraire_ligne_centrale_haies(
  haies_rectangles,
  output_type = c("tous", "lignes", "extremites")
)
```

## Arguments

- haies_rectangles:

  Objet sf avec les rectangles des haies (résultat de
  extraire_classifier_haies_lidar)

- output_type:

  Type de sortie: "lignes" pour les lignes centrales, "extremites" pour
  les points d'extrémité, ou "tous" pour les deux

## Value

Selon output_type: - "lignes": sf LINESTRING avec la ligne centrale de
chaque haie - "extremites": sf POINT avec les deux extrémités de chaque
haie (avec colonne extremite = "debut" ou "fin") - "tous": liste avec
lignes (sf LINESTRING) et extremites (sf POINT)
