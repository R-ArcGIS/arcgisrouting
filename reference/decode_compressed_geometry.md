# Decode Compressed Geometry

Decodes ArcGIS compressed geometry strings and converts them to an sfc
object.

## Usage

``` r
decode_compressed_geometry(geometry)
```

## Arguments

- geometry:

  Character vector containing compressed geometry strings.

## Value

An `sfc` object with `LIENSTRING` geometries in EPSG:4326.

## References

[maslke/arcgis-compressed-geometry](https://github.com/maslke/arcgis-compressed-geometry)
