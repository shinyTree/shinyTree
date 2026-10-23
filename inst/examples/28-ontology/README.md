# 28-ontology

Browses an ontology with `ontologyToTree()` and `renderTree(lazy = TRUE)`.

## aeo.obo

The Anatomical Entity Ontology (AEO), release 2022-09-13, from
<http://purl.obolibrary.org/obo/aeo.obo>. It is the ontology used as the
example in shinyTree issue #33.

AEO is maintained at
<https://github.com/obophenotype/human-developmental-anatomy-ontology> and
listed by the OBO Foundry at <https://obofoundry.org/ontology/aeo.html>. It is
released under the Creative Commons Attribution 4.0 International licence
(CC BY 4.0, <https://creativecommons.org/licenses/by/4.0/>). The file is
included unchanged.

## Other ontologies

Any `.obo` or `.owl` (RDF/XML) file works. For example, the Gene Ontology:

```r
download.file("http://purl.obolibrary.org/obo/go/go-basic.obo", "go-basic.obo")
```

Then choose "Upload a file" in the app. It has about 48,000 terms and takes a
few seconds to read.
