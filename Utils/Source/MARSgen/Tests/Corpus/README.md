# MARSgen test corpus

The documents the MARSgen tests read (`Tests\MARSgenTests`).

## Written for the tests

Part of MARS-Curiosity, same license.

| File | Content |
| --- | --- |
| `petstore-3.0.yaml` | A pet store in OpenAPI 3.0 (YAML): `$ref` to every kind of component, `allOf`, security. |
| `features-3.1.json` | The edge cases of 3.1 (JSON): type arrays, `const`, recursion, `$ref` chains, `pathItems`, discriminator, and constructs that must be diagnosed. |
| `mapping-3.0.yaml` | The edge cases of the mapping to Delphi: reserved and invalid names, enumerations, name collisions, a cycle of records, inline types of operations. |
| `swagger-2.0.json` | A Swagger 2.0 document, refused. |
| `samples\*.json` | JSON samples: two orders that differ on purpose, an array of customers. |

## OpenAPI Initiative examples (`oai`)

The examples of the OpenAPI Initiative, unmodified, from
[OAI/learn.openapis.org](https://github.com/OAI/learn.openapis.org), folder `examples`, commit
`82b82f372aa185a07ea11bc158619895ab21f741` (2026-10-10). File names: `<version>-<name>.yaml`
for `examples/<version>/<name>.yaml`.

Licensed under [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/)
(CC BY 4.0), © The OpenAPI Initiative contributors.

## Excerpts of real APIs (`real`)

Excerpts made with `real\extract.py`: some paths, whole, with every component they reference, and
`openapi`, `info`, `servers`, `security` and the security schemes of the document. The content is
the original one; only what is not reachable from the chosen paths is left out.

| File | Source | Paths | License |
| --- | --- | --- | --- |
| `github-issues.yaml` | [github/rest-api-description](https://github.com/github/rest-api-description), `descriptions/api.github.com/api.github.com.yaml`, commit `58b1e0c00b39b9e46c24dc4a0ba6a2669c90e9c1` (2026-10-10) | `/repos/{owner}/{repo}/issues`, `/repos/{owner}/{repo}/issues/{issue_number}`, `/users/{username}` | MIT, © 2020 GitHub (`real\LICENSE-github.txt`) |
| `stripe-v2-persons.json` | [stripe/openapi](https://github.com/stripe/openapi), `latest/openapi.spec3.json`, commit `7a1f5c21117298a85995746eeeedb6207969dc4e` (2026-10-10) | `/v2/core/accounts/{account_id}/persons`, `/v2/core/accounts/{account_id}/persons/{id}`, `/v2/billing/meter_events` | MIT, © 2011- Stripe, Inc. (`real\LICENSE-stripe.txt`) |

Every Stripe v1 path reaches about 900 of the 1,700 components (the whole API, through its
expandable fields): the v2 paths above are self-contained.

To refresh an excerpt, download the document and run, i.e.:

```
python extract.py api.github.com.yaml github-issues.yaml "/repos/{owner}/{repo}/issues" "/repos/{owner}/{repo}/issues/{issue_number}" "/users/{username}"
```

(it needs PyYAML for YAML documents).

## Whole documents

Whole descriptions (GitHub 10 MB, Stripe 5 MB) are not in the repository. To check them, put them
in a folder and set `MARSGEN_FULL_CORPUS` to it: the `FullDocuments` test reads every `.json`,
`.yaml` and `.yml` file there and writes its unit. Checked on 2026-10-11 with the versions above:
both read without errors or warnings in a few seconds, and their units (179,000 lines of Delphi in
all) compile.
