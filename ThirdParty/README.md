# Third-party libraries

MARS ships a copy of the third-party sources it compiles, so that a plain clone (or the ZIP of
a release) builds without fetching anything else. Only the files MARS needs are copied, with
the upstream folder layout. This page records where each copy comes from, so that it can be
checked against upstream and refreshed.

| Folder | Upstream | Version | License | Local changes |
| --- | --- | --- | --- | --- |
| `DCS` | [winddriver/Delphi-Cross-Socket](https://github.com/winddriver/Delphi-Cross-Socket) | `07cf3dc` (2026-07-16); `Net/Net.CrossHttpClient.pas` from `c024fb6` (2026-06-29) | LGPL-3.0 (`DCS/LICENSE`) | none |
| `DCS/ThirdParty/cnvcl` | [cnpack/cnvcl](https://github.com/cnpack/cnvcl) (CnPack Component Package), the `Source/Common` and `Source/Crypto` units Delphi-Cross-Socket depends on | `133ba90a` (2026-06-29) | CnPack Agreement License (`cnvcl/License.enu.txt`) | none |
| `delphi-jose-jwt` | [andrea-magni/delphi-jose-jwt](https://github.com/andrea-magni/delphi-jose-jwt), a fork of [paolo-rossi/delphi-jose-jwt](https://github.com/paolo-rossi/delphi-jose-jwt) | `4d9938b` (2019-05-29) | Apache-2.0 (`delphi-jose-jwt/License.txt`) | `Source/JOSE.Types.Arrays.pas`: unused local variable commented out (compiler hint) |
| `mORMot/Source` | [synopse/mORMot](https://github.com/synopse/mORMot) (mORMot 1.18), the units used by the mORMot JWT backend and by dmustache | `21d2618d` (2026-01-01), same content as `22e12fc6` (2026-06-05) for these files | MPL 1.1 / GPL 2.0 / LGPL 2.1 tri-license (unit headers) | `SynCrypto.pas`: `{$HINTS OFF}` added at the top |
| `Neslib.Yaml` | [neslib/Neslib.Yaml](https://github.com/neslib/Neslib.Yaml) | `ecab30b` (2019-12-12) | Simplified BSD (`Neslib.Yaml/License.txt`) | none |
| `Neslib.Yaml/Neslib` | [neslib/Neslib](https://github.com/neslib/Neslib), the submodule Neslib.Yaml points to at `ecab30b` | `8efc4bd` (2019-05-31) | Simplified BSD (`Neslib.Yaml/Neslib/License.txt`) | none |

Versions are upstream commit ids. Files that differ from upstream only in line endings are not
listed as changes.

## Refreshing a library

1. Check out the upstream repository at the commit (or tag) you want.
2. Replace the files in the MARS folder with the same files from upstream, keeping the list of
   files: add a file only when MARS starts needing it, remove the ones upstream deleted.
3. Re-apply the local changes listed above, if still needed.
4. Build the packages and run the test suite (`tests/MARSTestsProject.dproj`), then update the
   table on this page.
