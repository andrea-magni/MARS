# Third-party libraries

MARS ships a copy of the third-party sources it compiles, with the upstream folder layout and
only the files MARS needs. The exceptions are delphi-jose-jwt and Delphi-Mocks, git submodules (see
below). This
page records where each copy comes from, so that it can be checked against upstream and
refreshed.

| Folder | Upstream | Version | License | Local changes |
| --- | --- | --- | --- | --- |
| `DCS` | [winddriver/Delphi-Cross-Socket](https://github.com/winddriver/Delphi-Cross-Socket) | `07cf3dc` (2026-07-16); `Net/Net.CrossHttpClient.pas` from `c024fb6` (2026-06-29) | LGPL-3.0 (`DCS/LICENSE`) | none |
| `DCS/ThirdParty/cnvcl` | [cnpack/cnvcl](https://github.com/cnpack/cnvcl) (CnPack Component Package), the `Source/Common` and `Source/Crypto` units Delphi-Cross-Socket depends on | `133ba90a` (2026-06-29) | CnPack Agreement License (`cnvcl/License.enu.txt`) | none |
| `mORMot/Source` | [synopse/mORMot](https://github.com/synopse/mORMot) (mORMot 1.18), the units used by the mORMot JWT backend and by dmustache | `21d2618d` (2026-01-01), same content as `22e12fc6` (2026-06-05) for these files | MPL 1.1 / GPL 2.0 / LGPL 2.1 tri-license (unit headers) | `SynCrypto.pas`: `{$HINTS OFF}` added at the top |
| `Neslib.Yaml` | [neslib/Neslib.Yaml](https://github.com/neslib/Neslib.Yaml) | `ecab30b` (2019-12-12) | Simplified BSD (`Neslib.Yaml/License.txt`) | none |
| `Neslib.Yaml/Neslib` | [neslib/Neslib](https://github.com/neslib/Neslib), the submodule Neslib.Yaml points to at `ecab30b` | `8efc4bd` (2019-05-31) | Simplified BSD (`Neslib.Yaml/Neslib/License.txt`) | none |

Versions are upstream commit ids. Files that differ from upstream only in line endings are not
listed as changes.

## delphi-jose-jwt (git submodule)

`delphi-jose-jwt` is a git submodule of
[paolo-rossi/delphi-jose-jwt](https://github.com/paolo-rossi/delphi-jose-jwt) (MIT license), pinned
to tag `v4.0.2`. It is not copied because the library is also distributed on its own (TMS Smart
Setup product `rossi.delphi-jose-jwt`): the `MARS.JOSE` package *requires* its `JOSE` package
instead of containing its units, so the two never clash. `Packages\<version>\MARS.groupproj` builds
`JOSE` before `MARS.JOSE`.

Clone MARS with `git clone --recurse-submodules`, or run `git submodule update --init` in an
existing clone. To move to another release: `git -C ThirdParty/delphi-jose-jwt checkout <tag>`,
build and test, then commit the new submodule pointer.

## Delphi-Mocks (git submodule)

`Delphi-Mocks` is a git submodule of
[VSoftTechnologies/Delphi-Mocks](https://github.com/VSoftTechnologies/Delphi-Mocks) (Apache 2.0
license), pinned to commit `53cd36b` (2026-10-01): the last tag, `v0.3.1`, predates the fix of the
memory leaks of `TMock<T>` and `TStub<T>` (`9db73c2`). It is not part of any package: only
`Source\MARS.Tests.pas`, the test suite (`tests`) and the test projects of the demos and templates
(`Tests.Default.pas`) use it, through `ThirdParty\Delphi-Mocks\Source` in their search path (the
setup adds it to the IDE library path).

To move to another version: `git -C ThirdParty/Delphi-Mocks checkout <commit or tag>`, run the
test suite, then commit the new submodule pointer.

## Refreshing a library

1. Check out the upstream repository at the commit (or tag) you want.
2. Replace the files in the MARS folder with the same files from upstream, keeping the list of
   files: add a file only when MARS starts needing it, remove the ones upstream deleted.
3. Re-apply the local changes listed above, if still needed.
4. Build the packages and run the test suite (`tests/MARSTestsProject.dproj`), then update the
   table on this page.
