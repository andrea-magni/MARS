# Regenerating the documentation

The content of this VitePress site is **authored by reading the source code**, the
demos and the existing docs — there is no deterministic generator that reproduces
the prose from the `.pas` files. "Regenerating" therefore means having an AI agent
(Claude Code) re-synthesize **only the parts that changed** since the last sync.

This file is the playbook. It is meant to be cheap to repeat every now and then.

---

## TL;DR

1. Open **Claude Code** in the repository root (`C:\Sviluppo\Librerie\MARS`).
2. Paste the prompt from [§ Prompt to paste](#prompt-to-paste).
3. Review the changes, run a build (`build.cmd`), commit.

The agent diffs the code against the commit recorded in [`.docs-baseline`](./.docs-baseline),
updates the affected pages, and advances the baseline to the new `HEAD`.

---

## How it works

- [`.docs-baseline`](./.docs-baseline) holds the `commit=` the docs are aligned to.
- To find what changed since then:

  ```bash
  git diff <baseline-commit>..HEAD -- Source Demos
  git diff --stat <baseline-commit>..HEAD -- Source Demos   # quick overview
  ```

- The relevant Markdown lives under:
  - `guide/` — getting started & concepts
  - `server/` — engine, application, resources, attributes, injection, lifecycle, errors
  - `features/` — auth, authorization, FireDAC, serialization, OpenAPI, SSE, templates
  - `client/` — client components & usage
  - `demos/` — one page summarizing the demos
  - `reference/` — attributes / media types / parameters cheat-sheets

- Mapping hints (source → page):
  - `MARS.Core.Attributes.pas` → `server/attributes.md`, `reference/attributes.md`
  - `MARS.Core.Engine*.pas` → `server/engine.md`
  - `MARS.Core.Application*.pas` → `server/application.md`
  - `MARS.Core.Activation*.pas` → `server/request-lifecycle.md`
  - `MARS.Core.Injection*.pas` → `server/injection.md`
  - `MARS.Core.MessageBody*.pas` → `server/content-negotiation.md`
  - `MARS.Core.Exceptions.pas` → `server/error-handling.md`
  - `MARS.Core.Token*.pas`, `MARS.*JWT*.pas` → `features/authentication.md`, `features/authorization.md`
  - `MARS.Data.FireDAC*.pas`, `MARS.Data.UniDAC*.pas` → `features/firedac.md`
  - `MARS.Core.JSON.pas` → `features/serialization.md`
  - `MARS.OpenAPI*.pas`, `MARS.Metadata*.pas` → `features/openapi.md`
  - `MARS.Core.ServerSideEvents*.pas` → `features/sse.md`
  - `MARS.WebServer.Resources.pas`, `MARS.DelphiRazor.*`, WebStencils → `features/templates.md`
  - `MARS.Client.*.pas` → `client/*.md`
  - `MARS.Core.MediaType.pas` → `reference/media-types.md`
  - `MARS.Utils.Parameters.*`, JWT params → `reference/parameters.md`
  - new folders under `Demos/` → `demos/index.md`

---

## Prompt to paste

> Riallinea la documentazione VitePress in `docs/` ai sorgenti.
> 1. Leggi `docs/.docs-baseline` e prendi il valore `commit=`.
> 2. Esegui `git diff --stat <commit>..HEAD -- Source Demos` per vedere cosa è
>    cambiato, poi leggi i diff completi delle unit rilevanti.
> 3. Aggiorna **solo** le pagine Markdown interessate (usa le mappe in
>    `docs/REGEN.md`), conservando lo stile esistente: inglese, esempi Delphi reali
>    presi dai sorgenti/demo, niente API inventate — verifica le firme nei `.pas`.
> 4. Se sono comparse nuove demo in `Demos/`, aggiungile a `docs/demos/index.md`.
> 5. Aggiorna `commit=` e `date=` in `docs/.docs-baseline` al nuovo HEAD.
> 6. Lancia la build (`docs/build.cmd` oppure `npm run docs:build`) e correggi
>    eventuali link rotti (la build ha `ignoreDeadLinks: false`).
> Mostrami un riepilogo delle pagine modificate prima di concludere.

---

## After regenerating

```bat
build.cmd            REM static build into .vitepress/dist (validates links)
```

Then review with `git diff -- docs/` and commit. If you publish via GitHub Pages,
pushing to `master` triggers the deploy workflow (see `.github/workflows/docs.yml`).
