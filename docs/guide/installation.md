# Installation

MARS-Curiosity can be installed either with the executable installer (recommended) or manually from sources.

## Option 1 — Executable installer

The fastest way to get started:

1. Download the setup from the [latest release page](https://github.com/andrea-magni/MARS/releases/latest).
2. Run it. The installer configures the library paths and installs the design-time packages for your RAD Studio version.

## Option 2 — Manual installation

1. Get a copy of MARS (`git clone` or download the ZIP). Remember to initialize submodules if cloning:

   ```bash
   git clone --recurse-submodules https://github.com/andrea-magni/MARS.git
   ```

2. Add the following folders to your RAD Studio **Library Path** (Tools ▸ Options ▸ Language ▸ Delphi ▸ Library):

   - `[MARS Folder]\Source`
   - `[MARS Folder]\ThirdParty\delphi-jose-jwt\Source`
   - `[MARS Folder]\ThirdParty\mORMot\Source`
   - `[MARS Folder]\ThirdParty\Neslib.Yaml`
   - `[MARS Folder]\ThirdParty\Neslib.Yaml\Neslib`

3. Build the runtime/design-time packages. For example, on **13 Florence**:

   - Open `[MARS Folder]\Packages\13Florence\MARS.groupproj`
     - **Build All**
   - Open `[MARS Folder]\Packages\13Florence\MARSClient.groupproj`
     - **Build All**
     - **Install** `MARSClient.CoreDesign`
     - **Install** `MARSClient.FireDACDesign`

   Adjust the package folder to match your Delphi version.

::: tip Compatibility
Recent Delphi versions (from **10.4 Sydney** up to **13 Florence**) are fully supported. Older versions are largely compatible, down to **XE7**.
:::

## Bootstrap a new project with MARSCmd

MARS ships a small command-line utility that scaffolds a complete, ready-to-run project for you from the `MARSTemplate` demo.

1. Compile and run [`MARScmd_VCL.dproj`](https://github.com/andrea-magni/MARS/blob/master/Utils/Source/MARScmd/MARScmd_VCL.dproj) in `[MARS Folder]\Utils\Source\MARScmd`.
2. Follow the prompts. It clones `Demos\MARSTemplate` into a new folder with your chosen project name, giving you a server (console / VCL / FMX / service / ISAPI / Apache / daemon variants), a client, and a test project.

This is the recommended way to start a brand-new MARS application — see [Your First Server](/guide/getting-started) for a walkthrough of what the generated code does.

## Project structure

After installation, the repository layout is:

| Folder | Contents |
| --- | --- |
| `Source` | The MARS library units (server + client). |
| `Packages` | RAD Studio packages, one subfolder per Delphi version. |
| `Demos` | Ready-to-run sample projects (see [Demos](/demos/)). |
| `Utils` | Tools, including the `MARSCmd` project bootstrapper. |
| `ThirdParty` | Bundled dependencies (JOSE-JWT, mORMot, Neslib.Yaml, …). |
| `tests` | DUnitX test suite. |
