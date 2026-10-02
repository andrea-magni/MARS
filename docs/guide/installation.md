# Installation

MARS-Curiosity can be installed with the executable installer (recommended), with [TMS Smart Setup](https://github.com/tmssoftware/smartsetup), or manually from sources.

## Option 1 — Executable installer

The fastest way to get started:

1. Download the setup from the [latest release page](https://github.com/andrea-magni/MARS/releases/latest).
2. Run it. The installer configures the library paths and installs the design-time packages for your RAD Studio version.

## Option 2 — TMS Smart Setup {#tms-smart-setup}

[TMS Smart Setup](https://doc.tmssoftware.com/smartsetup/) is a free, open-source command-line tool that downloads, builds and registers Delphi libraries. MARS ships a `tmsbuild.yaml`, so Smart Setup can build it from sources for every supported Delphi version installed on your machine (**10.4 Sydney** and newer, Win32/Win64).

1. [Download and install Smart Setup](https://doc.tmssoftware.com/smartsetup/download/).
2. The community server, where open-source libraries are listed, is disabled by default. Enable it once:

   ```bash
   tms server-enable community true
   ```

3. Install MARS:

   ```bash
   tms install andreamagni.mars
   ```

Smart Setup clones the repository, compiles the runtime and design-time packages (Debug and Release), installs the design-time packages in the IDE and adds the MARS source folders to the library path. Later on, `tms update andreamagni.mars` gets the latest version and rebuilds it, and `tms uninstall andreamagni.mars` removes it.

::: tip Not listed yet?
If `tms install andreamagni.mars` reports that the product is unknown, it has not reached the community server yet. In the meantime, clone MARS into your Smart Setup folder (the folder containing `tms.config.yaml`; `tms config -print` shows it) and build it, running both commands from that folder:

```bash
git clone https://github.com/andrea-magni/MARS.git
tms build
```
:::

::: warning
Use one installation method only. If MARS is already installed with the executable installer or manually, remove it first, so the IDE doesn't load two copies of the same packages.
:::

`MARS.UniDAC` is not built by Smart Setup, because it requires Devart UniDAC: if you need it, build it manually from the `Packages` folder.

## Option 3 — Manual installation

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
2. Follow the prompts. It clones `Demos\MARSTemplate` into a new folder with your chosen project name, giving you a server (console / VCL / FMX / service / ISAPI / Apache / daemon variants), a client, and a test project. The `.ini` files of the new project get a freshly generated random `JWT.Secret`.

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
