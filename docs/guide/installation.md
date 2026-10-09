# Installation

MARS-Curiosity can be installed with the executable installer (recommended), with [TMS Smart Setup](https://github.com/tmssoftware/smartsetup), or manually from sources.

## Option 1 — Executable installer

The fastest way to get started:

1. Download the setup from the [latest release page](https://github.com/andrea-magni/MARS/releases/latest).
2. Run it. The installer configures the library paths and installs the design-time packages for your RAD Studio version.

::: warning Keep your projects out of the MARS folder
Uninstalling MARS, which the setup also does before installing a new version, deletes the content of the MARS folder. Since 1.8.1 the uninstaller leaves alone the folders of `Demos` that are not demos shipped with MARS, and the setup moves the ones it finds in the `Demos` folder of the previous version to `Documents\MARS Projects` before uninstalling it (older uninstallers delete the whole `Demos` folder). Anyway, create your projects somewhere else: [MARSCmd](#bootstrap-a-new-project-with-marscmd) proposes `Documents\MARS Projects`.
:::

## Option 2 — TMS Smart Setup {#tms-smart-setup}

[TMS Smart Setup](https://doc.tmssoftware.com/smartsetup/) is a free, open-source command-line tool that downloads, builds and registers Delphi libraries. MARS ships a `tmsbuild.yaml`, so Smart Setup can build it from sources for every supported Delphi version installed on your machine (**10.4 Sydney** and newer, Win32/Win64).

1. [Download and install Smart Setup](https://doc.tmssoftware.com/smartsetup/download/) (version 3.5 or later).
2. The community server, where open-source libraries are listed, is disabled by default. Enable it once:

   ```bash
   tms server-enable community true
   ```

3. Install MARS:

   ```bash
   tms install andreamagni.mars
   ```

Smart Setup clones the repository, compiles the runtime and design-time packages (Debug and Release), installs the design-time packages in the IDE and adds the MARS source folders to the library path. Later on, `tms update andreamagni.mars` gets the latest version and rebuilds it, and `tms uninstall andreamagni.mars` removes it.

The JOSE [JWT backend](/features/authentication#jwt-backends) (`MARS.JOSE` package) uses the [delphi-jose-jwt](https://github.com/paolo-rossi/delphi-jose-jwt) library, which Smart Setup installs as a product of its own: `MARS.JOSE` is built only when it is installed. The mORMot backend, the default on Windows, needs nothing else. To use JOSE:

```bash
tms install paolo-rossi.delphi-jose-jwt
```

Installing it after MARS is fine: Smart Setup rebuilds MARS and adds `MARS.JOSE`.

::: warning
Use one installation method only. If MARS is already installed with the executable installer or manually, remove it first, so the IDE doesn't load two copies of the same packages.
:::

`MARS.UniDAC`, `MARS.MyDAC` and `MARS.IBDAC` are not built by Smart Setup, because they require Devart UniDAC, MyDAC and IBDAC: if you need them, enable `MARS_UNIDAC`, `MARS_MYDAC` or `MARS_IBDAC` in `Source\MARS.inc` and build the package manually from the `Packages` folder.

The test projects (the `...Tests` project of an application created with MARSCmd, `MARS.Tests`) also need [Delphi-Mocks](https://github.com/VSoftTechnologies/Delphi-Mocks), which is not a Smart Setup product: clone it and add its `Source` folder to the library path.

## Option 3 — Manual installation

1. Get a copy of MARS with `git clone`, including its submodules ([delphi-jose-jwt](https://github.com/paolo-rossi/delphi-jose-jwt), used by the JOSE JWT backend, and [Delphi-Mocks](https://github.com/VSoftTechnologies/Delphi-Mocks), used by the test projects; the other third-party libraries are part of the repository):

   ```bash
   git clone --recurse-submodules https://github.com/andrea-magni/MARS.git
   ```

   In a clone made without `--recurse-submodules`, run `git submodule update --init`. The **Download ZIP** button of GitHub leaves `ThirdParty\delphi-jose-jwt` and `ThirdParty\Delphi-Mocks` empty: download them at the versions shown in [`ThirdParty/README.md`](https://github.com/andrea-magni/MARS/blob/master/ThirdParty/README.md) and extract them there.

2. Add the following folders to your RAD Studio **Library Path** (Tools ▸ Options ▸ Language ▸ Delphi ▸ Library):

   - `[MARS Folder]\Source`
   - `[MARS Folder]\ThirdParty\delphi-jose-jwt\Source\Common`
   - `[MARS Folder]\ThirdParty\delphi-jose-jwt\Source\JOSE`
   - `[MARS Folder]\ThirdParty\mORMot\Source`
   - `[MARS Folder]\ThirdParty\Neslib.Yaml`
   - `[MARS Folder]\ThirdParty\Neslib.Yaml\Neslib`
   - `[MARS Folder]\ThirdParty\Delphi-Mocks\Source` (test projects: `MARS.Tests`, the `...Tests` project of a new application)

3. Build the runtime/design-time packages. For example, on **13 Florence**:

   - Open `[MARS Folder]\Packages\13Florence\MARS.groupproj`
     - **Build All** (it also builds the `JOSE` package of delphi-jose-jwt, required by `MARS.JOSE`)
   - Open `[MARS Folder]\Packages\13Florence\MARSClient.groupproj`
     - **Build All**
     - **Install** `MARSClient.CoreDesign`
     - **Install** `MARSClient.FireDACDesign`

   Adjust the package folder to match your Delphi version.

::: tip Compatibility
MARS supports Delphi **10.4 Sydney** up to **13 Florence**. Earlier versions are not supported: compiling MARS with them stops with an explicit error.
:::

## Bootstrap a new project with MARSCmd

MARS ships a small command-line utility that scaffolds a complete, ready-to-run project for you from a template:

- `MARSTemplate`: Indy, endpoints as resource classes;
- `MARSTemplateDCS`: Delphi Cross Socket, endpoints as resource classes;
- `MARSTemplateRoutes`: Indy, endpoints as [routes](/server/routes) defined in code (Express style).

1. Compile and run [`MARScmd_VCL.dproj`](https://github.com/andrea-magni/MARS/blob/master/Utils/Source/MARScmd/MARScmd_VCL.dproj) in `[MARS Folder]\Utils\Source\MARScmd`.
2. Follow the prompts. Choose the template on the first page: MARSCmd lists the `Demos\MARSTemplate*` folders (`...` picks a template from another folder). It clones the template into a new folder with your chosen project name, giving you a server (console / VCL / FMX / service / ISAPI / Apache / daemon variants), a client, and a test project. The `.ini` files of the new project get a freshly generated random `JWT.Secret`.

The settings of the new project are in `bin\Server.ini`, shared by all its server flavors (console, VCL, FMX, service, daemon, ISAPI, Apache, FastCGI): each flavor has its own small `.ini`, named after the executable, that includes `Server.ini` with an [`[Include]` section](/reference/parameters#shared-configuration-include) and can override any value (i.e. a different `Port`). The generated `JWT.Secret` is in `Server.ini`.

The new project goes to `Documents\MARS Projects\<project name>` by default; next time MARSCmd proposes the folder used last (saved in `%APPDATA%\MARS-Curiosity\MARSCmd.ini`; a saved folder that no longer exists, or that is inside the MARS folder or the temp folder, is ignored). A destination inside the MARS folder asks for confirmation, as uninstalling or upgrading MARS would delete it, and an existing folder that is not empty is never overwritten.

The template refers to the MARS folder with relative paths (`..\..\Source`). Outside the MARS folder, MARSCmd writes them as `$(MARSDIR)\Source`, `$(MARSDIR)\ThirdParty\...`: `MARSDIR` is the IDE environment variable set by the setup to the MARS folder (Tools ▸ Options ▸ IDE ▸ Environment Variables). With TMS Smart Setup or a manual installation, define it yourself or rely on the library path.

This is the recommended way to start a brand-new MARS application — see [Your First Server](/guide/getting-started) for a walkthrough of what the generated code does.

## Project structure

After installation, the repository layout is:

| Folder | Contents |
| --- | --- |
| `Source` | The MARS library units (server + client). |
| `Packages` | RAD Studio packages, one subfolder per Delphi version. |
| `Demos` | Ready-to-run sample projects (see [Demos](/demos/)). |
| `Utils` | Tools, including the `MARSCmd` project bootstrapper. |
| `ThirdParty` | Bundled dependencies (Delphi-Cross-Socket, JOSE-JWT, mORMot, Neslib.Yaml, Delphi-Mocks): origin, version and license of each in [`ThirdParty/README.md`](https://github.com/andrea-magni/MARS/blob/master/ThirdParty/README.md). |
| `tests` | DUnitX test suite. |
