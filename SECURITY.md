# Security Policy

## Supported Versions

Security fixes are developed on the `develop` branch and then merged into `master` (stable) and, when applicable, backported to the most recent tagged release.

| Version                    | Supported          |
| -------------------------- | ------------------ |
| `master` / latest release  | :white_check_mark: |
| `develop` (pre-release)    | :white_check_mark: |
| Older releases             | :x:                |

If you are using an older version of MARS, please consider upgrading to the latest release before reporting an issue, as it may already be fixed.

## Reporting a Vulnerability

**Please do not report security vulnerabilities through public GitHub issues.**

Instead, use one of these private channels:

- **GitHub private vulnerability reporting** (preferred): go to the [Security tab](https://github.com/andrea-magni/MARS/security) of the repository and click *"Report a vulnerability"*.
- **Email**: contact the maintainer directly. Please include `[MARS SECURITY]` in the subject line.

When reporting, please include as much of the following as possible:

- A description of the vulnerability and its potential impact
- The affected component (e.g. Core, JWT/Token handling, FireDAC integration, messagebody readers/writers)
- Steps to reproduce, ideally with a minimal code sample or request/response trace
- The Delphi version, target platform, and MARS version/commit you tested against
- Any suggested fix or mitigation, if you have one

## What to Expect

- You will receive an acknowledgement of your report, typically within a few days.
- The report will be investigated and its impact assessed. You may be contacted for additional details.
- Once confirmed, a fix will be developed and released. You will be credited in the release notes and/or commit message, unless you prefer to remain anonymous.
- Please allow reasonable time for a fix to be released before any public disclosure (coordinated disclosure).

## Scope

MARS is a REST library: the overall security of an application built with it also depends on how it is configured and deployed. The following are generally **in scope**:

- Vulnerabilities in the MARS source code (e.g. injection, authentication/authorization bypass, unsafe deserialization, memory corruption)
- Insecure default configurations shipped with the library or its templates
- Issues in the JWT/token handling and security-related attributes

The following are generally **out of scope**:

- Vulnerabilities caused solely by application code built on top of MARS
- Issues in third-party dependencies (please report them upstream; a heads-up is still appreciated so mitigations can be documented)
- Use of known-weak configurations that the documentation already warns against (e.g. deploying with a default/development JWT secret)

## Security Best Practices for Users

When deploying MARS-based servers, please make sure to:

- **Always set a strong, unique JWT secret** in your configuration — never ship the default value to production
- Use parameterized queries / FireDAC parameters instead of interpolating user input into SQL or macros
- Run the server behind a reverse proxy (e.g. nginx) with TLS termination and rate limiting for public deployments
- Keep your MARS version up to date

Thank you for helping keep MARS and its users safe!
