JEDI Uses Wizard

This wizard watches for compiler error messages 'Undeclared identifier' (localized Delphi versions are also supported).
It keeps a static list of unit names and identifiers (stored in external text files) so it can automatically insert
appropriate unit(s) into the appropriate uses clause (with an optional confirmation prompt).
To resolve the error, a unit name may need to be added to interface uses, added to implementation uses, or moved from
implementation uses to interface uses (creating a new uses clause if needed).

The wizard is not activated automatically after installation of this package.
To activate it, do the following:

Install the package.
Go to Environment Options dialog.
On 'JEDI Options' tab, specify full path to the configuration file (e.g. C:\MyPath\JEDIUsesWizard.ini).
Check 'Active' checkbox.
You may want to check 'Prompt to confirm changes' checkbox, too.

Preferences are stored in HKEY_CURRENT_USER\Software\Borland\Delphi\6.0\JCL registry key.

Note that the JCL 1.20 identifier lists have been created manually and never tested.
The wizard code itself probably needs more testing, too.

TOndrej (tondrej@t-online.de)
