# Maintenance Scripts

These scripts are developer tools. The app does not source or execute them at
startup, and they are not part of the automated regression suite.

| Script | Purpose | Writes source files? |
| --- | --- | --- |
| `check_controller_parity.R` | Compare extracted module code against a specified historical Git revision. Later intentional changes can make this comparison fail. | No |
| `extract_module_sections.R` | Move selected server expressions into controller files during modularization. | Yes |
| `extract_module_ui.R` | Move a module UI definition into a view builder during modularization. | Yes |

Run them only from the repository root with the arguments shown in each
script's usage message. The two extraction scripts rewrite files and are meant
for controlled migration work, not routine app operation. Use `tests/` for
repeatable behavior checks instead.
