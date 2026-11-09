# Module Views

This directory contains the static UI builders for the four tabs:

| Builder | Entry point | Main screen |
| --- | --- | --- |
| `build_setup_ui(id)` | `mod_setup_ui(id)` | Model Build, Model Update, and Model Refresh. |
| `build_channels_ui(id)` | `mod_channels_ui(id)` | Channel manager, audit, breaks, and aliases. |
| `build_process_ui(id)` | `mod_process_ui(id)` | Channel actions, modeled/ForIndices tables, SAP, and Total Check. |
| `build_export_ui(id)` | `mod_export_ui(id)` | Readiness, SCWA review, and download controls. |

Each builder creates a namespace with `NS(id)`. Keep the IDs stable: the
corresponding controller registers outputs and observers under those IDs, and
`server.R` depends on the existing module entry points. Moving markup into a
view does not create a new Shiny module or change the `server.R` interface.

The app shell, top-level navigation, and shared operation-status overlay live
in `ui.R`. Styling lives in `www/styles.css`; browser behavior, including the
operation-status modal, lives in `www/custom.js`. Views should not read data,
perform calculations, or write session state. Dynamic content belongs in
`renderUI` and other controller outputs, with placeholders here.

When changing a screen, update the view and its controller together. Check
desktop and mobile layout, long channel names, output IDs, and whether the
modal or a native Shiny notification is duplicating progress. Parse the view
and run the relevant module-state regression scripts before deploying.
