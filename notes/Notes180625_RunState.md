# Run State

There should be a standard interface for "running" `Obj` instances. This should produce a state which is observable
and can be integrated with views in Mellite, so there can be running stuff without necessarily having a GUI view
corresponding to that, and allowing elements of a workspace to be run "headless". There should be (optional) progress
tracking and the possibility to cancel or stop the process.

## Objects and their Run Actions

|Object                 |Action                     |Type
|-----------------------|---------------------------|-----------
|`Action`               |Execute action             |instant
|`Artifact`             |n/a                        |
|`ArtifactLocation`     |n/a                        |
|`Code`                 |n/a                        |
|`DataSource`\*         |n/a                        |
|`Eye`\*                |?                          |
|`Ensemble`             |Play (aural) ensemble      |indefinite
|`Folder`               |?                          |
|`FadeSpec`             |n/a                        |
|`FScape`               |Render graph               |indefinite/progress
|`FreesoundRetrieval`   |n/a                        |
|`Grapheme`             |Play (aural) grapheme ?    |indefinite
|`Markdown`             |n/a                        |
|`Negatum`\*            |?                          |
|`Nuages`               |?                          |
|`OscNode`\*            |Run receiver               |indefinite
|`ParamSpec`            |n/a                        |
|`Pattern`              |?                          |indefinite
|`Plot`\*               |n/a                        |
|`Proc`                 |Play (aural) proc          |indefinite/progress
|`SOM`\*                |?                          |
|`Sonification`\*       |Play (aural) sonification  |indefinite/progress
|`SphereGNG`\*          |?                          |
|`SVMModel`\*           |?                          |
|`Timeline`             |Play (aural) timeline      |indefinite

\* non-standard-party objects

So we have a hybrid between actions that require an aural context and those that don't. What would be the best
behaviour for containers, like `Folder`, `Ensemble`, `Timeline`; should they require that the server is booted?
Should they gracefully handle both cases? `Ensemble` currently requires the aural context, ensuring that we don't
execute an action before the server is booted, so it will not be out-of-sync with other elements such as `Proc`.
Have a look at timeline editor which controls a transport irrespective of aural system being booted.
