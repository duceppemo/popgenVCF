# Analysis module contract

Every registry module must declare:

1. A unique name and contract version.
2. Prerequisite modules.
3. An enablement rule.
4. Declared analysis-object outputs.
5. A runner accepting `(analysis, context)`.
6. A validator returning `PopgenVCFValidation`.
7. Scientific references.
8. A resource class: `light`, `standard`, `heavy`, or `external`.
9. Unit tests, numerical validation, and representative output fixtures.
10. Tables and figures with stable schemas documented in the data dictionary.

A runner is committed only after all declared outputs exist and validation
passes. Invalid results never replace the last valid analysis state.
