# Changelog

All notable changes to this project will be documented in this file.

## [0.5.0](https://github.com/NN-AI-Analytics/carts/compare/v0.4.0..v0.5.0) - 2025-05-07

### Developer

- Changing package name from `trialsim` to `carts` ([#172](https://github.com/NN-AI-Analytics/carts/issues/172)) - ([ff8fe0c](https://github.com/NN-AI-Analytics/carts/commit/ff8fe0cdd8114eb7337c1105e9c44327aa042cd4))

## [0.4.0](https://github.com/NN-AI-Analytics/carts/compare/v0.3.0..v0.4.0) - 2025-04-30

### Features

- *(Trial)* Adding `estimates` public field to store output of `Trial$run` ([#163](https://github.com/NN-AI-Analytics/carts//issues/163)) - ([9ec2174](https://github.com/NN-AI-Analytics/carts/commit/9ec21745cd4012cfac6a53129528968f301161c0))
- *(Trial)* [**breaking**] New `estimators` method to replace `add_estimators` and `update_estimators` ([#153](https://github.com/NN-AI-Analytics/carts//issues/153)) - ([fb7513a](https://github.com/NN-AI-Analytics/carts/commit/fb7513a52de83f428d5a3529c172b7adb9b2b8ad))
- *(Trial)* Adding `estimators` argument to `Trial$estimate_power` ([#165](https://github.com/NN-AI-Analytics/carts//issues/165)) - ([406e10b](https://github.com/NN-AI-Analytics/carts/commit/406e10b28a8be841dbcb52d6d7212608326f8372))
- *(Trial)* Improving print method ([#158](https://github.com/NN-AI-Analytics/carts//issues/158)) - ([8d3e1b8](https://github.com/NN-AI-Analytics/carts/commit/8d3e1b8d03ac559a7e17eaad60e9e53d3e4a7b90))
- *(Trial)* [**breaking**] Make `model.args` a private attribute  ([#144](https://github.com/NN-AI-Analytics/carts//issues/144)) - ([25c9d98](https://github.com/NN-AI-Analytics/carts/commit/25c9d985cc67265f6482a5071211526185d13ca3))
- *(Trial)* `Trial$estimate_samplesize` now supports calls with list of estimators ([#127](https://github.com/NN-AI-Analytics/carts//issues/127)) - ([36f741b](https://github.com/NN-AI-Analytics/carts/commit/36f741be391a8530c0316e97b7a8ef8fa5575eac))
- *(estimators)* `est_glm` allows to pass on arguments to `lava::estimate` ([#145](https://github.com/NN-AI-Analytics/carts//issues/145)) - ([d9ee9f3](https://github.com/NN-AI-Analytics/carts/commit/d9ee9f361dd8d693e5b3ccfc8483550595141731))
- Adding recurrent events with terminal events outcome (experimental feature) ([#138](https://github.com/NN-AI-Analytics/carts//issues/138)) - ([ccdf16c](https://github.com/NN-AI-Analytics/carts/commit/ccdf16c45aa846cef1dc6b0c503c8d227d5d2ac6))
- [**breaking**] Deprecate `ml_models` ([#137](https://github.com/NN-AI-Analytics/carts//issues/137)) - ([3a8b3a6](https://github.com/NN-AI-Analytics/carts/commit/3a8b3a678c988f8e0ba2ace96a63bb9191728e24))

### Bug Fixes

- Failing build process for `gettingstarted.Rmd` ([#170](https://github.com/NN-AI-Analytics/carts//issues/170)) - ([30abacc](https://github.com/NN-AI-Analytics/carts/commit/30abacc6d8bb60595f5d7b79bd195717f8b9a199))

### Documentation

- *(CONTRIBUTING)* Mentioning documentation of argument types for roxygen documentation ([#159](https://github.com/NN-AI-Analytics/carts//issues/159)) - ([95d113e](https://github.com/NN-AI-Analytics/carts/commit/95d113eebd22e3b08c259907195a14691c207302))
- *(Trial)* Adding roxygen documentation for object returned by `Trial$run()` ([#154](https://github.com/NN-AI-Analytics/carts//issues/154)) - ([4bfcb57](https://github.com/NN-AI-Analytics/carts/commit/4bfcb57fbfa9ccbbfc5475c3df87aec6d562f21c))

### Developer

- *(Trial)* Refactoring `Trial$simulate()` and `Trial$estimate_samplesize()` ([#155](https://github.com/NN-AI-Analytics/carts//issues/155)) - ([497938e](https://github.com/NN-AI-Analytics/carts/commit/497938e97e13a84dbc97ef282885259f644fee41))
- *(Trial)* `private$get_set_args_estimator` returns single element when returned list has length 1 ([#152](https://github.com/NN-AI-Analytics/carts//issues/152)) - ([1b07ae7](https://github.com/NN-AI-Analytics/carts/commit/1b07ae7a766d26e7f68c62b5445a1dc09b19926a))
- *(workflows)* Check roxygen documentation sync between `man` and `R` ([#148](https://github.com/NN-AI-Analytics/carts//issues/148)) - ([11c29da](https://github.com/NN-AI-Analytics/carts/commit/11c29da01bd0b93f0ef7997dab257e41b8541714))
- Removing `future::plan("multicore")` from all unit tests ([#162](https://github.com/NN-AI-Analytics/carts//issues/162)) - ([3d0cf33](https://github.com/NN-AI-Analytics/carts/commit/3d0cf33192c3484540c2bcc979c64492663e7821))
- Adding make targets for git cliff ([#151](https://github.com/NN-AI-Analytics/carts//issues/151)) - ([b8f5256](https://github.com/NN-AI-Analytics/carts/commit/b8f525628f6e82b7a702b1ee99aa95e7adf83e45))
- Fix `targeted::cate` arguments + simply Makefile test rule ([#131](https://github.com/NN-AI-Analytics/carts//issues/131)) - ([ad1bf11](https://github.com/NN-AI-Analytics/carts/commit/ad1bf11eb37ea7bda0973fb79e5189c9dbc8d22b))
- Removing `devtools::load_all` from test rule and use only `tinytest::test_package` ([#126](https://github.com/NN-AI-Analytics/carts//issues/126)) - ([8e12b33](https://github.com/NN-AI-Analytics/carts/commit/8e12b33677de2a19bbf074c2bf1b4e8e5c970328))

## [0.3.0](https://github.com/NN-AI-Analytics/carts/compare/v0.2.0..v0.3.0) - 2024-11-21

### Bug Fixes

- *(summary.runtrials)* Rejection function uses now correct critical value for non-inferiority test when no alternative argument is supplied ([#117](https://github.com/NN-AI-Analytics/carts//issues/117)) - ([81f46c8](https://github.com/NN-AI-Analytics/carts/commit/81f46c8bace1795ac8d6266ad245ec33805421de))
- `trial_default_args` works now as expected when `trialsim::setargs` is used to change default arguments of the covariate, outcome or exclusion model ([#113](https://github.com/NN-AI-Analytics/carts//issues/113)) - ([9222eb9](https://github.com/NN-AI-Analytics/carts/commit/9222eb9e122fe99405e19ad30b49c6310f972850))
- Fixing `trial_default_args` to not fail when function argument is a function added with setargs ([#85](https://github.com/NN-AI-Analytics/carts//issues/85)) - ([c215190](https://github.com/NN-AI-Analytics/carts/commit/c215190e6766bd51c9eb8a40630e7903211166f2))

### Developer

- *(Trial)* Enable sharing param definitions across `Trial` methods by moving shared param fields to class definition. ([#89](https://github.com/NN-AI-Analytics/carts//issues/89)) - ([40d6fcf](https://github.com/NN-AI-Analytics/carts/commit/40d6fcfb3ef1316969c78de4c672b8ddf87403ea))
- Log final estimate of `Trial` ([#119](https://github.com/NN-AI-Analytics/carts//issues/119)) - ([c9191c4](https://github.com/NN-AI-Analytics/carts/commit/c9191c42fd168395d90b620d54c16847bfe77bb5))
- Adding additional tests for `summary.runtrials` ([#108](https://github.com/NN-AI-Analytics/carts//issues/108)) - ([04da3eb](https://github.com/NN-AI-Analytics/carts/commit/04da3eb48ba21ba2358b0922a8e9e5c548acf344))
- Fix badges in `README.Rmd` ([#110](https://github.com/NN-AI-Analytics/carts//issues/110)) - ([b2fc301](https://github.com/NN-AI-Analytics/carts/commit/b2fc3012f82ec38d646b82bddc0a34cedb0f92b3))
- Combine tests for `estimate_samplesize` in `test_Trial.R` ([#107](https://github.com/NN-AI-Analytics/carts//issues/107)) - ([13df352](https://github.com/NN-AI-Analytics/carts/commit/13df352956302543e21dd801b6378c86ee08bb9f))
- Fix github actions workflows failing during pdftools installation ([#93](https://github.com/NN-AI-Analytics/carts//issues/93)) - ([9104b10](https://github.com/NN-AI-Analytics/carts/commit/9104b104639dbb2642c3630f769c105ddcb6be51))

### Documentation

- *(Trial)* Updating documentation of initialize method ([#104](https://github.com/NN-AI-Analytics/carts//issues/104)) - ([9327122](https://github.com/NN-AI-Analytics/carts/commit/9327122790520d5d43642b71fba34079a11a8f15))

### Features

- *(Trial)* Adding `args_summary` method to get, specify and update the `summary.args` attribute ([#103](https://github.com/NN-AI-Analytics/carts//issues/103)) - ([f6e1f95](https://github.com/NN-AI-Analytics/carts/commit/f6e1f9531b800bd1b292953703325d863fb20d39))
- *(Trial)* Rename `parameter` into `args_model` method ([#102](https://github.com/NN-AI-Analytics/carts//issues/102)) - ([255682f](https://github.com/NN-AI-Analytics/carts/commit/255682f3494d84cb56a11a349d0378c0ce173d64))
- *(Trial)* Adding `summary.args` as argument to Trial initializer ([#95](https://github.com/NN-AI-Analytics/carts//issues/95)) - ([a334ad4](https://github.com/NN-AI-Analytics/carts/commit/a334ad42ad45c52dbd442c24a1e3723bf3bcda8e))
- *(Trial)* Adding check to `trial_simulate` for raising error when trying to simulate a trial dataset with duplicated column names ([#84](https://github.com/NN-AI-Analytics/carts//issues/84)) - ([7c96cb5](https://github.com/NN-AI-Analytics/carts/commit/7c96cb532c9466644e64f0a65a51d753f732f18f))
- *(Trial)* Removing unused seed argument from simulate method and adding sub-section on reproducibility to gettingstarted vignette ([#87](https://github.com/NN-AI-Analytics/carts//issues/87)) - ([dd0fd45](https://github.com/NN-AI-Analytics/carts/commit/dd0fd45225be40b72f809376f52af2c6e43a92cd))
- *(Trial)* removing `model.args` argument from `Trial$update` method ([#90](https://github.com/NN-AI-Analytics/carts//issues/90)) - ([3922e2f](https://github.com/NN-AI-Analytics/carts/commit/1bca15b476735e0e7dab38e642296f49e655e056))
- *(setargs)* Adding `setargs.warn = TRUE` to inform when trying to set default values of arguments that do not exist in function f ([#121](https://github.com/NN-AI-Analytics/carts//issues/121)) - ([3922e2f](https://github.com/NN-AI-Analytics/carts/commit/3922e2f0ffbaa7edf8d67dc887d28a2d5caeb585))
- *(outcome_count)* exposure argument is now allowed to be a function which takes the covariate data as its first input ([#86](https://github.com/NN-AI-Analytics/carts//issues/86)) - ([1bca15b](https://github.com/NN-AI-Analytics/carts/commit/7c754c9ccaae4c92ee8e3cb57499eb0eeee8866c))

## [0.2.0](https://github.com/NN-AI-Analytics/carts/compare/v0.1.0..v0.2.0) - 2024-08-14

### Bug Fixes

- *(glm_est)* Handle cases gracefully when model estimation fails ([#33](https://github.com/NN-AI-Analytics/carts//issues/33)) - ([1b9c261](https://github.com/NN-AI-Analytics/carts/commit/1b9c261fa4bac51606a0da6d1a994f5523f6d23d))
- *(optim_sa)* Fixing integer optimization + control arguments ([#66](https://github.com/NN-AI-Analytics/carts//issues/66)) - ([95dffd6](https://github.com/NN-AI-Analytics/carts/commit/95dffd699f146f90ed6dfbccadd293783496d71d))

### Documentation

- Adding CONTRIBUTING.md ([#75](https://github.com/NN-AI-Analytics/carts//issues/75)) - ([6a6f369](https://github.com/NN-AI-Analytics/carts/commit/6a6f369edbcd7b9fc0c2b775687f1b698aca462b))
- *(utils)* Improving documentation for setargs and setallargs to indicate behavior when base::missing is used to check missing arguments ([#65](https://github.com/NN-AI-Analytics/carts//issues/65)) - ([cab9ade](https://github.com/NN-AI-Analytics/carts/commit/cab9adecc08c359168e1b4696c4db38505fc2063))

### Features

- *(Trial)* Adding summary.args argument to estimate_samplesize method + improve the method's documentation ([#60](https://github.com/NN-AI-Analytics/carts//issues/60)) - ([cddb361](https://github.com/NN-AI-Analytics/carts/commit/cddb3616c2ac54766566bedab9c2b10da377204f))
- *(Trial)* Adding trial_default_args function for getting function arguments from covariate, outcome and exclusion models.  ([#59](https://github.com/NN-AI-Analytics/carts//issues/59)) - ([43e4f96](https://github.com/NN-AI-Analytics/carts/commit/43e4f96cfcc17ffc4091e70f0139028fac1b6e99))
- *(Trial)* Adding optional arguments to add_estimators and update_estimators ([#62](https://github.com/NN-AI-Analytics/carts//issues/62)) - ([d11fcd7](https://github.com/NN-AI-Analytics/carts/commit/d11fcd7a11557be54879c2d7b6bc5d8e1488ae68))
- *(Trial)* Adding model.args argument to Trial$parameter method ([#32](https://github.com/NN-AI-Analytics/carts//issues/32)) - ([db19179](https://github.com/NN-AI-Analytics/carts/commit/db191798fd8cec90cf485cda8819f5ab135354d9))
- *(Trial)* Updating simulate method to use model.args attribute when set for trial object - ([03f2941](https://github.com/NN-AI-Analytics/carts/commit/03f29417f6f8a73d4b03c76298dd00f9631de3cb))
- *(covar_sim)* Additional tests and documentation for add_covar and covar_join ([#63](https://github.com/NN-AI-Analytics/carts//issues/63)) - ([33cd43a](https://github.com/NN-AI-Analytics/carts/commit/33cd43a1cf46c740b9aae82d7f3d33587eb04ca7))
- *(ml_models)* General improvements for targeted::ml_model wrappers ([#58](https://github.com/NN-AI-Analytics/carts//issues/58)) - ([cffed52](https://github.com/NN-AI-Analytics/carts/commit/cffed520132746880fa60c5546311dba585104d1))
- *(optimization)* Improving optimization methods (bisection + optim_sa) ([#34](https://github.com/NN-AI-Analytics/carts//issues/34)) - ([e81260a](https://github.com/NN-AI-Analytics/carts/commit/e81260ad8c4772c70272c407b48b170f32adc8ee))
- *(utils)* Changing behavior of append method when appending to an unnamed list ([#64](https://github.com/NN-AI-Analytics/carts//issues/64)) - ([50ef26a](https://github.com/NN-AI-Analytics/carts/commit/50ef26aa2f2fa7a4d95d385c6dde6c8533ad0366))
- Changing outcome_name argument to outcome.name in all outcome models ([#76](https://github.com/NN-AI-Analytics/carts//issues/76)) - ([b2f01dc](https://github.com/NN-AI-Analytics/carts/commit/b2f01dcc582c12ac87c023cc82f2ec4a9892d789))

### Developer

- Improving continuous integration pipeline ([#70](https://github.com/NN-AI-Analytics/carts//issues/70)) - ([fa126cb](https://github.com/NN-AI-Analytics/carts/commit/fa126cb1544eb4a46e222a9873255e7b788bd34b))
- Organize test files to match the organization of R/ files ([#77](https://github.com/NN-AI-Analytics/carts//issues/77)) - ([f3a07ce](https://github.com/NN-AI-Analytics/carts/commit/f3a07ceeb4381e99d498fc3a8a58942f074d5bfd))

## [0.1.0] - 2024-05-31

Initial stable release.
