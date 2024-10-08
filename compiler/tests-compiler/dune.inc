
(library
 ;; compiler/tests-compiler/array_access.ml
 (name array_access_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules array_access)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/build_path_prefix_map.ml
 (name build_path_prefix_map_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules build_path_prefix_map)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/call_gen.ml
 (name call_gen_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules call_gen)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/compact.ml
 (name compact_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules compact)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/cond.ml
 (name cond_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules cond)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/direct_calls.ml
 (name direct_calls_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules direct_calls)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/effects.ml
 (name effects_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects) (>= %{ocaml_version} 5)))
 (modules effects)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/effects_continuations.ml
 (name effects_continuations_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules effects_continuations)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/effects_exceptions.ml
 (name effects_exceptions_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules effects_exceptions)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/effects_toplevel.ml
 (name effects_toplevel_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules effects_toplevel)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/eliminate_exception_handler.ml
 (name eliminate_exception_handler_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules eliminate_exception_handler)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/empty_cma.ml
 (name empty_cma_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules empty_cma)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/end_to_end.ml
 (name end_to_end_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules end_to_end)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/error.ml
 (name error_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules error)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/es6.ml
 (name es6_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules es6)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/exceptions.ml
 (name exceptions_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules exceptions)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/exports.ml
 (name exports_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules exports)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/getenv.ml
 (name getenv_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules getenv)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh1007.ml
 (name gh1007_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules gh1007)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh1051.ml
 (name gh1051_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects) %{arch_sixtyfour}))
 (modules gh1051)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects) %{arch_sixtyfour}))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh1320.ml
 (name gh1320_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules gh1320)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh1349.ml
 (name gh1349_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules gh1349)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh1354.ml
 (name gh1354_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules gh1354)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh1390.ml
 (name gh1390_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules gh1390)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh1481.ml
 (name gh1481_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules gh1481)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh1494.ml
 (name gh1494_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules gh1494)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh1559.ml
 (name gh1559_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules gh1559)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh1599.ml
 (name gh1599_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules gh1599)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh1659.ml
 (name gh1659_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules gh1659)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gh747.ml
 (name gh747_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules gh747)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/gl507.ml
 (name gl507_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules gl507)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/global_deadcode.ml
 (name global_deadcode_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules global_deadcode)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/inlining.ml
 (name inlining_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules inlining)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/js_parser_printer.ml
 (name js_parser_printer_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules js_parser_printer)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/jsopt.ml
 (name jsopt_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules jsopt)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/lambda_lifting.ml
 (name lambda_lifting_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules lambda_lifting)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/lazy.ml
 (name lazy_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules lazy)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects) (>= %{ocaml_version} 5)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/loops.ml
 (name loops_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules loops)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/macro.ml
 (name macro_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules macro)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/match_with_exn.ml
 (name match_with_exn_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules match_with_exn)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/minify.ml
 (name minify_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules minify)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/mutable_closure.ml
 (name mutable_closure_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules mutable_closure)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/obj.ml
 (name obj_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects) (>= %{ocaml_version} 5)))
 (modules obj)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects) (>= %{ocaml_version} 5)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/obj_dup.ml
 (name obj_dup_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules obj_dup)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/scopes.ml
 (name scopes_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules scopes)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/side_effect.ml
 (name side_effect_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules side_effect)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/sourcemap.ml
 (name sourcemap_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules sourcemap)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/static_eval.ml
 (name static_eval_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules static_eval)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/sys_command.ml
 (name sys_command_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules sys_command)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/sys_fs.ml
 (name sys_fs_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules sys_fs)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/tailcall.ml
 (name tailcall_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules tailcall)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/target_env.ml
 (name target_env_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules target_env)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/test_string.ml
 (name test_string_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules test_string)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/unix_fs.ml
 (name unix_fs_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules unix_fs)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))

(library
 ;; compiler/tests-compiler/variable_declaration_output.ml
 (name variable_declaration_output_15)
 (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
 (modules variable_declaration_output)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if (and (<> %{profile} wasm) (<> %{profile} wasm-effects)))
  (deps
   (file %{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))
