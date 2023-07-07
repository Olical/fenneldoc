(import-macros {: defn : defn- : ns : def} :cljlib)

(ns parser
  "A module that evaluates Fennel code and obtains documentation for each
item in the module.  Supports sandboxing."
  (:require
   [fennel :refer [dofile metadata]]
   [fennel.compiler]
   [cljlib :refer [get-in]]
   [markdown :refer [gen-function-signature gen-item-signature gen-item-documentation]]))

(defn- sandbox-module [module file]
  (setmetatable
   {}
   {:__index (fn []
               (io.stderr:write
                (.. "ERROR: access to '" module
                    "' module detected in file: " file
                    " while loading\n"))
               (os.exit 1))}))

(defn create-sandbox
  "Create sandboxed environment to run `file` containing documentation,
and tests from that documentation.

Does not allow any IO, loading files or Lua code via `load`,
`loadfile`, and `loadstring`, using `rawset`, `rawset`, and `module`,
and accessing such modules as `os`, `debug`, `package`, `io`.

This means that your files must not use these modules on the top
level, or run any code when file is loaded that uses those modules.

You can provide an `overrides` table, which contains function name as
a key, and function as a value.  This function will be used instead of
specified function name in the sandbox.  For example, you can wrap IO
functions to only throw warning, and not error."
  ([file] (create-sandbox file {}))
  ([file overrides]
   (let [env {: assert                  ; allowed modules
              : bit32
              : collectgarbage
              : coroutine
              : dofile
              : error
              : getmetatable
              : ipairs
              : math
              : next
              : pairs
              : pcall
              : rawequal
              : rawlen
              : require
              : select
              : setmetatable
              : string
              : table
              : tonumber
              : tostring
              : type
              : unpack
              : utf8
              : xpcall

              :load nil                 ; disallowed modules
              :loadfile nil
              :loadstring nil
              :rawget nil
              :rawset nil
              :module nil

              :arg []                   ; sandboxed modules
              :print (fn []
                       (io.stderr:write "ERROR: IO detected in file: " file " while loading\n")
                       (os.exit 1))
              :os (sandbox-module :os file)
              :debug (sandbox-module :debug file)
              :package (sandbox-module :package file)
              :io (sandbox-module :io file)}]
     (set env._G env)
     (each [k v (pairs overrides)]
       (tset env k v))
     env)))

(defn- function-name-from-file [file]
  (let [sep (package.config:sub 1 1)]
    (-> file
        (string.gsub (.. ".*" sep) "")
        (string.gsub "%.fnl$" ""))))

(defn- get-module-docs
  ([module config]
   (get-module-docs {} module config nil))
  ([docs module config parent]
   (each [id val (pairs module)]
     (when (not= (string.sub id 1 1) :_)
       (match (type val)
         :table (get-module-docs docs val config id)
         _ (tset docs
                 (if parent (.. parent "." id) id)
                 {:docstring (metadata:get val :fnl/docstring)
                  :arglist (metadata:get val :fnl/arglist)}))))
   docs))

(defn- module-from-file [file]
  (let [sep (package.config:sub 1 1)
        module (-> file
                   (string.gsub sep ".")
                   (string.gsub "%.fnl$" ""))]
    module))

(defn- require-module
  "Require file as module in protected call.  Returns multiple values
with first value corresponding to pcall result."
  [file config]
  (let [env (when config.sandbox
              (create-sandbox file))]
    (match (pcall dofile
                  file
                  {:useMetadata true
                   :env env
                   :allowedGlobals false}
                  (module-from-file file))
      (true ?module) (let [module (or ?module {})]
                       (values (type module) module :functions))
      ;; try again, now with compiler env
      (false) (match (pcall dofile
                            file
                            {:useMetadata true
                             :env :_COMPILER
                             :allowedGlobals false
                             :scope (. compiler :scopes :compiler)}
                            (module-from-file file))
                (true module) (values (type module) module :macros)
                (false msg) (values false msg)))))

(def :private
  warned {})

(defn module-info
  "Returns table containing all relevant information accordingly to
`config` about the module in `file` for which documentation is
generated."
  [file config]
  (match (require-module file config)
    (:table module module-type) {:module (or (get-in config [:modules-info file :name])
                                             file)
                                 :file file
                                 :type module-type
                                 :f-table (if (= module-type :macros) {} module)
                                 :requirements (get-in config [:test-requirements file] "")
                                 :version (or (get-in config [:modules-info file :version])
                                              config.project-version)
                                 :description (get-in config [:modules-info file :description])
                                 :copyright (or (get-in config [:modules-info file :copyright])
                                                config.project-copyright)
                                 :license (or (get-in config [:modules-info file :license])
                                              config.project-license)
                                 :items (get-module-docs module config)
                                 :doc-order (or (get-in config [:modules-info file :doc-order])
                                                (get-in config [:project-doc-order file])
                                                [])}
    ;; function modules have no version, license, or description keys,
    ;; as there's no way of adding this as a metadata or embed into
    ;; function itself.  So module description is set to a combination
    ;; of function docstring and signature if allowed by config.
    ;; Table of contents is also omitted.
    (:function function) (let [docstring (metadata:get function :fnl/docstring)
                               arglist (metadata:get function :fnl/arglist)
                               fname (function-name-from-file file)]
                           {:module (get-in config [:modules-info file :name] file)
                            :file file
                            :f-table {fname function}
                            :type :function-module
                            :requirements (get-in config [:test-requirements file] "")
                            :documented? (not (not docstring)) ;; convert to Boolean
                            :description (.. (get-in config [:modules-info file :description] "")
                                             "\n"
                                             (gen-function-signature fname arglist config)
                                             "\n"
                                             (gen-item-documentation docstring config.inline-references))
                            : arglist
                            :items {}})
    (false err) (do (io.stderr:write "Error loading " file "\n" err "\n")
                    nil)
    _ (do (io.stderr:write "Error loading " file "\nunhandled error!\n" (tostring _))
          nil)))

parser

;; LocalWords:  sandboxed Lua loadfile loadstring rawset os io config
;; LocalWords:  metadata docstring fenneldoc
