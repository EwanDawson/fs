(ns me.raynes.fs.attr
  "File attributes"
  (:refer-clojure :exclude [name parents])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [me.raynes.fs :as fs])
  (:import [java.io File FilenameFilter]
           [java.nio ByteBuffer]
           [java.nio.file Files LinkOption]
           [java.nio.file.attribute UserDefinedFileAttributeView]
           [java.nio.charset StandardCharsets]))

(defn- xattr-view
  [path]
  (Files/getFileAttributeView (.toPath (fs/file path)) UserDefinedFileAttributeView (make-array LinkOption 0)))

(defn list-xattr
  "Retrieves the user-defined extended attributes of `path`"
  [path]
  (map #(keyword %1) (.list (xattr-view path))))

(defn read-xattr
  "For the file at `path`, retrieves the value of attribute `attr` as a byte vector"
  [path attr]
  (let [view (xattr-view path)
        attr-name (name attr)
        buf (ByteBuffer/allocate (.size view attr-name))]
    (do
      (.read view attr-name buf)
      (.flip buf)
      (vec (.array buf)))))

(defn read-xattr-str
  "For the file at `path`, retrieves the value of attrubute `attr` as a UTF-8 string"
  [path attr]
  (String. (byte-array (read-xattr path attr)) StandardCharsets/UTF_8))

(defn write-xattr
  "For the file at `path`, sets the attribute `attr` with the given `byte-vector`. Returns the number of bytes writted"
  [path attr byte-vector]
  (let [view (xattr-view path)
        attr-name (name attr)
        attr-value (byte-array byte-vector)]
    (.write view attr-name attr-value)))

(defn write-xattr-str
  "For the file at `path`, sets the attribute `attr` with the given string `value`. Returns the number of bytes written"
  [path attr value]
  (let [view (xattr-view path)
        attr-name (name attr)
        attr-value (.encode StandardCharsets/UTF_8 value)]
    (.write view attr-name attr-value)))

(defn xattr-str-map
  "For the file at `path`, read all the extended attributes into a map, converting the values into UTF-8 strings"
  [path]
  (let [attrs (list-xattr path)]
    (zipmap attrs (map #(read-attr-str path %1) attrs))))
