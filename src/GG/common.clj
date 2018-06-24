(ns GG.common
  (:require [clojure.java.io :as io])
  (:import [org.apache.batik.transcoder.image ImageTranscoder]
           [org.apache.batik.transcoder TranscoderInput]
           [java.awt.image BufferedImage]
           [org.w3c.dom Document]
           [org.apache.batik.util SVGConstants XMLResourceDescriptor ]
           [org.apache.batik.dom.util SAXDocumentFactory]
           [org.apache.batik.anim.dom SVGDOMImplementation]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn load-svg
  "Load image into Batik Document"
  ^TranscoderInput  [svg-filename]
  (TranscoderInput. ^Document (.createDocument (SAXDocumentFactory.
                                                (SVGDOMImplementation/getDOMImplementation)
                                                (XMLResourceDescriptor/getXMLParserClassName))
                                               SVGConstants/SVG_NAMESPACE_URI
                                               SVGConstants/SVG_SVG_TAG
                                               (str "file:///" svg-filename)
                                               (io/input-stream (io/file svg-filename)))))

(defn transcode-svg
  "Convert transcoder input into BufferedImage"
  [^TranscoderInput input ^long w ^long h]
  (let [img (atom nil)
        transcoder (proxy [ImageTranscoder] []
                     (createImage [w h] (BufferedImage. w h BufferedImage/TYPE_INT_ARGB))
                     (writeImage [image output] (reset! img image)))]
    (.addTranscodingHint transcoder ImageTranscoder/KEY_WIDTH (float w))
    (.addTranscodingHint transcoder ImageTranscoder/KEY_HEIGHT (float h))
    (.transcode transcoder input nil)
    @img))

