# [Ray Tracing](https://github.com/RayTracing/raytracing.github.io/tree/book1-review) series

Based on development branch.

Notes:

* Each change and resulting image has it's own separate folder
* Rendering is computed using several parallel threads (via `pmap`)
* Perlin is implemented but `fastmath.random` version is used (for a speed)
* `fastmath.vector` is used for vector operations
* Some minor enhancements are made
* The result is displayed in a window during rendering. `clojure2d.pixels` buffer is used as a storage.

Yes, it's slow (c++ version is also slow :) )

Optimization ideas:

* Access fields directly (currently via keywords) in records
* Avoid operations on map `assoc` and `update`
* Unroll some loops and lazy sequence generators
