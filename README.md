Parasol
=======

Parasol is a painting application written in Common Lisp in combination with Qt through [CommonQt](http://common-lisp.net/project/commonqt/). Its focus lies on tablet input with a flexible brush engine and infinite canvas support. There is no stable release planned for the time being, but the repository should be in buildable and runnable state most of the time. Development is currently done in SBCL on Linux; as such building on other implementations or platforms may fail for a multitude of reasons. If you want to test it regardless, I would appreciate [feedback](https://github.com/Shinmera/parasol/issues) a lot.

Rough roadmap:

 * DONE Tablet input
 * DONE Incremental spline curve interpolation
 * DONE Color picker
 * DONE Layers
 * DONE Infinite Canvas
 * DONE Basic history
 * DONE CL-style programmatical brush engine
 * DONE Whole-stroke compositing support
 * DONE Layer compositing support
 * TODO Efficient history algorithm
 * DONE Cutoff/Crop support
 * DONE OpenRaster save/load
 * TODO Zoom support
 * TODO Global history
 * TODO Texture-blended brushes
 * TODO Brush setting presets
 * TODO User-defined brushes
 * TODO Layer stacks/groups
 * TODO Tool engine
 * TODO Efficient real layer size detection
 * TODO Usability testing
 * TODO Optimization
 * TODO Multi-Platform testing
 * TODO Multi-Implementation testing
 
And probably loads more.
