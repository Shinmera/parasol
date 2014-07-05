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
 * DONE Zoom support
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
 * TODO Perspective and modelling tools
 * TODO Brush components brush with pipeline UI
 
And probably loads more.

Some Screenshots:
![screenshot-1](http://33.media.tumblr.com/67f4819cc504af01d2b499fcb2c17a14/tumblr_n6x6vqNcWj1qgi4tso1_1280.png)
![screenshot-2](http://shinmera.tymoon.eu/public/screenshot-2014.06.13-21:10:30.png)
![screenshot-3](http://33.media.tumblr.com/4319b5308519895ff48f81985f0364a3/tumblr_n7yafz479W1qgi4tso1_1280.png)
