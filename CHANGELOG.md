# v0.2.2

* Added new interface functions to *Fields* for working with the tree of expressions within any `AbstractField`

# v0.2.1

* Fixed broken export of `OrthographicProjection`

# v0.2.0

* Removed unused project dependencies
* **breaking** Clarified that Cam3D's field-of-view field is vertical
* **breaking** Brought back Cam3D changes that were accidentally lost when refactoring B+ into the three sub-packages
* Fixed bug with ECS components when inheriting from a parent with type params, that uses those type params in fields

# v0.1.1

* Fix bugs with ECS queries involving UnionAll component types

# v0.1.0

* Initial public release