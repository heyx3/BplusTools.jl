# v0.3.2

\[none, just syncing with updates to BplusCore and BplusApp]

# v0.3.1

* Fix bug with FileCacher's error-handling

# v0.3.0

\[none, just syncing with BplusCore's updates]

# v0.2.1

* Fixed broken export of `OrthographicProjection`
* Numerous improvements to **Fields**:
  * Added new interface functions to work with the tree of expressions within any `AbstractField`
  * Added more math fields: `asin`, `acos`, `atan`, `atan2`
  * Added an overload of `get_field()` for working with floats directly rather than `Vec{1}`.
  * Added special handling to `pow` field for invalid inputs (e.x. `pow(-1, 0.5)`)

# v0.2.0

* Removed unused project dependencies
* **breaking** Clarified that Cam3D's field-of-view field is vertical
* **breaking** Brought back Cam3D changes that were accidentally lost when refactoring B+ into the three sub-packages
* Fixed bug with ECS components when inheriting from a parent with type params, that uses those type params in fields

# v0.1.1

* Fix bugs with ECS queries involving UnionAll component types

# v0.1.0

* Initial public release