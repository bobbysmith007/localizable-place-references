# Localizable-place-references

This common lisp library facilitates referencing places.  In object
graphs/trees it is often desirable for one object to reference data in
a slot on a different (eg: parent) object.  This libary creates 
objects that can find the place we are looking for and cache references
to that place.

It also allows any place reference to have a value.  This can be handy
as shown in the following example: A tree of web page fragments
representing a data object. All children should refer to their parent
data object.  Some of these will fragments will be displaying as
editable forms and some as just display.  Generally everything should
inherit its parent editable state unless it has been given its own
state explicitly.  That way when a parent control is set to be
uneditable all of its children automatically reflect that state.

An object that represents a reference to another place

localizable: we can stop referencing the distant place and start using
       a local value
place: the standard CL place: essentially any form, but mostly ones
       that can be in (setf place value)
reference: the standard definition: an object that can retrieve or set
       a value that would not normally be in scope

Example Usages can be found in the test file